"""Trains a 64-32-10 MLP on scikit-learn's 8x8 digits with numpy, quantizes it to int8 for the
accelerator, and writes a test program for the end-to-end simulation.

    python3 train.py [out_dir]

Writes model.npz, program.bin (the bytes to send) and expected.txt (labels and the int32
logits the hardware must return, one image per line) into out_dir.
"""

import sys
from pathlib import Path

import numpy as np
from sklearn.datasets import load_digits
from sklearn.model_selection import train_test_split

import model
import protocol

N = 4
HIDDEN = 32
TEST_IMAGES = 32
INPUT_SCALE = 7  # pixel values 0..16 become 0..112


def train(x, y, epochs=300, lr=0.1, seed=0):
    rng = np.random.default_rng(seed)
    w1 = rng.normal(0, np.sqrt(2 / x.shape[1]), (x.shape[1], HIDDEN))
    b1 = np.zeros(HIDDEN)
    w2 = rng.normal(0, np.sqrt(2 / HIDDEN), (HIDDEN, 10))
    b2 = np.zeros(10)
    onehot = np.eye(10)[y]
    for _ in range(epochs):
        for idx in np.array_split(rng.permutation(len(x)), len(x) // 32):
            xb, tb = x[idx], onehot[idx]
            h = np.maximum(xb @ w1 + b1, 0)
            z = h @ w2 + b2
            p = np.exp(z - z.max(1, keepdims=True))
            p /= p.sum(1, keepdims=True)
            dz = (p - tb) / len(idx)
            dh = (dz @ w2.T) * (h > 0)
            w2 -= lr * h.T @ dz
            b2 -= lr * dz.sum(0)
            w1 -= lr * xb.T @ dh
            b1 -= lr * dh.sum(0)
    return w1, b1, w2, b2


def quantize(w1, b1, w2, b2, x_train):
    """Symmetric int8 weights. The hidden layer's shift is picked so the largest training
    activation lands near 127; the logits stay int32."""
    s_x = 1 / (16 * INPUT_SCALE)
    s_w1 = np.abs(w1).max() / 127
    s_w2 = np.abs(w2).max() / 127
    h_max = np.maximum(x_train @ w1 + b1, 0).max()
    shift1 = int(round(np.log2(h_max / 127 / (s_x * s_w1))))
    s_h = s_x * s_w1 * 2**shift1
    layers = [
        model.Layer(np.round(w1 / s_w1).astype(np.int64), np.round(b1 / (s_x * s_w1)).astype(np.int64), shift1, True),
        model.Layer(np.round(w2 / s_w2).astype(np.int64), np.round(b2 / (s_h * s_w2)).astype(np.int64), 0, False),
    ]
    return [model.pad_layer(l, N) for l in layers]


def to_input(images):
    return (images * INPUT_SCALE).astype(np.int64)


def main(out_dir):
    digits = load_digits()
    x_train, x_test, y_train, y_test = train_test_split(digits.data, digits.target, test_size=0.25, random_state=0)
    w1, b1, w2, b2 = train(x_train / 16, y_train)
    float_acc = (np.argmax(np.maximum(x_test / 16 @ w1 + b1, 0) @ w2 + b2, 1) == y_test).mean()

    layers = quantize(w1, b1, w2, b2, x_train / 16)
    logits = model.forward(to_input(x_test), layers)[:, :10]
    int_acc = (np.argmax(logits, 1) == y_test).mean()
    print(f"test accuracy: float {float_acc:.3f}, int8 {int_acc:.3f} on {len(y_test)} images")

    out = Path(out_dir)
    out.mkdir(parents=True, exist_ok=True)
    np.savez(out / "model.npz", **{f"{k}{i}": getattr(l, k) for i, l in enumerate(layers) for k in ("w", "b", "shift", "relu")})
    x = to_input(x_test[:TEST_IMAGES])
    data, reply_len = protocol.program(layers, x, N)
    (out / "program.bin").write_bytes(data)
    want = model.forward(x, layers)
    with open(out / "expected.txt", "w") as f:
        f.write(f"{len(x)} {want.shape[1]} {reply_len}\n")
        for label, row in zip(y_test[:TEST_IMAGES], want):
            f.write(f"{label} " + " ".join(str(v) for v in row) + "\n")
    print(f"wrote {len(data)} program bytes for {len(x)} images to {out}")


if __name__ == "__main__":
    main(sys.argv[1] if len(sys.argv) > 1 else "../src/test/resources/digits")

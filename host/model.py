from dataclasses import dataclass

import numpy as np


@dataclass
class Layer:
    w: np.ndarray  # (K, N) int8
    b: np.ndarray  # (N,) int32
    shift: int
    relu: bool


def pad_to(a, n, axis):
    extra = (-a.shape[axis]) % n
    if extra == 0:
        return a
    widths = [(0, 0)] * a.ndim
    widths[axis] = (0, extra)
    return np.pad(a, widths)


def pad_layer(layer, n):
    w = pad_to(pad_to(layer.w, n, 0), n, 1)
    b = pad_to(layer.b, n, 0)
    return Layer(w, b, layer.shift, layer.relu)


def post(acc, b, shift, relu):
    half = (1 << shift) >> 1
    v = (acc + b.astype(np.int64) + half) >> shift
    if relu:
        v = np.maximum(v, 0)
    return v


def wrap32(v):
    return ((v + (1 << 31)) % (1 << 32)) - (1 << 31)


def clamp8(v):
    return np.clip(v, -128, 127)


def forward(x, layers):
    a = x.astype(np.int64)
    for i, l in enumerate(layers):
        acc = a @ l.w.astype(np.int64)
        assert np.all(np.abs(acc) < (1 << 31)), "accumulator would overflow int32"
        v = post(acc, l.b, l.shift, l.relu)
        a = wrap32(v) if i == len(layers) - 1 else clamp8(v)
    return a

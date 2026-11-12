from dataclasses import dataclass

import numpy as np

WRITE_WEIGHTS = 0x01
WRITE_BIAS = 0x02
WRITE_ACTS = 0x03
WRITE_LAYER = 0x04
RUN = 0x05
READ_OUT = 0x06
PING = 0x07
RUN_DONE = 0xD0
PONG = 0xA5


def u16(v):
    assert 0 <= v < 1 << 16
    return bytes([(v >> 8) & 0xFF, v & 0xFF])


def weight_entries(w, n):
    k, m = w.shape
    return np.array([w[kt * n + r, nt * n:(nt + 1) * n]
                     for nt in range(m // n) for kt in range(k // n) for r in range(n)])


def act_entries(x, n):
    rows, k = x.shape
    return np.array([x[row, kt * n:(kt + 1) * n] for kt in range(k // n) for row in range(rows)])


def bias_entries(b, n):
    return b.reshape(-1, n)


def write(cmd, addr, entries, dtype):
    data = np.asarray(entries).astype(dtype).tobytes()
    return bytes([cmd]) + u16(addr) + u16(len(entries)) + data


@dataclass
class Placement:
    w_base: int
    b_base: int
    in_base: int
    out_base: int
    to_out: bool


def write_layer(idx, p, m, k_tiles, n_tiles, shift, relu):
    flags = (1 if relu else 0) | (2 if p.to_out else 0)
    return (bytes([WRITE_LAYER, idx]) + u16(p.w_base) + u16(p.b_base) + u16(p.in_base)
            + u16(p.out_base) + u16(m) + bytes([k_tiles, n_tiles, shift, flags]))


def program(layers, x, n, act_split=1024):
    m = x.shape[0]
    out = bytearray()
    w_base = b_base = 0
    for i, l in enumerate(layers):
        last = i == len(layers) - 1
        p = Placement(w_base, b_base,
                      in_base=0 if i % 2 == 0 else act_split,
                      out_base=0 if last else (act_split if i % 2 == 0 else 0),
                      to_out=last)
        we = weight_entries(l.w, n)
        be = bias_entries(l.b, n)
        out += write(WRITE_WEIGHTS, w_base, we, np.int8)
        out += write(WRITE_BIAS, b_base, be, ">i4")
        out += write_layer(i, p, m, l.w.shape[0] // n, l.w.shape[1] // n, l.shift, l.relu)
        w_base += len(we)
        b_base += len(be)
    out += write(WRITE_ACTS, 0, act_entries(x, n), np.int8)
    out += bytes([RUN, len(layers)])
    n_tiles = layers[-1].w.shape[1] // n
    out += bytes([READ_OUT]) + u16(0) + u16(n_tiles * m)
    return bytes(out), 1 + n_tiles * m * n * 4


def decode_out(reply, m, n_tiles, n):
    words = np.frombuffer(reply, dtype=">i4").astype(np.int64)
    entries = words.reshape(n_tiles, m, n)
    return np.concatenate([entries[nt] for nt in range(n_tiles)], axis=1)

import unittest

import numpy as np

import model
import protocol


def naive_layer(x, w, b, shift, relu):
    """Element by element, with Python ints, independent of model.post."""
    out = []
    for row in x:
        vals = []
        for c in range(w.shape[1]):
            acc = sum(int(row[k]) * int(w[k, c]) for k in range(w.shape[0]))
            v = acc + int(b[c]) + ((1 << shift) >> 1)
            v = v >> shift  # Python >> floors, like an arithmetic shift
            if relu and v < 0:
                v = 0
            vals.append(v)
        out.append(vals)
    return np.array(out, dtype=np.int64)


class PostTest(unittest.TestCase):
    def test_rounds_half_up(self):
        acc = np.array([5, 6, 7, -5, -3, -1, 1, 3])
        shifts = [1, 1, 1, 1, 2, 2, 2, 2]
        got = [int(model.post(np.array([a]), np.array([0]), s, False)[0]) for a, s in zip(acc, shifts)]
        self.assertEqual(got, [3, 3, 4, -2, -1, 0, 0, 1])

    def test_shift_zero_adds_bias_only(self):
        got = model.post(np.array([10, -10]), np.array([1, 2]), 0, False)
        self.assertEqual(got.tolist(), [11, -8])

    def test_relu(self):
        got = model.post(np.array([-1, 0, 1]), np.array([0, 0, 0]), 0, True)
        self.assertEqual(got.tolist(), [0, 0, 1])

    def test_clamp_and_wrap(self):
        self.assertEqual(model.clamp8(np.array([127, 128, -128, -129])).tolist(), [127, 127, -128, -128])
        self.assertEqual(model.wrap32(np.array([2**31, -2**31 - 1, 5])).tolist(), [-2**31, 2**31 - 1, 5])


class ForwardTest(unittest.TestCase):
    def setUp(self):
        self.rng = np.random.default_rng(0)

    def rand_layer(self, k, n, shift, relu):
        return model.Layer(self.rng.integers(-128, 128, (k, n)), self.rng.integers(-5000, 5000, n), shift, relu)

    def test_matches_naive_two_layers(self):
        x = self.rng.integers(-128, 128, (7, 12))
        l1 = self.rand_layer(12, 9, 8, True)
        l2 = self.rand_layer(9, 5, 3, False)
        hidden = np.clip(naive_layer(x, l1.w, l1.b, l1.shift, True), -128, 127)
        want = naive_layer(hidden, l2.w, l2.b, l2.shift, False)
        self.assertEqual(model.forward(x, [l1, l2]).tolist(), want.tolist())

    def test_padding_keeps_outputs(self):
        x = self.rng.integers(-128, 128, (3, 10))
        layers = [self.rand_layer(10, 7, 7, True), self.rand_layer(7, 3, 2, False)]
        padded = [model.pad_layer(l, 4) for l in layers]
        got = model.forward(model.pad_to(x, 4, 1), padded)
        self.assertEqual(got.shape, (3, 4))
        self.assertEqual(got[:, :3].tolist(), model.forward(x, layers).tolist())
        self.assertEqual(got[:, 3].tolist(), [0, 0, 0])

    def test_rejects_accumulator_overflow(self):
        x = np.full((1, 2**17 + 4), -128)
        l = model.Layer(np.full((2**17 + 4, 1), -128), np.zeros(1, dtype=np.int64), 0, False)
        with self.assertRaises(AssertionError):
            model.forward(x, [l])


class LayoutTest(unittest.TestCase):
    def test_weight_tiles_are_nt_outer_kt_inner(self):
        n = 2
        w = np.arange(4 * 6).reshape(4, 6)  # 2 K-tiles, 3 N-tiles
        e = protocol.weight_entries(w, n)
        self.assertEqual(e.shape, (12, 2))
        # tile (kt=1, nt=2) is entry block nt * kTiles + kt = 5; its row 1 is W[3, 4:6]
        self.assertEqual(e[5 * n + 1].tolist(), w[3, 4:6].tolist())

    def test_act_rows_are_kt_major(self):
        x = np.arange(3 * 4).reshape(3, 4)
        e = protocol.act_entries(x, 2)
        self.assertEqual(e[1 * 3 + 2].tolist(), x[2, 2:4].tolist())

    def test_decode_out_inverts_the_output_layout(self):
        m, n_tiles, n = 3, 2, 4
        want = np.arange(m * n_tiles * n).reshape(m, n_tiles * n) - 10
        entries = [want[row, nt * n:(nt + 1) * n] for nt in range(n_tiles) for row in range(m)]
        reply = np.array(entries).astype(">i4").tobytes()
        self.assertEqual(protocol.decode_out(reply, m, n_tiles, n).tolist(), want.tolist())


class ProgramTest(unittest.TestCase):
    def test_bytes_of_a_single_layer(self):
        n = 4
        l = model.Layer(np.eye(4, dtype=np.int64), np.array([-2, -2, -2, -2]), 0, False)
        x = np.array([[1, 2, 3, 4]])
        data, reply = protocol.program([l], x, n)
        self.assertEqual(reply, 1 + 16)
        # weights: cmd, addr 0, 4 entries, 16 bytes of the identity
        self.assertEqual(data[:5], bytes([0x01, 0, 0, 0, 4]))
        self.assertEqual(data[5:21], np.eye(4, dtype=np.int8).tobytes())
        # bias: one entry of four big-endian int32
        self.assertEqual(data[21:26], bytes([0x02, 0, 0, 0, 1]))
        self.assertEqual(data[26:30], bytes([0xFF, 0xFF, 0xFF, 0xFE]))
        # layer 0: bases 0, m 1, 1 x 1 tiles, shift 0, flags toOut
        self.assertEqual(data[42:58], bytes([0x04, 0]) + bytes(8) + bytes([0, 1, 1, 1, 0, 2]))
        self.assertEqual(data[-7:], bytes([0x05, 1, 0x06, 0, 0, 0, 1]))


if __name__ == "__main__":
    unittest.main()

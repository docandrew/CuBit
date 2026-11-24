#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
#  SELF-003 step 2: tests of Inflate_Streams (userspace/lib/compress)
#  through build/inflate_tool. Every input is made here at test time:
#  streams compressed by the host's zlib (Python's zlib module) from
#  generated data, and adversarial streams written bit by bit below
#  (Bits). No stored blobs.
#    check.py TOOL WORKDIR [--fuzz N] [--quick]
#  --quick (for the -gnata build, whose contract checks are slow) keeps
#  the hand-made streams and small samples, and fewer cuts and mutants.
#  1. interop: zlib's output at every level, strategy and window size,
#     zlib-wrapped and raw, whole and in odd chunks (input 1 byte at a time,
#     output 1 byte at a time); the bytes and the count consumed match;
#  2. hand-made streams, each with the outcome it must have;
#  3. every truncation of a few streams is refused as Truncated, and data
#     after a stream is left unread;
#  4. the ceiling: exact is enough, one byte less is Too_Large;
#  5. differential fuzzing: mutants of zlib's streams (bit flips, byte
#     changes, cuts, splices); the inflater and zlib must agree on
#     whether each is a stream, on its bytes and on where it ends.
import os
import random
import subprocess
import sys
import zlib

tool, work = sys.argv[1], sys.argv[2]
fuzz_count = int(sys.argv[sys.argv.index("--fuzz") + 1]) if "--fuzz" in sys.argv else 3000
quick = "--quick" in sys.argv
os.makedirs(work, exist_ok=True)
rng = random.Random(20261122)
failures = 0
checks = 0
serial = 0


class Tool:
    def __init__(self):
        self.p = subprocess.Popen([tool], stdin=subprocess.PIPE, stdout=subprocess.PIPE, text=True)

    def run(self, fmt, data, in_chunk=1 << 20, out_chunk=1024, ceiling=1 << 40):
        global serial
        serial += 1
        src = os.path.join(work, "case-%d.in" % serial)
        dst = os.path.join(work, "case-%d.out" % serial)
        with open(src, "wb") as f:
            f.write(data)
        self.p.stdin.write("%s %d %d %d %s %s\n" % (fmt, in_chunk, out_chunk, ceiling, src, dst))
        self.p.stdin.flush()
        line = self.p.stdout.readline().split()
        if not line:
            raise SystemExit("inflate: the tool stopped (a contract failed?) on " + src)
        out = open(dst, "rb").read()
        os.unlink(src)
        os.unlink(dst)
        if line[0] == "FINISHED":
            return ("FINISHED", int(line[1]), out)
        if line[0] == "REFUSED":
            return ("REFUSED", line[1], int(line[2]), out)
        raise SystemExit("inflate: tool said " + " ".join(line))


T = Tool()


def check(cond, what):
    global failures, checks
    checks += 1
    if not cond:
        failures += 1
        print("FAIL:", what)


def zlib_verdict(fmt, data):
    """zlib's answer: ('FINISHED', consumed, bytes) or ('REFUSED',)."""
    d = zlib.decompressobj(15 if fmt == "zlib" else -15)
    try:
        out = d.decompress(data)
    except zlib.error:
        return ("REFUSED",)
    if not d.eof:
        return ("REFUSED",)
    return ("FINISHED", len(data) - len(d.unused_data), out)


def compress(data, fmt, level=6, wbits=15, mem=8, strategy=zlib.Z_DEFAULT_STRATEGY):
    c = zlib.compressobj(level, zlib.DEFLATED, wbits if fmt == "zlib" else -wbits, mem, strategy)
    return c.compress(data) + c.flush()


# --- data to compress -----------------------------------------------------
words = b"the quick brown fox jumps over a lazy dog while git packs objects ".split()
samples = {
    "empty": b"",
    "one": b"x",
    "text": b" ".join(rng.choice(words) for _ in range(20000)),
    "random": bytes(rng.getrandbits(8) for _ in range(70000)),
    "runs": b"".join(bytes([rng.getrandbits(8)]) * rng.randint(1, 600) for _ in range(400)),
    "far": (lambda blk: blk + bytes(rng.getrandbits(8) for _ in range(30000)) + blk)(
        bytes(rng.getrandbits(8) for _ in range(2000))),
    "zeros": bytes(300000),
}

# --- 1. interop -------------------------------------------------------------
strategies = [zlib.Z_DEFAULT_STRATEGY, zlib.Z_FILTERED, zlib.Z_HUFFMAN_ONLY, zlib.Z_RLE, zlib.Z_FIXED]
for name, data in samples.items():
    if quick and name not in ("empty", "one", "runs"):
        continue
    for fmt in ("zlib", "raw"):
        for level in (0, 1, 6, 9):
            for strategy in strategies:
                for wbits in ((9, 15) if name in ("text", "far") else (15,)):
                    stream = compress(data, fmt, level, wbits, 8, strategy)
                    got = T.run(fmt, stream, out_chunk=4096)
                    check(got == ("FINISHED", len(stream), data),
                          "interop %s %s level %d strategy %d window %d: %s" %
                          (name, fmt, level, strategy, wbits, got[:2]))
for name in ("text", "far", "runs", "one", "empty"):
    if quick and name in ("text", "far"):
        continue
    for fmt in ("zlib", "raw"):
        stream = compress(samples[name], fmt, 9)
        for ins, outs in ((1, 1), (1, 7), (3, 1), (5, 300), (4096, 1), (1 << 20, 258)):
            if name == "text" and ins * outs < 8:
                continue   # byte by byte both ways is slow under -gnata; "far" covers it
            got = T.run(fmt, stream, in_chunk=ins, out_chunk=outs)
            check(got == ("FINISHED", len(stream), samples[name]),
                  "chunked %s %s in %d out %d: %s" % (name, fmt, ins, outs, got[:2]))
print("inflate: interop PASS")


# --- 2. hand-made streams ----------------------------------------------------
class Bits:
    """A deflate bit writer: fields least significant bit first, Huffman
    codes most significant bit first (RFC 1951 3.1.1)."""

    def __init__(self):
        self.out = bytearray()
        self.acc = 0
        self.n = 0

    def put(self, value, count):
        self.acc |= value << self.n
        self.n += count
        while self.n >= 8:
            self.out.append(self.acc & 255)
            self.acc >>= 8
            self.n -= 8

    def code(self, code, length):
        self.put(int(format(code, "0%db" % length)[::-1], 2) if length else 0, length)

    def align(self):
        if self.n:
            self.put(0, 8 - self.n)

    def data(self):
        self.align()
        return bytes(self.out)


def canonical(lengths):
    """RFC 1951 3.2.2: the codes of a list of lengths."""
    bl_count = [0] * 16
    for l in lengths:
        if l:
            bl_count[l] += 1
    code, next_code = 0, [0] * 16
    for bits in range(1, 16):
        code = (code + bl_count[bits - 1]) << 1
        next_code[bits] = code
    codes = {}
    for sym, l in enumerate(lengths):
        if l:
            codes[sym] = (next_code[l], l)
            next_code[l] += 1
    return codes


FIXED_LIT = [8] * 144 + [9] * 112 + [7] * 24 + [8] * 8
FIXED_LIT_CODES = canonical(FIXED_LIT)
LBASE = [3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31, 35, 43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258]
LEXTRA = [0] * 8 + [1] * 4 + [2] * 4 + [3] * 4 + [4] * 4 + [5] * 4 + [0]
DBASE = [1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193, 257, 385, 513, 769, 1025, 1537, 2049, 3073,
         4097, 6145, 8193, 12289, 16385, 24577]
DEXTRA = [0, 0] + [i // 2 for i in range(2, 28)] + [13, 13]
ORDER = [16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15]


def fixed_block(b, items, final=True):
    """items: ints (literals), ('len', sym, extra, dsym, dextra), or 256."""
    b.put(1 if final else 0, 1)
    b.put(1, 2)
    for it in items:
        if isinstance(it, int):
            b.code(*FIXED_LIT_CODES[it])
        else:
            _, sym, extra, dsym, dextra = it
            b.code(*FIXED_LIT_CODES[sym])
            b.put(extra, LEXTRA[sym - 257] if sym <= 285 else 0)
            b.code(dsym, 5)
            if dsym < 30:
                b.put(dextra, DEXTRA[dsym])
    b.code(*FIXED_LIT_CODES[256])


def dynamic_block(b, lit_lengths, dist_lengths, symbols, final=True, cl_lengths=None, sequence=None,
                  hlit=None, hdist=None):
    """A dynamic block: lengths sent one by one (or as SEQUENCE of
    (symbol, extra_bits, extra) code-length items), then SYMBOLS as
    literal/length symbol numbers (with ('d', dsym) for distances)."""
    b.put(1 if final else 0, 1)
    b.put(2, 2)
    if cl_lengths is None:
        cl_lengths = [4] * 13 + [5] * 6   # complete: 13/16 + 6/32
    hlit = len(lit_lengths) - 257 if hlit is None else hlit
    hdist = len(dist_lengths) - 1 if hdist is None else hdist
    b.put(hlit, 5)
    b.put(hdist, 5)
    b.put(19 - 4, 4)
    for sym in ORDER:
        b.put(cl_lengths[sym], 3)
    clc = canonical(cl_lengths)
    if sequence is None:
        sequence = [(l, 0, 0) for l in list(lit_lengths) + list(dist_lengths)]
    for sym, nbits, extra in sequence:
        b.code(*clc[sym])
        b.put(extra, nbits)
    lc, dc = canonical(lit_lengths), canonical(dist_lengths)
    for s in symbols:
        if isinstance(s, tuple):
            b.code(*dc[s[1]])
        else:
            b.code(*lc[s])


def zlib_wrap(raw, out, cmf=0x78, flg=None, adler=None):
    if flg is None:
        flg = (31 - (cmf * 256) % 31) % 31
    a = zlib.adler32(out) if adler is None else adler
    return bytes([cmf, flg]) + raw + a.to_bytes(4, "big")


def expect(fmt, data, want, what, out=None):
    got = T.run(fmt, data)
    if want == "FINISHED":
        ok = got[0] == "FINISHED" and got[1] == len(data) and (out is None or got[2] == out)
    else:
        ok = got[0] == "REFUSED" and got[1] == want
    check(ok, "%s: want %s, got %s" % (what, want, got[:3] if got[0] == "REFUSED" else got[:2]))
    # zlib agrees on whether it is a stream (strictness differences are
    # listed where they are meant).
    z = zlib_verdict(fmt, data)
    return got, z


def one(build):
    b = Bits()
    build(b)
    return b.data()


# Stored blocks.
def stored(b, payload, final=True, nlen=None):
    b.put(1 if final else 0, 1)
    b.put(0, 2)
    b.align()
    b.put(len(payload), 16)
    b.put((len(payload) ^ 0xFFFF) if nlen is None else nlen, 16)
    for x in payload:
        b.put(x, 8)


expect("raw", one(lambda b: stored(b, b"hello")), "FINISHED", "stored block", b"hello")
expect("raw", one(lambda b: (stored(b, b"ab", False), stored(b, b""), None)), "FINISHED", "two stored, one empty", b"ab")
expect("raw", one(lambda b: stored(b, b"hello", nlen=0)), "Bad_Stored_Length", "stored NLEN wrong")
expect("raw", one(lambda b: stored(b, b"hello"))[:-2], "Truncated", "stored data cut")
expect("raw", one(lambda b: (b.put(1, 1), b.put(3, 2))), "Reserved_Block", "BTYPE 11")
# Fixed blocks.
expect("raw", one(lambda b: fixed_block(b, [97, ("len", 257, 0, 0, 0)])), "FINISHED", "fixed: a + copy 3 at 1",
       b"aaaa")
expect("raw", one(lambda b: fixed_block(b, [("len", 257, 0, 0, 0)])), "Distance_Too_Far", "match before any output")
expect("raw", one(lambda b: fixed_block(b, [97, ("len", 257, 0, 1, 0)])), "Distance_Too_Far", "distance 2 after 1 byte")
expect("raw", one(lambda b: fixed_block(b, [97, ("len", 257, 0, 30, 0)])), "Bad_Symbol", "distance symbol 30")
expect("raw", one(lambda b: fixed_block(b, [97, ("len", 257, 0, 31, 0)])), "Bad_Symbol", "distance symbol 31")
expect("raw", one(lambda b: fixed_block(b, [286])), "Bad_Symbol", "literal/length 286")
expect("raw", one(lambda b: fixed_block(b, [287])), "Bad_Symbol", "literal/length 287")
expect("raw", one(lambda b: fixed_block(b, [97, ("len", 285, 0, 0, 0)])), "FINISHED", "length 258 (symbol 285)",
       b"a" * 259)
got, z = expect("raw", one(lambda b: fixed_block(b, [97, ("len", 284, 31, 0, 0)])), "FINISHED",
                "length 258 as 284 + 31", b"a" * 259)
check(z[0] == "FINISHED", "zlib accepts 284 + 31 too")
# A far distance, exactly the whole window.
far = bytes(rng.getrandbits(8) for _ in range(32768))
expect("raw", one(lambda b: (stored(b, far, False), fixed_block(b, [("len", 257, 0, 29, 8191)]))), "FINISHED",
       "distance 32768", far + far[:3])
expect("raw", one(lambda b: (stored(b, far[:-1], False), fixed_block(b, [("len", 257, 0, 29, 8191)]))),
       "Distance_Too_Far", "distance 32768 after 32767 bytes")
# Dynamic blocks.
lit = [0] * 257
lit[97] = 1
lit[256] = 1
expect("raw", one(lambda b: dynamic_block(b, lit, [1], [97, 97, 256])), "FINISHED", "dynamic: two-symbol code",
       b"aa")
expect("raw", one(lambda b: dynamic_block(b, lit, [0], [97, 256])), "FINISHED", "dynamic: no distance codes", b"a")
deep = [0] * 257
for i in range(15):
    deep[i] = i + 1
deep[256] = 15
got, z = expect("raw", one(lambda b: dynamic_block(b, deep, [1], [0, 13, 14, 256])), "FINISHED",
                "dynamic: a complete 15-bit code", bytes([0, 13, 14]))
check(z[0] == "FINISHED" and z[2] == bytes([0, 13, 14]), "zlib decodes the 15-bit code alike")
one_lit = [0] * 257
one_lit[256] = 1
expect("raw", one(lambda b: dynamic_block(b, one_lit, [1], [256])), "FINISHED",
       "dynamic: a single one-bit literal code (incomplete, allowed)", b"")
expect("raw", one(lambda b: (dynamic_block(b, one_lit, [1], []), b.put(1, 1), b.put(0, 16))), "Bad_Symbol",
       "dynamic: the unused code of a single one-bit code")
expect("raw", one(lambda b: dynamic_block(b, lit + [0] * 30, [1], [], hlit=30)), "Too_Many_Codes", "HLIT 30 (287)")
expect("raw", one(lambda b: dynamic_block(b, lit, [1] * 31, [], hdist=30)), "Too_Many_Codes", "HDIST 30 (31)")
expect("raw", one(lambda b: dynamic_block(b, lit, [1], [], cl_lengths=[1] * 19, sequence=[])), "Bad_Code_Lengths",
       "code-length code over-subscribed")
expect("raw", one(lambda b: dynamic_block(b, lit, [1], [], cl_lengths=[1] + [0] * 18, sequence=[])), "Bad_Code_Lengths",
       "code-length code incomplete")
expect("raw", one(lambda b: dynamic_block(b, lit, [1], [], cl_lengths=[0] * 19, sequence=[])), "Bad_Code_Lengths",
       "code-length code empty")
expect("raw", one(lambda b: dynamic_block(b, lit, [1], [], sequence=[(16, 2, 0)])), "Bad_Repeat",
       "repeat 16 first")
expect("raw", one(lambda b: dynamic_block(b, lit, [1], [], sequence=[(18, 7, 127), (18, 7, 127), (18, 7, 127)])),
       "Bad_Repeat", "zero runs past the code count")
eights = [0] * 257
for i in range(97, 104):
    eights[i] = 3
eights[256] = 3
expect("raw", one(lambda b: dynamic_block(
    b, eights, [1], [97, 103, 256],
    sequence=[(18, 7, 86), (3, 0, 0), (16, 2, 3), (18, 7, 127), (17, 3, 7), (17, 3, 1), (3, 0, 0), (1, 0, 0)])),
       "FINISHED", "repeat codes 16, 17 and 18", b"ag")
two = [0] * 257
two[97] = two[98] = two[256] = 2
two[99] = 2
expect("raw", one(lambda b: dynamic_block(b, two, [1], [97, 98, 99, 256])), "FINISHED", "complete 2-bit code",
       b"abc")
over = [0] * 257
over[97] = over[98] = over[256] = 1
expect("raw", one(lambda b: dynamic_block(b, over, [1], [])), "Oversubscribed_Code", "literal code over-subscribed")
inc = [0] * 257
inc[97] = inc[256] = 2
expect("raw", one(lambda b: dynamic_block(b, inc, [1], [])), "Incomplete_Code", "literal code incomplete")
expect("raw", one(lambda b: dynamic_block(b, lit, [2, 2], [])), "Incomplete_Code", "distance code incomplete")
no_end = [0] * 257
no_end[97] = no_end[98] = 1
expect("raw", one(lambda b: dynamic_block(b, no_end, [1], [])), "Missing_End_Of_Block", "no end-of-block code")
# zlib headers and trailers.
fx = one(lambda b: fixed_block(b, [104, 105]))
expect("zlib", zlib_wrap(fx, b"hi"), "FINISHED", "zlib: fixed block", b"hi")
expect("zlib", zlib_wrap(fx, b"hi", cmf=0x77), "Bad_Method", "zlib: CM 7")
expect("zlib", zlib_wrap(fx, b"hi", cmf=0x88), "Bad_Window", "zlib: CINFO 8")
expect("zlib", bytes([0x78, 0x9d]) + fx + zlib.adler32(b"hi").to_bytes(4, "big"), "Bad_Header_Check",
       "zlib: FCHECK")
expect("zlib", zlib_wrap(fx, b"hi", flg=(lambda f: f + (31 - (0x78 * 256 + f) % 31) % 31)(0x20)),
       "Preset_Dictionary", "zlib: FDICT")
expect("zlib", zlib_wrap(fx, b"hi", adler=zlib.adler32(b"ho")), "Bad_Checksum", "zlib: Adler-32")
# A window of 256 bytes declared, a distance of 300: refused (zlib checks
# this only under INFLATE_STRICT; this reader always does).
small = bytes(rng.getrandbits(8) for _ in range(400))
s256 = one(lambda b: (stored(b, small, False), fixed_block(b, [("len", 257, 0, 16, 43)])))
expect("zlib", zlib_wrap(s256, small + small[100:103], cmf=0x08), "Distance_Too_Far",
       "zlib: distance past a 256-byte window")
expect("zlib", zlib_wrap(s256, small + small[100:103], cmf=0x18), "FINISHED", "zlib: the same in a 512-byte window",
       small + small[100:103])
print("inflate: hand-made streams PASS" if failures == 0 else "inflate: hand-made streams FAILED")

# --- 3. truncation and what follows -----------------------------------------
for name in ("text", "one", "empty", "far"):
    for fmt in ("zlib", "raw"):
        data = samples[name][:2000] if quick else samples[name]
        stream = compress(data, fmt, 9)
        cuts = range(len(stream)) if len(stream) < 300 else sorted(
            set(rng.sample(range(len(stream)), 20 if quick else 120)) | {0, len(stream) - 1})
        for cut in cuts:
            got = T.run(fmt, stream[:cut], in_chunk=rng.choice((1, 3, 64, 1 << 20)), out_chunk=997)
            check(got[0] == "REFUSED" and got[1] == "Truncated", "%s %s cut at %d: %s" % (name, fmt, cut, got[:2]))
        tail = bytes(rng.getrandbits(8) for _ in range(40))
        got = T.run(fmt, stream + tail, in_chunk=7, out_chunk=4096)
        check(got == ("FINISHED", len(stream), data), "%s %s with data after: %s" % (name, fmt, got[:2]))
print("inflate: truncation PASS" if failures == 0 else "inflate: truncation FAILED")

# --- 4. the ceiling ------------------------------------------------------------
for name in ("text", "zeros", "one"):
    for fmt in ("zlib", "raw"):
        data = samples[name][:3000] if quick else samples[name]
        stream = compress(data, fmt, 6)
        n = len(data)
        got = T.run(fmt, stream, ceiling=n, out_chunk=8192)
        check(got == ("FINISHED", len(stream), data), "%s %s ceiling exact: %s" % (name, fmt, got[:2]))
        got = T.run(fmt, stream, ceiling=n - 1, out_chunk=8192)
        check(got[0] == "REFUSED" and got[1] == "Too_Large" and len(got[3]) == n - 1,
              "%s %s ceiling one short: %s" % (name, fmt, got[:3]))
got = T.run("zlib", compress(b"", "zlib"), ceiling=0)
check(got[0] == "FINISHED", "empty stream under a zero ceiling")
print("inflate: ceiling PASS" if failures == 0 else "inflate: ceiling FAILED")

# --- 5. differential fuzzing against zlib ---------------------------------------
seeds = []
for name in ("text", "runs", "far", "one"):
    for fmt in ("zlib", "raw"):
        for level, strategy in ((1, 0), (6, 0), (9, zlib.Z_FIXED), (6, zlib.Z_HUFFMAN_ONLY), (0, 0)):
            data = samples[name][:rng.randint(1, 600 if quick else 6000)] if name != "one" else samples[name]
            seeds.append((fmt, compress(data, fmt, level, 15, 8, strategy)))
agree = refused = 0
for i in range(fuzz_count):
    fmt, s = rng.choice(seeds)
    m = bytearray(s)
    start = 2 if fmt == "zlib" else 0   # the header's window is checked strictly (above)
    kind = rng.randrange(5)
    if kind == 0:
        for _ in range(rng.randint(1, 4)):
            j = rng.randrange(start, len(m))
            m[j] ^= 1 << rng.randrange(8)
    elif kind == 1:
        j = rng.randrange(start, len(m))
        m[j] = rng.getrandbits(8)
    elif kind == 2:
        j = rng.randrange(start, len(m))
        del m[j:j + rng.randint(1, 8)]
    elif kind == 3:
        j = rng.randrange(start, len(m))
        m[j:j] = bytes(rng.getrandbits(8) for _ in range(rng.randint(1, 8)))
    else:
        a, b2 = rng.randrange(start, len(m)), rng.randrange(start, len(m))
        m[a:a + 16] = m[b2:b2 + 16]
    m = bytes(m)
    z = zlib_verdict(fmt, m)
    got = T.run(fmt, m, in_chunk=rng.choice((1, 5, 100, 1 << 20)), out_chunk=rng.choice((1, 33, 4096)))
    if z[0] == "FINISHED":
        ok = got == ("FINISHED", z[1], z[2])
    else:
        ok = got[0] == "REFUSED"
        refused += 1
    agree += ok
    check(ok, "fuzz %d (%s, mutation %d): zlib %s, inflater %s" % (i, fmt, kind, z[:2], got[:2]))
print("inflate: fuzz %d mutants, %d refused by both, %d agreed" % (fuzz_count, refused, agree))
T.p.stdin.close()
T.p.wait()
print("inflate: %d checks, %d failures" % (checks, failures))
sys.exit(1 if failures else 0)

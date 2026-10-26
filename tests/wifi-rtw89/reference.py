#!/usr/bin/env python3
"""Independent reference implementations for rtw89.drv's differential tests.

Written from the specifications (IEEE Std 802.11-2020 9.2-9.4 for frames;
rtw89 pci.h/txrx.h/core.c for RX buffers; rtw89 phy.c rtw89_phy_init_reg
for the conditional tables), not from the Ada code. run.sh feeds the same
inputs to both and requires identical results line by line.

  reference.py corpus <out.hex> [pcap ...]   seeds (constructed frames and
        beacons/probe responses from pcap files) plus deterministic
        hostile mutations, one hex MPDU (no FCS) per line
  reference.py frames <in.hex> <out.txt>     decode as IEEE80211_Beacons does
  reference.py rxcorpus <out.hex>            RX buffers, valid and hostile
  reference.py rx <in.hex> <out.txt>         decode as RTW89_RX does
  reference.py tables <source> <out.txt>     apply the five 8852B tables for
        RFE 1 / cut 1; <source> is rtw8852b_table.c or the generated
        rtw89_tables_8852b.ads
  reference.py compare <a> <b>               identical, else list differences
"""
import random
import re
import struct
import sys
import zlib

MAX_FRAME = 4096


# --------------------------------------------------------------------------
# 802.11 beacon / probe response
# --------------------------------------------------------------------------
def decode_frame(f):
    if len(f) < 36:
        return 'too-short'
    fc0, fc1 = f[0], f[1]
    if fc0 & 0x03:
        return 'not-version-0'
    if fc0 & 0x0C != 0:
        return 'not-management'
    stype = fc0 >> 4
    if stype not in (5, 8):
        return 'not-beacon'
    if fc1 & 0x40:
        return 'protected'
    body = 24 + (4 if fc1 & 0x80 else 0)
    if len(f) < body + 12:
        return 'too-short'
    interval = f[body + 8] | f[body + 9] << 8
    cap = f[body + 10] | f[body + 11] << 8
    ssid = None
    ds = 0
    ht = 0
    rsn = False
    elements = 0
    skipped = 0
    pos = body + 12
    while pos < len(f):
        if len(f) - pos < 2:
            return 'element-overrun'
        eid, elen = f[pos], f[pos + 1]
        if elen > len(f) - pos - 2:
            return 'element-overrun'
        data = f[pos + 2:pos + 2 + elen]
        elements += 1
        taken = False
        if eid == 0:
            if elen > 32:
                return 'ssid-too-long'
            if ssid is None:
                ssid = bytes(data)
                taken = True
        elif eid == 3:
            if elen != 1:
                return 'ds-params-malformed'
            if ds == 0:
                ds = data[0]
                taken = True
        elif eid == 61:
            if elen >= 22 and ht == 0:
                ht = data[0]
                taken = True
        elif eid == 48:
            if elen >= 2 and not rsn:
                rsn = True
                taken = True
        if not taken:
            skipped += 1
        pos += 2 + elen
    kind = 'beacon' if stype == 8 else 'probe'
    return ('accepted kind=%s bssid=%s ssid=%s ds=%d ht=%d cap=%d int=%d rsn=%d elements=%d skipped=%d'
            % (kind, f[16:22].hex(), 'none' if ssid is None else '[' + ssid.hex() + ']', ds, ht, cap,
               interval, 1 if rsn else 0, elements, skipped))


def element(eid, data):
    return bytes([eid, len(data)]) + bytes(data)


def constructed_frames():
    hdr = bytes([0x80, 0, 0, 0]) + b'\xff' * 6 + bytes.fromhex('021122334455') * 2 + b'\x10\x00'
    fixed = bytes(range(1, 9)) + b'\x64\x00\x31\x04'
    rsn = bytes([1, 0, 0, 0x0F, 0xAC, 4, 1, 0, 0, 0x0F, 0xAC, 4, 1, 0, 0, 0x0F, 0xAC, 2, 0x8C, 0])
    ht_op = bytes([44]) + bytes(21)
    seeds = [
        hdr + fixed + element(0, b'home') + element(1, b'\x82\x84\x8b\x96') + element(3, b'\x06')
        + element(48, rsn) + element(221, b'\x00\x50\xf2\x02\x01\x01\x00'),
        bytes([0x50]) + hdr[1:] + fixed + element(0, b'') + element(61, ht_op) + element(48, rsn),
        hdr + fixed + element(0, b'caf\xc3\xa9 \\ net') + element(3, b'\x0b') + element(7, b'US \x01\x0b\x1e'),
        hdr + fixed,
        hdr[:1] + b'\x80' + hdr[2:] + b'\x00\x00\x00\x00' + fixed + element(0, b'ht-control'),
        hdr + fixed + element(0, b'x' * 32) + element(3, b'\x01') + element(0, b'dup'),
    ]
    return seeds


def pcap_frames(path):
    """Beacons and probe responses from a pcap (linktype 105 or 127)."""
    data = open(path, 'rb').read()
    if len(data) < 24:
        return []
    magic = struct.unpack('<I', data[:4])[0]
    if magic != 0xA1B2C3D4:
        return []
    linktype = struct.unpack('<I', data[20:24])[0]
    out = []
    pos = 24
    while pos + 16 <= len(data):
        _, _, incl, _ = struct.unpack('<IIII', data[pos:pos + 16])
        pkt = data[pos + 16:pos + 16 + incl]
        pos += 16 + incl
        if linktype == 127:
            if len(pkt) < 4:
                continue
            rlen = pkt[2] | pkt[3] << 8
            pkt = pkt[rlen:]
        elif linktype != 105:
            continue
        if len(pkt) < 28:
            continue
        # Drop a trailing FCS when it checks (radiotap may or may not say so).
        if zlib.crc32(pkt[:-4]) & 0xFFFFFFFF == struct.unpack('<I', pkt[-4:])[0]:
            pkt = pkt[:-4]
        if pkt[0] in (0x80, 0x50):
            out.append(pkt)
    # One of each distinct frame is enough: sequence number and timestamp
    # (octets 22..23 and 24..31) differ in every beacon.
    seen = set()
    unique = []
    for p in out:
        key = p[:22] + p[32:]
        if key not in seen:
            seen.add(key)
            unique.append(p)
    return unique


def mutate(rng, f):
    f = bytearray(f)
    choice = rng.randrange(8)
    if choice == 0 and len(f) > 0:                       # truncate
        del f[rng.randrange(len(f)):]
    elif choice == 1 and len(f) > 0:                     # flip bytes
        for _ in range(rng.randrange(1, 6)):
            f[rng.randrange(len(f))] ^= 1 << rng.randrange(8)
    elif choice == 2 and len(f) > 37:                    # corrupt an element length
        f[rng.randrange(37, len(f))] = rng.choice([0, 1, 2, 21, 22, 32, 33, 254, 255])
    elif choice == 3:                                    # append junk
        f += bytes(rng.randrange(256) for _ in range(rng.randrange(1, 40)))
    elif choice == 4 and len(f) > 2:                     # frame control games
        f[0] = rng.choice([0x80, 0x50, 0x40, 0x88, 0x81, 0x84, 0xd4])
        f[1] = rng.choice([0, 0x40, 0x80, 0xC0, 0x08])
    elif choice == 5:                                    # oversized SSID
        f += element(0, bytes(33))[:2] + bytes(33)
    elif choice == 6:                                    # wrong DS length
        f += bytes([3, rng.choice([0, 2, 3]), 6, 6, 6])
    else:                                                # random bytes after header
        f = f[:36] + bytearray(rng.randrange(256) for _ in range(rng.randrange(0, 200)))
    return bytes(f[:MAX_FRAME])


def make_corpus(out, pcaps):
    rng = random.Random(8852)
    seeds = constructed_frames()
    for p in pcaps:
        seeds += pcap_frames(p)
    lines = []
    for s in seeds:
        lines.append(s.hex())
        for _ in range(1000):
            m = s
            for _ in range(rng.randrange(1, 4)):
                m = mutate(rng, m)
            lines.append(m.hex())
    for _ in range(2000):                                # pure noise
        lines.append(bytes(rng.randrange(256) for _ in range(rng.randrange(0, 300))).hex())
    with open(out, 'w') as fh:
        fh.write('\n'.join(lines) + '\n')
    print('corpus: %d seeds (%d from pcap), %d frames' % (len(seeds), len(seeds) - len(constructed_frames()),
                                                         len(lines)))


# --------------------------------------------------------------------------
# RX buffers (rtw89 AX, PCIe)
# --------------------------------------------------------------------------
IE_LEN = [16, 32, 24, 24, 8, 8, 8, 8, 0, 8, 0, 176, 0, 0, 0, 0,
          0, 0, 16, 24, 0, 0, 0, 0, 24, 24, 24, 24, 32, 32, 32, 32]
IE_VAR = {8, 10, 12, 13, 14, 15, 16, 17, 20, 21, 22}


def u32(b, at):
    return struct.unpack_from('<I', b, at)[0]


def decode_rx(b):
    b = b.ljust(4096, b'\x00')[:4096]
    info = u32(b, 0)
    size = info & 0x3FFF
    if size > 4096:
        return 'size-too-large'
    if not info & 0x8000:
        return 'not-first-segment'
    if not info & 0x4000:
        return 'multi-buffer'
    if size < 20:
        return 'size-too-small'
    w0, w1, w3 = u32(b, 4), u32(b, 8), u32(b, 16)
    long_rxd = bool(w0 & 0x80000000)
    dlen = 32 if long_rxd else 16
    if size < 4 + dlen:
        return 'size-too-small'
    length = w0 & 0x3FFF
    shift = (w0 >> 14) & 3
    kind = (w0 >> 24) & 0xF
    if shift:
        return 'shift-nonzero'
    offset = 4 + dlen + ((w0 >> 28) & 7) * 8
    if offset > size or length > size - offset:
        return 'payload-overrun'
    first = offset if offset != 4096 else 0
    if offset == 4096:
        length = 0
    ppdu = (w1 >> 4) & 7
    rate = (w1 >> 16) & 0x1FF
    a1 = w3 & 1
    out = 'decoded type=%d first=%d length=%d ppdu=%d rate=%d crc=%d a1=%d' % (
        kind, first, length, ppdu, rate, (w3 >> 9) & 1, a1)
    if kind == 1:
        out += ' ppdu_status=' + decode_ppdu(b, first, length, bool(w0 & 0x00800000), a1, rate)
    elif kind == 10:
        if length < 8:
            out += ' c2h=invalid'
        else:
            c0, c1 = u32(b, first), u32(b, first + 4)
            clen = c1 & 0x3FFF
            out += ' c2h=' + ('invalid' if clen < 8 or clen > length
                              else '%d/%d' % ((c0 >> 2) & 0x3F, (c0 >> 8) & 0xFF))
    return out


def decode_ppdu(b, first, length, mac_info, a1, rate):
    last = first + length
    phy = first
    if mac_info:
        if length < 8:
            return 'mac-info-short'
        m0, m1 = u32(b, first), u32(b, first + 4)
        users = m0 & 0xF
        if users > 4:
            return 'too-many-users'
        block = 8 + 4 * users + (4 if users % 2 else 0) + (96 if m0 & (1 << 29) else 0) + ((m1 >> 16) & 0xFF) * 8
        if block > length:
            return 'mac-info-short'
        phy = first + block
    plen = last - phy
    if plen < 8:
        return 'phy-too-short'
    h0, h1 = u32(b, phy), u32(b, phy + 4)
    if not h0 & 0x80:
        return 'phy-not-valid'
    if ((h0 >> 8) & 0xFF) * 8 != plen:
        return 'phy-length-mismatch'
    cls = h0 & 0x1F
    walked = 0
    ch_idx = 0
    if cls >= 11 and not h0 & 0x20:
        pos = phy + 8
        ok = True
        while pos < last:
            if last - pos < 4:
                ok = False
                break
            ie = u32(b, pos)
            t = ie & 0x1F
            ln = ((ie >> 5) & 0x7F) * 8 if t in IE_VAR else IE_LEN[t]
            if ln == 0 or ln > last - pos:
                ok = False
                break
            if ln >= 16 and t == 1:
                ch_idx = (ie >> 16) & 0xFF
            pos += ln
        walked = 1 if ok else 0
    return 'taken class=%d rssi_a=%d rssi_b=%d walked=%d ch_idx=%d' % (
        cls, h1 & 0xFF, (h1 >> 8) & 0xFF, walked, ch_idx)


def rx_buffer(rng, kind, payload, long_rxd=True, drv=0, size_delta=0, flags=0xC000, mac_info=False):
    dlen = 32 if long_rxd else 16
    size = 4 + dlen + drv * 8 + len(payload) + size_delta
    w0 = len(payload) | kind << 24 | drv << 28 | (0x80000000 if long_rxd else 0) | (0x00800000 if mac_info else 0)
    w1 = rng.randrange(8) << 4 | rng.choice([0, 4, 11, 0x80, 0x105]) << 16
    w3 = rng.randrange(2) | rng.randrange(2) << 9
    desc = struct.pack('<IIII', w0, w1, rng.getrandbits(32), w3) + (bytes(16) if long_rxd else b'')
    return struct.pack('<I', (max(0, size) & 0x3FFF) | flags) + desc + bytes(drv * 8) + payload


def make_rx_corpus(out):
    rng = random.Random(1050)
    lines = []
    for f in constructed_frames():
        lines.append(rx_buffer(rng, 0, f + b'\x00\x00\x00\x00', drv=rng.randrange(3)).hex())
    for _ in range(300):
        # PPDU status with MAC info, a PHY header and IEs.
        users = rng.randrange(6)
        mac = struct.pack('<II', users | (rng.randrange(2) << 29), rng.randrange(4) << 16)
        mac += bytes(4 * users + (4 if users % 2 else 0))
        ies = b''
        for _ in range(rng.randrange(4)):
            t = rng.choice([0, 1, 9, 10, 23, 11, 2])
            ln = IE_LEN[t] if t not in IE_VAR else 8 * rng.randrange(0, 3)
            ies += struct.pack('<I', t | (ln // 8) << 5 | rng.randrange(256) << 16) + bytes(max(0, ln - 4))
        phy_len = 8 + len(ies)
        claimed = phy_len // 8 if rng.randrange(4) else rng.randrange(256)
        hdr = struct.pack('<II', rng.choice([11, 12, 13, 15, 3]) | (0x80 if rng.randrange(5) else 0) | claimed << 8
                          | rng.randrange(256) << 24, rng.getrandbits(16))
        lines.append(rx_buffer(rng, 1, mac + hdr + ies, mac_info=True).hex())
    for _ in range(100):
        c2h = struct.pack('<II', 1 | rng.randrange(64) << 2 | rng.randrange(256) << 8, rng.choice([8, 12, 40, 200]))
        lines.append(rx_buffer(rng, 10, c2h + bytes(rng.randrange(40)), long_rxd=False).hex())
    for _ in range(1500):
        b = bytearray(rx_buffer(rng, rng.randrange(16), bytes(rng.randrange(256) for _ in range(rng.randrange(80))),
                                long_rxd=bool(rng.randrange(2)), drv=rng.randrange(8),
                                size_delta=rng.choice([0, 0, 0, -1, 1, 5000]),
                                flags=rng.choice([0xC000, 0x8000, 0x4000, 0])))
        for _ in range(rng.randrange(3)):
            b[rng.randrange(len(b))] ^= 1 << rng.randrange(8)
        lines.append(bytes(b).hex())
    with open(out, 'w') as fh:
        fh.write('\n'.join(lines) + '\n')
    print('rx corpus: %d buffers' % len(lines))


# --------------------------------------------------------------------------
# Conditional PHY tables (rtw89_phy_sel_headline / rtw89_phy_init_reg)
# --------------------------------------------------------------------------
def load_tables(path):
    text = open(path).read()
    names = ['bb', 'gain', 'radio_a', 'radio_b', 'nctl']
    if path.endswith('.c'):
        arrays = ['rtw89_8852b_phy_bb_regs', 'rtw89_8852b_phy_bb_reg_gain', 'rtw89_8852b_phy_radioa_regs',
                  'rtw89_8852b_phy_radiob_regs', 'rtw89_8852b_phy_nctl_regs']
        pat = re.compile(r'\{\s*(0x[0-9A-Fa-f]+)\s*,\s*(0x[0-9A-Fa-f]+)\s*\}')
        out = {}
        for n, a in zip(names, arrays):
            start = text.index(a + '[] = {')
            body = text[start:text.index('};', start)]
            out[n] = [(int(x, 16), int(y, 16)) for x, y in pat.findall(body)]
        return out
    arrays = ['BB_Registers', 'BB_Gain', 'Radio_A', 'Radio_B', 'NCTL']
    pat = re.compile(r'\(16#([0-9A-F_]+)#, 16#([0-9A-F_]+)#\)')
    out = {}
    for n, a in zip(names, arrays):
        start = text.index('   ' + a + ' : constant')
        body = text[start:text.index('];', start)]
        out[n] = [(int(x.replace('_', ''), 16), int(y.replace('_', ''), 16)) for x, y in pat.findall(body)]
    return out


def apply_table(regs, rfe, cv):
    n = 0
    while n < len(regs) and regs[n][0] >> 28 == 0xF:
        n += 1
    target_of = lambda a: a & 0x0FFFFFFF
    idx = None
    if n == 0:
        cfg = target_of(regs[0][0])
    else:
        for i in range(n):
            if target_of(regs[i][0]) == (rfe << 16 | cv):
                idx = i
                break
        if idx is None:
            for i in range(n):
                if target_of(regs[i][0]) == (rfe << 16 | 0xFF):
                    idx = i
                    break
        if idx is None:
            best = None
            for i in range(n):
                a = regs[i][0]
                if (a >> 16) & 0xFF == rfe and (best is None or (a & 0xFF) >= best):
                    best, idx = a & 0xFF, i
        if idx is None:
            best = None
            for i in range(n):
                a = regs[i][0]
                if (a >> 16) & 0xFF == 0xFF and (best is None or (a & 0xFF) >= best):
                    best, idx = a & 0xFF, i
        if idx is None:
            return []
        cfg = target_of(regs[idx][0])
    applied = []
    matched, found, target = True, False, 0
    for a, d in regs[n:]:
        c = a >> 28
        if c in (0x8, 0x9):
            target = target_of(a)
        elif c == 0xA:
            matched = False
            if not found:
                return applied
        elif c == 0xB:
            matched, found = True, False
        elif c == 0x4:
            if found:
                matched = False
            elif target == cfg:
                matched, found = True, True
            else:
                matched, found = False, False
        elif matched:
            applied.append((a, d))
    return applied


def tables(source, out):
    t = load_tables(source)
    with open(out, 'w') as fh:
        for name in ['bb', 'gain', 'radio_a', 'radio_b', 'nctl']:
            for a, d in apply_table(t[name], 1, 1):
                fh.write('%-7s %08x %08x\n' % (name, a, d))


# --------------------------------------------------------------------------
def decode_file(fn, src, out):
    with open(src) as fi, open(out, 'w') as fo:
        for line in fi:
            fo.write(fn(bytes.fromhex(line.strip())[:MAX_FRAME]) + '\n')


def compare(a, b):
    la, lb = open(a).read().splitlines(), open(b).read().splitlines()
    bad = [(i, x, y) for i, (x, y) in enumerate(zip(la, lb)) if x != y]
    if len(la) != len(lb) or bad:
        print('FAIL: %s and %s differ (%d vs %d lines, %d mismatches)' % (a, b, len(la), len(lb), len(bad)))
        for i, x, y in bad[:10]:
            print('  line %d:\n    %s\n    %s' % (i + 1, x, y))
        return 1
    accepted = sum(1 for x in la if x.startswith(('accepted', 'decoded')))
    print('PASS: %s: %d lines identical (%d accepted)' % (a, len(la), accepted))
    return 0


def main(argv):
    if len(argv) >= 3 and argv[1] == 'corpus':
        make_corpus(argv[2], argv[3:])
    elif len(argv) == 4 and argv[1] == 'frames':
        decode_file(decode_frame, argv[2], argv[3])
    elif len(argv) == 3 and argv[1] == 'rxcorpus':
        make_rx_corpus(argv[2])
    elif len(argv) == 4 and argv[1] == 'rx':
        decode_file(decode_rx, argv[2], argv[3])
    elif len(argv) == 4 and argv[1] == 'tables':
        tables(argv[2], argv[3])
    elif len(argv) == 4 and argv[1] == 'compare':
        return compare(argv[2], argv[3])
    else:
        raise SystemExit(__doc__)
    return 0


if __name__ == '__main__':
    sys.exit(main(sys.argv))

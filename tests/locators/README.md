# Locators and the network address type

`CuBit.Net_Address` (the one 16-byte address type; IPv4 as `::ffff:a.b.c.d`),
`CuBit.Locators` (`@authority:rest` and its field parsers) and
`CuBit.Net_Locator` (the `@net` grammar), from `userspace/runtime/gnat`.
Design: `docs/security-model.md` ("Names and locators") and
`docs/control-language.md` ("Locators and authority kinds").

```sh
nix develop -c sh -c 'cd tests/locators && gprbuild -p -P locators_tests.gpr && build/main'
nix develop -c sh -c 'cd tests/locators && gnatprove -P locators_tests.gpr -j0 --checks-as-errors=on'
nix develop -c tests/locators/mutations.sh
```

| What | Proved (level 1) | Tested (Linux-hosted) |
|---|---|---|
| `Net_Address` | prefix matching and containment over 128 bits; `Mapped` round-trips through `IPv4_Of` | prefix cases |
| `Locators` | no access outside the text, no overflow however long a digit run; an authority is word characters followed by `:`; an unbracketed field has no `:`; a port is 1 .. 65535; a dotted literal is held mapped | address text against Linux's `inet_pton`: 2,000,000 random and 24 chosen texts, no disagreement (RFC 4291 forms, `::`, IPv4 tails; no leading zeros in octets) |
| `Net_Locator` | a valid name host is 1 .. 64 name characters; plus the above | 19 grammar cases: IPv6 in brackets, bracketed or mapped IPv4 refused, trailing fields, ports 0 and 65536, unclosed brackets, long names |

Mutants (`mutations.sh`): an unbounded port or octet, port 0, a field
running past `:`, any character in an authority, an unbounded IPv6 group
count, an unchecked host name. Each must fail to prove.

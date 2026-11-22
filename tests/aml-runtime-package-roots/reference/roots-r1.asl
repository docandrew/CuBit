DefinitionBlock ("", "SSDT", 1, "CUBIT", "PKGROOTS", 1)
{
 Name (MARK, Zero)
 Method (RSET, 0) { Store (Zero, MARK) }
 Method (CNUM, 1) { Store (Buffer (64) {0xA5}, Local0) Increment (MARK) Return (Arg0) }
 Method (PRES, 0) { Store (Buffer (128) {0x5A}, Local0) Return (Zero) }
 Method (PASS, 2) { Return (Arg0) }
 Method (NAM0, 0) { Name (TEMP, Package () {
 Buffer (CNUM (4)) {0x55,0xAA,0x00,0xFF},
 Package () { Buffer (CNUM (3)) {0x11,0x22,0x33},
              Package () {Buffer (CNUM (5)) {0x77,0x88,0x99,0xAA,0xBB}} },
 Buffer (CNUM (2)) {0xCC,0xDD}
}) Return (TEMP) }
 Method (RET0, 0) { Return (Package () {
 Buffer (CNUM (4)) {0x55,0xAA,0x00,0xFF},
 Package () { Buffer (CNUM (3)) {0x11,0x22,0x33},
              Package () {Buffer (CNUM (5)) {0x77,0x88,0x99,0xAA,0xBB}} },
 Buffer (CNUM (2)) {0xCC,0xDD}
}) }
 Method (CAL0, 0) { Return (PASS (Package () {
 Buffer (CNUM (4)) {0x55,0xAA,0x00,0xFF},
 Package () { Buffer (CNUM (3)) {0x11,0x22,0x33},
              Package () {Buffer (CNUM (5)) {0x77,0x88,0x99,0xAA,0xBB}} },
 Buffer (CNUM (2)) {0xCC,0xDD}
}, PRES ())) }
 Method (CEL0, 0) { Store (RET0 (), Local0) Return (PASS (Local0, PRES ())) }
}

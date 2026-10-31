DefinitionBlock ("", "DSDT", 2, "CUBIT", "MODNINE", 1)
{
 Name (INP0, Buffer (8) {0x88,0x77,0x66,0x55,0x44,0x33,0x22,0x11})
 Name (OUT0, Buffer (8) {})
 OperationRegion (REG0, EmbeddedControl, Zero, 9)
 Field (REG0, ByteAcc, NoLock, Preserve) {
   C000,8, C001,8, C002,8, C003,8, C004,8, C005,8, C006,8, C007,8, C008,8
 }
 Field (REG0, ByteAcc, NoLock, Preserve) { ,4, PAIR,64 }
 Method (_INI,0) {
   Store (0xA5,C000) Store (0xA5,C001) Store (0xA5,C002)
   Store (0xA5,C003) Store (0xA5,C004) Store (0xA5,C005)
   Store (0xA5,C006) Store (0xA5,C007) Store (0xA5,C008)
   Store (INP0,PAIR) Store (PAIR,OUT0)
 }
}

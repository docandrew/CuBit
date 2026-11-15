// Controlled namespace/history fixture; no hardware access.
DefinitionBlock ("", "DSDT", 2, "CUBIT", "HISMUTAT", 1) {
 Name (ASTA, 0x0F) Name (BINI, Zero) Name (LINI, Zero)
 Name (DATA, Zero)
 Scope (_SB) { Device (PC00) { Name (_HID, EisaId ("PNP0A08"))
 Device (BRG0) { Name (_ADR, Zero)
 Method (_STA, 0, Serialized) { Return (ASTA) }
 Method (_INI, 0) { Increment (BINI) CopyObject (Buffer (2) {3, 4}, DATA) }
 Device (I2C1) { Name (_ADR, 0x150001)
 Device (TPD0) { Name (_HID, "SYNA2BA6") Name (_CID, "PNP0C50")
 Method (_INI, 0) { Increment (LINI)  }
 } } } } } }

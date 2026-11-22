// Controlled processor ancestry policy fixture; no hardware/thermal action.
DefinitionBlock ("", "DSDT", 2, "CUBIT", "STATKIND", 1) {
 Name (ASTA, 0x0F) Name (BINI, Zero) Name (LINI, Zero)
 Scope (_SB) { Device (PC00) { Name (_HID, EisaId ("PNP0A08"))
 Processor (BRG0, 0, 0, 0) { 
 Name (_CID, "PNP0C50")
 Method (_STA, 0) { Return (ASTA) }
 Method (_INI, 0) { Increment (BINI) }
 Device (I2C1) { Name (_ADR, 0x150001)
 Device (TPD0) { Name (_HID, "SYNA2BA6") Name (_CID, "PNP0C50")
 Method (_INI, 0) { Increment (LINI) Store (Zero, ASTA) }
 } } } } } }

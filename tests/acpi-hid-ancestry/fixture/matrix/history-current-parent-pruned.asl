// Controlled policy fixture; not captured laptop firmware.
DefinitionBlock ("", "DSDT", 2, "CUBIT", "PATHSTA0", 1) {
 Name (PSTA, 0x0F) Name (LSTA, 0x0F)
 Scope (_SB) { Device (PC00) { Name (_HID, EisaId ("PNP0A08")) Method (_STA, 0) { Return (PSTA) }
 Device (I2C1) { Name (_ADR, 0x150001) Device (TPD0) { Name (_HID, "SYNA2BA6") Name (_CID, "PNP0C50")
 Method (_STA, 0) { Return (LSTA) }
 Method (_INI, 0) { Store (Zero, PSTA) }
 } } } }
}

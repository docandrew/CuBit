// Controlled status-transition policy fixture, not captured firmware.
DefinitionBlock ("", "DSDT", 2, "CUBIT", "PATHSTA1", 1) {
 Name (SCNT, Zero) Name (INCT, Zero)
 Scope (_SB) { Device (PC00) { Name (_HID, EisaId ("PNP0A08"))
 Device (I2C1) { Name (_ADR, 0x150001) Device (TPD0) { Name (_HID, "SYNA2BA6")
 Name (_CID, "PNP0C50")
 Method (_STA, 0) { Increment (SCNT) If (SCNT == One) { Return (0x08) } Return (0x0F) }
 Method (_INI, 0) { Increment (INCT) }
 } } } } }

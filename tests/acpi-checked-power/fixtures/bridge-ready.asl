DefinitionBlock ("", "DSDT", 2, "CUBIT", "CHECKPWR", 1) { Device (LID0) { Name (_HID, EisaId ("PNP0C0D")) Method (_INI, 0) { Noop } Name (PASS, Zero) Name (SVAL, Zero)
 Method (_STA, 0) {
  Increment (PASS)
  If (LEqual (PASS, One)) { Return (0x0F) }
  Return (0x0F)
 } Method (_LID, 0) { Return (One) } }
 Device (AC00) { Name (_HID, "ACPI0003") Method (_INI, 0) { Noop } Name (PASS, Zero) Name (SVAL, Zero)
 Method (_STA, 0) {
  Increment (PASS)
  If (LEqual (PASS, One)) { Return (0x0F) }
  Return (0x0F)
 } Method (_PSR, 0) { Return (One) } } }

DefinitionBlock ("", "DSDT", 2, "CUBIT", "SAMPLEEV", 1) {
 Name (LCNT, Zero) Name (SELF, /*SELF*/)
 Device (LID0) { Name (_HID, "/*HID*/")
   Method (_LID, 0) { Increment (\LCNT) If (LAnd (\SELF, LEqual (\LCNT, One))) { Notify (LID0, 0x80) } Return (One) }
 }
 Device (AC00) { Name (_HID, "ACPI0003")
   Method (_PSR, 0) { Notify (\LID0, 0x80) Return (One) }
 }
}

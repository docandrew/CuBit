DefinitionBlock ("", "DSDT", 2, "CUBIT", "STACAC", 1) {
 Name (LSCT, Zero) Name (PSCT, Zero)
 Device (LID0) { Name (_HID, "PNP0C0D") Name (SVAL, Zero)
   Method (_INI, 0) { Noop }
   /*LID_STATUS*/
   Method (_LID, 0) { Return (One) }
 }
 Device (AC00) { Name (_HID, "ACPI0003") Name (SVAL, Zero)
   Method (_INI, 0) { Noop }
   /*POWER_STATUS*/
   Method (_PSR, 0) { Return (One) }
 }
}

DefinitionBlock ("", "DSDT", 2, "CUBIT", "FAMEVENT", 1) {
 Name (MCNT, Zero) Name (SELF, /*SELF*/)
 Device (LID0) { Name (_HID, "/*LID_HID*/")
   Method (_LID, 0) { /*LID_BODY*/ Return (One) }
 }
 Device (AC00) { Name (_HID, "/*POWER_HID*/")
   Method (_PSR, 0) { /*POWER_BODY*/ Return (One) }
 }
}

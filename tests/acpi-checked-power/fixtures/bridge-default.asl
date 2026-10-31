DefinitionBlock ("", "DSDT", 2, "CUBIT", "CHECKPWR", 1) { Device (LID0) { Name (_HID, EisaId ("PNP0C0D"))  Method (_LID, 0) { Return (One) } }
 Device (AC00) { Name (_HID, "ACPI0003")  Method (_PSR, 0) { Return (One) } } }

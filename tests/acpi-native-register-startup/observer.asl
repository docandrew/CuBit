DefinitionBlock ("", "DSDT", 2, "CUBIT", "OBSMODE", 1) {
    Name (GUID, Buffer () { 0x10, 0x20, 0x30, 0x40 })
    Method (CNST) { Return (0x42) }
}

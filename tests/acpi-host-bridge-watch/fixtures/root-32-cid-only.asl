DefinitionBlock ("", "DSDT", 1, "CUBIT", "CIDTEST", 1) { Scope (\_SB) { Device (HST0) { Name (_ADR, Zero)  Name (_CID, "PNP0A08") Method (_CRS, 0) { Return (Buffer () { 0x79, 0x00 }) } } } }

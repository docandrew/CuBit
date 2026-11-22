DefinitionBlock ("", "DSDT", 2, "CUBIT", "TOBTARG", 1) { Name (DEST, "XYZ") Method (CTRL,0) {Return (One)} Method (TEST,0) {Return (ToBuffer ("AB", DEST))} }

DefinitionBlock ("", "DSDT", 2, "CUBIT", "TOBTARG", 1) { Name (DEST, 9) Method (CTRL,0) {Return (One)} Method (TEST,0) {ToBuffer ("AB", DEST) Return (ObjectType (DEST))} }

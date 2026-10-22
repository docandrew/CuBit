DefinitionBlock ("", "DSDT", 1, "CUBIT", "TOBUFF", 1) {
Name (DEST, Buffer (1) {9})
Method (CTRL,0) { Return (One) }
Method (TEST,0) { ToBuffer ("AB", DEST) Return (DEST) }
}

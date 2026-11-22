DefinitionBlock ("", "DSDT", 2, "CUBIT", "TOBUFF", 1) {
Name (BUFF, Buffer (2) {0x41,0x42}) Name (DEST, Buffer (1) {0})
Method (CTRL,0) { Return (One) }
Method (TEST,0) { ToBuffer (BUFF, DEST) Store (9, Index (DEST, Zero)) Return (BUFF) }
}

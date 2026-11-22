DefinitionBlock ("", "DSDT", 2, "CUBIT", "TOBUFF", 1) {
Name (BUFF, Buffer (2) {0x41,0x42})
Method (CTRL,0) { Return (One) }
Method (TEST,0) { ToBuffer (BUFF, Local0) Store (9, Index (Local0, Zero)) Return (BUFF) }
}

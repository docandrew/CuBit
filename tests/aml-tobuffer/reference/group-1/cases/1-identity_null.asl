DefinitionBlock ("", "DSDT", 1, "CUBIT", "TOBUFF", 1) {
Name (BUFF, Buffer (2) {0x41,0x42})
Method (CTRL,0) { Return (One) }
Method (TEST,0) { Store (9, Index (ToBuffer (BUFF), Zero)) Return (BUFF) }
}

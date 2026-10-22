DefinitionBlock ("", "DSDT", 1, "CUBIT", "TOBUFF", 1) {
Name (TEXT, "AB")
Method (CTRL,0) { Return (One) }
Method (TEST,0) { ToBuffer (TEXT, Local0) Store (9, Index (Local0, Zero)) Return (ToBuffer (TEXT)) }
}

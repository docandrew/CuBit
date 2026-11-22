DefinitionBlock ("", "DSDT", 1, "CUBIT", "TOBUFF", 1) {
Name (TEXT, "ABC")
Method (CTRL,0) { Return (One) }
Method (TEST,0) { ToBuffer (TEXT, Local3) Return (Local3) }
}

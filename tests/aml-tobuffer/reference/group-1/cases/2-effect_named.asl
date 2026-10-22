DefinitionBlock ("", "DSDT", 2, "CUBIT", "TOBUFF", 1) {
Name (MARK, Zero) Name (DEST, Buffer (1) {}) Method (SRC0,0) { Increment (MARK) Return ("AB") }
Method (CTRL,0) { Return (One) }
Method (TEST,0) { ToBuffer (SRC0 (), DEST) Return (MARK) }
}

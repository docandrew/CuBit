DefinitionBlock ("", "DSDT", 2, "CUBIT", "TOBUFF", 1) {
Name (MARK, Zero) Name (DEST, Buffer (2) {}) Method (SRC0,0) { Store (Add (Multiply (MARK,10),One),MARK) Return ("AB") } Method (TARG,0) { Store (Add (Multiply (MARK,10),2),MARK) Return (Zero) }
Method (CTRL,0) { Return (One) }
Method (TEST,0) { ToBuffer (SRC0 (), Index (DEST, TARG ())) Return (MARK) }
}

DefinitionBlock ("", "DSDT", 2, "CUBIT", "TOBUFF", 1) {

Method (CTRL,0) { Return (One) }
Method (TEST,0) { Store (Buffer (Zero) {}, Local4) ToBuffer (Local4, Local3) Return (Local4) }
}

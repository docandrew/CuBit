DefinitionBlock ("", "DSDT", 1, "CUBIT", "TOBUFF", 1) {

Method (CTRL,0) { Return (One) }
Method (TEST,0) { Return (ToBuffer (Buffer (3) {0x41,0,0xFF})) }
}

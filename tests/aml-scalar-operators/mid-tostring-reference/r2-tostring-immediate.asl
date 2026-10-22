DefinitionBlock ("", "DSDT", 2, "CUBIT", "MIDTIME", 1) {
 Name (SRC0, 3) Method (TEST, 0) { ToString (Buffer (8) {0x41,0x42,0x43,0x44,0x45,0x46,0x47,0x48}, DerefOf (RefOf (SRC0)), Local0) Return (Local0) }
 Method (POST, 0) { Return (SRC0) } }

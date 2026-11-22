DefinitionBlock ("", "SSDT", 1, "CUBIT", "GCRETRY", 1) {
 Method (TEMP, 0) { Store (Buffer (1) {0x55}, Local0) Return (One) }
 Method (BUFF, 0) { Return (Buffer (1) {0x55}) }
 Method (QUOT, 0) { Return (Package (1) {0x55}) }
 }

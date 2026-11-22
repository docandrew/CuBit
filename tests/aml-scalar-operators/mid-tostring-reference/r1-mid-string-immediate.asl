DefinitionBlock ("", "DSDT", 1, "CUBIT", "MIDTIME", 1) {
 Name (SRC0, 3) Method (TEST, 0) { Mid ("ABCDEFGH", One, DerefOf (RefOf (SRC0)), Local0) Return (Local0) }
 Method (POST, 0) { Return (SRC0) } }

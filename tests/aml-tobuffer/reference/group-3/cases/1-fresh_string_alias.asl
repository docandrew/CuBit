DefinitionBlock ("", "DSDT", 1, "CUBIT", "TOBREF", 1) {  Method (CTRL,0) {Return (One)} Method (TEST,0) {Store (9, Index (ToBuffer ("AB",Local0),Zero)) Return (Local0)} }

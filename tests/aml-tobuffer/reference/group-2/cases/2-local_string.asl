DefinitionBlock ("", "DSDT", 2, "CUBIT", "TOBTARG", 1) {  Method (CTRL,0) {Return (One)} Method (TEST,0) {Store ("XYZ", Local0) ToBuffer ("AB", Local0) Return (Local0)} }

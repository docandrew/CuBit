DefinitionBlock ("", "DSDT", 2, "CUBIT", "COERMAIN", 1) {
Method (LD00,5) { Return (LoadTable (Arg0,Arg1,Arg2,Arg3,Arg4,Zero)) }
Method (GO00,0) { Return (LD00 ("OEMX",Buffer (One) {0xAB},"00000001","","")) }
}

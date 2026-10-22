DefinitionBlock ("", "DSDT", 2, "CUBIT", "NESTBUF", 1)
{
 Method (TEST, 0) { Store (51, Local0) Name (P000, Package () { Buffer (Local0) {}, Buffer (Local0) {}, Buffer (Local0) {} }) Return (Add (Add (SizeOf (DerefOf (Index (P000, Zero))), SizeOf (DerefOf (Index (P000, One)))), SizeOf (DerefOf (Index (P000, 2))))) }
 Method (DRCT, 0) { Store (51, Local0) Name (B000, Buffer (Local0) {}) Return (SizeOf (B000)) }
}

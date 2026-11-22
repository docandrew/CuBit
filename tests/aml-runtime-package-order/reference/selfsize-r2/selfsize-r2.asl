DefinitionBlock ("", "DSDT", 2, "CUBIT", "PKGORDER", 1)
{
 Name (MARK, Zero)
 Method (OBS0, 0) { Return (MARK) }
 Method (TEST, 0) { Store (Zero, MARK) Name (P000, Package (2) { Buffer (Add (Increment (MARK), SizeOf (P000))) {}, One }) Return (SizeOf (DerefOf (Index (P000, Zero)))) }
}

DefinitionBlock ("", "DSDT", 1, "CUBIT", "PKGORDER", 1)
{
 Name (MARK, Zero)
 Method (OBS0, 0) { Return (MARK) }
 Method (TEST, 0) { Store (Zero, MARK) Name (P000, Package (2) { Buffer (Add (Increment (MARK), ObjectType (P000))) {}, One }) Return (Add (Multiply (ObjectType (P000), 100), SizeOf (DerefOf (Index (P000, Zero))))) }
}

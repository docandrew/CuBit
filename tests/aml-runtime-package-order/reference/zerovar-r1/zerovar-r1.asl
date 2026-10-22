DefinitionBlock ("", "DSDT", 1, "CUBIT", "PKGORDER", 1)
{
 Name (MARK, Zero)
 Method (OBS0, 0) { Return (MARK) }
 Method (TEST, 0) { Store (Zero, MARK) Store (Zero, Local0) Name (P000, Package (Local0) { Buffer (Increment (MARK)) {} }) Return (SizeOf (P000)) }
}

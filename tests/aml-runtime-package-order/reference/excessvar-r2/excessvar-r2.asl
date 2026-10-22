DefinitionBlock ("", "DSDT", 2, "CUBIT", "PKGORDER", 1)
{
 Name (MARK, Zero)
 Method (OBS0, 0) { Return (MARK) }
 Method (TEST, 0) { Store (Zero, MARK) Store (One, Local0) Name (P000, Package (Local0) { Buffer (Increment (MARK)) {}, Buffer (Increment (MARK)) {} }) Return (SizeOf (P000)) }
}

DefinitionBlock ("", "DSDT", 2, "CUBIT", "PKGORDER", 1)
{
 Name (MARK, Zero)
 Method (OBS0, 0) { Return (MARK) }
 Method (TEST, 0) { Store (Zero, MARK) Store (Zero, Local0) Name (P000, Package (2) { Buffer (Increment (MARK)) {}, Buffer (Divide (One, Local0)) {} }) Return (SizeOf (P000)) }
}

DefinitionBlock ("", "DSDT", 2, "CUBIT", "PKGORDER", 1)
{
 Name (MARK, Zero)
 Method (OBS0, 0) { Return (MARK) }
 Method (TEST, 0) { Store (Zero, MARK) Name (P000, Package (2) { P000, Buffer (Increment (MARK)) {} }) Return (SizeOf (DerefOf (Index (P000, Zero)))) }
}

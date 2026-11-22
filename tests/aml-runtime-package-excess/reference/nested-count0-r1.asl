DefinitionBlock ("", "DSDT", 1, "CUBIT", "EXCESSNM", 1)
{
 External (MISS, UnknownObj)
 Name (MARK, Zero)
 Method (OBS0, 0) { Return (MARK) }
 Method (TEST, 0) { Store (Zero, MARK) Store (0, Local0) Store (Zero, Local1)
 Name (P000, Package (Local0) { Package (Local1) { MISS, Buffer (Increment (MARK)) {} } })
 Return (SizeOf (P000)) }
}

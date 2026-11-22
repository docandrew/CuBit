DefinitionBlock ("", "DSDT", 2, "CUBIT", "EXCESSNM", 1)
{
 External (MISS, UnknownObj)
 Name (GOOD, 42)
 Name (MARK, Zero)
 Method (OBS0, 0) { Return (MARK) }
 Method (TEST, 0) { Store (Zero, MARK) Store (1, Local0)
 Name (P000, Package (Local0) { One, Package (2) { MISS, Buffer (Increment (MARK)) {} } })
 Return (SizeOf (P000)) }
}

// Bounded runtime Package capture witness; normal compilation only.
DefinitionBlock ("", "DSDT", 2, "CUBIT", "FLDPKG", 1)
{
    Name (MARK, Zero)
    Method (CTRL, 0, NotSerialized) { Return (One) }
    Method (WIDT, 0, NotSerialized) { Return (ShiftRight (Ones, 32)) }
    Method (TEST, 0, NotSerialized)
    {
        Name (BUFF, Buffer (One) { 0x41 })
        CreateField (BUFF, Zero, 8, FLD0)
        Name (PKG0, Package (One) { FLD0 })
        Store (One, MARK)
        Store (0x42, FLD0)
        Store (2, MARK)
        ToInteger (DerefOf (Index (PKG0, Zero)), Local1)
        ToInteger (FLD0, Local2)
        Store (3, MARK)
        Return (Add (Multiply (Local1, 0x100), Local2))
    }
}

DefinitionBlock ("", "DSDT", 2, "CUBIT", "HIDI2C", 1)
{
    Name (UUID, ToUUID ("3CDFF6F7-4267-4555-AD05-B30A3D8938DE"))
    Name (PKG0, Package (0) {})
    Name (BUF0, Buffer (2) {0x11, 0x22})
    OperationRegion (RGN0, SystemMemory, 0x1000, One)
    Field (RGN0, ByteAcc, NoLock, Preserve) { FLD0, 8 }
    Method (_DSM, 4, NotSerialized)
    {
        If (LNotEqual (Arg0, UUID)) { Return (Zero) }
        If (LNotEqual (Arg1, One)) { Return (Zero) }
        If (LNotEqual (ObjectType (Arg3), 4)) { Return (Zero) }
        If (LNotEqual (SizeOf (Arg3), Zero)) { Return (Zero) }
        If (LEqual (Arg2, Zero)) { Return (Buffer (One) {3}) }
        If (LEqual (Arg2, One)) { Return (0x1234) }
        Return (Zero)
    }
    Method (ECHO, 1, NotSerialized) { Return (Arg0) }
    Method (MUT0, 1, NotSerialized)
    {
        Store (0x5A, Index (Arg0, Zero))
        Return (Arg0)
    }
    Method (SCAL, 2, NotSerialized) { Return (Add (Arg0, Arg1)) }
    Method (ALL0, 0, NotSerialized) { Return (_DSM (UUID, One, Zero, PKG0)) }
    Method (ALL1, 0, NotSerialized) { Return (_DSM (UUID, One, One, PKG0)) }
}

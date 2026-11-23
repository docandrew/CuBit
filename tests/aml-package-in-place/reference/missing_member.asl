DefinitionBlock ("", "DSDT", 2, "CuBit", "PKGFAIL", 1)
{
    External (MISS, IntObj)
    Name (MARK, Zero)
    Name (PKG0, Package (2) {0xAA, 0xBB})
    Method (BAD0, 0)
    {
        Increment (MARK)
        Name (BUFF, Buffer (2) {0x41, 0x42})
        CreateByteField (BUFF, Zero, FLD0)
        Store (Package (2) {FLD0, MISS}, PKG0)
        Return (One)
    }
    Method (READ, 0) { Return (MARK) }
    Method (PKGR, 0) { Return (PKG0) }
}

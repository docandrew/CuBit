DefinitionBlock ("", "SSDT", 2, "CUBIT", "DYNLOAD", 1)
{
    External (BCNT, IntObj)
    Increment (BCNT)
    Method (DNUM, 0) { Return (0x2A) }
}

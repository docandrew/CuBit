DefinitionBlock ("", "DSDT", 2, "CUBIT", "OWNTEST", 1)
{
    Method (TEST, 0, NotSerialized)
    {
        Device (TMPD)
        {
            Name (_ADR, Zero)
            Name (SLOT, 11)
        }
        Return (TMPD.SLOT)
    }
    Method (OBS0, 0, NotSerialized) { Return (0) }
    Method (GOOD, 0, NotSerialized) { Return (1) }
}

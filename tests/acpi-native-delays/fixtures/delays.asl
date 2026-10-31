DefinitionBlock ("", "DSDT", 2, "CUBIT", "NATWAIT", 1)
{
    Method (STAL, 0) { Stall (10) Return (18) }
    Method (SLEP, 0) { Sleep (2) Return (19) }
    Method (ZDEL, 0) { Sleep (0) Stall (0) Return (17) }
    Method (BOTH, 0) { STAL () SLEP () Return (20) }
    Method (BAD0, 1) { Stall (Arg0) Return (21) }
}

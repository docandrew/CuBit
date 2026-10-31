DefinitionBlock ("", "DSDT", 2, "CUBIT", "DUPBOOT", 1)
{
    Method (DUPL, 0) { Return (LoadTable ("SSDT", "CUBIT", "DUPCOPY", "", "", Zero)) }
}

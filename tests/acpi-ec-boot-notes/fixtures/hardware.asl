DefinitionBlock ("", "DSDT", 2, "CUBIT", "ECHARD", 1)
{
    OperationRegion (HWIO, SystemMemory, 0x12340000, One)
    Scope (\_SB) { Device (LID0) { Name (_HID, EisaId ("PNP0C0D")) } }
}

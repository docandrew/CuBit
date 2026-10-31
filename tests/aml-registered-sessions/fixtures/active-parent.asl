DefinitionBlock ("", "OEM1", 2, "CUBIT", "PARENT", 1) {
    External (ORD0, IntObj)
    External (ENIO, IntObj)
    External (FVAL, IntObj)
    External (FLD0, FieldUnitObj)
    External (OK02, IntObj)
    Name (BEF0, 17)
    Store (Add (Multiply (\ORD0, 10), One), \ORD0)
    If (\ENIO) { Store (\FLD0, \FVAL) }
    Store (LoadTable ("OEM2", "", "", "\\DST0", "", Zero), \OK02)
    Name (AFT0, 19)
    Store (Add (Multiply (\ORD0, 10), 3), \ORD0)
}

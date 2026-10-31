DefinitionBlock ("", "OEM2", 2, "CUBIT", "CHILD", 1) {
    External (ORD0, IntObj)
    External (ENIO, IntObj)
    External (FVAL, IntObj)
    External (FLD0, FieldUnitObj)
    Name (CHLD, 22)
    If (\ENIO) { Store (\FLD0, \FVAL) }
    Store (Add (Multiply (\ORD0, 10), 2), \ORD0)
}

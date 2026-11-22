DefinitionBlock ("", "OEM1", 2, "CUBIT", "PARENT", 1) {
    External (ORD0, IntObj)
    External (OK02, IntObj)
    Name (BEF0, 17)
    Store (Add (Multiply (\ORD0, 10), One), \ORD0)
    Store (LoadTable ("OEM1", "", "", "\\DST0", "PRM0", 77), \OK02)
    Name (AFT0, 19)
    Store (Add (Multiply (\ORD0, 10), 3), \ORD0)
}

DefinitionBlock ("", "DSDT", 1, "CUBIT", "MUTEX", 1)
{
Mutex (M000, 0)
Mutex (M001, 1)
Mutex (M002, 2)
Mutex (M012, 2)
Name (MARK, Zero)
Name (ZVAL, Zero)
Method (PROB, 0) { Store (Acquire (M000, Zero), Local0) If (LEqual (Local0, Zero)) { Release (M000) } Return (Local0) }
Method (TEST, 0) { Store (Acquire (M002, Zero), Local0) Store (Acquire (M012, Zero), Local1) Release (M002) Release (M012) Store (Acquire (M001, Zero), Local2) Release (M001) Store (One, MARK) Return (Or (Or (Local0, Local1), Local2)) }
}

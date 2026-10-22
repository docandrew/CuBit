DefinitionBlock ("", "DSDT", 2, "CUBIT", "MUTEX", 1)
{
Mutex (M000, 0)
Mutex (M001, 1)
Mutex (M002, 2)
Mutex (M012, 2)
Name (MARK, Zero)
Name (ZVAL, Zero)
Method (PROB, 0) { Store (Acquire (M000, Zero), Local0) If (LEqual (Local0, Zero)) { Release (M000) } Return (Local0) }
Method (TEST, 0) { Store (Acquire (M000, Zero), Local0) Store (Acquire (M000, Zero), Local1) Release (M000) Release (M000) Store (One, MARK) Return (Or (Local0, Local1)) }
}

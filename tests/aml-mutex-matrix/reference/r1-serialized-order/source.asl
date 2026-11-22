DefinitionBlock ("", "DSDT", 1, "CUBIT", "MUTEX", 1)
{
Mutex (M000, 0)
Mutex (M001, 1)
Mutex (M002, 2)
Mutex (M012, 2)
Name (MARK, Zero)
Name (ZVAL, Zero)
Method (PROB, 0) { Store (Acquire (M000, Zero), Local0) If (LEqual (Local0, Zero)) { Release (M000) } Return (Local0) }
Method (SER0, 0, Serialized, 1) { Store (2, MARK) Return (One) } Method (TEST, 0) { Store (Acquire (M002, Zero), Local0) Store (One, MARK) Store (SER0 (), Local1) Release (M002) Return (Or (Local0, Local1)) }
}

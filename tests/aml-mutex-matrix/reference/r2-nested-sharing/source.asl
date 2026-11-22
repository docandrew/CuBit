DefinitionBlock ("", "DSDT", 2, "CUBIT", "MUTEX", 1)
{
Mutex (M000, 0)
Mutex (M001, 1)
Mutex (M002, 2)
Mutex (M012, 2)
Name (MARK, Zero)
Name (ZVAL, Zero)
Method (PROB, 0) { Store (Acquire (M000, Zero), Local0) If (LEqual (Local0, Zero)) { Release (M000) } Return (Local0) }
Method (GET0, 0) { Return (Acquire (M000, Zero)) } Method (PUT0, 0) { Release (M000) Return (One) } Method (TEST, 0) { Store (GET0 (), Local0) Store (PUT0 (), MARK) Return (Local0) }
}

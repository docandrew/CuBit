DefinitionBlock ("", "DSDT", 2, "CUBIT", "LATEREG", 2) {
Name (ERRS, Zero) Name (RMRC, One)
Name (RP0P, Package () {":STST:synthetic:late-region:PASS:"})
Name (CONS, Zero) Name (SIDE, Zero)
Device (DEV0) { Name (_ADR, Zero) }
OperationRegion (LOGR, SystemMemory, 0x2000, One)
Field (LOGR, ByteAcc, NoLock, Preserve) { SINK, 8 }
Method (SIDF, 0, NotSerialized) { Increment (SIDE) Store (SIDE, SINK) Return (0x100) }
External (\DEV0.CEL0, FieldUnitObj)
Method (_LID, 0, NotSerialized) { Return (ShiftLeft (\DEV0.CEL0, SIDF ())) }
}

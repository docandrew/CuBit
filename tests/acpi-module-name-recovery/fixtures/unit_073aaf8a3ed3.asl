DefinitionBlock ("", "SSDT", 2, "CUBIT", "LATEREG", 3) {
External (DEV0, DeviceObj) External (BASE, UnknownObj) External (CONS, IntObj) External (SINK, FieldUnitObj) External (SIDF, MethodObj)
Scope (\DEV0) { OperationRegion (RGN0, SystemMemory, Add (BASE, 0x100), 4) Field (RGN0, ByteAcc, NoLock, Preserve) { CEL0, 8 } Store (ShiftLeft (SIDF (), CEL0), \SINK) }
Store (One, CONS)
}

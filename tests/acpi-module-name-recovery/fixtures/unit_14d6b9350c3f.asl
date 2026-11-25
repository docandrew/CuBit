DefinitionBlock ("", "SSDT", 2, "CUBIT", "LATEREG", 4) {
External (DEV0, DeviceObj) External (BASE, UnknownObj) External (CONS, IntObj)
External (SIDF, MethodObj)
Store (One, CONS)
Scope (\DEV0) { OperationRegion (RGN0, SystemMemory, Add (SIDF (), BASE), 4) Field (RGN0, ByteAcc, NoLock, Preserve) { CEL0, 8 } }
}

DefinitionBlock ("", "SSDT", 2, "CUBIT", "LATEREG", 6) {
OperationRegion (NVS0, SystemMemory, 0x1000, 4) Field (NVS0, DWordAcc, Lock, Preserve) { BASE, 32 }
Name (TAIL, One)
}

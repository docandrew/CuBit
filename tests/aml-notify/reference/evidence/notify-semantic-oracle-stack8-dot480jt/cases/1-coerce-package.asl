DefinitionBlock("", "DSDT", 1, "CUBIT", "NOTIFY", 1){
Device(DEV0){Name(_HID,"TEST0001")} Device(DEV1){Name(_HID,"TEST0002")} ThermalZone(THM0){} Processor(CPU0,0,0,0){} Alias(DEV0,ALS0)
Name(MARK,Zero) Name(INT0,One) Name(STR0,"80") Name(BUF0,Buffer(){0x80}) Name(PKG0,Package(){One}) Mutex(MTX0,0)
Method(CTRL,0){Return(One)} Method(WIDT,0){Return(ShiftRight(Ones,32))}
Method (VAL0,0) { Store (2, MARK) Return (Package(){One}) }
Method(TEST,0){Store(One,MARK) Notify (DEV0, VAL0()) Store(3,MARK) Return(MARK)}
}

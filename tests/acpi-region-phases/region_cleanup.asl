DefinitionBlock("", "DSDT", 2, "CUBIT", "REGCLEAN", 1) {
 Name(BUF0,Buffer(4){1,2,3,4})
 Name(MARK,Zero)
 Method(READ,0){Return(MARK)}
 OperationRegion(TREG,EmbeddedControl,Zero,4)
 Method(_REG,2){If(LEqual(Arg1,One)){Store(One,MARK) /*REG*/} Else {Store(Add(MARK,0x10),MARK)}}
 Method(_INI,0){/*INI*/}
 Scope(\_SB){Device(DEV0){Name(_HID,"TEST0001")}}
}

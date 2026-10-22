DefinitionBlock("","DSDT",2,"CUBIT","DYNBUFF",1){
Name(MARK,Zero) Name(CALL,Zero)
Method(CTRL,0){Return(One)}
Method(WIDT,0){Return(ShiftRight(Ones,32))}
Method(CNTM,0){Increment(CALL) Store(2,MARK) Return(8192)}
Method(TEST,0){Store(One,Local0) Store(Buffer(Local0){0x70,1,0x4D,0x41,0x52,0x4B,0xA4,1},Local1) Return(SizeOf(Local1))}
Method(POST,0){Return(Add(Multiply(CALL,16),MARK))}
}

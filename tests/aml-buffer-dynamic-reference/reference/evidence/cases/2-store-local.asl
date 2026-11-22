DefinitionBlock("","DSDT",2,"CUBIT","DYNBUFF",1){
Name(MARK,Zero) Name(CALL,Zero)
Method(CTRL,0){Return(One)}
Method(WIDT,0){Return(ShiftRight(Ones,32))}
Method(CNTM,0){Increment(CALL) Store(2,MARK) Return(8192)}
Method(TEST,0){Store(8192,Local0) Store(Buffer(Local0){0xA5,0x5A},Local1) Return(Add(SizeOf(Local1),DerefOf(Index(Local1,One))))}
Method(POST,0){Return(Add(Multiply(CALL,16),MARK))}
}

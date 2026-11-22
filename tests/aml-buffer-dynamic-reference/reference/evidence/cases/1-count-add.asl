DefinitionBlock("","DSDT",1,"CUBIT","DYNBUFF",1){
Name(MARK,Zero) Name(CALL,Zero)
Method(CTRL,0){Return(One)}
Method(WIDT,0){Return(ShiftRight(Ones,32))}
Method(CNTM,0){Increment(CALL) Store(2,MARK) Return(8192)}
Method(TEST,0){Store(8191,Local0) Store(Buffer(Add(Local0,One,MARK)){0xA5},Local1) Return(SizeOf(Local1))}
Method(POST,0){Return(Add(Multiply(CALL,16),MARK))}
}

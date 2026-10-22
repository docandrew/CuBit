DefinitionBlock("","DSDT",2,"CUBIT","LARGBUFF",1){
Name(MARK,Zero) Name(CALL,Zero)
Method(CTRL,0){Return(One)}
Method(WIDT,0){Return(ShiftRight(Ones,32))}
Method(CNTM,0){Increment(CALL) Store(2,MARK) Return(Buffer(Zero){})}
Method(TEST,0){Store(One,MARK) Name(BUF0,Buffer(CNTM()){0x46}) Store(3,MARK) Return(SizeOf(BUF0))}
Method(POST,0){Return(Add(Multiply(CALL,16),MARK))}
}

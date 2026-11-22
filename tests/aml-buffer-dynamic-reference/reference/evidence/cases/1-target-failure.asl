DefinitionBlock("","DSDT",1,"CUBIT","DYNBUFF",1){
Name(MARK,Zero) Name(CALL,Zero)
Method(CTRL,0){Return(One)}
Method(WIDT,0){Return(ShiftRight(Ones,32))}
Method(CNTM,0){Increment(CALL) Store(2,MARK) Return(8192)}
Name(TARG,Buffer(One){0}) Method(TEST,0){Store(One,MARK) Store(Buffer(CNTM()){0xA5},Index(TARG,99)) Store(3,MARK) Return(One)}
Method(POST,0){Return(Add(Multiply(CALL,16),MARK))}
}

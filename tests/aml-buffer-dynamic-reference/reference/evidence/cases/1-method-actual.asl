DefinitionBlock("","DSDT",1,"CUBIT","DYNBUFF",1){
Name(MARK,Zero) Name(CALL,Zero)
Method(CTRL,0){Return(One)}
Method(WIDT,0){Return(ShiftRight(Ones,32))}
Method(CNTM,0){Increment(CALL) Store(2,MARK) Return(8192)}
Method(SIZE,1){Return(SizeOf(Arg0))} Method(TEST,0){Store(8192,Local0) Return(SIZE(Buffer(Local0){0xA5}))}
Method(POST,0){Return(Add(Multiply(CALL,16),MARK))}
}

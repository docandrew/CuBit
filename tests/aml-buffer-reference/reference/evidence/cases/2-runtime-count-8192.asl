DefinitionBlock("","DSDT",2,"CUBIT","LARGBUFF",1){
Name(MARK,Zero) Name(CALL,Zero)
Method(CTRL,0){Return(One)}
Method(WIDT,0){Return(ShiftRight(Ones,32))}
Name(CNT0,8192) Method(CNTM,0){Increment(CALL) Store(2,MARK) Return(CNT0)}
Method(TEST,0){Store(One,MARK) Name(BUF0,Buffer(CNTM()){0x44}) Store(3,MARK) Return(Add(Multiply(SizeOf(BUF0),256),DerefOf(Index(BUF0,Zero))))}
Method(POST,0){Return(Add(Multiply(CALL,16),MARK))}
}

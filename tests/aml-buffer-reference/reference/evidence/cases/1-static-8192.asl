DefinitionBlock("","DSDT",1,"CUBIT","LARGBUFF",1){
Name(MARK,Zero) Name(CALL,Zero)
Method(CTRL,0){Return(One)}
Method(WIDT,0){Return(ShiftRight(Ones,32))}
Name(BUF0,Buffer(8192){0x42})
Method(TEST,0){Store(One,MARK) Return(Add(Multiply(SizeOf(BUF0),256),DerefOf(Index(BUF0,8191))))}
Method(POST,0){Return(Add(Multiply(CALL,16),MARK))}
}

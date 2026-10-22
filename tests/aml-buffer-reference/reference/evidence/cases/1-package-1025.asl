DefinitionBlock("","DSDT",1,"CUBIT","LARGBUFF",1){
Name(MARK,Zero) Name(CALL,Zero)
Method(CTRL,0){Return(One)}
Method(WIDT,0){Return(ShiftRight(Ones,32))}
Name(PKG0,Package(){Buffer(1025){0x5A}})
Method(TEST,0){Store(One,MARK) CopyObject(DerefOf(Index(PKG0,Zero)),Local0) Return(Add(Multiply(SizeOf(Local0),256),DerefOf(Index(Local0,Zero))))}
Method(POST,0){Return(Add(Multiply(CALL,16),MARK))}
}

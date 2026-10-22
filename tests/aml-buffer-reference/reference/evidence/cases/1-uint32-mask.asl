DefinitionBlock("","DSDT",1,"CUBIT","LARGBUFF",1){
Name(MARK,Zero) Name(CALL,Zero)
Method(CTRL,0){Return(One)}
Method(WIDT,0){Return(ShiftRight(Ones,32))}

Method(TEST,0){Store(One,MARK) Store(Buffer(0x100000002){0x42},Local0) Store(2,MARK) Return(Add(Multiply(SizeOf(Local0),256),DerefOf(Index(Local0,One))))}
Method(POST,0){Return(Add(Multiply(CALL,16),MARK))}
}

DefinitionBlock("","DSDT",2,"CUBIT","LARGBUFF",1){
Name(MARK,Zero) Name(CALL,Zero)
Method(CTRL,0){Return(One)}
Method(WIDT,0){Return(ShiftRight(Ones,32))}

Method(TEST,0){Store(One,MARK) Return(ToInteger(Buffer(8192){1,2,3,4,5,6,7,8,9}))}
Method(POST,0){Return(Add(Multiply(CALL,16),MARK))}
}

DefinitionBlock("","DSDT",2,"CUBIT","DYNBUFF",1){
Name(MARK,Zero) Name(CALL,Zero)
Method(CTRL,0){Return(One)}
Method(WIDT,0){Return(ShiftRight(Ones,32))}
Method(CNTM,0){Increment(CALL) Store(2,MARK) Return(8192)}
Method(MAKE,1){Return(Buffer(Arg0){0xA5,0x5A})} Method(TEST,0){Store(MAKE(8192),Local0) Return(SizeOf(Local0))}
Method(POST,0){Return(Add(Multiply(CALL,16),MARK))}
}

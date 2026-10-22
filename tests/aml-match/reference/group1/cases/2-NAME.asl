DefinitionBlock("","DSDT",2,"CUBIT","MATCH",1){
Name(MARK,Zero)
Name(IVAL,One) Name(PKG0,Package(){IVAL})
Method(CTRL,0){Return(One)}
Method(WIDT,0){Return(ShiftRight(Ones,32))}
Method(TEST,0){Store(2,IVAL) Return(Match(PKG0,MEQ,2,MTR,Zero,Zero))}
}

DefinitionBlock("","DSDT",2,"CUBIT","MATCH",1){
Name(MARK,Zero)
Name(PKG0,Package(){1}) Name(IVAL,One) Method(STRT,0){Store(2,IVAL) Store(2,Index(PKG0,Zero)) Return(Zero)}
Method(CTRL,0){Return(One)}
Method(WIDT,0){Return(ShiftRight(Ones,32))}
Method(TEST,0){Return(Match(PKG0,MEQ,IVAL,MTR,Zero,STRT()))}
}

DefinitionBlock("","DSDT",2,"CUBIT","MATCH",1){
Name(MARK,Zero)
Name(PKG0,Package(){2})
Method(CTRL,0){Return(One)}
Method(WIDT,0){Return(ShiftRight(Ones,32))}
Method(TEST,0){Store(One,Local0) Return(Match(PKG0,MEQ,Local0,MTR,Zero,Subtract(Store(2,Local0),2)))}
}

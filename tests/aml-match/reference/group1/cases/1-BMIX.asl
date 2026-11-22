DefinitionBlock("","DSDT",1,"CUBIT","MATCH",1){
Name(MARK,Zero)
Name(PKG0,Package(){"A",Buffer(){65,0}})
Method(CTRL,0){Return(One)}
Method(WIDT,0){Return(ShiftRight(Ones,32))}
Method(TEST,0){Return(Match(PKG0,MEQ,Buffer(){65,0},MTR,Zero,Zero))}
}

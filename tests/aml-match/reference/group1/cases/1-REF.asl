DefinitionBlock("","DSDT",1,"CUBIT","MATCH",1){
Name(MARK,Zero)
Name(PKG0,Package(){1}) Name(IVAL,One)
Method(CTRL,0){Return(One)}
Method(WIDT,0){Return(ShiftRight(Ones,32))}
Method(TEST,0){CopyObject(RefOf(IVAL),Local0) Return(Match(PKG0,MEQ,Local0,MTR,Zero,Zero))}
}

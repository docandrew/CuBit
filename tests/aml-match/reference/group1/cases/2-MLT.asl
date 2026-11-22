DefinitionBlock("","DSDT",2,"CUBIT","MATCH",1){
Name(MARK,Zero)
Name(PKG0,Package(){2,3,4})
Method(CTRL,0){Return(One)}
Method(WIDT,0){Return(ShiftRight(Ones,32))}
Method(TEST,0){Return(Match(PKG0,MLT,3,MTR,Zero,Zero))}
}

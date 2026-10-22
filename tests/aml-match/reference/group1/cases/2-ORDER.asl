DefinitionBlock("","DSDT",2,"CUBIT","MATCH",1){
Name(MARK,Zero)
Name(PKG0,Package(){1}) Method(PGET,0){Store(Add(Multiply(MARK,10),1),MARK) Return(PKG0)} Method(MONE,0){Store(Add(Multiply(MARK,10),2),MARK) Return(2)} Method(MTWO,0){Store(Add(Multiply(MARK,10),3),MARK) Return(1)} Method(STRT,0){Store(Add(Multiply(MARK,10),4),MARK) Return(Zero)}
Method(CTRL,0){Return(One)}
Method(WIDT,0){Return(ShiftRight(Ones,32))}
Method(TEST,0){Return(Match(PGET(),MEQ,MONE(),MEQ,MTWO(),STRT()))}
}

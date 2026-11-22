DefinitionBlock("","DSDT",1,"CUBIT","BUFCOERC",1){Name(CNT0,Buffer(){3,0,0,0}) Name(BUF0,Buffer(CNT0){}) Method(TEST,0){ Return(SizeOf(BUF0))}}

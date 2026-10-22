DefinitionBlock("","DSDT",2,"CUBIT","BUFCOERC",1){Name(CNT0,Buffer(){3}) Name(BUF0,Buffer(CNT0){}) Method(TEST,0){Store(5,Index(CNT0,0)) Return(SizeOf(BUF0))}}

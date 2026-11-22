DefinitionBlock("","DSDT",2,"CUBIT","BUFCOERC",1){Name(CNT0,Buffer(16){3,0,0,0,1,0,0,0,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF}) Name(BUF0,Buffer(CNT0){}) Method(TEST,0){ Return(SizeOf(BUF0))}}

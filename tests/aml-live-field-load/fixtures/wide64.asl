DefinitionBlock("","DSDT",2,"CUBIT","WIDEFLD",1){Name(ERRS,0)Name(RMRC,1)Name(RP0P,Package(){":STST:synthetic:widefield:PASS:"}) OperationRegion(REG0,SystemMemory,0x100,8) Field(REG0,ByteAcc,NoLock,Preserve){FLD0,64} Name(SRCB,Buffer(8){0xA5,0xA5,0xA5,0xA5,0xA5,0xA5,0xA5,0xA5}) Method(MN00,0,Serialized){Store(SRCB,FLD0) Store(FLD0,Local0)
If(ObjectType(Local0)!=1){Return(1)}
If(Local0 != 0xa5a5a5a5a5a5a5a5){Return(2)}
Return(0)}}

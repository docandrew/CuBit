DefinitionBlock("", "DSDT", 1, "CUBIT", "COMPARE", 1) { Name(MARK,Zero) Method(TEST,0){Store(Buffer(){1},Local0) Return(LEqual(Local0,Store(Buffer(){2},Local0)))} }

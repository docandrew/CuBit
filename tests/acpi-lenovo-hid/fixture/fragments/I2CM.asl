Method (I2CM, 3, Serialized)
        {
            Switch (ToInteger (Arg0))
            {
                Case (Zero)
                {
                    Name (IIC0, Buffer (0x23)
                    {
                        /* 0000 */  0x8E, 0x1E, 0x00, 0x01, 0x00, 0x01, 0x02, 0x00,  // ........
                        /* 0008 */  0x00, 0x01, 0x06, 0x00, 0x80, 0x1A, 0x06, 0x00,  // ........
                        /* 0010 */  0x00, 0x00, 0x5C, 0x5F, 0x53, 0x42, 0x2E, 0x50,  // ..\_SB.P
                        /* 0018 */  0x43, 0x30, 0x30, 0x2E, 0x49, 0x32, 0x43, 0x30,  // C00.I2C0
                        /* 0020 */  0x00, 0x79, 0x00                                 // .y.
                    })
                    CreateWordField (IIC0, 0x10, DAD0)
                    CreateDWordField (IIC0, 0x0C, DSP0)
                    DAD0 = Arg1
                    DSP0 = Arg2
                    Return (IIC0) /* \_SB_.PC00.I2CM.IIC0 */
                }
                Case (One)
                {
                    Name (IIC1, Buffer (0x23)
                    {
                        /* 0000 */  0x8E, 0x1E, 0x00, 0x01, 0x00, 0x01, 0x02, 0x00,  // ........
                        /* 0008 */  0x00, 0x01, 0x06, 0x00, 0x80, 0x1A, 0x06, 0x00,  // ........
                        /* 0010 */  0x00, 0x00, 0x5C, 0x5F, 0x53, 0x42, 0x2E, 0x50,  // ..\_SB.P
                        /* 0018 */  0x43, 0x30, 0x30, 0x2E, 0x49, 0x32, 0x43, 0x31,  // C00.I2C1
                        /* 0020 */  0x00, 0x79, 0x00                                 // .y.
                    })
                    CreateWordField (IIC1, 0x10, DAD1)
                    CreateDWordField (IIC1, 0x0C, DSP1)
                    DAD1 = Arg1
                    DSP1 = Arg2
                    Return (IIC1) /* \_SB_.PC00.I2CM.IIC1 */
                }
                Case (0x02)
                {
                    Name (IIC2, Buffer (0x23)
                    {
                        /* 0000 */  0x8E, 0x1E, 0x00, 0x01, 0x00, 0x01, 0x02, 0x00,  // ........
                        /* 0008 */  0x00, 0x01, 0x06, 0x00, 0x80, 0x1A, 0x06, 0x00,  // ........
                        /* 0010 */  0x00, 0x00, 0x5C, 0x5F, 0x53, 0x42, 0x2E, 0x50,  // ..\_SB.P
                        /* 0018 */  0x43, 0x30, 0x30, 0x2E, 0x49, 0x32, 0x43, 0x32,  // C00.I2C2
                        /* 0020 */  0x00, 0x79, 0x00                                 // .y.
                    })
                    CreateWordField (IIC2, 0x10, DAD2)
                    CreateDWordField (IIC2, 0x0C, DSP2)
                    DAD2 = Arg1
                    DSP2 = Arg2
                    Return (IIC2) /* \_SB_.PC00.I2CM.IIC2 */
                }
                Case (0x03)
                {
                    Name (IIC3, Buffer (0x23)
                    {
                        /* 0000 */  0x8E, 0x1E, 0x00, 0x01, 0x00, 0x01, 0x02, 0x00,  // ........
                        /* 0008 */  0x00, 0x01, 0x06, 0x00, 0x80, 0x1A, 0x06, 0x00,  // ........
                        /* 0010 */  0x00, 0x00, 0x5C, 0x5F, 0x53, 0x42, 0x2E, 0x50,  // ..\_SB.P
                        /* 0018 */  0x43, 0x30, 0x30, 0x2E, 0x49, 0x32, 0x43, 0x33,  // C00.I2C3
                        /* 0020 */  0x00, 0x79, 0x00                                 // .y.
                    })
                    CreateWordField (IIC3, 0x10, DAD3)
                    CreateDWordField (IIC3, 0x0C, DSP3)
                    DAD3 = Arg1
                    DSP3 = Arg2
                    Return (IIC3) /* \_SB_.PC00.I2CM.IIC3 */
                }
                Case (0x04)
                {
                    Name (IIC4, Buffer (0x23)
                    {
                        /* 0000 */  0x8E, 0x1E, 0x00, 0x01, 0x00, 0x01, 0x02, 0x00,  // ........
                        /* 0008 */  0x00, 0x01, 0x06, 0x00, 0x80, 0x1A, 0x06, 0x00,  // ........
                        /* 0010 */  0x00, 0x00, 0x5C, 0x5F, 0x53, 0x42, 0x2E, 0x50,  // ..\_SB.P
                        /* 0018 */  0x43, 0x30, 0x30, 0x2E, 0x49, 0x32, 0x43, 0x34,  // C00.I2C4
                        /* 0020 */  0x00, 0x79, 0x00                                 // .y.
                    })
                    CreateWordField (IIC4, 0x10, DAD4)
                    CreateDWordField (IIC4, 0x0C, DSP4)
                    DAD4 = Arg1
                    DSP4 = Arg2
                    Return (IIC4) /* \_SB_.PC00.I2CM.IIC4 */
                }
                Case (0x05)
                {
                    Name (IIC5, Buffer (0x23)
                    {
                        /* 0000 */  0x8E, 0x1E, 0x00, 0x01, 0x00, 0x01, 0x02, 0x00,  // ........
                        /* 0008 */  0x00, 0x01, 0x06, 0x00, 0x80, 0x1A, 0x06, 0x00,  // ........
                        /* 0010 */  0x00, 0x00, 0x5C, 0x5F, 0x53, 0x42, 0x2E, 0x50,  // ..\_SB.P
                        /* 0018 */  0x43, 0x30, 0x30, 0x2E, 0x49, 0x32, 0x43, 0x35,  // C00.I2C5
                        /* 0020 */  0x00, 0x79, 0x00                                 // .y.
                    })
                    CreateWordField (IIC5, 0x10, DAD5)
                    CreateDWordField (IIC5, 0x0C, DSP5)
                    DAD5 = Arg1
                    DSP5 = Arg2
                    Return (IIC5) /* \_SB_.PC00.I2CM.IIC5 */
                }
                Case (0x0C)
                {
                    Name (IIC6, Buffer (0x23)
                    {
                        /* 0000 */  0x8E, 0x1E, 0x00, 0x01, 0x00, 0x01, 0x02, 0x00,  // ........
                        /* 0008 */  0x00, 0x01, 0x06, 0x00, 0x80, 0x1A, 0x06, 0x00,  // ........
                        /* 0010 */  0x00, 0x00, 0x5C, 0x5F, 0x53, 0x42, 0x2E, 0x50,  // ..\_SB.P
                        /* 0018 */  0x43, 0x30, 0x30, 0x2E, 0x49, 0x32, 0x43, 0x36,  // C00.I2C6
                        /* 0020 */  0x00, 0x79, 0x00                                 // .y.
                    })
                    CreateWordField (IIC6, 0x10, DAD6)
                    CreateDWordField (IIC6, 0x0C, DSP6)
                    DAD6 = Arg1
                    DSP6 = Arg2
                    Return (IIC6) /* \_SB_.PC00.I2CM.IIC6 */
                }
                Case (0x0D)
                {
                    Name (IIC7, Buffer (0x23)
                    {
                        /* 0000 */  0x8E, 0x1E, 0x00, 0x01, 0x00, 0x01, 0x02, 0x00,  // ........
                        /* 0008 */  0x00, 0x01, 0x06, 0x00, 0x80, 0x1A, 0x06, 0x00,  // ........
                        /* 0010 */  0x00, 0x00, 0x5C, 0x5F, 0x53, 0x42, 0x2E, 0x50,  // ..\_SB.P
                        /* 0018 */  0x43, 0x30, 0x30, 0x2E, 0x49, 0x32, 0x43, 0x37,  // C00.I2C7
                        /* 0020 */  0x00, 0x79, 0x00                                 // .y.
                    })
                    CreateWordField (IIC7, 0x10, DAD7)
                    CreateDWordField (IIC7, 0x0C, DSP7)
                    DAD7 = Arg1
                    DSP7 = Arg2
                    Return (IIC7) /* \_SB_.PC00.I2CM.IIC7 */
                }
                Default
                {
                    Return (Zero)
                }

            }
        }

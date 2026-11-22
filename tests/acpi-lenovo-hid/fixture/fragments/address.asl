If (((IM01 == One) || (IM01 == Zero)))
            {
                Method (_ADR, 0, NotSerialized)  // _ADR: Address
                {
                    Return (0x00150001)
                }
            }

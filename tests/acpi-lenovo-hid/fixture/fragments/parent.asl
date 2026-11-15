Name (I2CN, Zero)
        Name (I2CX, Zero)
        Name (I2CI, One)
        Method (_INI, 0, NotSerialized)  // _INI: Initialize
        {
            I2CN = SDS1 /* \SDS1 */
            I2CX = One
        }

        

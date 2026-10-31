DefinitionBlock ("", "DSDT", 2, "CUBIT", "SLEEPOWN", 1) {
Name (CALL, Zero)
Name (_S0, Package () { Zero, Zero, Zero, Zero })
Name (_S1, Package () { One, 2, Zero, Zero })
Method (_S3, 0) { Increment (CALL) Return (Package () { 3, 4, Zero, Zero }) }
Method (_S4, 0) { Return (Package () { CALL, Zero, Zero, Zero }) }
Name (_S5, Package () { 5, 6, Zero, Zero })
}

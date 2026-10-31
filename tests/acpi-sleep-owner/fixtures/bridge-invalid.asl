DefinitionBlock ("", "DSDT", 2, "CUBIT", "SLEEPOWN", 1) {
Name (_S0, Package () { Zero, Zero, Zero, Zero })
Name (BAD0, 8)
Method (_S1, 0) { Return (Package () { BAD0, Zero, Zero, Zero }) }
Method (_S3, 0) { Fatal (Zero, One, Zero) Return (Package () { Zero, Zero, Zero, Zero }) }
}

DefinitionBlock ("", "DSDT", 2, "CUBIT", "SLEEPOWN", 1) {
Name (_S0, Package () { Zero, Zero, Zero, Zero })
Method (_S1, 0) { Fatal (Zero, One, Zero) Return (Package () { Zero, Zero, Zero, Zero }) }
Method (_S3, 0) { Fatal (Zero, 2, Zero) Return (Package () { Zero, Zero, Zero, Zero }) }
}

DefinitionBlock ("", "DSDT", 2, "CUBIT", "BUSYREL", 1) {
 Name (PKG0, Package () {One})
 Method (BSY0,0) { Stall (One) Return (One) }
}

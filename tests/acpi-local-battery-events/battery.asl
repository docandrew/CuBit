DefinitionBlock ("", "DSDT", 2, "CUBIT", "BATTEVNT", 1) {
 Name (ST0, 0x1F) Name (BCNT, Zero) Name (SELF, /*SELF*/) Name (CHNG, /*CHNG*/)
 Method (XPKG, 0) { /*BAD_BIX*/ Return (Package (20) {0,1,42000,40000,1,11000,2000,1000,0xFFFFFFFF,100000,0xFFFFFFFF,10,500,5,1,10,"CUBIT","S1","LION",""}) }
 Method (BPKG, 0) { Return (Package (13) {1,42000,40000,1,11000,2000,1000,1,10,"CUBIT","S2","LION",""}) }
 Device (BAT0) { Name (_HID, "/*HID0*/") Method (_STA, 0) { Return (\ST0) }
   Method (_BIX, 0) { If (\CHNG) { Store (0x0F, \ST0) } Return (XPKG ()) }
   Method (_BIF, 0) { Return (BPKG ()) }
   Method (_BST, 0) { Increment (\BCNT) If (LAnd (\SELF, LEqual (\BCNT, One))) { Notify (BAT0, 0x80) } Return (Package (4) {2,10,20000,11000}) }
 }
 Device (BAT1) { Name (_HID, "/*HID1*/") Name (_STA, /*STA1*/)
   Method (_BIF, 0) { Return (BPKG ()) } Method (_BST, 0) { Return (Package (4) {2,10,20000,11000}) }
 }
 /*EXTRA*/
}

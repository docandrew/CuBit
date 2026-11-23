DefinitionBlock ("", "DSDT", 2, "CUBIT", "PKGCOPY", 1)
{
    Name (ERRS, Zero)
    Name (RMRC, 3)
    Name (RP0P, Package (3) {
        ":STST:package-independent:PASS:",
        ":STST:buffer-slot-copy:PASS:",
        ":STST:string-slot-copy:PASS:"
    })
    Name (PKGS, Package (1) { Buffer (2) {0x11, 0x22} })
    Name (PKGD, Package (1) { Zero })
    Method (MN00, 0)
    {
        Store (PKGS, Index (PKGD, Zero))
        Store (0x55, Index (DerefOf (Index (PKGS, Zero)), Zero))
        Store (DerefOf (Index (PKGD, Zero)), Local0)
        Store (DerefOf (Index (Local0, Zero)), Local1)
        If (LNotEqual (DerefOf (Index (Local1, Zero)), 0x11)) {
            Increment (ERRS)
        }
        Store (Buffer (2) {0x12, 0x34}, Index (PKGD, Zero))
        Store (DerefOf (Index (PKGD, Zero)), Local0)
        If (LNotEqual (DerefOf (Index (Local0, One)), 0x34)) {
            Increment (ERRS)
        }
        Store ("abc", Index (PKGD, Zero))
        If (LNotEqual (SizeOf (DerefOf (Index (PKGD, Zero))), 3)) {
            Increment (ERRS)
        }
        Return (Zero)
    }
}

/* Read-only AML reach gate. Status D has no read-to-clear interrupt effect.
 * Compile with the pinned IASL at test time; do not commit a binary. */
DefinitionBlock ("", "SSDT", 2, "CUBIT ", "CMOSRCH", 1)
{
    OperationRegion (ZCMR, SystemCMOS, 0, 0x40)
    Field (ZCMR, ByteAcc, NoLock, Preserve)
    {
        Offset (0x0D), ZCMD, 8
    }
    Name (ZCMV, Zero)
    Store (ZCMD, ZCMV)
}

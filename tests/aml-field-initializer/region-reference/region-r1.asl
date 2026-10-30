DefinitionBlock ("", "DSDT", 1, "CUBIT", "REGINIT", 1)
{
    Name (CALL, Zero) Name (LAST, Zero) Name (CONN, Zero)
    Name (DONE, Zero) Name (ROOT, Zero) Name (SEQN, Zero)
    OperationRegion (MEM0, SystemMemory, 0x4000, One)
    Field (MEM0, ByteAcc, NoLock, Preserve) { HWFL, 8 }
    Method (_REG, 2, NotSerialized) { Increment (ROOT) }
    Method (_INI, 0, NotSerialized) { Increment (SEQN) }
    Scope (\_SB) {
        Method (_INI, 0, NotSerialized) { Increment (\SEQN) }
        Device (DEV0) {
            Name (_HID, "CUB0001")
            OperationRegion (EC00, EmbeddedControl, Zero, One)
            OperationRegion (EC01, EmbeddedControl, Zero, One)
            Method (_REG, 2, NotSerialized) {
                Increment (\CALL)
                Store (Arg0, \LAST) Store (Arg1, \CONN) Store (Arg1, \HWFL)
            }
            Method (_STA, 0, NotSerialized) { Return (0x0F) }
            Method (_INI, 0, NotSerialized) { Store (\CALL, \DONE) }
        }
    }
    Method (RSET, 0, NotSerialized) {
        Store (Zero, CALL) Store (Zero, LAST) Store (Zero, CONN)
        Store (Zero, DONE) Store (Zero, ROOT) Store (Zero, SEQN) Store (Zero, HWFL)
    }
    Method (QCNT, 0, NotSerialized) { Return (CALL) }
    Method (QSPC, 0, NotSerialized) { Return (LAST) }
    Method (QCON, 0, NotSerialized) { Return (CONN) }
    Method (QVAL, 0, NotSerialized) { Return (HWFL) }
    Method (QDON, 0, NotSerialized) { Return (DONE) }
    Method (QROT, 0, NotSerialized) { Return (ROOT) }
    Method (QSEQ, 0, NotSerialized) { Return (SEQN) }
}

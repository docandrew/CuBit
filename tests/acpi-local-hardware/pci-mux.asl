DefinitionBlock ("", "DSDT", 2, "CUBIT", "POOLCOMP", 1)
{
    Name (BASE, 0xFED00000)
    Name (LEN0, 0x20)
    Method (GMHB, 0) { Return (BASE) }
    OperationRegion (MBAR, SystemMemory, Add (GMHB (), 0x10), LEN0)
    Field (MBAR, ByteAcc, Lock, Preserve)
    {
        DAT0, 8,
        Offset (3), FRES, 8,
        Offset (7), LOW4, 4,
        Offset (8), WIDE, 64
    }
    OperationRegion (IOP0, SystemIO, 0x0600, 1)
    Field (IOP0, ByteAcc, NoLock, Preserve) { SEL0, 8 }
    BankField (MBAR, SEL0, 3, ByteAcc, Lock, Preserve) { Offset (16), BANK, 8 }
    Name (BOOT, Zero)
    Store (DAT0, BOOT)
    Store (0x5A, DAT0)
    Name (ANS0, Zero)
    Store (DAT0, ANS0)
    Store (0x12345678, WIDE)
    Store (0x0A, LOW4)
    Store (0xAB, BANK)
    Method (BRW0, 1) { Store (Arg0, BANK) Return (BANK) }
    Method (FRD0, 1) { Store (Arg0, FRES) Return (FRES) }
    BankField (MBAR, SEL0, 13, ByteAcc, Lock, Preserve) { Offset (18), FBNK, 8 }
    Method (FBK0, 1) { Store (Arg0, FBNK) Return (FBNK) }
    Field (MBAR, ByteAcc, Lock, Preserve) { Offset (20), FDAT, 8 }
    IndexField (SEL0, FDAT, ByteAcc, Lock, Preserve) { FIND, 8 }
    Method (FIR0, 1) { Store (Arg0, FIND) Return (FIND) }
    Method (DBW0, 1)
    {
        BankField (MBAR, SEL0, Arg0, ByteAcc, Lock, Preserve) { Offset (22), DBNK, 8 }
        Store (Arg0, DBNK)
        Return (DBNK)
    }
    Method (DIW0, 1)
    {
        IndexField (SEL0, FDAT, ByteAcc, Lock, Preserve) { DIDX, 8 }
        Store (Arg0, DIDX)
        Return (DIDX)
    }
    Method (WRW0, 1) { Store (Arg0, WIDE) Return (WIDE) }
    Name (NBAS, 0x80000000)
    Name (NLEN, 0x20)
    OperationRegion (NVRS, SystemMemory, Add (NBAS, 0x10), NLEN)
    BankField (NVRS, SEL0, 7, ByteAcc, Lock, Preserve) { NVD0, 8 }
    Name (NANS, Zero)
    Store (NVD0, NANS)
    Method (NRD0, 1) { Return (NVD0) }
    Method (NWR0, 1) { Store (Arg0, NVD0) Return (NVD0) }
    OperationRegion (NCTL, SystemMemory, Add (NBAS, 0x1000), NLEN)
    BankField (NCTL, SEL0, 9, ByteAcc, Lock, Preserve) { CTBY, 8 }
    Method (CFWR, 1) { Store (Arg0, CTBY) Return (CTBY) }
    OperationRegion (PCIR, SystemMemory, 0xE00FF000, LEN0)
    BankField (PCIR, SEL0, 11, ByteAcc, Lock, Preserve) { PCBY, 8 }
    Name (PANS, Zero)
    Store (PCBY, PANS)
    Method (PRD0, 1) { Return (PCBY) }
    Method (PWR0, 1) { Store (Arg0, PCBY) Return (PCBY) }
    OperationRegion (PCL0, SystemMemory, 0xDFFFFFFF, 2)
    Field (PCL0, ByteAcc, NoLock, Preserve) { CUT0, 8 }
    Method (CUTR, 1) { Return (CUT0) }
    OperationRegion (PCL1, SystemMemory, 0xE00FFFFF, 2)
    Field (PCL1, ByteAcc, NoLock, Preserve) { CUT1, 8 }
    Method (CUTE, 1) { Return (CUT1) }
    Device (PCI0)
    {
        Name (_HID, EisaId ("PNP0A08"))
        Name (_SEG, 17)
        Name (_BBN, 9)
        Device (DEV0)
        {
            Name (_ADR, 0x001F0007)
            OperationRegion (PCFG, PCI_Config, 0, 0x20)
            Field (PCFG, ByteAcc, Lock, Preserve) { VID0, 8 }
            Method (LRD0, 1) { Return (VID0) }
        }
    }
    Method (PART, 1) { Store (Arg0, LOW4) Return (LOW4) }
}

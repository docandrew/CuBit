DefinitionBlock ("", "DSDT", 2, "CUBIT", "PCIPROBE", 1) {
 Name (OUT0, Zero)
 Name (ALV0, Zero)
 Scope (\_SB) {
  Device (PC00) {
   Name (_HID, EisaId ("PNP0A08"))
   Name (_CID, EisaId ("PNP0A03"))
   Name (_SEG, 7)
   Name (_BBN, One)
   Device (MC) {
    Name (_ADR, Zero)
    OperationRegion (HBUS, PCI_Config, Zero, 0x100)
    Field (HBUS, DWordAcc, NoLock, Preserve) { Offset (0x60), , 26, PXBR, 6 }
    Store (PXBR, \OUT0)
   }
  }
 }
 OperationRegion (PCMM, SystemMemory, 0xE00D8000, 0x480)
 Field (PCMM, DWordAcc, NoLock, Preserve) { Offset (0x400), , 26, ALBR, 6 }
 Store (ALBR, ALV0)
}

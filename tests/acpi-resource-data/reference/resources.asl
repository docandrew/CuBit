DefinitionBlock ("", "DSDT", 2, "CUBIT", "RESFRAME", 1)
{
    Device (DVC0)
    {
        Name (_HID, EisaId ("PNP0C02"))
        Name (_CRS, ResourceTemplate () {})
    }
    Device (DVC1)
    {
        Name (_HID, EisaId ("PNP0C02"))
        Name (_CRS, ResourceTemplate ()
        {
            FixedIO (0x03F8, 0x08)
            IRQNoFlags () {5}
            Memory32Fixed (ReadWrite, 0xFEC00000, 0x1000)
        })
    }
    Device (DVC2)
    {
        Name (_HID, EisaId ("PNP0C02"))
        Name (_CRS, ResourceTemplate ()
        {
            VendorShort () {0xAA, 0xBB}
            VendorLong () {0xCC, 0xDD}
        })
    }
    Device (DVC3)
    {
        Name (_HID, EisaId ("PNP0C02"))
        Name (_CRS, ResourceTemplate ()
        {
            StartDependentFnNoPri ()
            {
                FixedIO (0x02F8, 0x08)
            }
            EndDependentFn ()
        })
    }
    Device (DVC4)
    {
        Name (_HID, EisaId ("PNP0C02"))
        Name (_CRS, ResourceTemplate ()
        {
            IO (Decode16, 0x1200, 0xFE00, 0x10, 0x08)
            IRQ (Edge, ActiveHigh, SharedAndWake) {5}
            IRQ (Level, ActiveLow, SharedAndWake) {9}
        })
    }
    Device (DVC5)
    {
        Name (_HID, EisaId ("PNP0C02"))
        Name (_CRS, ResourceTemplate ()
        {
            Interrupt (ResourceConsumer, Level, ActiveLow, SharedAndWake) {0x10203040}
        })
    }
    Device (DVC6)
    {
        Name (_HID, EisaId ("PNP0C02"))
        Name (_PRS, ResourceTemplate ()
        {
            Interrupt (ResourceProducer, Edge, ActiveLow, Exclusive) {16, 255, 0xFFFFFFFF}
        })
    }
    Device (DVC7)
    {
        Name (_HID, EisaId ("PNP0C02"))
        Name (_CRS, ResourceTemplate ()
        {
            Interrupt (ResourceConsumer, Edge, ActiveHigh, SharedAndWake, 2, "\\_SB.PCI0") {33}
        })
    }
    Device (DVC8)
    {
        Name (_HID, EisaId ("PNP0C02"))
        Name (_CRS, ResourceTemplate ()
        {
            WordBusNumber (ResourceProducer, MinFixed, MaxFixed, PosDecode,
                0, 0, 0xFF, 0, 0x100)
            DWordMemory (ResourceProducer, PosDecode, MinFixed, MaxFixed,
                NonCacheable, ReadWrite, 0, 0x80000000, 0x8FFFFFFF, 0, 0x10000000)
            QWordMemory (ResourceProducer, PosDecode, MinFixed, MaxFixed,
                Prefetchable, ReadWrite, 0, 0x100000000, 0x1FFFFFFFF, 0, 0x100000000)
        })
    }
    Device (DVC9)
    {
        Name (_HID, EisaId ("PNP0C02"))
        Name (_CRS, ResourceTemplate ()
        {
            WordIO (ResourceProducer, MinFixed, MaxFixed, PosDecode, EntireRange,
                0, 0x1000, 0x1FFF, 0, 0x1000, 7, "\\_SB.PCI0")
            DWordIO (ResourceProducer, MinFixed, MaxFixed, PosDecode, EntireRange,
                0, 0x10000, 0x1FFFF, 0x100000, 0x10000, 3, "\\_SB.PCI0")
            QWordIO (ResourceProducer, MinFixed, MaxFixed, PosDecode, EntireRange,
                0, 0x100000000, 0x100000FFF, 0, 0x1000, 0, "\\_SB.PCI0")
        })
    }
    Device (DVCA)
    {
        Name (_HID, EisaId ("PNP0C02"))
        Name (_CRS, ResourceTemplate ()
        {
            Memory24 (ReadWrite, 0x1000, 0x2000, 0x1000, 0x100)
            Memory24 (ReadOnly, 0x3000, 0x3000, 0, 1)
        })
    }
    Device (DVCB)
    {
        Name (_HID, EisaId ("PNP0C02"))
        Name (_CRS, ResourceTemplate ()
        {
            Memory32 (ReadWrite, 0x80000000, 0x90000000, 0x1000, 0x100000)
            Memory32 (ReadOnly, 0, 0xFFF, 0, 0x1000)
        })
    }
    Device (DVCC)
    {
        Name (_HID, EisaId ("PNP0C02"))
        Name (_CRS, ResourceTemplate ()
        {
            Register (SystemMemory, 16, 8, 0x123456789ABCDEF0, 3)
            Register (SystemIO, 8, 0, 0x3F8, 1)
            Register (FFixedHW, 64, 0, 0, 0)
        })
    }
    Device (DVCD)
    {
        Name (_HID, EisaId ("PNP0C02"))
        Name (_CRS, ResourceTemplate ()
        {
            DMA (Compatibility, NotBusMaster, Transfer8) {2}
            DMA (TypeF, BusMaster, Transfer8_16) {1, 3, 7}
            FixedDMA (0x1234, 0xABCD, Width256Bit)
        })
    }
    Device (DVCE)
    {
        Name (_HID, EisaId ("PNP0C02"))
        Name (_PRS, ResourceTemplate ()
        {
            VendorShort () {0xAA}
            StartDependentFn (0, 1)
            {
                FixedIO (0x03F8, 8)
                IRQNoFlags () {5}
            }
            StartDependentFn (1, 0)
            {
                FixedIO (0x02F8, 8)
                IRQNoFlags () {9}
            }
            EndDependentFn ()
            VendorShort () {0xBB}
        })
    }
    Device (DVCF)
    {
        Name (_HID, EisaId ("PNP0C02"))
        Name (_CRS, ResourceTemplate ()
        {
            ExtendedMemory (ResourceConsumer, PosDecode, MinFixed, MaxFixed,
                Cacheable, ReadWrite, 0, 0x200000000, 0x2000FFFFF, 0, 0x100000, 8)
            ExtendedIO (ResourceProducer, MinFixed, MaxFixed, PosDecode, EntireRange,
                0, 0x5000, 0x5FFF, 0, 0x1000, 0)
            ExtendedSpace (0xC0, ResourceConsumer, PosDecode, MinFixed, MaxFixed,
                0xA5, 0, 0x6000, 0x6FFF, 0, 0x1000, 0)
        })
    }
    Device (GPI0)
    {
        Name (_HID, EisaId ("PNP0C02"))
        Name (_CRS, ResourceTemplate ()
        {
            GpioInt (Edge, ActiveBoth, SharedAndWake, PullUp, 1234, "\\_SB.GPO0", 0,
                ResourceConsumer, , RawDataBuffer () {0xAA, 0x55}) {0x1234}
            GpioIo (Shared, PullDown, 4321, 5678, IoRestrictionOutputOnly,
                "GPO1", 0, ResourceConsumer) {0, 0x1234, 0xFFFF}
            GpioIo (Exclusive, 0x80, 0, 65535, IoRestrictionNoneAndPreserve,
                "^GPO2", 0, ResourceProducer, , RawDataBuffer () {0xDE, 0xAD}) {0xABCD, 0xABCE}
        })
    }
    Device (RCQ0)
    {
        Name (_HID, EisaId ("PNP0C02"))
        Name (CNT0, Zero)
        Method (_CRS, 0) { Increment (CNT0) Return (ResourceTemplate () {FixedIO (0x03F8, 8)}) }
        Name (BCHK, Buffer () {0x79, 1})
        Name (BEND, Buffer () {0x4B, 0xF8, 3, 8})
        Name (BTRL, Buffer () {0x79, 0, 0xAA})
        Name (BSHT, Buffer (0) {})
        Name (BFRM, Buffer () {0x80})
        Name (BSTR, "text")
        Name (BPKG, Package () {1})
    }
    Device (PIRQ)
    {
        Name (_HID, EisaId ("PNP0A03"))
        Name (_PRT, Package ()
        {
            Package () {0x0000FFFF, 0, Zero, 23},
            Package () {0x0002FFFF, 1, LNKA, 0},
            Package () {0x0003FFFF, 2, \PIRQ.LNKA, 0},
            Package () {0x001FFFFF, 3, ALNK, 0}
        })
        Device (LNKA)
        {
            Name (_HID, EisaId ("PNP0C0F"))
            Name (_CRS, ResourceTemplate () { IRQNoFlags () {5} })
        }
        Alias (LNKA, ALNK)
        Name (CNTM, Zero)
        Method (MTH0, 0) { Increment (CNTM) Return (One) }
        Method (SNAP, 0) { CopyObject (LNKA, Local0) Return (Local0) }
        Method (RTPK, 0) { Return (Package () {MTH0}) }
        Method (RUNC, 0) { Return (MTH0 ()) }
        Method (RPNM, 0) { Name (TEMP, Package () {MTH0}) Return (TEMP) }
        Name (DAT0, Buffer () {1, 2})
        Name (NPKG, "not a routing package")
        Name (BADT, Package ()
        {
            One,
            Package () {0x1FFFF, 0, Zero},
            Package () {0x1FFFF, 0, Zero, 23, 0},
            Package () {"address", 0, Zero, 23},
            Package () {0x10000, 0, Zero, 23},
            Package () {0x2000FFFF, 0, Zero, 23},
            Package () {0x1FFFF, 4, Zero, 23},
            Package () {0x1FFFF, 0, One, 23},
            Package () {0x1FFFF, 0, "LNKA", 0},
            Package () {0x1FFFF, 0, MTH0, 0},
            Package () {0x1FFFF, 0, DAT0, 0},
            Package () {0x1FFFF, 0, Zero, "index"},
            Package () {0x1FFFF, 0, Zero, 0x100000000},
            Package () {0x10001FFFF, 0, Zero, 23},
            Package () {0x1FFFF, 0x100000000, Zero, 23},
            Package (4) {},
            Package () {},
            Package () {0x1FFFF, 0, LNKA, 0xFFFFFFFF}
        })
    }
    Device (SBL0)
    {
        Name (_HID, EisaId ("PNP0C02"))
        Name (_CRS, ResourceTemplate ()
        {
            I2cSerialBus (0x5A, ControllerInitiated, 400000, AddressingMode7Bit,
                "\\_SB.I2C0", 3, ResourceConsumer, I2A0, RawDataBuffer () {0xDA, 0xDB})
            I2cSerialBusV2 (0x2FF, DeviceInitiated, 1000000, AddressingMode10Bit,
                "\\_SB.I2C1", 2, ResourceProducer, I2A1, Shared, RawDataBuffer () {0x11})
            SpiSerialBus (0xBEEF, PolarityHigh, ThreeWireMode, 16, ControllerInitiated,
                10000000, ClockPolarityHigh, ClockPhaseSecond, "\\_SB.SPI0", 0,
                ResourceConsumer, SPA0, RawDataBuffer () {0xAA, 0x55})
            SpiSerialBusV2 (1, PolarityLow, FourWireMode, 8, DeviceInitiated,
                25000000, ClockPolarityLow, ClockPhaseFirst, "\\_SB.SPI1", 0,
                ResourceProducer, SPA1, Shared, RawDataBuffer () {0x7E})
            UartSerialBus (115200, DataBitsEight, StopBitsOne, 0xFC, BigEndian,
                ParityTypeOdd, FlowControlHardware, 64, 128, "\\_SB.UAR0", 0,
                ResourceConsumer, UAA0, RawDataBuffer () {0x12, 0x34})
            UartSerialBusV2 (921600, DataBitsNine, StopBitsTwo, 0x80, LittleEndian,
                ParityTypeEven, FlowControlXon, 0x1234, 0xFEDC, "\\_SB.UAR1", 0,
                ResourceProducer, UAA1, Shared, RawDataBuffer () {0xFE})
            Csi2Bus (DeviceInitiated, 1, 17, "\\_SB.CSI0", 7,
                ResourceConsumer, CSA0, RawDataBuffer () {0xAB, 0xCD})
        })
    }
    Device (PIN0)
    {
        Name (_HID, EisaId ("PNP0C02"))
        Name (_CRS, ResourceTemplate ()
        {
            PinFunction (Shared, PullDown, 0xBEEF, "\\PIN0", 0,
                ResourceConsumer, PFN0, RawDataBuffer () {0xDA, 0xDB}) {0, 0x1234, 0xFFFF}
            PinConfig (Exclusive, 0x0D, 1, "\\PIN0", 0,
                ResourceConsumer, PCF0, RawDataBuffer () {0x11}) {1, 2}
            PinGroup ("GRP", ResourceProducer, PGR0,
                RawDataBuffer () {0xAA, 0x55}) {5, 6, 7}
            PinGroupFunction (Shared, 0x1234, "\\PIN0", 0, "GRP",
                ResourceConsumer, PGF0, RawDataBuffer () {0x22})
            PinGroupConfig (Exclusive, 0x80, 0xFFFFFFFF, "\\PIN0", 0, "GRP",
                ResourceProducer, PGC0, RawDataBuffer () {0x33, 0x44})
        })
    }
    Device (CLK0)
    {
        Name (_HID, EisaId ("PNP0C02"))
        Name (_CRS, ResourceTemplate ()
        {
            ClockInput (100, 3, MHz, Fixed,,)
            ClockInput (19200, 1, Hz, Variable, "\\_SB.CLK4", 3)
            ClockInput (0xFFFFFFFF, 0xFFFF, KHz, Fixed, "\\_SB.CLK5", 0)
        })
    }
}

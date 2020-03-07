-------------------------------------------------------------------------------
-- CubitOS
-- Copyright (C) 2019 Jon Andrew
--
-- Local APIC - Advanced Programmable Interrupt Controller
--
-------------------------------------------------------------------------------
-- for "+" on System.Address
with System.Storage_Elements; use System.Storage_Elements;

with BootAllocator;
with BuddyAllocator;
with cmos;
with Mem_mgr;
with Textmode;
with Time;
with x86;

package body Lapic
    with SPARK_Mode => On
is
    type LAPICRegister is new Unsigned_32;
    
    -- type LAPICRegisterRW is access all      LAPICRegister;
    -- type LAPICRegisterRO is access constant LAPICRegister;
    -- subtype LAPICRegisterWO is LAPICRegisterRW;

    ---------------------------------------------------------------------------
    -- Offsets for the various LAPIC registers from the base address.
    -- See Vol 3, Section 10.4.1 in Intel x86 manual for descriptions
    ---------------------------------------------------------------------------
    id                  : LAPICRegister with Import, 
        Address => To_Address(To_Integer(baseAddr) + 16#0020#);
    version             : LAPICRegister with Import, 
        Address => To_Address(To_Integer(baseAddr) + 16#0030#);
    tpr                 : LAPICRegister with Import, 
        Address => To_Address(To_Integer(baseAddr) + 16#0080#);
    apr                 : LAPICRegister with Import, 
        Address => To_Address(To_Integer(baseAddr) + 16#0090#);
    ppr                 : LAPICRegister with Import, 
        Address => To_Address(To_Integer(baseAddr) + 16#00A0#);
    eoi                 : LAPICRegister with Import, 
        Address => To_Address(To_Integer(baseAddr) + 16#00B0#);
    rrd                 : LAPICRegister with Import, 
        Address => To_Address(To_Integer(baseAddr) + 16#00C0#);
    ldr                 : LAPICRegister with Import, 
        Address => To_Address(To_Integer(baseAddr) + 16#00D0#);
    dfr                 : LAPICRegister with Import, 
        Address => To_Address(To_Integer(baseAddr) + 16#00E0#);
    svr                 : LAPICRegister with Import, 
        Address => To_Address(To_Integer(baseAddr) + 16#00F0#);
    isr0                : LAPICRegister with Import, 
        Address => To_Address(To_Integer(baseAddr) + 16#0100#);
    isr1                : LAPICRegister with Import, 
        Address => To_Address(To_Integer(baseAddr) + 16#0110#);
    isr2                : LAPICRegister with Import, 
        Address => To_Address(To_Integer(baseAddr) + 16#0120#);
    isr3                : LAPICRegister with Import, 
        Address => To_Address(To_Integer(baseAddr) + 16#0130#);
    isr4                : LAPICRegister with Import, 
        Address => To_Address(To_Integer(baseAddr) + 16#0140#);
    isr5                : LAPICRegister with Import, 
        Address => To_Address(To_Integer(baseAddr) + 16#0150#);
    isr6                : LAPICRegister with Import, 
        Address => To_Address(To_Integer(baseAddr) + 16#0160#);
    isr7                : LAPICRegister with Import, 
        Address => To_Address(To_Integer(baseAddr) + 16#0170#);
    tmr0                : LAPICRegister with Import, 
        Address => To_Address(To_Integer(baseAddr) + 16#0180#);
    tmr1                : LAPICRegister with Import, 
        Address => To_Address(To_Integer(baseAddr) + 16#0190#);
    tmr2                : LAPICRegister with Import, 
        Address => To_Address(To_Integer(baseAddr) + 16#01A0#);
    tmr3                : LAPICRegister with Import, 
        Address => To_Address(To_Integer(baseAddr) + 16#01B0#);
    tmr4                : LAPICRegister with Import, 
        Address => To_Address(To_Integer(baseAddr) + 16#01C0#);
    tmr5                : LAPICRegister with Import, 
        Address => To_Address(To_Integer(baseAddr) + 16#01D0#);
    tmr6                : LAPICRegister with Import, 
        Address => To_Address(To_Integer(baseAddr) + 16#01E0#);
    tmr7                : LAPICRegister with Import, 
        Address => To_Address(To_Integer(baseAddr) + 16#01F0#);
    irr0                : LAPICRegister with Import, 
        Address => To_Address(To_Integer(baseAddr) + 16#0200#);
    irr1                : LAPICRegister with Import, 
        Address => To_Address(To_Integer(baseAddr) + 16#0210#);
    irr2                : LAPICRegister with Import, 
        Address => To_Address(To_Integer(baseAddr) + 16#0220#);
    irr3                : LAPICRegister with Import, 
        Address => To_Address(To_Integer(baseAddr) + 16#0230#);
    irr4                : LAPICRegister with Import, 
        Address => To_Address(To_Integer(baseAddr) + 16#0240#);
    irr5                : LAPICRegister with Import, 
        Address => To_Address(To_Integer(baseAddr) + 16#0250#);
    irr6                : LAPICRegister with Import, 
        Address => To_Address(To_Integer(baseAddr) + 16#0260#);
    irr7                : LAPICRegister with Import, 
        Address => To_Address(To_Integer(baseAddr) + 16#0270#);
    esr                 : LAPICRegister with Import, 
        Address => To_Address(To_Integer(baseAddr) + 16#0280#);
    lvtCMCI             : LAPICRegister with Import, 
        Address => To_Address(To_Integer(baseAddr) + 16#02F0#);
    icr0                : LAPICRegister with Import, 
        Address => To_Address(To_Integer(baseAddr) + 16#0300#);
    icr1                : LAPICRegister with Import, 
        Address => To_Address(To_Integer(baseAddr) + 16#0310#);
    lvtTimer            : LAPICRegister with Import, 
        Address => To_Address(To_Integer(baseAddr) + 16#0320#);
    lvtThermalSensor    : LAPICRegister with Import, 
        Address => To_Address(To_Integer(baseAddr) + 16#0330#);
    lvtPerfMon          : LAPICRegister with Import, 
        Address => To_Address(To_Integer(baseAddr) + 16#0340#);
    lvtLINT0            : LAPICRegister with Import, 
        Address => To_Address(To_Integer(baseAddr) + 16#0350#);
    lvtLINT1            : LAPICRegister with Import, 
        Address => To_Address(To_Integer(baseAddr) + 16#0360#);
    lvtError            : LAPICRegister with Import, 
        Address => To_Address(To_Integer(baseAddr) + 16#0370#);
    timerInitialCount   : LAPICRegister with Import, 
        Address => To_Address(To_Integer(baseAddr) + 16#0380#);
    timerCurrentCount   : LAPICRegister with Import, 
        Address => To_Address(To_Integer(baseAddr) + 16#0390#);
    timerDivideConf     : LAPICRegister with Import, 
        Address => To_Address(To_Integer(baseAddr) + 16#03E0#);

    ---------------------------------------------------------------------------
    -- Spurious vector: setting bit 8 enables the APIC,
    -- Low byte is the spurious vector number, lowest nibble is all 1s on
    -- earlier processors. (Cubit uses 0xFF for the spurious vector)
    ---------------------------------------------------------------------------
    SVR_ENABLE          : constant Unsigned_32 := 16#0000_0100#;

    ---------------------------------------------------------------------------
    -- Values for the Interrupt Command Register low dword (icr0):
    -- Low byte is the vector.
    ---------------------------------------------------------------------------
    -- Bits 8-10: Delivery mode
    ICR_FIXED           : constant Unsigned_32 := 16#0000_0000#;
    ICR_LOWEST          : constant Unsigned_32 := 16#0000_0100#;
    ICR_SMI             : constant Unsigned_32 := 16#0000_0200#;
    -- (16#0000_0300# is reserved)
    ICR_NMI             : constant Unsigned_32 := 16#0000_0400#;
    ICR_INIT            : constant Unsigned_32 := 16#0000_0500#;
    ICR_STARTUP         : constant Unsigned_32 := 16#0000_0600#;    -- SIPI
    -- (16#0000_0700# is reserved)
    
    -- Bit 12: delivery status (idle vs send pending)
    ICR_IDLE            : constant Unsigned_32 := 16#0000_0000#;
    ICR_SEND_PENDING    : constant Unsigned_32 := 16#0000_1000#;

    -- Bit 14: level 0 = de-assert, 1 = assert
    ICR_LEVEL_ASSERT    : constant Unsigned_32 := 16#0000_4000#;
    ICR_LEVEL_DEASSERT  : constant Unsigned_32 := 16#0000_0000#;

    -- Bit 15: trigger mode 0 = edge, 1 = level
    ICR_TRIGGER_EDGE    : constant Unsigned_32 := 16#0000_0000#;
    ICR_TRIGGER_LEVEL   : constant Unsigned_32 := 16#0000_8000#;

    -- Bits 18-19: Destination
    -- 00: n/a, 01: self, 10: everybody, 11: everybody except this CPU
    ICR_DEST_SELF       : constant Unsigned_32 := 16#0004_0000#;
    ICR_DEST_BROADCAST  : constant Unsigned_32 := 16#0008_0000#;
    ICR_DEST_BCASTSELF  : constant Unsigned_32 := 16#000C_0000#;

    ---------------------------------------------------------------------------
    -- LVT masking, bit 16
    ---------------------------------------------------------------------------
    LVT_MASKED          : constant Unsigned_32 := 16#0001_0000#;

    ---------------------------------------------------------------------------
    -- Values for the LVT Timer Register (lvtTimer), bits 17-18
    ---------------------------------------------------------------------------
    TIMER_ONESHOT       : constant Unsigned_32 := 16#0000_0000#;
    TIMER_PERIODIC      : constant Unsigned_32 := 16#0002_0000#;
    TIMER_DEADLINE      : constant Unsigned_32 := 16#0004_0000#;

    ---------------------------------------------------------------------------
    -- Values for the divide configuration register, bits 0-1 and 3
    ---------------------------------------------------------------------------
    DIVIDE_BY_2         : constant Unsigned_32 := 16#0000_0000#;
    DIVIDE_BY_4         : constant Unsigned_32 := 16#0000_0001#;
    DIVIDE_BY_8         : constant Unsigned_32 := 16#0000_0002#;
    DIVIDE_BY_16        : constant Unsigned_32 := 16#0000_0003#;
    DIVIDE_BY_32        : constant Unsigned_32 := 16#0000_0008#;
    DIVIDE_BY_64        : constant Unsigned_32 := 16#0000_0009#;
    DIVIDE_BY_128       : constant Unsigned_32 := 16#0000_000A#;
    DIVIDE_BY_1         : constant Unsigned_32 := 16#0000_000B#;

    ---------------------------------------------------------------------------
    -- timerInterval is the number of APIC ticks per ms on this system,
    -- set in setupLAPIC
    ---------------------------------------------------------------------------
    timerInterval : Unsigned_32;

    ---------------------------------------------------------------------------
    -- write - writes to a LAPIC register and then performs a read to ensure
    -- that the write is completed.
    -- @param reg - a writable LAPICRegister pointer
    -- @param val - value to write to the register
    ---------------------------------------------------------------------------
    procedure write(reg : in out LAPICRegister; val : in Unsigned_32);

    procedure write(reg : in out LAPICRegister; val : in Unsigned_32)
        with SPARK_Mode => On
    is
        ignore : Unsigned_32;
    begin
        reg := LAPICRegister(val);
        ignore := Unsigned_32(id);
    end write;

    ---------------------------------------------------------------------------
    -- goodPowerOnState
    -- Checks that the LAPIC registers are where we would expect them to be
    -- after a power-on or h/w reset. We use this to confirm that the LAPIC is
    -- correctly mapped and where we expect it to be.
    --
    -- Note: VirtualBox, Bochs and QEMU all disagree on what the power-up
    -- values of some of these LAPIC registers should be (commented out below)
    ---------------------------------------------------------------------------
    function goodPowerOnState return Boolean
        with SPARK_Mode => On
    is
    begin
            -- textmode.print("isr0: "); textmode.println(isr0.all);
            -- textmode.print("isr1: "); textmode.println(isr1.all);
            -- textmode.print("isr2: "); textmode.println(isr2.all);
            -- textmode.print("isr3: "); textmode.println(isr3.all);
            -- textmode.print("isr4: "); textmode.println(isr4.all);
            -- textmode.print("isr5: "); textmode.println(isr5.all);
            -- textmode.print("isr6: "); textmode.println(isr6.all);
            -- textmode.print("isr7: "); textmode.println(isr7.all);
            -- textmode.print("tmr0: "); textmode.println(tmr0.all);
            -- textmode.print("tmr1: "); textmode.println(tmr1.all);
            -- textmode.print("tmr2: "); textmode.println(tmr2.all);
            -- textmode.print("tmr3: "); textmode.println(tmr3.all);
            -- textmode.print("tmr4: "); textmode.println(tmr4.all);
            -- textmode.print("tmr5: "); textmode.println(tmr5.all);
            -- textmode.print("tmr6: "); textmode.println(tmr6.all);
            -- textmode.print("tmr7: "); textmode.println(tmr7.all);
            -- textmode.print("irr0: "); textmode.println(irr0.all);
            -- textmode.print("irr1: "); textmode.println(irr1.all);
            -- textmode.print("irr2: "); textmode.println(irr2.all);
            -- textmode.print("irr3: "); textmode.println(irr3.all);
            -- textmode.print("irr4: "); textmode.println(irr4.all);
            -- textmode.print("irr5: "); textmode.println(irr5.all);
            -- textmode.print("irr6: "); textmode.println(irr6.all);
            -- textmode.print("irr7: "); textmode.println(irr7.all);
            -- textmode.print("timerInitialCount: "); textmode.println(timerInitialCount.all);
            -- textmode.print("timerCurrentCount: "); textmode.println(timerCurrentCount.all);
            -- textmode.print("timerDivideConf:   "); textmode.println(timerDivideConf.all  );
            -- textmode.print("destFormat:        "); textmode.println(destFormat.all       );
            -- textmode.print("svr:               "); textmode.println(svr.all              );
            -- textmode.print("lvtCMCI:           "); textmode.println(lvtCMCI.all          );
            -- textmode.print("lvtLINT0:          "); textmode.println(lvtLINT0.all         );
            -- textmode.print("lvtLINT1:          "); textmode.println(lvtLINT1.all         );
            -- textmode.print("lvtError:          "); textmode.println(lvtError.all         );
            -- textmode.print("lvtPerfMon:        "); textmode.println(lvtPerfMon.all       );
            -- textmode.print("lvtThermalSensor:  "); textmode.println(lvtThermalSensor.all );

        -- Check for the power-up/reset state of these registers:
        return 
            isr0              = 0 and
            isr1              = 0 and
            isr2              = 0 and
            isr3              = 0 and
            isr4              = 0 and
            isr5              = 0 and
            isr6              = 0 and
            isr7              = 0 and
            tmr0              = 0 and
            tmr1              = 0 and
            tmr2              = 0 and
            tmr3              = 0 and
            tmr4              = 0 and
            tmr5              = 0 and
            tmr6              = 0 and
            tmr7              = 0 and
            irr0              = 0 and
            irr1              = 0 and
            irr2              = 0 and
            irr3              = 0 and
            irr4              = 0 and
            irr5              = 0 and
            irr6              = 0 and
            irr7              = 0 and
            timerInitialCount = 0 and
            timerCurrentCount = 0 and
            timerDivideConf   = 0 and
            dfr               = 16#FFFF_FFFF# and
            -- svr.all                 = 16#FF# and
            -- lvtCMCI.all             = LVT_MASKED and
            -- lvtLINT0.all            = LVT_MASKED and
            -- lvtLINT1.all            = LVT_MASKED and
            lvtError          = LAPICRegister(LVT_MASKED) and
            lvtPerfMon        = LAPICRegister(LVT_MASKED) and
            lvtThermalSensor  = LAPICRegister(LVT_MASKED);
    end;

    ---------------------------------------------------------------------------
    -- calibrateAPICTimer
    --
    -- @return number of APIC ticks / ms
    ---------------------------------------------------------------------------
    function calibrateAPICTimer return Unsigned_32
        with SPARK_Mode => On
    is
        APICTicksIn10ms : Unsigned_32;
    begin
        write(timerDivideConf, DIVIDE_BY_16);
        write(timerInitialCount, 16#FFFF_FFFF#);

        -- sleep for 10ms using the PIT
        Time.bootCalibrationSleep(10);

        -- stop the timer
        write(lvtTimer, LVT_MASKED);

        APICTicksIn10ms := 16#FFFF_FFFF# - Unsigned_32(timerCurrentCount);

        return APICTicksIn10ms / 10;
    end calibrateAPICTimer;

    ---------------------------------------------------------------------------
    -- setupLAPIC_BSP
    --
    -- Set up LAPIC for the bootstrap processor (BSP).
    --
    -- Maps the LAPIC registers into the higher-half of memory, and performs
    -- the timer calibration.
    -- 
    -- @param lapicPhysAddr - physical base address of the LAPIC registers
    ---------------------------------------------------------------------------
    procedure setupLAPIC_BSP
        with SPARK_Mode => On
    is
        RemapFail   : exception;

        remapped    : constant Virtmem.VirtAddress := 
            Virtmem.V2P(To_Integer(baseAddr));
        remappedOK  : Boolean;

        function mapIOFrame is new Mem_mgr.mapIOFrame(BuddyAllocator.allocFrame);
        --function mapIOFrame is new Mem_mgr.mapIOFrame(BootAllocator.allocFrame);
    begin
        -- re-map as an I/O page.
        Textmode.print("Re-mapping LAPIC registers to ");
        Textmode.println(remapped);
        remappedOK := mapIOFrame(remapped);

        if not remappedOK then
            raise RemapFail with "Unable to re-map LAPIC registers.";
            -- TODO: not a huge deal here if we have to fall back to PIC,
            -- but need to return an error code or something here to allow the
            -- fall-back.
        end if;

        -- Sanity-check the LAPIC registers using the known startup values.
        -- This is necessary if we ever default to the spec value 0xFEE0_0000#.
        if not goodPowerOnState then
            Textmode.println("WARNING: LAPIC Power-On State does not match what's expected.",
                Textmode.YELLOW, Textmode.BLACK);
        end if;

        -- Enable local APIC by setting spurious interrupt vector
        write(svr, SVR_ENABLE or Unsigned_32(InterruptNumbers.SPURIOUS));

        timerInterval := calibrateAPICTimer;
        Textmode.print(" APIC timer calibration: ");
        Textmode.printd(timerInterval);
        Textmode.println(" ticks/ms");

        -- Once finished calibrating, we can disable interrupts so we aren't
        -- getting PIC and APIC interrupts at the same time. Re-enable after
        -- we disable the PIC.
        x86.cli;

        setupLAPIC_AP;
    end setupLAPIC_BSP;

    ---------------------------------------------------------------------------
    -- Enable the LAPIC interrupts on this CPU
    ---------------------------------------------------------------------------
    procedure setupLAPIC_AP is
    begin
        write(timerDivideConf, DIVIDE_BY_16);
        write(lvtTimer, Unsigned_32(InterruptNumbers.TIMER) or TIMER_PERIODIC);
        write(timerInitialCount, timerInterval);

        -- Disable LINT0, LINT1
        write(lvtLINT0, Unsigned_32(lvtLINT0) and LVT_MASKED);
        write(lvtLINT1, Unsigned_32(lvtLINT1) and LVT_MASKED);

        -- clear error status reg
        write(esr, 0);
        write(esr, 0);

        -- if any interrupts are outstanding, clear them
        write(eoi, 0);

        -- Enable LAPIC interrupts (CPU interrupts still disabled until sti)
        write(lapic.tpr, 0);
    end setupLAPIC_AP;

    procedure finishIRQ
        with SPARK_Mode => On
    is
    begin
        write(eoi, 0);
    end finishIRQ;

    ---------------------------------------------------------------------------
    -- Notes: writing the icr0 causes the IPI to be sent.
    ---------------------------------------------------------------------------
    procedure sendIPI(cpuNum : in Unsigned_8; intCommand : in Unsigned_32) with
        SPARK_Mode => On
    is
    begin
        -- make sure no IPI is pending
        while (Unsigned_32(icr0) and ICR_SEND_PENDING) = ICR_SEND_PENDING loop
            null;
        end loop;

        -- specify the destination in the upper 8 bits of icr1
        --textmode.print("Writing to CPU: "); textmode.print(cpuNum);
        write(icr1, Shift_Left(Unsigned_32(cpuNum), 24));

        -- send the IPI
        --textmode.print(" Command: "); textmode.println(intCommand);
        write(icr0, intCommand);
    end;

    ---------------------------------------------------------------------------
    -- The warm reset vector specifies the boot address based on the CMOS
    -- "shutdown." It's a segment:offset address.
    ---------------------------------------------------------------------------
    warmResetVectorSeg : Unsigned_16 
            with Import, Address => To_Address(virtmem.P2V(16#0469#));
    warmResetVectorOff : Unsigned_16
            with Import, Address => To_Address(virtmem.P2V(16#0467#));

    ---------------------------------------------------------------------------
    -- setWarmResetVector
    --
    -- Set the warm reset vector (DWORD based at 40:67) to point to our AP
    --  startup code. Address: (Segment Value * 16) + offset value. Since the
    --  AP has to use an address in the lowest 64KiB anyhow, we just accept a
    --  U16 here and set the offset to 0.
    ---------------------------------------------------------------------------
    procedure setWarmResetVector(startAddr : in Unsigned_16) with
        SPARK_Mode => On
    is
    begin
        warmResetVectorSeg := Unsigned_16(Shift_Right(startAddr, 4));
        warmResetVectorOff := 0;
    end;

    ---------------------------------------------------------------------------
    -- clearWarmResetVector
    --
    -- Zero out the warm reset vector
    ---------------------------------------------------------------------------
    procedure clearWarmResetVector with
        SPARK_Mode => On
    is
    begin
        warmResetVectorSeg := 0;
        warmResetVectorOff := 0;
    end;

    ---------------------------------------------------------------------------
    -- Per Intel MP specification, 
    -- BSP must initialize BIOS shutdown code to 0xAH and warm reset vector
    -- (DWORD based at 40:67) to point to the AP startup code, then:
    --
    -- The universal start-up algorithm is:
    --  1. send AP the INIT IPI
    --  2. wait 10ms
    --  3. send AP the STARTUP IPI
    --  4. wait 200us
    --  5. if not initialized, wait 200us, and send another STARTUP IPI
    --
    -- INIT is edge-triggered, and vector field must be 0x00
    ---------------------------------------------------------------------------
    procedure bootAP(cpuNum : in Unsigned_8; startAddr : in Unsigned_16) with
        SPARK_Mode => On
    is
    begin
        cmos.write(cmos.ShutdownStatus, cmos.JMP_PTR_NO_EOI);
        setWarmResetVector(startAddr);

        sendIPI(cpuNum, ICR_INIT or ICR_TRIGGER_EDGE or ICR_LEVEL_ASSERT);
        time.sleep(10 * time.Milliseconds);

        sendIPI(cpuNum, ICR_STARTUP or ICR_TRIGGER_EDGE or 
                Shift_Right(Unsigned_32(startAddr), 12));

        time.sleep(200 * time.Microseconds);
        sendIPI(cpuNum, ICR_STARTUP or ICR_TRIGGER_EDGE or
                Shift_Right(Unsigned_32(startAddr), 12));
    end;

    ---------------------------------------------------------------------------
    -- LAPICRegisters subpackage
    -- Isolates the non-SPARK pointer magic we need to get to the 
    -- LAPIC registers using Access types.
    ---------------------------------------------------------------------------
    -- package body LAPICRegisters
    --     with SPARK_Mode => Off
    -- is

    --     package lapicAcc is new System.Address_To_Access_Conversions(LAPICRegister);

        -----------------------------------------------------------------------
        -- setupRegisters
        -- Determine addresses for the LAPIC access types
        -- @param lapicBaseAddr - the virtual, re-mapped I/O base address of
        --  the LAPIC page
        -----------------------------------------------------------------------
        -- procedure setupRegisters(lapicBase : in virtmem.VirtAddress)
        --     with SPARK_Mode => Off -- use of System.Address_to_Access_Conversions
        -- is
        --     --lapicBase : Integer_Address := To_Integer(lapicBaseAddr);
        -- begin
        --     id                  := LAPICRegisterRW(lapicAcc.To_Pointer(To_Address(lapicBase + 16#0020#)));
        --     version             := LAPICRegisterRO(lapicAcc.To_Pointer(To_Address(lapicBase + 16#0030#)));
        --     tpr                 := LAPICRegisterRW(lapicAcc.To_Pointer(To_Address(lapicBase + 16#0080#)));
        --     apr                 := LAPICRegisterRO(lapicAcc.To_Pointer(To_Address(lapicBase + 16#0090#)));
        --     ppr                 := LAPICRegisterRO(lapicAcc.To_Pointer(To_Address(lapicBase + 16#00A0#)));
        --     eoi                 := LAPICRegisterWO(lapicAcc.To_Pointer(To_Address(lapicBase + 16#00B0#)));
        --     rrd                 := LAPICRegisterRO(lapicAcc.To_Pointer(To_Address(lapicBase + 16#00C0#)));
        --     ldr                 := LAPICRegisterRW(lapicAcc.To_Pointer(To_Address(lapicBase + 16#00D0#)));
        --     dfr                 := LAPICRegisterRW(lapicAcc.To_Pointer(To_Address(lapicBase + 16#00E0#)));
        --     svr                 := LAPICRegisterRW(lapicAcc.To_Pointer(To_Address(lapicBase + 16#00F0#)));
        --     isr0                := LAPICRegisterRO(lapicAcc.To_Pointer(To_Address(lapicBase + 16#0100#)));
        --     isr1                := LAPICRegisterRO(lapicAcc.To_Pointer(To_Address(lapicBase + 16#0110#)));
        --     isr2                := LAPICRegisterRO(lapicAcc.To_Pointer(To_Address(lapicBase + 16#0120#)));
        --     isr3                := LAPICRegisterRO(lapicAcc.To_Pointer(To_Address(lapicBase + 16#0130#)));
        --     isr4                := LAPICRegisterRO(lapicAcc.To_Pointer(To_Address(lapicBase + 16#0140#)));
        --     isr5                := LAPICRegisterRO(lapicAcc.To_Pointer(To_Address(lapicBase + 16#0150#)));
        --     isr6                := LAPICRegisterRO(lapicAcc.To_Pointer(To_Address(lapicBase + 16#0160#)));
        --     isr7                := LAPICRegisterRO(lapicAcc.To_Pointer(To_Address(lapicBase + 16#0170#)));
        --     tmr0                := LAPICRegisterRO(lapicAcc.To_Pointer(To_Address(lapicBase + 16#0180#)));
        --     tmr1                := LAPICRegisterRO(lapicAcc.To_Pointer(To_Address(lapicBase + 16#0190#)));
        --     tmr2                := LAPICRegisterRO(lapicAcc.To_Pointer(To_Address(lapicBase + 16#01A0#)));
        --     tmr3                := LAPICRegisterRO(lapicAcc.To_Pointer(To_Address(lapicBase + 16#01B0#)));
        --     tmr4                := LAPICRegisterRO(lapicAcc.To_Pointer(To_Address(lapicBase + 16#01C0#)));
        --     tmr5                := LAPICRegisterRO(lapicAcc.To_Pointer(To_Address(lapicBase + 16#01D0#)));
        --     tmr6                := LAPICRegisterRO(lapicAcc.To_Pointer(To_Address(lapicBase + 16#01E0#)));
        --     tmr7                := LAPICRegisterRO(lapicAcc.To_Pointer(To_Address(lapicBase + 16#01F0#)));
        --     irr0                := LAPICRegisterRO(lapicAcc.To_Pointer(To_Address(lapicBase + 16#0200#)));
        --     irr1                := LAPICRegisterRO(lapicAcc.To_Pointer(To_Address(lapicBase + 16#0210#)));
        --     irr2                := LAPICRegisterRO(lapicAcc.To_Pointer(To_Address(lapicBase + 16#0220#)));
        --     irr3                := LAPICRegisterRO(lapicAcc.To_Pointer(To_Address(lapicBase + 16#0230#)));
        --     irr4                := LAPICRegisterRO(lapicAcc.To_Pointer(To_Address(lapicBase + 16#0240#)));
        --     irr5                := LAPICRegisterRO(lapicAcc.To_Pointer(To_Address(lapicBase + 16#0250#)));
        --     irr6                := LAPICRegisterRO(lapicAcc.To_Pointer(To_Address(lapicBase + 16#0260#)));
        --     irr7                := LAPICRegisterRO(lapicAcc.To_Pointer(To_Address(lapicBase + 16#0270#)));
        --     esr                 := LAPICRegisterRW(lapicAcc.To_Pointer(To_Address(lapicBase + 16#0280#)));
        --     lvtCMCI             := LAPICRegisterRW(lapicAcc.To_Pointer(To_Address(lapicBase + 16#02F0#)));
        --     icr0                := LAPICRegisterRW(lapicAcc.To_Pointer(To_Address(lapicBase + 16#0300#)));
        --     icr1                := LAPICRegisterRW(lapicAcc.To_Pointer(To_Address(lapicBase + 16#0310#)));
        --     lvtTimer            := LAPICRegisterRW(lapicAcc.To_Pointer(To_Address(lapicBase + 16#0320#)));
        --     lvtThermalSensor    := LAPICRegisterRW(lapicAcc.To_Pointer(To_Address(lapicBase + 16#0330#)));
        --     lvtPerfMon          := LAPICRegisterRW(lapicAcc.To_Pointer(To_Address(lapicBase + 16#0340#)));
        --     lvtLINT0            := LAPICRegisterRW(lapicAcc.To_Pointer(To_Address(lapicBase + 16#0350#)));
        --     lvtLINT1            := LAPICRegisterRW(lapicAcc.To_Pointer(To_Address(lapicBase + 16#0360#)));
        --     lvtError            := LAPICRegisterRW(lapicAcc.To_Pointer(To_Address(lapicBase + 16#0370#)));
        --     timerInitialCount   := LAPICRegisterRW(lapicAcc.To_Pointer(To_Address(lapicBase + 16#0380#)));
        --     timerCurrentCount   := LAPICRegisterRO(lapicAcc.To_Pointer(To_Address(lapicBase + 16#0390#)));
        --     timerDivideConf     := LAPICRegisterRW(lapicAcc.To_Pointer(To_Address(lapicBase + 16#03E0#)));
        -- end setupRegisters;
    -- end LAPICRegisters;
end lapic;
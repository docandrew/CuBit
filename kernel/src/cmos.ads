-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2019 Jon Andrew
--
-- CMOS & Real-Time Clock (RTC)
-------------------------------------------------------------------------------
with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;

package cmos with
    SPARK_Mode => On
is

    type CMOSRegister is new Unsigned_8;
    
    ---------------------------------------------------------------------------
    -- Real-time Clock control registers
    ---------------------------------------------------------------------------
    RTCSeconds      : constant CMOSRegister := 16#00#;
    RTCSecondAlarm  : constant CMOSRegister := 16#01#;
    RTCMinutes      : constant CMOSRegister := 16#02#;
    RTCMinuteAlarm  : constant CMOSRegister := 16#03#;
    RTCHours        : constant CMOSRegister := 16#04#;
    RTCHourAlarm    : constant CMOSRegister := 16#05#;
    RTCDayOfWeek    : constant CMOSRegister := 16#06#;
    RTCDateOfMonth  : constant CMOSRegister := 16#07#;
    RTCMonth        : constant CMOSRegister := 16#08#;
    RTCYear         : constant CMOSRegister := 16#09#;
    RTCStatusA      : constant CMOSRegister := 16#0A#;
    RTCStatusB      : constant CMOSRegister := 16#0B#;
    RTCStatusC      : constant CMOSRegister := 16#0C#;
    RTCStatusD      : constant CMOSRegister := 16#0D#;

    -- BCD value for the century
    RTCCentury      : constant CMOSRegister := 16#37#;

    ---------------------------------------------------------------------------
    -- Status Register A
    ---------------------------------------------------------------------------
    type StatusRegisterA is
    record
        rate                : Natural range 0..15;
        bankControl         : Boolean;
        selectDivider       : Natural range 0..3;
        updateInProgress    : Boolean;
    end record with Size => 8;

    for StatusRegisterA use
    record
        rate                at 0 range 0..3;
        bankControl         at 0 range 4..4;
        selectDivider       at 0 range 5..6;
        updateInProgress    at 0 range 7..7;
    end record;

    function toU8 is new Ada.Unchecked_Conversion(StatusRegisterA, Unsigned_8);

    ---------------------------------------------------------------------------
    -- Status Register B
    -- @field enableDST - if set, daylight savings time enabled
    -- @field use24HourClock - if set, clock will use 24-hour mode, if clear,
    --  12-hour mode.
    -- @field useBinary - if set, then binary will be used for time/date, 
    --  otherwise, BCD.
    -- @field squareWaveEnable - if set, square wave generator enabled
    -- @field interruptOnUpdate - if set, interrupts will occur when the RTC
    --  has completed an update cycle.
    -- @field interruptOnAlarm - if set, when time matches the alarm values
    --  an interrupt will occur.
    -- @field interruptPeriodic - if set, an interrupt will occur at a rate
    --  determined by the rate and divider bits in Status Register A
    -- @field setClock - if set, clock will stop updating and the 14 time bytes
    --  can be set without updates occuring. If clear, clock will self-update.
    ---------------------------------------------------------------------------
    type StatusRegisterB is
    record
        enableDST           : Boolean;
        use24HourClock      : Boolean;
        useBinary           : Boolean;
        squareWaveEnable    : Boolean;
        interruptOnUpdate   : Boolean;
        interruptOnAlarm    : Boolean;
        interruptPeriodic   : Boolean;
        setClock            : Boolean;
    end record with Size => 8;

    for StatusRegisterB use
    record
        enableDST           at 0 range 0..0;
        use24HourClock      at 0 range 1..1;
        useBinary           at 0 range 2..2;
        squareWaveEnable    at 0 range 3..3;
        interruptOnUpdate   at 0 range 4..4;
        interruptOnAlarm    at 0 range 5..5;
        interruptPeriodic   at 0 range 6..6;
        setClock            at 0 range 7..7;
    end record;

    function toU8 is new Ada.Unchecked_Conversion(StatusRegisterB, Unsigned_8);

    ---------------------------------------------------------------------------
    -- Status Register C gives the cause of RTC interrupts
    -- @field updateOccurred - if 1, an update-ended interrupt occurred
    -- @field alarmOccurred - if 1, an alarm interrupt occurred
    -- @field periodicOccurred - if 1, a periodic interrupt occurred
    -- @field interruptPending - if 1, the RTC has a system interrupt pending.
    ---------------------------------------------------------------------------
    type StatusRegisterC is
    record
        reserved            : Natural range 0..15;
        updateOccurred      : Boolean;
        alarmOccurred       : Boolean;
        periodicOccurred    : Boolean;
        interruptPending    : Boolean;
    end record with Size => 8;

    for StatusRegisterC use
    record
        reserved            at 0 range 0..3;
        updateOccurred      at 0 range 4..4;
        alarmOccurred       at 0 range 5..5;
        periodicOccurred    at 0 range 6..6;
        interruptPending    at 0 range 7..7;
    end record;

    function toU8 is new Ada.Unchecked_Conversion(StatusRegisterC, Unsigned_8);

    ---------------------------------------------------------------------------
    -- Status Register D
    -- @field validRAM - if set, CMOS RAM is considered valid.
    ---------------------------------------------------------------------------
    type StatusRegisterD is
    record
        reserved            : Natural range 0..127;
        validRAM            : Boolean;
    end record with Size => 8;

    for StatusRegisterD use
    record
        reserved            at 0 range 0..6;
        validRAM            at 0 range 7..7;
    end record;

    function toU8 is new Ada.Unchecked_Conversion(StatusRegisterD, Unsigned_8);

    ---------------------------------------------------------------------------
    -- CMOS Shutdown Status register, most stuff here is a concern for BIOS,
    --  but we need to specify "JMP double word pointer without EOI" for some
    --  systems, so when AP CPUs boot up, they will reset and start executing
    --  at the instruction pointed to by the "warm reset vector" (see lapic).
    ---------------------------------------------------------------------------
    ShutdownStatus  : constant CMOSRegister := 16#0F#;
    JMP_PTR_NO_EOI  : constant Unsigned_8 := 16#0A#;

    ---------------------------------------------------------------------------
    -- write - write to CMOS memory
    -- Note: Interrupts are disabled during this procedure.
    --
    -- @param reg - CMOS register to write
    -- @param val - value to write
    ---------------------------------------------------------------------------
    procedure write(reg : in CMOSRegister; val : in Unsigned_8);

    ---------------------------------------------------------------------------
    -- read - read from CMOS memory
    -- Note: Interrupts are disabled during this procedure.
    --
    -- @param reg - CMOS register to read
    -- @param ret - output value from this procedure.
    ---------------------------------------------------------------------------
    procedure read(reg : in CMOSRegister; ret : out Unsigned_8);

end cmos;
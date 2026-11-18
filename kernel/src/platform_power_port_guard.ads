-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary The port writes no user process may make to the FADT's power
-- registers (KERN-004).
--
-- @description Sleep and reset go only through the platform power object
-- (Platform_Power), which the kernel admits and sequences itself: it drains
-- the console, parks the CPUs and writes PM1b before PM1a. A process holding
-- an I/O-port capability over the PM1 or reset ports (acpi.svc does, for
-- AML's own register work) must therefore not be able to start a sleep or a
-- reset by a raw port write. Refused is any user OUT that:
--  - sets SLP_EN (bit 13 of a PM1 control word, byte 1 bit 5) in PM1a or
--    PM1b control;
--  - sets SLP_EN (bit 5) in the hardware-reduced sleep control register;
--  - touches any byte of the FADT reset register.
-- Every other write, other bits of the same registers included (SLP_TYP,
-- GBL_RLS, SCI_EN), passes: AML writes them. Writes are judged byte by byte,
-- so 8-, 16- and 32-bit writes at any offset into a register are caught. A
-- write whose bytes run past port 0xFFFF is refused. Only System I/O
-- registers are judged here; memory-mapped ones are outside the port path.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with Firmware_Tables.Power_Registers;

package Platform_Power_Port_Guard with SPARK_Mode is
    package PR renames Firmware_Tables.Power_Registers;
    use type PR.Register_Space;

    subtype Port is Unsigned_32 range 0 .. 16#FFFF#;
    subtype Width is Positive range 1 .. 4;

    PM1_SLP_EN_Byte  : constant := 1;
    PM1_SLP_EN_Bit   : constant Unsigned_8 := 16#20#;
    Reduced_SLP_EN   : constant Unsigned_8 := 16#20#;

    -- PCI configuration mechanism #1: a dword write at CONFIG_ADDRESS
    -- (0xCF8) is one access to that register and does not reach the byte
    -- at 0xCF9, where the FADT reset register usually is (PCI Local Bus
    -- 3.0 section 3.2.2.3.2; the host bridge claims 0xCF8 only for dword
    -- accesses). devmgr writes it for every configuration access.
    PCI_Config_Address_Port : constant := 16#CF8#;
    PCI_Config_Address_Width : constant := 4;

    -- The byte of Value a write of W bytes puts at port P + I.
    function Byte_Of (Value : Unsigned_32; I : Natural) return Unsigned_8 is
      (Unsigned_8 (Shift_Right (Value, 8 * I) and 16#FF#))
    with Pre => I <= 3;

    -- Port P is byte Offset of R, an I/O-space register. Addresses of I/O
    -- registers are at most 0xFFFF (Consistent), so the sum cannot overflow.
    function Is_IO_Port_Byte (R : PR.Register; P : Unsigned_32; Offset : Natural) return Boolean is
      (R.Present and then R.Space = PR.System_IO
       and then Offset < R.Bytes
       and then R.Address <= PR.Port_Space_Last
       and then R.Address + Unsigned_64 (Offset) = Unsigned_64 (P));

    function Refuses
      (D : PR.Description; P : Port; W : Width; Value : Unsigned_32) return Boolean;
end Platform_Power_Port_Guard;

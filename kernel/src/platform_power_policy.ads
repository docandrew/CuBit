-------------------------------------------------------------------------------
-- CuBitOS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary
-- The platform power object's decisions (KERN-004): which requests are
-- admitted, which registers are written in which order, and with which
-- values. No hardware access; Platform_Power is the adapter that performs
-- the plan.
--
-- @description
-- Enter_State (Platform_Power_ABI) arrives as a Call on a CAP_PLATFORM_POWER
-- handle. Decode reads the message words at full width; Admit checks the
-- request against the kernel's own FADT registers
-- (Firmware_Tables.Power_Registers, decoded at boot). Only an admitted
-- request reaches Plan, and only S5 and Reset are admitted; S3 and S4 are
-- refused Not_Yet.
--
-- The plan (ACPI 6.5, 7.4.2 and 16.1.7; the PM1 bits as
-- userspace/services/acpi/power/acpi_sleep_entry.adb treats them):
--   Drain_Console, Park_CPUs   before any register write, so every line
--                              queued before the call reaches the console
--                              (LOG-009) and no other CPU runs during the
--                              transition;
--   S5, PM1:      Read_B, Read_A, Stage_B, Stage_A, Enable_B, Enable_A:
--                 every value keeps the register's bits other than
--                 GBL_RLS, SLP_TYP and SLP_EN as read, writes SLP_TYP, and
--                 sets SLP_EN only in the last write to each block, PM1b
--                 before PM1a (PM1a's write is the one that takes the
--                 machine off on a split pair);
--   S5, reduced:  Reduced_Enable (SLP_TYP with SLP_EN, one byte write);
--   Reset:        Write_Reset (the FADT's reset value).
-- Then Settle: if the machine is still running, the CPUs are released and
-- the caller gets Did_Not_Take_Effect.
-------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Platform_Power_ABI;
with Firmware_Tables.Power_Registers;

package Platform_Power_Policy with SPARK_Mode => On, Pure is
    package ABI renames Platform_Power_ABI;
    package PR  renames Firmware_Tables.Power_Registers;
    use type ABI.Power_State, ABI.Refusal, PR.Register_Space, PR.Sleep_Model;

    subtype Sleep_Type is ABI.Sleep_Type;

    ---------------------------------------------------------------------------
    -- Requests
    ---------------------------------------------------------------------------
    type Request (Valid : Boolean := False) is record
        case Valid is
            when True =>
                State  : ABI.Power_State := ABI.S5;
                Type_A : Sleep_Type      := 0;
                Type_B : Sleep_Type      := 0;
            when False =>
                Why    : ABI.Refusal     := ABI.Malformed;
        end case;
    end record;

    -- The message as the caller sent it. Writable: the handle carries Write.
    function Decode (Writable : Boolean;
                     Label    : Unsigned_32;
                     Length   : Unsigned_8;
                     State_Word, Type_A_Word, Type_B_Word, Unused_Word : Unsigned_64)
                     return Request
    with Post =>
        (if Decode'Result.Valid then
             Writable
             and then Label = ABI.Operation'Enum_Rep (ABI.Enter_State)
             and then Length = ABI.Request_Words
             and then Unused_Word = 0
             and then State_Word = ABI.Power_State'Enum_Rep (Decode'Result.State)
             and then Type_A_Word = Decode'Result.Type_A
             and then Type_B_Word = Decode'Result.Type_B
             and then (if Decode'Result.State = ABI.Reset then
                           Type_A_Word = 0 and then Type_B_Word = 0)
         else
             (if not Writable then Decode'Result.Why = ABI.Not_Permitted
              elsif Label /= ABI.Operation'Enum_Rep (ABI.Enter_State)
              then Decode'Result.Why = ABI.Unknown_Operation));

    ---------------------------------------------------------------------------
    -- Admission against the kernel's registers
    ---------------------------------------------------------------------------
    type Admission (Admitted : Boolean := False) is record
        case Admitted is
            when True  => null;
            when False => Why : ABI.Refusal := ABI.Malformed;
        end case;
    end record;

    function Usable (R : PR.Register) return Boolean is
      (R.Present and then R.Space = PR.System_IO);

    function Admit (Item : Request; Registers : PR.Description) return Admission
    with Pre  => Item.Valid and then PR.Consistent (Registers),
         Post =>
           (if Admit'Result.Admitted then
                (case Item.State is
                    when ABI.S3 | ABI.S4 => False,
                    when ABI.S5 =>
                        (case Registers.Model is
                            when PR.No_Sleep_Registers => False,
                            when PR.PM1A_Only => Usable (Registers.PM1A_Control),
                            when PR.PM1A_And_B =>
                                Usable (Registers.PM1A_Control)
                                and then Usable (Registers.PM1B_Control),
                            when PR.Hardware_Reduced => Usable (Registers.Sleep_Control)),
                    when ABI.Reset => Usable (Registers.Reset))
            else
                (if Item.State in ABI.S3 | ABI.S4 then Admit'Result.Why = ABI.Not_Yet));

    ---------------------------------------------------------------------------
    -- Register values
    ---------------------------------------------------------------------------
    subtype Control_Word is Unsigned_16;
    Global_Release : constant Control_Word := 16#0004#;  -- GBL_RLS, bit 2
    SLP_TYP_Shift  : constant := 10;                     -- bits 10 .. 12
    SLP_TYP_Mask   : constant Control_Word := 16#1C00#;
    SLP_EN         : constant Control_Word := 16#2000#;  -- bit 13
    Command_Bits   : constant Control_Word := Global_Release or SLP_TYP_Mask or SLP_EN;

    Reduced_SLP_TYP_Shift : constant := 2;               -- bits 2 .. 4
    Reduced_SLP_EN        : constant Unsigned_8 := 16#20#;  -- bit 5

    -- A PM1 control value: the observed bits but the command bits, the sleep
    -- type, and SLP_EN only when Enable.
    function PM1_Value (Observed : Control_Word; Kind : Sleep_Type; Enable : Boolean)
                        return Control_Word
    is ((Observed and not Command_Bits)
        or Shift_Left (Control_Word (Kind), SLP_TYP_Shift)
        or (if Enable then SLP_EN else 0))
    with Post =>
        (PM1_Value'Result and not Command_Bits) = (Observed and not Command_Bits)
        and then Shift_Right (PM1_Value'Result and SLP_TYP_Mask, SLP_TYP_Shift)
                   = Control_Word (Kind)
        and then (PM1_Value'Result and Global_Release) = 0
        and then ((PM1_Value'Result and SLP_EN) /= 0) = Enable;

    -- The hardware-reduced sleep control byte: SLP_TYP with SLP_EN.
    function Reduced_Value (Kind : Sleep_Type) return Unsigned_8 is
      (Shift_Left (Unsigned_8 (Kind), Reduced_SLP_TYP_Shift) or Reduced_SLP_EN)
    with Post =>
        Shift_Right (Reduced_Value'Result, Reduced_SLP_TYP_Shift) mod 2 ** ABI.Sleep_Type_Bits
          = Unsigned_8 (Kind)
        and then (Reduced_Value'Result and Reduced_SLP_EN) /= 0;

    ---------------------------------------------------------------------------
    -- The plan
    ---------------------------------------------------------------------------
    type Step is
      (Drain_Console, Park_CPUs,
       Read_B, Read_A, Stage_B, Stage_A, Enable_B, Enable_A,
       Reduced_Enable, Write_Reset,
       Settle);

    Maximum_Steps : constant := 9;
    subtype Step_Count is Natural range 0 .. Maximum_Steps;
    subtype Step_Index is Positive range 1 .. Maximum_Steps;
    type Step_List is array (Step_Index) of Step;

    type Plan_Type is record
        Steps : Step_List  := [others => Settle];
        Count : Step_Count := 0;
    end record;

    function Writes (S : Step) return Boolean is
      (S in Stage_B | Stage_A | Enable_B | Enable_A | Reduced_Enable | Write_Reset);

    function Plan (Item : Request; Registers : PR.Description) return Plan_Type
    with Pre  => Item.Valid and then PR.Consistent (Registers)
                 and then Admit (Item, Registers).Admitted,
         Post =>
           Plan'Result.Count >= 3
           -- Drain, then park, before anything else; settle last.
           and then Plan'Result.Steps (1) = Drain_Console
           and then Plan'Result.Steps (2) = Park_CPUs
           and then Plan'Result.Steps (Plan'Result.Count) = Settle
           and then (for all I in 3 .. Plan'Result.Count - 1 =>
                         Plan'Result.Steps (I) not in Drain_Console | Park_CPUs | Settle)
           -- Exactly the writes the state and model call for, in order.
           and then (case Item.State is
                       when ABI.Reset =>
                           Plan'Result.Count = 4
                           and then Plan'Result.Steps (3) = Write_Reset,
                       when ABI.S5 =>
                           (case Registers.Model is
                               when PR.Hardware_Reduced =>
                                   Plan'Result.Count = 4
                                   and then Plan'Result.Steps (3) = Reduced_Enable,
                               when PR.PM1A_Only =>
                                   Plan'Result.Count = 6
                                   and then Plan'Result.Steps (3) = Read_A
                                   and then Plan'Result.Steps (4) = Stage_A
                                   and then Plan'Result.Steps (5) = Enable_A,
                               when PR.PM1A_And_B =>
                                   Plan'Result.Count = Maximum_Steps
                                   and then Plan'Result.Steps (3) = Read_B
                                   and then Plan'Result.Steps (4) = Read_A
                                   and then Plan'Result.Steps (5) = Stage_B
                                   and then Plan'Result.Steps (6) = Stage_A
                                   and then Plan'Result.Steps (7) = Enable_B
                                   and then Plan'Result.Steps (8) = Enable_A,
                               when PR.No_Sleep_Registers => False),
                       when ABI.S3 | ABI.S4 => False);

end Platform_Power_Policy;

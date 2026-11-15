pragma Ada_2022;
with Interfaces; use Interfaces;
with ACPI_FADT;
-- SCI-DELIVERY: the registers an SCI is read from and acknowledged at, from
-- the FADT (ACPI 6.5, 4.8.3 / 4.8.4 / 5.2.9). Pure layout: no port access.
--
-- PM1a/PM1b event blocks: the status register in the first half, the enable
-- register in the second, each 16 bits (4.8.4.1). GPE0/GPE1 blocks: status
-- bytes in the first half, enable bytes in the second, one bit per GPE;
-- GPE0 numbers from 0, GPE1 from the FADT's GPE1_BASE (4.8.4.2, 5.2.9).
-- Only the I/O space is served: a block the FADT puts in another address
-- space is refused with its reason, never guessed.
package ACPI_SCI_Registers with SPARK_Mode, Pure is
   subtype Port_Number is Unsigned_16;
   Port_Space : constant := 65_536;

   ---------------------------------------------------------------------------
   -- PM1 fixed events (4.8.4.1.1 status, 4.8.4.1.2 enable): the bit is the
   -- same in both registers. Status bits are write-1-to-clear.
   ---------------------------------------------------------------------------
   type Fixed_Event is
     (Timer_Overflow, Global_Lock_Release, Power_Button, Sleep_Button, RTC_Alarm);
   Fixed_Bit : constant array (Fixed_Event) of Natural :=
     [Timer_Overflow => 0, Global_Lock_Release => 5, Power_Button => 8,
      Sleep_Button => 9, RTC_Alarm => 10];
   function Fixed_Mask (Event : Fixed_Event) return Unsigned_16 is
     (Shift_Left (Unsigned_16'(1), Fixed_Bit (Event)));
   type Fixed_Set is array (Fixed_Event) of Boolean;
   No_Fixed : constant Fixed_Set := [others => False];

   -- PM1 control (4.8.4.2.1): SCI_EN, set when the platform is in ACPI mode.
   SCI_Enable_Mask : constant Unsigned_16 := 2 ** 0;

   -- PM1 event register width (16 bits): an event block is two of them at
   -- least.
   PM1_Register_Bytes : constant := 2;
   -- SMI_CMD is one byte-wide port.
   SMI_Command_Bytes : constant := 1;
   Minimum_PM1_Event_Bytes : constant := 2 * PM1_Register_Bytes;

   -- FADT feature flags (5.2.9, Table 5.10): a power or sleep button that is
   -- a control-method device instead of a fixed event.
   Power_Button_Control_Method_Flag : constant Unsigned_32 := 2 ** 4;
   Sleep_Button_Control_Method_Flag : constant Unsigned_32 := 2 ** 5;

   -- FADT generic address space (5.2.3.2): the system I/O space.
   System_IO_Space : constant Unsigned_8 := 1;

   ---------------------------------------------------------------------------
   -- GPEs: at most 256 numbers (GPE1_BASE is a byte; 5.2.9).
   ---------------------------------------------------------------------------
   Maximum_GPE : constant := 255;
   subtype GPE_Number is Natural range 0 .. Maximum_GPE;
   GPE_Count : constant := Maximum_GPE + 1;
   GPE_Register_Bits : constant := 8;
   subtype GPE_Bit is Natural range 0 .. GPE_Register_Bits - 1;
   Maximum_GPE_Registers : constant := GPE_Count / GPE_Register_Bits;
   subtype GPE_Register_Count is Natural range 0 .. Maximum_GPE_Registers;
   subtype GPE_Register_Index is Natural range 0 .. Maximum_GPE_Registers - 1;
   type GPE_Set is array (GPE_Number) of Boolean;
   No_GPE : constant GPE_Set := [others => False];

   type PM1_Kind is (PM1a, PM1b);
   type GPE_Kind is (GPE0, GPE1);

   type PM1_Block is record
      Present : Boolean := False;
      Status  : Port_Number := 0;   -- 16-bit status register
      Enable  : Port_Number := 0;   -- 16-bit enable register
   end record;

   type GPE_Block is record
      Present   : Boolean := False;
      Status    : Port_Number := 0;      -- first status byte
      Enable    : Port_Number := 0;      -- first enable byte
      Registers : GPE_Register_Count := 0;  -- status (= enable) bytes
      First_GPE : GPE_Number := 0;
   end record;

   type PM1_Blocks is array (PM1_Kind) of PM1_Block;
   type GPE_Blocks is array (GPE_Kind) of GPE_Block;

   -- Why the SCI's registers could not be served.
   type Refusal is
     (No_Refusal,
      Hardware_Reduced,        -- HW_REDUCED_ACPI: no SCI, no PM1/GPE blocks
      No_PM1a_Event_Block,     -- the one event block every SCI platform has
      Block_Not_In_IO_Space,   -- a block the FADT puts outside the I/O space
      Malformed_Event_Block,   -- length odd, too short, or past the port space
      Malformed_GPE_Block,     -- length odd, or past the port space
      GPE1_Overlaps_GPE0,      -- GPE1_BASE inside GPE0's numbers
      GPE1_Past_Last_GPE);     -- GPE1's numbers run past 255

   type Layout is record
      Events          : PM1_Blocks;
      GPEs            : GPE_Blocks;
      -- GPE1 alone may be refused (its reason) without losing the rest.
      GPE1_Refused    : Refusal := No_Refusal;
      -- PM1a control (SCI_EN), when it is an I/O block.
      Control_Present : Boolean := False;
      Control         : Port_Number := 0;
      -- Fixed events the FADT says are fixed hardware (not control methods).
      Fixed_Hardware  : Fixed_Set := No_Fixed;
      -- The ACPI-mode transition (4.8.2.1): the FADT's SMI_CMD port and the
      -- ACPI_ENABLE value written to it. ACPI_DISABLE is never kept: this
      -- service never leaves ACPI mode.
      SMI_Command_Present : Boolean := False;
      SMI_Command         : Port_Number := 0;
      ACPI_Enable         : Unsigned_8 := 0;
   end record;

   type Layout_Result (Valid : Boolean := False) is record
      case Valid is
         when True  => Value : Layout;
         when False => Why : Refusal := No_PM1a_Event_Block;
      end case;
   end record;

   function Resolve (Item : ACPI_FADT.Description) return Layout_Result
     with Post => (if Resolve'Result.Valid then
                     Resolve'Result.Value.Events (PM1a).Present
                     and then (if Resolve'Result.Value.SMI_Command_Present then
                                 Resolve'Result.Value.SMI_Command /= 0
                                 and then Resolve'Result.Value.ACPI_Enable /= 0)
                   else Resolve'Result.Why /= No_Refusal);

   ---------------------------------------------------------------------------
   -- ACPI mode at Arm, from PM1a control's SCI_EN (read through its window).
   ---------------------------------------------------------------------------
   type Mode_Decision is
     (Already_ACPI_Mode,     -- SCI_EN is set: nothing to write
      Switch_To_ACPI_Mode,   -- write ACPI_ENABLE to SMI_CMD, then wait for SCI_EN
      No_Mode_Transition,    -- SMI_CMD or ACPI_ENABLE is zero (or SMI_CMD is
                             -- past the port space): the FADT defines none
      Control_Unreadable);   -- no PM1a control register to read SCI_EN from

   function Decide_Mode
     (Item : Layout; Control_Read : Boolean; Control : Unsigned_16) return Mode_Decision
   is (if not Item.Control_Present or else not Control_Read then Control_Unreadable
       elsif (Control and SCI_Enable_Mask) /= 0 then Already_ACPI_Mode
       elsif Item.SMI_Command_Present then Switch_To_ACPI_Mode
       else No_Mode_Transition)
   with Pre  => (if Item.SMI_Command_Present then
                   Item.SMI_Command /= 0 and then Item.ACPI_Enable /= 0),
        Post => (if Decide_Mode'Result = Switch_To_ACPI_Mode then
                   Item.SMI_Command_Present and then Item.SMI_Command /= 0
                   and then Item.ACPI_Enable /= 0);

   -- Where GPE lives: its block, status/enable byte offset and bit.
   type GPE_Location (Present : Boolean := False) is record
      case Present is
         when True =>
            Block    : GPE_Kind;
            Register : GPE_Register_Index;
            Bit      : GPE_Bit;
         when False => null;
      end case;
   end record;

   function Locate (Item : Layout; GPE : GPE_Number) return GPE_Location;

   function Bit_Mask (Bit : GPE_Bit) return Unsigned_8 is
     (Shift_Left (Unsigned_8'(1), Bit));
end ACPI_SCI_Registers;

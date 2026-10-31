------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The exact I/O-port ranges of the ACPI fixed-hardware register blocks
--  (coordination/acpi-region-boundary.md, slice 1): what the launcher may
--  offer acpi-backend.svc as I/O windows.
--
--  @description
--  From the FADT (ACPI 6.5, 5.2.9: PM1a/PM1b event and control blocks, the
--  PM timer, GPE0/GPE1) and, if present, the ECDT (5.2.16: the embedded
--  controller's command and data ports). Each block is exactly its
--  firmware-stated port range: the FADT's length field for that block (one
--  port for each EC register), at the 64-bit address when the table has
--  one and it is nonzero (an X_ block in another address space is not an
--  I/O window and is left out), else at the 32-bit one. Nothing is rounded
--  or widened: a block that is absent (address or length zero) or runs
--  past the port space is left out. Overlapping blocks become their union
--  (no port outside some block); separate blocks stay separate windows,
--  even when adjacent.
--
--  Firmware tables are untrusted bytes: every field is read only inside
--  both the buffer and the table's own Length, after its signature.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package CuBit.ACPI_Fixed_Windows with Pure, SPARK_Mode is

   type Byte_Array is array (Positive range <>) of Unsigned_8;

   --  The fixed blocks: seven FADT blocks, two EC registers.
   type Block_Kind is
     (PM1a_Event, PM1b_Event, PM1a_Control, PM1b_Control, PM_Timer, GPE0, GPE1,
      EC_Command, EC_Data);
   subtype FADT_Block is Block_Kind range PM1a_Event .. GPE1;
   subtype EC_Block is Block_Kind range EC_Command .. EC_Data;

   Port_Space : constant := 65_536;
   subtype Port_Number is Unsigned_16;
   subtype Port_Count is Unsigned_32 range 1 .. Port_Space;

   type Port_Range is record
      First : Port_Number := 0;
      Count : Port_Count := 1;
   end record;

   function Last_Plus_One (Item : Port_Range) return Unsigned_32 is
     (Unsigned_32 (Item.First) + Item.Count);

   Maximum_Windows : constant := Block_Kind'Pos (Block_Kind'Last) + 1;
   subtype Window_Count is Natural range 0 .. Maximum_Windows;
   subtype Window_Index is Positive range 1 .. Maximum_Windows;
   type Window_Array is array (Window_Index) of Port_Range;

   type Window_List is record
      Count   : Window_Count := 0;
      Windows : Window_Array := [others => (others => <>)];
   end record;

   --  Inside the port space, ascending, each ending before the next begins.
   function Well_Formed (Item : Window_List) return Boolean is
     ((for all K in 1 .. Item.Count => Last_Plus_One (Item.Windows (K)) <= Port_Space)
      and then (for all K in 2 .. Item.Count =>
                  Last_Plus_One (Item.Windows (K - 1)) <= Unsigned_32 (Item.Windows (K).First)));

   ---------------------------------------------------------------------------
   --  Table layouts (ACPI 6.5): byte offsets from a table's start.
   ---------------------------------------------------------------------------
   Signature_Bytes      : constant := 4;
   Length_Offset        : constant := 4;
   Header_Bytes         : constant := 36;
   FADT_Signature       : constant String := "FACP";
   ECDT_Signature       : constant String := "ECDT";

   --  FADT 32-bit block addresses (u32) and lengths (u8).
   PM1a_Event_Offset    : constant := 56;
   PM1b_Event_Offset    : constant := 60;
   PM1a_Control_Offset  : constant := 64;
   PM1b_Control_Offset  : constant := 68;
   PM_Timer_Offset      : constant := 76;
   GPE0_Offset          : constant := 80;
   GPE1_Offset          : constant := 84;
   PM1_Event_Length_Offset   : constant := 88;
   PM1_Control_Length_Offset : constant := 89;
   PM_Timer_Length_Offset    : constant := 91;
   GPE0_Length_Offset        : constant := 92;
   GPE1_Length_Offset        : constant := 93;
   --  FADT 64-bit (generic address structure) blocks.
   X_PM1a_Event_Offset   : constant := 148;
   X_PM1b_Event_Offset   : constant := 160;
   X_PM1a_Control_Offset : constant := 172;
   X_PM1b_Control_Offset : constant := 184;
   X_PM_Timer_Offset     : constant := 208;
   X_GPE0_Offset         : constant := 220;
   X_GPE1_Offset         : constant := 232;
   --  ECDT: the EC's command/status and data registers (GAS).
   EC_Control_Offset     : constant := 36;
   EC_Data_Offset        : constant := 48;
   EC_Register_Ports     : constant := 1;

   --  Generic address structure (5.2.3.2): space, widths, then the address.
   GAS_Bytes          : constant := 12;
   GAS_Space_Offset   : constant := 0;
   GAS_Address_Offset : constant := 4;
   System_IO_Space    : constant := 1;

   Maximum_Table_Bytes : constant := 2 ** 20;

   --  FADT and ECDT as the snapshot holds them; an ECDT of length zero (or
   --  any buffer that is not that table) contributes nothing.
   procedure Compute (FADT, ECDT : Byte_Array; Result : out Window_List)
   with Pre  => FADT'First = 1 and then ECDT'First = 1
                and then FADT'Length <= Maximum_Table_Bytes
                and then ECDT'Length <= Maximum_Table_Bytes,
        Post => Well_Formed (Result);

end CuBit.ACPI_Fixed_Windows;

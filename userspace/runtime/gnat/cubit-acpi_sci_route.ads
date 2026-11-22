------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Where an ISA interrupt, and ACPI's System Control Interrupt (SCI),
--  arrives: its global system interrupt (GSI), trigger mode and polarity,
--  from the FADT and the MADT, and the legacy line (Interrupt_Line_ABI)
--  that delivers it.
--
--  @description
--  ACPI 6.5, 5.2.9 (FADT): SCI_INT is the SCI's interrupt; in APIC mode it
--  is looked up like an ISA IRQ in the MADT's Interrupt Source Overrides
--  (5.2.12.5: bus 0, Source = SCI_INT gives the GSI and the MPS INTI
--  flags), and without an override it is the GSI itself. The SCI is "a
--  sharable, level, active low interrupt" (5.2.9): an override flag that
--  "conforms to the bus" gives level / active low for the SCI, edge /
--  active high for any other ISA IRQ. Reserved flag encodings are refused,
--  never guessed. A hardware-reduced platform (FADT flag HW_REDUCED_ACPI)
--  has no SCI.
--
--  The GSI must be an input of the I/O APIC the kernel drives (the MADT's
--  first, which must serve GSI 0) and a routable legacy line (inputs
--  1 .. 15): a GSI another I/O APIC serves, or 16 or more, is refused with
--  its reason. So is an identity-mapped IRQ whose GSI an override gives to
--  another source, and two overrides for one source.
--
--  Firmware tables are untrusted bytes. The whole table must be in the
--  buffer (its Length field no longer than the buffer); every entry is
--  checked before it is read, and a malformed MADT is refused as a whole.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Interrupt_Line_ABI;
with CuBit.ACPI_Fixed_Windows;

package CuBit.ACPI_SCI_Route with Pure, SPARK_Mode is

   package LA renames Interrupt_Line_ABI;
   use type LA.Trigger_Mode, LA.Pin_Polarity;
   subtype Byte_Array is CuBit.ACPI_Fixed_Windows.Byte_Array;

   ---------------------------------------------------------------------------
   --  Table layouts (ACPI 6.5): byte offsets from a table's or entry's start.
   ---------------------------------------------------------------------------
   Signature_Bytes : constant := 4;
   Length_Offset   : constant := 4;
   Header_Bytes    : constant := 36;
   FADT_Signature  : constant String := "FACP";
   MADT_Signature  : constant String := "APIC";

   --  FADT (5.2.9, Table 5.9).
   SCI_INT_Offset        : constant := 46;     --  u16
   SCI_INT_Bytes         : constant := 2;
   FADT_Flags_Offset     : constant := 112;    --  u32
   FADT_Flags_Bytes      : constant := 4;
   HW_Reduced_ACPI_Bit   : constant := 20;

   --  MADT (5.2.12): the entries follow the local APIC address and flags.
   MADT_Entries_Offset   : constant := 44;
   Entry_Type_Offset     : constant := 0;
   Entry_Length_Offset   : constant := 1;
   Entry_Header_Bytes    : constant := 2;

   IO_APIC_Type          : constant := 1;      --  5.2.12.3
   IO_APIC_Bytes         : constant := 12;
   IO_APIC_GSI_Offset    : constant := 8;      --  u32 Global System Interrupt Base

   Override_Type         : constant := 2;      --  5.2.12.5
   Override_Bytes        : constant := 10;
   Override_Bus_Offset   : constant := 2;      --  u8, 0 = ISA
   Override_Source_Offset : constant := 3;     --  u8, the bus-relative IRQ
   Override_GSI_Offset   : constant := 4;      --  u32
   Override_Flags_Offset : constant := 8;      --  u16, MPS INTI flags
   ISA_Bus               : constant := 0;

   --  MPS INTI flags (5.2.12.5, Table 5.26): polarity bits 1:0, trigger
   --  mode bits 3:2.
   Polarity_Mask         : constant := 2#0011#;
   Trigger_Shift         : constant := 2;
   Trigger_Mask          : constant := 2#0011#;
   Flags_Conforming      : constant := 2#00#;
   Flags_Active_High     : constant := 2#01#;  --  polarity field
   Flags_Active_Low      : constant := 2#11#;
   Flags_Edge            : constant := 2#01#;  --  trigger field
   Flags_Level           : constant := 2#11#;

   --  The largest table this reads (a MADT for thousands of CPUs is well
   --  under it); a larger one is Too_Large, not cut.
   Maximum_Table_Bytes : constant := CuBit.ACPI_Fixed_Windows.Maximum_Table_Bytes;

   ---------------------------------------------------------------------------
   --  Results
   ---------------------------------------------------------------------------
   subtype ISA_IRQ is Natural range 0 .. 255;   --  an override's Source byte
   subtype GSI_Number is Unsigned_32;

   --  Why an interrupt has no route.
   type Refusal is
     (No_FADT,                  --  no FADT, or one too short for SCI_INT
      Hardware_Reduced,         --  HW_REDUCED_ACPI: no SCI
      No_SCI_Line,              --  SCI_INT is 0
      No_MADT,                  --  no MADT: no I/O APIC routing known
      Malformed_MADT,           --  an entry runs past the table, or is short
      Too_Large,                --  a table longer than Maximum_Table_Bytes
      Ambiguous_Override,       --  two overrides for one source
      Reserved_Override_Flags,  --  polarity or trigger field 2#10#
      GSI_Overridden_Away,      --  identity GSI given to another source
      No_IO_APIC,               --  the MADT names no I/O APIC
      First_IO_APIC_Not_GSI_0,  --  the driven I/O APIC does not start at 0
      Other_IO_APIC,            --  another I/O APIC serves the GSI
      Timer_Input,              --  GSI 0 is the kernel's timer input
      Beyond_Legacy_Lines);     --  GSI 16 or more: no line vector yet

   type Line_Route is record
      GSI        : GSI_Number := 0;
      Trigger    : LA.Trigger_Mode := LA.Edge;
      Polarity   : LA.Pin_Polarity := LA.Active_High;
      --  From an Interrupt Source Override (else the identity default).
      Overridden : Boolean := False;
   end record;

   type Resolution (Routed : Boolean := False) is record
      case Routed is
         when True =>
            Route  : Line_Route;
            Vector : LA.Line_Vector;   --  the line that delivers Route.GSI
         when False =>
            Why    : Refusal := No_MADT;
            --  What was decoded before the refusal (GSI and flags when the
            --  override or default was found), for the log.
            Seen   : Line_Route;
      end case;
   end record;

   function Valid (Item : Resolution) return Boolean is
     (if Item.Routed then
        Item.Route.GSI = GSI_Number (LA.Input_Of (Item.Vector)));

   --  What the FADT says about the SCI.
   type SCI_Field (Present : Boolean := False) is record
      case Present is
         when True  => Source : Unsigned_16;   --  SCI_INT, nonzero
         when False => Why : Refusal := No_FADT;
      end case;
   end record;

   function SCI_Of (FADT : Byte_Array) return SCI_Field
   with Pre  => FADT'First = 1 and then FADT'Length <= Maximum_Table_Bytes,
        Post => (if SCI_Of'Result.Present then SCI_Of'Result.Source /= 0);

   --  ISA IRQ Source as the MADT routes it; without an override, GSI =
   --  Source in the mode a conforming ISA interrupt has (edge, active high),
   --  or, with SCI, the SCI's (level, active low).
   function Resolve_ISA
     (MADT : Byte_Array; Source : ISA_IRQ; SCI : Boolean) return Resolution
   with Pre  => MADT'First = 1 and then MADT'Length <= Maximum_Table_Bytes,
        Post => Valid (Resolve_ISA'Result);

   --  The SCI's line.
   function Resolve_SCI (FADT, MADT : Byte_Array) return Resolution
   with Pre  => FADT'First = 1 and then FADT'Length <= Maximum_Table_Bytes
                and then MADT'First = 1 and then MADT'Length <= Maximum_Table_Bytes,
        Post => Valid (Resolve_SCI'Result);

   ---------------------------------------------------------------------------
   --  The MPS INTI flags decoder (exposed for its tests and proofs).
   ---------------------------------------------------------------------------
   type Flag_Decoding (Defined : Boolean := False) is record
      case Defined is
         when True =>
            Trigger  : LA.Trigger_Mode;
            Polarity : LA.Pin_Polarity;
         when False => null;
      end case;
   end record;

   function Decode_Flags (Flags : Unsigned_16; SCI : Boolean) return Flag_Decoding
   with Post =>
     Decode_Flags'Result.Defined =
       ((Flags and Polarity_Mask) /= 2#10#
        and then (Shift_Right (Flags, Trigger_Shift) and Trigger_Mask) /= 2#10#)
     and then
       (if Decode_Flags'Result.Defined then
          (Decode_Flags'Result.Polarity = LA.Active_Low) =
            ((Flags and Polarity_Mask) = Flags_Active_Low
             or else (SCI and then (Flags and Polarity_Mask) = Flags_Conforming))
          and then
          (Decode_Flags'Result.Trigger = LA.Level) =
            ((Shift_Right (Flags, Trigger_Shift) and Trigger_Mask) = Flags_Level
             or else (SCI and then (Shift_Right (Flags, Trigger_Shift) and Trigger_Mask)
                                     = Flags_Conforming)));

end CuBit.ACPI_SCI_Route;

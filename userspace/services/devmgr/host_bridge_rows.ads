pragma Ada_2022;
with Interfaces; use Interfaces;
with ACPI_Host_Bridge_Wire;
with PCI_BAR_Placement;
------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Which of a host bridge's _CRS descriptors are memory windows a BAR may
--  be placed in, and which buses the bridge decodes (PCI-BAR-001), from
--  the descriptor rows acpi.svc publishes (ACPI_Host_Bridge_Wire).
--
--  @description
--  As Linux reads a PCI root's _CRS (acpi_pci_probe_root_resources):
--  - A window is an address-space descriptor (Word, DWord, QWord or
--    Extended Address) of the memory kind that the bridge produces (General
--    Flags bit 0 clear: "produces and consumes"), with its minimum and
--    maximum present. Fixed Memory32, Memory24 and Memory32 descriptors are
--    the bridge's own registers, never windows.
--  - A window needs a nonzero length: a zero-length descriptor with fixed
--    bounds is invalid and one with variable bounds describes _PRS, not an
--    assigned _CRS window (ACPI 6.6, table 6.44; Linux's
--    acpi_dev_resource_len_valid).
--  - A window with a nonzero translation (bus address /= CPU address) is
--    left out: devmgr writes bus addresses and x86 has none.
--  - The window is [Minimum, Maximum], shortened to Length bytes when a
--    length is present, then rounded inward to whole 4 KiB pages.
--  - It is prefetchable when its cache type is 3 (ACPI 6.5, 6.4.3.5.5).
--  - The buses are the bus-kind producer descriptor's range, or the
--    root's base bus up to 255 when there is none.
------------------------------------------------------------------------------
package Host_Bridge_Rows with SPARK_Mode, Pure is
   package W renames ACPI_Host_Bridge_Wire;
   package PL renames PCI_BAR_Placement;

   Page_Bytes : constant := 4_096;
   Consumer_Only_Flag : constant W.Word := 16#1#;   --  General Flags bit 0
   Prefetchable_Cache : constant W.Word := 3;
   Last_Bus_Number : constant := 255;

   function Is_Address_Form (Form : W.Word) return Boolean is
     (Form in W.Word_Address | W.DWord_Address | W.QWord_Address | W.Extended_Address);

   function Has (R : W.Row; Claim : W.Word) return Boolean is ((R (W.Claims) and Claim) /= 0);

   function Producer (R : W.Row) return Boolean is
     (Has (R, W.Has_General) and then (R (W.General_Flags) and Consumer_Only_Flag) = 0);

   --  R is a memory window this package takes.
   function Is_Window (R : W.Row) return Boolean is
     (Is_Address_Form (R (W.Form)) and then R (W.Kind) = W.Memory_Kind and then Producer (R)
      and then Has (R, W.Has_Minimum) and then Has (R, W.Has_Maximum)
      and then Has (R, W.Has_Length) and then R (W.Length) /= 0
      and then R (W.Minimum) <= R (W.Maximum)
      and then (not Has (R, W.Has_Translation) or else R (W.Translation) = 0));

   --  A window's pages: [Minimum, Maximum] (or Length bytes from Minimum)
   --  rounded inward; empty when no whole page fits or past the page limit.
   function Window_Span (R : W.Row) return PL.Span
   with Pre  => Is_Window (R),
        Post => (if PL.Valid (Window_Span'Result) then
                   Window_Span'Result.Past <= PL.Page_Limit
                   and then Window_Span'Result.First * Page_Bytes >= R (W.Minimum)
                   and then Window_Span'Result.Past * Page_Bytes - 1 <= R (W.Maximum));

   function Prefetchable (R : W.Row) return Boolean is
     (Has (R, W.Has_Cache) and then R (W.Cache) = Prefetchable_Cache);

   type Flags is array (Positive range <>) of Boolean;

   --  The windows of Items, in row order, into Spans/Prefetch (Count used).
   procedure Windows
     (Items : W.Rows; Spans : out PL.Span_Array; Prefetch : out Flags;
      Count : out Natural)
   with Pre  => Spans'Length = Items'Length and then Prefetch'Length = Items'Length
                and then Spans'First = 1 and then Prefetch'First = 1,
        Post => Count <= Items'Length
                and then (for all I in 1 .. Count => PL.Valid (Spans (I)));

   --  The buses the root decodes.
   procedure Buses
     (Items : W.Rows; Base_Bus_Present : Boolean; Base_Bus : W.Word;
      First, Last : out Unsigned_8)
   with Post => First <= Last;
end Host_Bridge_Rows;

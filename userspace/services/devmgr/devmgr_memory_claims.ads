------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Which physical pages are ACPI's: devmgr's record of the firmware-memory
--  windows it holds for the ACPI hardware grant (the kernel's NVS runs and
--  FACS page), so no driver and no devmgr mapping ever overlaps them
--  (CuBit.ACPI_Window_Exchange, coordination/acpi-region-boundary.md).
--
--  @description
--  The kernel enforces each capability's own range and keeps NVS pages out
--  of device memory, but the FACS page may be firmware-reserved, which
--  device memory also names; this ledger is the exclusivity record. Claims
--  are whole pages and are never removed: a window stays ACPI's for the rest
--  of the boot, whether or not it is mapped (claims are frozen).
--
--  * An ACPI claim is refused unless it is whole pages, inside the
--    kernel's physical limit, and either the same as or disjoint from every
--    earlier claim (the same window again is granted: a restarted backend).
--  * Any other range (a driver's BAR, devmgr's own mapping) is clear only
--    if no page of it, partial pages included, is in an ACPI claim.
--  * A full ledger refuses every new claim (fail closed).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Devmgr_Memory_Claims with Pure, SPARK_Mode is

   Page_Bytes : constant := 4096;
   --  The kernel's physical limit (Physical_Ranges.Address_Limit), in pages.
   Page_Limit : constant := 2 ** 35;
   Byte_Limit : constant := Page_Limit * Page_Bytes;

   --  Pages [First, Ends).
   type Claim is record
      First : Unsigned_64 := 0;
      Ends  : Unsigned_64 := 0;
   end record;

   --  One per firmware-memory root (Launch_Configuration.
   --  Maximum_Firmware_Roots; checked where both are seen).
   Maximum_Claims : constant := 8;
   subtype Claim_Count is Natural range 0 .. Maximum_Claims;
   subtype Claim_Index is Positive range 1 .. Maximum_Claims;
   type Claim_Array is array (Claim_Index) of Claim;

   type Ledger is record
      Count  : Claim_Count := 0;
      Claims : Claim_Array := [others => (others => <>)];
   end record;

   --  Every claim is a nonempty page range inside the limit.
   function Valid (Item : Ledger) return Boolean is
     (for all K in 1 .. Item.Count =>
        Item.Claims (K).First < Item.Claims (K).Ends
        and then Item.Claims (K).Ends <= Page_Limit);

   --  [Physical, Physical + Bytes) is whole pages inside the limit.
   function Whole_Pages (Physical, Bytes : Unsigned_64) return Boolean is
     (Bytes > 0 and then Physical mod Page_Bytes = 0 and then Bytes mod Page_Bytes = 0
      and then Physical < Byte_Limit and then Bytes <= Byte_Limit - Physical);

   --  [Physical, Physical + Bytes) is nonempty and does not wrap.
   function Valid_Bytes (Physical, Bytes : Unsigned_64) return Boolean is
     (Bytes > 0 and then Bytes - 1 <= Unsigned_64'Last - Physical);

   --  No page of [Physical, Physical + Bytes) is in claim C.
   function Disjoint (C : Claim; Physical, Bytes : Unsigned_64) return Boolean is
     ((Physical + (Bytes - 1)) / Page_Bytes < C.First or else Physical / Page_Bytes >= C.Ends)
   with Pre => Valid_Bytes (Physical, Bytes);

   --  The ACPI claim for whole pages [Physical, Physical + Bytes).
   function Pages_Of (Physical, Bytes : Unsigned_64) return Claim is
     ((First => Physical / Page_Bytes, Ends => (Physical + Bytes) / Page_Bytes))
   with Pre => Whole_Pages (Physical, Bytes);

   procedure Initialize (Item : out Ledger)
   with Post => Item.Count = 0 and then Valid (Item);

   --  Claim whole pages [Physical, Physical + Bytes) for ACPI.
   procedure Claim_ACPI
     (Item : in out Ledger; Physical, Bytes : Unsigned_64; Granted : out Boolean)
   with Pre  => Valid (Item),
        Post => Valid (Item)
                and then Item.Count >= Item'Old.Count
                and then (for all K in 1 .. Item'Old.Count =>
                            Item.Claims (K) = Item'Old.Claims (K))
                and then
                (if Granted then
                   Whole_Pages (Physical, Bytes)
                   --  It is recorded, and it never partly overlaps another.
                   and then (for some K in 1 .. Item.Count =>
                               Item.Claims (K) = Pages_Of (Physical, Bytes))
                   and then (for all K in 1 .. Item'Old.Count =>
                               Item'Old.Claims (K) = Pages_Of (Physical, Bytes)
                               or else Disjoint (Item'Old.Claims (K), Physical, Bytes))
                 else Item.Count = Item'Old.Count);

   --  No page of [Physical, Physical + Bytes) is ACPI's: a driver or devmgr
   --  may have it.
   function Clear_Of_ACPI (Item : Ledger; Physical, Bytes : Unsigned_64) return Boolean is
     (Valid_Bytes (Physical, Bytes)
      and then (for all K in 1 .. Item.Count => Disjoint (Item.Claims (K), Physical, Bytes)));

end Devmgr_Memory_Claims;

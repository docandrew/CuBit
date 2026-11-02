------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Which physical pages are ACPI's and which a device's: devmgr's record of
--  the firmware-memory windows it holds for the ACPI hardware grant (the
--  kernel's NVS runs, FACS page and PCI slice), the table-named firmware
--  data it has granted, and the device memory it has handed out or mapped,
--  so ACPI and devices never share a page (CuBit.ACPI_Window_Exchange,
--  CuBit.Firmware_Data_Exchange, coordination/acpi-region-boundary.md).
--
--  @description
--  The kernel enforces each capability's own range and keeps NVS pages out
--  of device memory, but firmware-reserved pages (the FACS page, firmware
--  data) are device memory to the kernel too; this ledger is the
--  exclusivity record. Claims are pages and are never removed: an ACPI
--  window or data envelope stays ACPI's for the rest of the boot, whether
--  or not it is mapped, and so does a page handed to a device. It is the
--  proved core over an array the caller provides and grows
--  (Devmgr_Claim_Ledgers keeps it on the heap, LIM-001b).
--
--  * An ACPI window is whole pages inside the kernel's physical limit,
--    the same as or disjoint from every earlier ACPI claim (the same window
--    again is granted: a restarted backend), and disjoint from every device
--    claim.
--  * A firmware-data envelope is whole pages, disjoint from every ACPI
--    window and every device claim; it may overlap other data envelopes
--    (several declarations can share a page; all are uncached, all ACPI's).
--  * A device claim (a driver's window, devmgr's own mapping) covers every
--    page its bytes touch and is disjoint from every ACPI claim; device
--    claims may overlap each other (devmgr's own concern).
--  * A claim that needs a new entry when the array is full is Needs_Room:
--    the caller grows the array and asks again.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Devmgr_Memory_Claims with Pure, SPARK_Mode is

   Page_Bytes : constant := 4096;
   --  The kernel's physical limit (Physical_Ranges.Address_Limit), in pages.
   Page_Limit : constant := 2 ** 35;
   Byte_Limit : constant := Page_Limit * Page_Bytes;

   type Holder is (ACPI_Window, ACPI_Data, Device);

   function Is_ACPI (By : Holder) return Boolean is (By /= Device);

   --  Pages [First, Ends).
   type Claim is record
      First : Unsigned_64 := 0;
      Ends  : Unsigned_64 := 0;
      By    : Holder := Device;
   end record;

   type Claim_Array is array (Positive range <>) of Claim;

   --  Every claim is a nonempty page range inside the limit.
   function Valid (Claims : Claim_Array; Count : Natural) return Boolean is
     (Count <= Claims'Last
      and then (for all K in 1 .. Count =>
                  Claims (K).First < Claims (K).Ends and then Claims (K).Ends <= Page_Limit))
   with Pre => Claims'First = 1;

   --  [Physical, Physical + Bytes) is whole pages inside the limit.
   function Whole_Pages (Physical, Bytes : Unsigned_64) return Boolean is
     (Bytes > 0 and then Physical mod Page_Bytes = 0 and then Bytes mod Page_Bytes = 0
      and then Physical < Byte_Limit and then Bytes <= Byte_Limit - Physical);

   --  [Physical, Physical + Bytes) is nonempty and does not wrap.
   function Valid_Bytes (Physical, Bytes : Unsigned_64) return Boolean is
     (Bytes > 0 and then Bytes - 1 <= Unsigned_64'Last - Physical);

   --  [Physical, Physical + Bytes) is nonempty and inside the limit.
   function In_Limit (Physical, Bytes : Unsigned_64) return Boolean is
     (Bytes > 0 and then Physical < Byte_Limit and then Bytes <= Byte_Limit - Physical);

   --  No page of [Physical, Physical + Bytes) is in claim C.
   function Disjoint (C : Claim; Physical, Bytes : Unsigned_64) return Boolean is
     ((Physical + (Bytes - 1)) / Page_Bytes < C.First or else Physical / Page_Bytes >= C.Ends)
   with Pre => Valid_Bytes (Physical, Bytes);

   --  The pages [Physical, Physical + Bytes) touches, partial ones too.
   function Pages_Of (Physical, Bytes : Unsigned_64; By : Holder) return Claim is
     ((First => Physical / Page_Bytes, Ends => (Physical + (Bytes - 1)) / Page_Bytes + 1,
       By => By))
   with Pre => In_Limit (Physical, Bytes);

   type Claim_Result is
     (Granted,          --  recorded (or already recorded exactly)
      Overlaps_ACPI,    --  a page of it is an ACPI window or data envelope
      Overlaps_Device,  --  a page of it is a device's
      Malformed,        --  not whole pages (ACPI), empty, or past the limit
      Needs_Room);      --  a new entry, and the array is full

   --  Who may not share a page with a claim by By: ACPI windows exclude
   --  everything but the same window; data excludes windows and devices;
   --  devices exclude ACPI.
   function Conflicts (Earlier : Holder; By : Holder) return Boolean is
     (case By is
        when ACPI_Window => True,
        when ACPI_Data   => Earlier /= ACPI_Data,
        when Device      => Earlier /= Device);

   procedure Claim_Pages
     (Claims : in out Claim_Array; Count : in out Natural;
      Physical, Bytes : Unsigned_64; By : Holder; Result : out Claim_Result)
   with Pre  => Claims'First = 1 and then Valid (Claims, Count),
        Post => Valid (Claims, Count)
                and then Count >= Count'Old
                and then (for all K in 1 .. Count'Old => Claims (K) = Claims'Old (K))
                and then (if Result /= Granted then Count = Count'Old)
                and then (if Result = Needs_Room then Count'Old = Claims'Last)
                and then
                (if Result = Granted then
                   In_Limit (Physical, Bytes)
                   and then (if Is_ACPI (By) then Whole_Pages (Physical, Bytes))
                   --  It is recorded, and shares no page with a claim it
                   --  conflicts with (an ACPI window: but itself).
                   and then (for some K in 1 .. Count =>
                               Claims (K) = Pages_Of (Physical, Bytes, By))
                   and then (for all K in 1 .. Count'Old =>
                               (if Conflicts (Claims'Old (K).By, By)
                                then Disjoint (Claims'Old (K), Physical, Bytes)
                                     or else (By = ACPI_Window
                                              and then Claims'Old (K) =
                                                         Pages_Of (Physical, Bytes, By)))));

   --  No page of [Physical, Physical + Bytes) is ACPI's (a window or data):
   --  a driver or devmgr may have it.
   function Clear_Of_ACPI
     (Claims : Claim_Array; Count : Natural; Physical, Bytes : Unsigned_64) return Boolean
   is (Valid_Bytes (Physical, Bytes)
       and then (for all K in 1 .. Count =>
                   not Is_ACPI (Claims (K).By) or else Disjoint (Claims (K), Physical, Bytes)))
   with Pre => Claims'First = 1 and then Count <= Claims'Last;

   --  No page of [Physical, Physical + Bytes) is a device's.
   function Clear_Of_Devices
     (Claims : Claim_Array; Count : Natural; Physical, Bytes : Unsigned_64) return Boolean
   is (Valid_Bytes (Physical, Bytes)
       and then (for all K in 1 .. Count =>
                   Claims (K).By /= Device or else Disjoint (Claims (K), Physical, Bytes)))
   with Pre => Claims'First = 1 and then Count <= Claims'Last;

end Devmgr_Memory_Claims;

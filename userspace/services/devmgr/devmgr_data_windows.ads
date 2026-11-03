------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  devmgr's decision on one firmware-data request (CuBit.Firmware_Data_
--  Exchange): which named refusal, or which root answers it.
--
--  @description
--  The kernel bounds each firmware root by class (Physical_Ranges); devmgr
--  adds what only it knows. An envelope is refused when any page of it is
--  the FACS page, PCI configuration space (the kernel's bus 0 slice or any
--  MCFG allocation), or a device's (its claim ledger: every PCI BAR's
--  whole extent and bridge window, sized at enumeration, and every
--  driver's window). Otherwise a page inside one NVS root is answered from that root
--  (write-back, its own tag); an envelope inside one firmware-data root is
--  granted from it (uncached); anything else is No_Root, or
--  Not_Firmware_Class when a page lies in no root at all (RAM, kernel
--  memory, ACPI tables: what the kernel gave devmgr no authority over).
--  Pure and proved: the caller gathers the roots and ranges; the decision
--  never maps, derives or claims anything.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Firmware_Data_Exchange;

package Devmgr_Data_Windows with Pure, SPARK_Mode is

   package FDX renames CuBit.Firmware_Data_Exchange;
   subtype Disposition is FDX.Disposition;
   use type FDX.Disposition;

   Page_Limit : constant := FDX.Page_Limit;

   --  Pages [First_Page, First_Page + Pages).
   type Page_Range is record
      First_Page : Unsigned_64 := 0;
      Pages      : Unsigned_64 := 0;
   end record;
   type Range_Array is array (Positive range <>) of Page_Range;

   function Valid_Range (R : Page_Range) return Boolean is
     (R.Pages >= 1 and then R.First_Page < Page_Limit
      and then R.Pages <= Page_Limit - R.First_Page);

   function Ranges_Valid (Items : Range_Array) return Boolean is
     (for all I in Items'Range => Valid_Range (Items (I)));

   --  Some page of A is a page of B.
   function Touches (A, B : Page_Range) return Boolean is
     (A.First_Page < B.First_Page + B.Pages and then B.First_Page < A.First_Page + A.Pages)
   with Pre => Valid_Range (A) and then Valid_Range (B);

   --  Every page of A is a page of B.
   function Inside (A, B : Page_Range) return Boolean is
     (B.First_Page <= A.First_Page
      and then A.First_Page + A.Pages <= B.First_Page + B.Pages)
   with Pre => Valid_Range (A) and then Valid_Range (B);

   function Touches_Any (A : Page_Range; Items : Range_Array) return Boolean is
     (for some I in Items'Range => Touches (A, Items (I)))
   with Pre => Valid_Range (A) and then Ranges_Valid (Items);

   --  The firmware roots devmgr holds, by kind (their kernel tags).
   type Root_Kind is (NVS_Root, Control_Root, PCI_Config_Root, Data_Root);
   type Root is record
      Pages : Page_Range;
      Kind  : Root_Kind := NVS_Root;
   end record;
   type Root_Array is array (Positive range <>) of Root;

   function Roots_Valid (Items : Root_Array) return Boolean is
     (for all I in Items'Range => Valid_Range (Items (I).Pages));

   type Decision is record
      Answer : Disposition := FDX.Malformed;
      Root   : Natural := 0;   --  the root answering a granted request
   end record;

   --  The envelope's pages, and what devmgr knows about them: its firmware
   --  roots, its device-memory roots, the MCFG's ECAM allocations, and
   --  whether a device claim holds a page of it (Devmgr_Memory_Claims: a
   --  PCI BAR's extent or bridge window, or a driver's window).
   function Decide
     (Envelope : Page_Range;
      Firmware : Root_Array;
      Devices  : Range_Array;
      ECAM     : Range_Array;
      Device_Claimed : Boolean) return Decision
   with Pre  => Roots_Valid (Firmware) and then Ranges_Valid (Devices)
                and then Ranges_Valid (ECAM),
        Post =>
          (if not Valid_Range (Envelope) then Decide'Result.Answer = FDX.Malformed)
          and then
          (if FDX.Granted (Decide'Result.Answer) then
             Valid_Range (Envelope)
             and then Decide'Result.Root in Firmware'Range
             and then Inside (Envelope, Firmware (Decide'Result.Root).Pages)
             and then Firmware (Decide'Result.Root).Kind =
                        (if Decide'Result.Answer = FDX.Granted_Data then Data_Root else NVS_Root)
             --  Never the FACS page, PCI configuration or a device's.
             and then not Device_Claimed
             and then not Touches_Any (Envelope, ECAM)
             and then (for all I in Firmware'Range =>
                         (if Firmware (I).Kind in Control_Root | PCI_Config_Root
                          then not Touches (Envelope, Firmware (I).Pages)))
             --  Never an uncached alias of an NVS page.
             and then (if Decide'Result.Answer = FDX.Granted_Data then
                         (for all I in Firmware'Range =>
                            (if Firmware (I).Kind = NVS_Root
                             then not Touches (Envelope, Firmware (I).Pages)))));

end Devmgr_Data_Windows;

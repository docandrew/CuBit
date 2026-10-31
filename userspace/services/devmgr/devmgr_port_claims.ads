------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Who holds which I/O ports: devmgr's record of the port ranges it has
--  handed out (or the kernel keeps), so the ACPI windows it grants for
--  acpi-backend.svc and its drivers' ports never overlap
--  (CuBit.ACPI_Window_Exchange, coordination/acpi-region-boundary.md).
--
--  @description
--  The kernel enforces each I/O-port capability's own range but keeps no
--  record of exclusivity; this ledger is that record. Entries are never
--  removed: an ACPI window stays claimed for the rest of the boot, and so
--  does a driver's range (a restarted driver asks for the same one).
--
--  * An ACPI claim is refused if any port of it is held by devmgr or a
--    driver, or if it partly overlaps an earlier ACPI claim; the same
--    ACPI range again is granted (a restarted backend).
--  * A driver or devmgr claim is refused if any port of it is in an ACPI
--    window; ranges of drivers and devmgr may repeat or overlap each other
--    (devmgr's own concern, as before).
--  * A full ledger refuses every new range (fail closed).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Devmgr_Port_Claims with Pure, SPARK_Mode is

   Port_Space : constant := 65_536;

   type Holder is (Kernel, Device_Manager, Driver, ACPI);

   type Claim is record
      First  : Unsigned_32 := 0;   --  first port
      Ends   : Unsigned_32 := 0;   --  one past the last port
      By     : Holder := Kernel;
   end record;

   Maximum_Claims : constant := 48;
   subtype Claim_Count is Natural range 0 .. Maximum_Claims;
   subtype Claim_Index is Positive range 1 .. Maximum_Claims;
   type Claim_Array is array (Claim_Index) of Claim;

   type Ledger is record
      Count  : Claim_Count := 0;
      Claims : Claim_Array := [others => (others => <>)];
   end record;

   function Valid_Range (First, Count : Unsigned_64) return Boolean is
     (Count >= 1 and then First < Port_Space and then Count <= Port_Space - First);

   function Overlaps (Item : Claim; First, Ends : Unsigned_32) return Boolean is
     (First < Item.Ends and then Item.First < Ends);

   --  The ranges the kernel and devmgr itself use before any driver runs:
   --  the interrupt controllers, the interval timer, the CMOS clock, the
   --  PCI configuration ports and the kernel's serial console.
   procedure Initialize (Item : out Ledger)
   with Post => Item.Count <= Maximum_Claims;

   procedure Take
     (Item : in out Ledger; First, Count : Unsigned_64; By : Holder;
      Granted : out Boolean)
   with Pre  => By /= Kernel,
        Post => (if Granted then Valid_Range (First, Count))
                and then
                --  An ACPI claim never overlaps another holder's range, and a
                --  granted claim of another holder never overlaps an ACPI one.
                (if Granted then
                   (for all K in 1 .. Item'Old.Count =>
                      (if (By = ACPI) /= (Item'Old.Claims (K).By = ACPI)
                       then not Overlaps (Item'Old.Claims (K), Unsigned_32 (First),
                                          Unsigned_32 (First + Count)))));

end Devmgr_Port_Claims;

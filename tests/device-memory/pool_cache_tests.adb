--  Platform-pool cache classes (ACPI-POOL-001). Pool mappings are uncached,
--  so a page another firmware authority already maps with its own cache
--  class must never be admitted through the pool: the FACS control page
--  (write-back, for the ACPI Global Lock; on q35 it lies in a
--  Firmware_Reserved span) and every validated PCI configuration page.
--  Device_Bytes, the check the pool used before, admits both; the pool now
--  uses Pool_Mappable_Bytes. Fails before Pool_Mappable_Bytes existed.
pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Physical_Ranges; use Physical_Ranges;
procedure Pool_Cache_Tests is
   Page       : constant Unsigned_64 := Page_Bytes;
   Top_First  : constant Unsigned_64 := 16#0FFE_0000#;
   Top_Last   : constant Unsigned_64 := 16#0FFF_FFFF#;
   FACS_Q35   : constant Unsigned_64 := 16#0FFE_0040#;   --  q35 (SeaBIOS)
   ECAM_Base  : constant Unsigned_64 := 16#B000_0000#;   --  q35's MCFG base
   ECAM_End   : constant Unsigned_64 := 16#B03F_FFFF#;   --  buses 0 .. 3
   Failures   : Natural := 0;
   Checks     : Natural := 0;
   procedure Expect (Good : Boolean; Name : String) is
   begin
      Checks := Checks + 1;
      if not Good then
         Failures := Failures + 1;
         Ada.Text_IO.Put_Line ("FAIL " & Name);
      end if;
   end Expect;
   S : Map := Empty_Map;
begin
   Add (S, 16#10_0000#, Top_First - 1, Usable_RAM);
   Add (S, Top_First, Top_Last, Firmware_Reserved);
   Add (S, ECAM_Base, ECAM_End, Firmware_Reserved);
   Add (S, 16#C000_0000#, 16#C00F_FFFF#, Firmware_Reserved);
   Add (S, 0, Low_Memory_Limit - 1, Low_Memory);
   Set_Control (S, FACS_Q35);
   Add_PCI_Config (S, (ECAM_Base, 0, 0, 3, 0));
   Seal (S);
   Expect (S.Control_Known, "control page recorded");
   --  The old check admits both pages (why the alias was possible).
   Expect (Device_Bytes (S, Top_First, Page), "FACS page is device-class");
   Expect (Device_Bytes (S, ECAM_Base, Page), "PCI page is device-class");
   --  The pool's check refuses them.
   Expect (not Pool_Mappable_Bytes (S, Top_First, Page), "FACS page refused");
   Expect (not Pool_Mappable_Bytes (S, Top_First + Page - 16#40#, 16#80#),
           "range touching the FACS page refused");
   Expect (not Pool_Mappable_Bytes (S, Top_First - Page, 2 * Page),
           "range ending on the FACS page refused (page before is RAM anyway)");
   Expect (Pool_Mappable_Bytes (S, Top_First + Page, Page), "page after the FACS admitted");
   Expect (not Pool_Mappable_Bytes (S, ECAM_Base, Page), "first PCI page refused");
   Expect (not Pool_Mappable_Bytes (S, ECAM_End + 1 - Page, Page), "last PCI page refused");
   Expect (not Pool_Mappable_Bytes (S, ECAM_End + 1 - Page, 2 * Page),
           "range leaving the PCI allocation refused");
   Expect (Pool_Mappable_Bytes (S, 16#C000_0000#, 16#10_0000#),
           "other firmware-reserved MMIO admitted");
   --  Empty, wrapping, out of range: refused like Device_Bytes.
   Expect (not Pool_Mappable_Bytes (S, 16#C000_0000#, 0), "empty refused");
   Expect (not Pool_Mappable_Bytes (S, Unsigned_64'Last - 4095, 8192), "wrapping refused");
   Expect (not Pool_Mappable_Bytes (S, Address_Limit, Page), "at the physical limit refused");
   Ada.Text_IO.Put_Line ("pool-cache-tests:" & Natural'Image (Checks) & " checks," &
                         Natural'Image (Failures) & " failures");
   if Failures /= 0 then
      raise Program_Error with "pool cache-class tests failed";
   end if;
end Pool_Cache_Tests;

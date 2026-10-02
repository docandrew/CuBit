pragma Ada_2022;
with Ada.Text_IO;
with Firmware_Tables.Catalog;
with Firmware_Tables.Exposure;
procedure Exposure_Tests is
   use Firmware_Tables;
   use Firmware_Tables.Catalog;
   use Firmware_Tables.Exposure;
   use type Address_Value;
   Checks : Natural := 0;
   procedure Check (B : Boolean) is
   begin
      Checks := Checks + 1;
      if not B then raise Program_Error with Checks'Image; end if;
   end Check;
   S : State;
   D : Descriptor := (Physical => 4096, Extent => 4096, Name => "DSDT", Revision => 2);
   W : Window;
   P : Plan;
begin
   for Offset in 0 .. Page_Size - 1 loop
      for Size in 0 .. 4 loop
         D.Physical := 4096 + Address_Value (Offset);
         D.Extent := (case Size is
           when 0 => 36, when 1 => 4095, when 2 => 4096,
           when 3 => 4097, when 4 => Table_Length'Last);
         W := Page_Window (D);
         Check (W.First = 4096 and W.Offset = Address_Value (Offset));
         Check (W.Pages =
           (Address_Value (Offset) + Address_Value (D.Extent) + Page_Size - 1) / Page_Size);
         Check (W.Last = 4096 + W.Pages * Page_Size - 1);
      end loop;
   end loop;
   D := (Physical => Address_Value'Last - 4095, Extent => 4096,
         Name => "DSDT", Revision => 2);
   Reset (S); Include (S, D); Seal (S);
   P := Describe (S, 1);
   Check (P.Kind = Retained_Candidate and P.Span.Last = Address_Value'Last);
   Check (P.Span.Pages = 1 and P.Span.Offset = 0);

   -- The same rounded page, with exact coverage, a one-byte gap, overlap,
   -- unknown leading/trailing bytes, and a chain in reverse discovery order.
   for Case_ID in 0 .. 5 loop
      Reset (S);
      case Case_ID is
         when 0 =>
            Include (S, (4096, 100, "SSDT", 2));
            Include (S, (4196, 3996, "DSDT", 2));
         when 1 =>
            Include (S, (4096, 100, "SSDT", 2));
            Include (S, (4197, 3995, "DSDT", 2));
         when 2 =>
            Include (S, (4096, 200, "SSDT", 2));
            Include (S, (4196, 3996, "DSDT", 2));
         when 3 => Include (S, (4097, 4095, "DSDT", 2));
         when 4 => Include (S, (4096, 4095, "DSDT", 2));
         when 5 =>
            Include (S, (7168, 1024, "SSDT", 2));
            Include (S, (6144, 1024, "SSDT", 2));
            Include (S, (5120, 1024, "SSDT", 2));
            Include (S, (4096, 1024, "DSDT", 2));
      end case;
      Seal (S);
      for I in 1 .. Count (S) loop
         P := Describe (S, I);
         Check (P.Kind = (if Case_ID in 0 | 2 | 5 then Retained_Candidate else Copy_Required));
         Check (P.Span.First = 4096 and P.Span.Last = 8191);
      end loop;
   end loop;
   -- Cover a multi-megabyte page-aligned table. Table_Length'Last is now a
   -- representation bound, not page-aligned; its geometry is exercised above
   -- at every starting offset without enumerating gigabytes of Ghost ranges.
   -- Unknown bytes in a second page still prevent direct exposure.
   Reset (S);
   Include (S, (4096, 2 * 1024 * 1024, "DSDT", 2));
   Seal (S);
   Check (Describe (S, 1).Kind = Retained_Candidate);
   Reset (S);
   Include (S, (4096, 4097, "DSDT", 2));
   Seal (S);
   Check (Describe (S, 1).Kind = Copy_Required);
   Ada.Text_IO.Put_Line ("ACPI-EXPOSURE: PASS" & Checks'Image);
end Exposure_Tests;

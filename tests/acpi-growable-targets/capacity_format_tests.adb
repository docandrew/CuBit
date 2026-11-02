pragma Ada_2022;
with Ada.Text_IO;
with Ada.Strings.Fixed;
with ACPI_Capacity_Diagnostics;
procedure Capacity_Format_Tests is
   package D renames ACPI_Capacity_Diagnostics;
   Item : D.Report := (D.NVS_Targets, D.Process_Heap, 33, 32, 2_147_483_647, 3_584, 0);
   Checks : Natural := 0;
   procedure Contains (Line, Needle : String) is
   begin
      Checks := Checks + 1;
      if Ada.Strings.Fixed.Index (Line, Needle) = 0 then
         raise Program_Error with "missing " & Needle & " in " & Line;
      end if;
   end Contains;
begin
   Contains (D.Format (Item), "cache=NVS_TARGETS");
   Contains (D.Format (Item), "limit=PROCESS_HEAP_BUDGET");
   Contains (D.Format (Item), "demand= 33");
   Contains (D.Format (Item), "capacity= 32");
   Contains (D.Format (Item), "bytes= 3584");
   Item.Failure := D.Entry_Representation;
   Contains (D.Format (Item), "limit=ENTRY_REPRESENTATION 2147483647");
   Item.Failure := D.Byte_Representation; Item.Byte_Limit := 9_223_372_036_854_775_807;
   Contains (D.Format (Item), "limit=BYTE_REPRESENTATION 9223372036854775807");
   Item.Cache := D.Combined_Targets;
   Contains (D.Format (Item), "cache=COMBINED_TARGETS");
   if D.Present (D.No_Report) or else not D.Present (Item) then
      raise Program_Error with "diagnostic presence";
   end if;
   Ada.Text_IO.Put_Line ("PASS capacity format checks=" & Checks'Image);
end Capacity_Format_Tests;

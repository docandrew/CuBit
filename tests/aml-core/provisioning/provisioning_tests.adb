pragma Ada_2022;
with Ada.Text_IO;
with Firmware_Tables.Catalog;
with Firmware_Tables.Provisioning;
procedure Provisioning_Tests is
   use Firmware_Tables;
   use Firmware_Tables.Catalog;
   use Firmware_Tables.Provisioning;
   S : Catalog.State;
   P : Plan;
   Checks : Natural := 0;
   procedure Check (B : Boolean) is
   begin
      Checks := Checks + 1;
      if not B then raise Program_Error with Checks'Image; end if;
   end Check;
begin
   Check (Measure (S) = (0, 0, 0));
   for N in 1 .. Catalog.Max_Tables loop
      Reset (S);
      for I in 1 .. N loop
         Include (S, (Physical => 4096, Extent => 36 + I * 4096,
                      Name => (if I = 1 then "DSDT" else "SSDT"),
                      Revision => 2));
      end loop;
      Check (Select_Capacity (S, 256, Positive'Last, Positive'Last).Status =
             Inventory_Unavailable);
      Seal (S);
      declare
         Expected : constant Natural := N * 36 + 4096 * (N * (N + 1) / 2);
         Largest : constant Positive := 36 + N * 4096;
      begin
         P := Select_Capacity (S, N, Expected, Largest);
         Check (P.Status = Approved);
         Check (P.Needed = (N, Byte_Count (Expected), Largest));
         Check (Select_Capacity (S, N, Expected - 1, Largest).Status = Byte_Quota);
         Check (Select_Capacity (S, N, Expected, Largest - 1).Status = Individual_Quota);
         if N > 1 then
            Check (Select_Capacity (S, N - 1, Expected, Largest).Status = Table_Quota);
         end if;
      end;
      Reject (S);
      Check (Measure (S) = (0, 0, 0));
   end loop;
   -- Largest representable descriptors without allocating their payloads:
   -- requirements must survive totals greater than an array's index range.
   Reset (S);
   for I in 1 .. Catalog.Max_Tables loop
      Include (S, (Physical => 4096, Extent => Positive'Last,
                   Name => (if I = 1 then "DSDT" else "SSDT"), Revision => 2));
   end loop;
   Seal (S);
   P := Select_Capacity (S, 256, Positive'Last, Positive'Last);
   Check (P.Status = Byte_Quota);
   Check (P.Needed = (256, 256 * Byte_Count (Positive'Last), Positive'Last));
   Include (S, (others => <>));
   Check (Select_Capacity (S, 256, Positive'Last, Positive'Last).Status =
          Inventory_Unavailable);
   Ada.Text_IO.Put_Line ("ACPI provisioning checks:" & Checks'Image);
end Provisioning_Tests;

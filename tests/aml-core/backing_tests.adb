pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with Multiboot_Memory_Map.Reclaim;
procedure Backing_Tests is
   use Multiboot_Memory_Map;
   use Multiboot_Memory_Map.Reclaim;
   Map : Entries (7 .. 10);
   Checks : Natural := 0;
   procedure Check (B : Boolean) is
   begin
      Checks := Checks + 1;
      if not B then raise Program_Error with Checks'Image; end if;
   end Check;
   function Oracle (M : Entries; First, Last : Unsigned_64) return Boolean is
      Found : Boolean;
   begin
      if First > Last then return False; end if;
      for R of M loop
         if not R.Empty and then R.First > R.Last then return False; end if;
      end loop;
      for A in First .. Last loop
         Found := False;
         for R of M loop
            if not R.Empty and then R.First <= A and then A <= R.Last then
               if R.Kind /= ACPI_Reclaim then return False; end if;
               Found := True;
            end if;
         end loop;
         if not Found then return False; end if;
      end loop;
      return True;
   end Oracle;
begin
   -- Independent per-byte oracle across gaps, overlap, kinds, empty entries
   -- and permuted discovery. Non-one-based arrays exercise adapter slicing.
   for Kind in Region_Kind loop
      for Start in 0 .. 8 loop
         for Finish in Start .. 8 loop
            for Reverse_Order in Boolean loop
               Map := [others => (others => <>)];
               Map (7) := (0, 3, ACPI_Reclaim, False);
               Map (8) := (4, 8, ACPI_Reclaim, False);
               Map (9) := (Unsigned_64 (Start), Unsigned_64 (Finish), Kind, False);
               if Reverse_Order then
                  declare
                     Temp : constant Decoded_Region := Map (7);
                  begin Map (7) := Map (9); Map (9) := Temp; end;
               end if;
               for First in 0 .. 9 loop
                  for Last in First .. 9 loop
                     Check (Covers (Map, Unsigned_64 (First), Unsigned_64 (Last)) =
                       Oracle (Map, Unsigned_64 (First), Unsigned_64 (Last)));
                  end loop;
               end loop;
            end loop;
         end loop;
      end loop;
   end loop;
   Map := [others => (others => <>)];
   Check (not Covers (Map, 0, 0));
   Map (7) := (0, 3, ACPI_Reclaim, False);
   Map (8) := (5, 8, ACPI_Reclaim, False);
   Check (not Covers (Map, 0, 8));
   Check (Covers (Map, 5, 8));
   Map := [others => (others => <>)];
   Map (7) := (Unsigned_64'Last - 2, Unsigned_64'Last, ACPI_Reclaim, False);
   Check (Covers (Map, Unsigned_64'Last - 2, Unsigned_64'Last));
   Check (not Covers (Map, Unsigned_64'Last - 3, Unsigned_64'Last));
   Map (8) := (Unsigned_64'Last, Unsigned_64'Last, ACPI_NVS, False);
   Check (not Covers (Map, Unsigned_64'Last - 2, Unsigned_64'Last));
   Map (8) := (99, 1, ACPI_Reclaim, False);
   Check (not Covers (Map, Unsigned_64'Last - 2, Unsigned_64'Last));
   Check (not Covers (Map, 9, 0));
   Check (not Covers (Map (7 .. 6), 0, 0));
   Ada.Text_IO.Put_Line ("ACPI-BACKING: PASS" & Checks'Image);
end Backing_Tests;

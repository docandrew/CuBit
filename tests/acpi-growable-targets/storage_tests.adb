pragma Ada_2022;
with Ada.Text_IO;
with ACPI_Capacity_Growth;
with ACPI_Target_Storage;
procedure Storage_Tests is
   package Growth renames ACPI_Capacity_Growth;
   package Data is new ACPI_Target_Storage (Natural, 2);
   Store : Data.Context;
   Checks : Natural := 0;
   Index : Natural;
   Result : Data.Outcome;
   use type Data.Outcome;
   procedure Check (Good : Boolean) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Checks'Image; end if;
   end Check;
begin
   for Current in 0 .. 128 loop
      for Demand in 0 .. 256 loop
         declare Next : constant Natural := Growth.Next_Capacity (Current, Demand, 2); begin
            Check (Next >= Current and then Next >= Demand);
            Check (Demand > Current or else Next = Current);
         end;
      end loop;
   end loop;
   Check (Growth.Next_Capacity (Natural'Last / 2 + 1, Natural'Last, 2) = Natural'Last);
   Check (Growth.Next_Capacity (0, Natural'Last, 2) = Natural'Last);
   Check (Growth.Next_Capacity (Natural'Last, 0, 2) = Natural'Last);
   for I in 1 .. 4_097 loop
      Data.Append (Store, I, Index, Result);
      Check (Result = Data.Stored and then Index = I and then Data.Length (Store) = I);
      Check (Data.Capacity (Store) >= I and then Data.Read (Store, I) = I);
      -- Every growth boundary must preserve all previously issued ordinals.
      if I = 1 or else I = 3 or else I = 33 or else I = 1_025 or else I = 4_097 then
         for J in 1 .. I loop Check (Data.Read (Store, J) = J); end loop;
      end if;
   end loop;
   Data.Release (Store);
   Check (Data.Length (Store) = 0 and then Data.Capacity (Store) = 0);
   Data.Release (Store);
   Data.Append (Store, 19, Index, Result);
   Check (Result = Data.Stored and then Index = 1 and then Data.Read (Store, 1) = 19);
   Data.Release (Store);
   Ada.Text_IO.Put_Line ("PASS growing target store checks=" & Checks'Image);
end Storage_Tests;

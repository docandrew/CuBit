pragma Ada_2022;
with Ada.Text_IO;
with ACPI_Target_Storage;
with Interfaces;
with ACPI_Capacity_Diagnostics;
procedure Failure_Tests is
   type Payload is array (Positive range 1 .. 4_096) of Character;
   Empty : constant Payload := [others => 'x'];
   package Data is new ACPI_Target_Storage (Payload, 1);
   Store : Data.Context;
   Result : Data.Outcome;
   Index, Before : Natural;
   use type Data.Outcome;
   use type Interfaces.Unsigned_64, ACPI_Capacity_Diagnostics.Failure_Kind;
begin
   -- Runner limits this process's virtual memory. The test must encounter
   -- allocator refusal during growth, while preserving existing contents.
   for I in 1 .. 100_000 loop
      Before := Data.Length (Store);
      Data.Append (Store, Empty, Index, Result);
      if Result = Data.Allocation_Refused then
         if Index /= 0 or else Data.Length (Store) /= Before
           or else Data.Observe (Store).Demand /= Interfaces.Unsigned_64 (Before) + 1
           or else Data.Observe (Store).Capacity /= Data.Capacity (Store)
         then raise Program_Error with "failed growth changed membership"; end if;
         if Data.Capacity_Failure (Store, ACPI_Capacity_Diagnostics.NVS_Targets).Failure /=
           ACPI_Capacity_Diagnostics.Process_Heap
         then raise Program_Error with "wrong capacity diagnostic"; end if;
         for J in 1 .. Before loop
            if Data.Read (Store, J) /= Empty then
               raise Program_Error with "failed growth damaged an old target";
            end if;
         end loop;
         Data.Release (Store);
         Data.Append (Store, Empty, Index, Result);
         if Result /= Data.Stored or else Index /= 1 then
            raise Program_Error with "storage was not reclaimed";
         end if;
         Data.Release (Store);
         Ada.Text_IO.Put_Line ("PASS allocation refusal preserved targets=" & Before'Image);
         return;
      elsif Result /= Data.Stored or else Index /= I then
         raise Program_Error with "unexpected append result";
      end if;
   end loop;
   raise Program_Error with "allocation refusal not exercised";
end Failure_Tests;

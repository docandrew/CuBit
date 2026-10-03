with Ada.Text_IO;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_Extent_Allocator;
with Intel_GPU_Extent_Growth;
with Intel_GPU_Metadata_Arena;
with Intel_GPU_Buffer_Reply;
procedure Extent_Growth_Tests is
   package V renames Intel_GPU_Buffer_Reply;
begin
   for Fault in 0 .. 4 loop
      declare
         Ready : Boolean := True;
         Calls, Reserves, Commits, Clears : Natural := 0;
         function Owner return Boolean is (Ready);
         function Allocate (CPU : Unsigned_64) return Unsigned_64 is
         begin
            pragma Assert (CPU = V.Layout.CPU_Base + Unsigned_64 (Calls) * 2 * 1024 * 1024);
            Calls := Calls + 1;
            return Unsigned_64 (Calls) * 4 * 1024 * 1024;
         end Allocate;
         package A is new Intel_GPU_Extent_Allocator (Owner, Allocate);
         Pool : A.Pool;
         type RAM is array (1 .. 65536) of Unsigned_8 with Alignment => 4096;
         Metadata : RAM;
         Base : constant Unsigned_64 := Unsigned_64 (To_Integer (Metadata'Address));
         function Reserve (Bytes : Unsigned_64) return Unsigned_64 is
         begin
            pragma Assert (Bytes = 65536 and Calls = 16);
            Reserves := Reserves + 1;
            return (if Fault = 1 then 0 else Base);
         end Reserve;
         function Commit (Address, Offset, Bytes : Unsigned_64) return Boolean is
         begin
            pragma Assert (Address = Base and Offset = 0 and Bytes = 65536 and Calls = 16);
            Commits := Commits + 1;
            return Fault /= 2;
         end Commit;
         function Clear (Address, Bytes : Unsigned_64) return Boolean is
         begin
            pragma Assert (Address = Base and Bytes = 65536 and Calls = 16);
            Clears := Clears + 1;
            Metadata := [others => 0];
            return Fault /= 3;
         end Clear;
         package Storage is new Intel_GPU_Metadata_Arena (Reserve, Commit, Clear);
         package Growth is new Intel_GPU_Extent_Growth (A, Pool, Storage, Owner);
         View, First : V.Extent_View;
         OK, More : Boolean;
      begin
         A.Configure_Heap (Pool, 64 * 1024 * 1024, 2 ** 32, OK); pragma Assert (OK);
         for Index in 1 .. 2 loop
            for Turn in 1 .. 16 loop
               declare Before : constant Natural := Calls; begin
                  Growth.Step (7, Index, 4096, 1, View, OK, More);
                  pragma Assert (Calls <= Before + 1);
               end;
               exit when not More;
            end loop;
            pragma Assert (OK and V.Valid (View) and Reserves = 0);
            if Index = 1 then First := View; end if;
         end loop;
         pragma Assert (Calls = 16);
         Growth.Step (7, 3, 512, 1, View, OK, More);
         pragma Assert (More and not OK and Calls = 16 and A.Required_Extent_Metadata (Pool) = 17);
         if Fault = 4 then Ready := False; end if;
         for Turn in 1 .. 10 loop
            declare Before : constant Natural := Calls; begin
               Growth.Step (7, 3, 512, 1, View, OK, More);
               pragma Assert (Calls <= Before + 1 and V.Valid (First));
            end;
            exit when not More;
         end loop;
         pragma Assert (not More);
         if Fault = 0 then
            pragma Assert (OK and Calls = 17 and Reserves = 1 and Commits = 1 and Clears = 1);
            pragma Assert (V.CPU_Address (View) = V.Layout.CPU_Base + 32 * 1024 * 1024);
         else
            pragma Assert (not OK and Calls = 16);
            Ready := True;
            Growth.Step (7, 3, 512, 1, View, OK, More);
            pragma Assert (not OK and not More and Calls = 16);
         end if;
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Supervisor extent growth PASS: bounded saved-request steps, seventeenth extent, independent metadata, reserve/commit/clear/owner faults, no physical replay");
end Extent_Growth_Tests;

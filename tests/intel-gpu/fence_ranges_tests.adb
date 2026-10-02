with Interfaces; use Interfaces;
with Intel_GPU_Fence_Ranges;
procedure Fence_Ranges_Tests is
   package Pool is new Intel_GPU_Fence_Ranges (100, 65535);
   package Edge is new Intel_GPU_Fence_Ranges (65532, 65535);
   Seen : array (Unsigned_16) of Boolean := [others => False];
   First, Last : Unsigned_16;
   OK : Boolean;
begin
   for Width in 4 .. 300 loop
      declare
         Object : Pool.Ledger;
         Previous : Natural := 99;
      begin
         Seen := [others => False];
         for Bad in 0 .. 3 loop
            Pool.Reserve (Object, Bad, First, Last, OK);
            pragma Assert (not OK and First = 0 and Last = 0 and Pool.Cursor (Object) = 100);
         end loop;
         Pool.Reserve (Object, Natural'Last, First, Last, OK);
         pragma Assert (not OK and Pool.Cursor (Object) = 100);
         while Pool.Remaining (Object) >= Width loop
            Pool.Reserve (Object, Width, First, Last, OK);
            pragma Assert (OK and Natural (First) = Previous + 1);
            pragma Assert (Natural (Last) = Natural (First) + Width - 1);
            for Fence in First .. Last loop
               pragma Assert (not Seen (Fence));
               Seen (Fence) := True;
            end loop;
            Previous := Natural (Last);
         end loop;
         Pool.Reserve (Object, Width, First, Last, OK);
         pragma Assert (not OK and First = 0 and Last = 0);
         pragma Assert (Pool.Cursor (Object) = Previous + 1);
      end;
   end loop;
   declare
      Object : Edge.Ledger;
   begin
      Edge.Reserve (Object, 4, First, Last, OK);
      pragma Assert (OK and First = 65532 and Last = 65535);
      Edge.Reserve (Object, 4, First, Last, OK);
      pragma Assert (not OK and First = 0 and Last = 0 and Edge.Cursor (Object) = 65536);
   end;
end Fence_Ranges_Tests;

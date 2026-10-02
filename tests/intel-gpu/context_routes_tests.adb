with Interfaces; use Interfaces;
with Intel_GPU_Context_Routes;
procedure Context_Routes_Tests with SPARK_Mode => Off is
   package R is new Intel_GPU_Context_Routes (3);
   Object : R.Registry;
   OK : Boolean;
   use type R.Disposition;
   Target : R.Destination;
begin
   pragma Assert (R.Count (Object) = 0 and not R.Contains (Object, 0));
   for Fence in Unsigned_16 loop
      pragma Assert (R.Owner (Object, Fence) = R.No_Context);
   end loop;
   R.Register (Object, 7, 100, 105, OK); pragma Assert (OK);
   R.Register (Object, 9, 106, 110, OK); pragma Assert (OK);
   for First in Unsigned_16 range 0 .. 112 loop
      for Last in Unsigned_16 range 0 .. 112 loop
         if First = 0 or else Last < First or else
           Unsigned_32 (Last) < Unsigned_32 (First) + 3 or else
           (First <= 110 and Last >= 100)
         then
            R.Register (Object, 11, First, Last, OK);
            pragma Assert (not OK and R.Count (Object) = 2);
         end if;
      end loop;
   end loop;
   R.Register (Object, 7, 200, 210, OK); pragma Assert (not OK);
   R.Register (Object, 65535, 200, 210, OK); pragma Assert (not OK);
   R.Register (Object, Unsigned_32'Last, 200, 210, OK); pragma Assert (not OK);
   R.Register (Object, 11, 65532, 65535, OK); pragma Assert (OK);
   R.Register (Object, 12, 200, 210, OK); pragma Assert (not OK);
   pragma Assert (R.Count (Object) = 3);
   for Fence in Unsigned_16 loop
      pragma Assert (R.Owner (Object, Fence) =
        (if Fence in 100 .. 105 then 7 elsif Fence in 106 .. 110 then 9
         elsif Fence >= 65532 then 11 else R.No_Context));
      Target := R.Select_Destination (Object, [16#E0000001#], Fence);
      pragma Assert (Target.ID = R.Owner (Object, Fence));
      pragma Assert (Target.Kind =
        (if Target.ID = R.No_Context then R.Unclaimed else R.Context_Message));
      -- Scheduling IDs override even another context's fence; CT fences
      -- are not the correlation key for this asynchronous event.
      Target := R.Select_Destination (Object, [16#90001002#, 9, 1], Fence);
      pragma Assert (Target.Kind = R.Context_Message and Target.ID = 9);
      Target := R.Select_Destination (Object, [16#90004600#, 7], Fence);
      pragma Assert (Target.Kind = R.Context_Message and Target.ID = 7);
      Target := R.Select_Destination (Object, [16#F0000000#], Fence);
      pragma Assert (Target.Kind = R.Unclaimed and Target.ID = R.No_Context);
   end loop;
   Target := R.Select_Destination (Object, [16#90001002#, 12, 1], 100);
   pragma Assert (Target.Kind = R.Unclaimed and Target.ID = R.No_Context);
   Target := R.Select_Destination (Object, [16#90001002#, 7, 2], 100);
   pragma Assert (Target.Kind = R.Invalid_Message and Target.ID = R.No_Context);
   Target := R.Select_Destination (Object, [16#E0000001#, 0], 100);
   pragma Assert (Target.Kind = R.Invalid_Message and Target.ID = R.No_Context);
   Target := R.Select_Destination (Object, [16#90004600#, 12], 100);
   pragma Assert (Target.Kind = R.Unclaimed and Target.ID = R.No_Context);
   Target := R.Select_Destination (Object, [16#90004600#, 7, 0], 100);
   pragma Assert (Target.Kind = R.Invalid_Message and Target.ID = R.No_Context);
   declare
      package Single is new Intel_GPU_Context_Routes (1);
      Edge : Single.Registry;
   begin
      Single.Register (Edge, 0, 1, 4, OK);
      pragma Assert (OK and Single.Contains (Edge, 0));
      pragma Assert (Single.Owner (Edge, 0) = Single.No_Context);
      pragma Assert (Single.Owner (Edge, 1) = 0);
      pragma Assert (Single.Owner (Edge, 4) = 0);
      pragma Assert (Single.Owner (Edge, 5) = Single.No_Context);
      Single.Register (Edge, 1, 5, 8, OK);
      pragma Assert (not OK and Single.Count (Edge) = 1);
      pragma Assert (Single.Owner (Edge, 1) = 0);
      pragma Assert (Single.Owner (Edge, 5) = Single.No_Context);
   end;
end Context_Routes_Tests;

with Interfaces; use Interfaces;
with Intel_GPU_GuC_Context_Event;
procedure GuC_Context_Event_Tests is
   package Events renames Intel_GPU_GuC_Context_Event;
   use type Events.Kind;
   Value : Events.Event;
begin
   for ID in Unsigned_32 range 0 .. 65534 loop
      Value := Events.Decode ([5 => 16#90004600#, 6 => ID], 77);
      pragma Assert (Value.Tag = Events.Deregister_Done and Value.ID = ID and
                     Value.Runnable = 0 and Value.Fence = 77);
      for Mode in Unsigned_32 range 0 .. 1 loop
         Value := Events.Decode ([5 => 16#90001002#, 6 => ID, 7 => Mode], 53);
         pragma Assert (Value.Tag = Events.Scheduling_Done and Value.ID = ID and
                        Value.Runnable = Mode and Value.Fence = 53);
      end loop;
   end loop;
   for Length in 0 .. 4 loop
      declare Data : constant Events.Words (1 .. 4) := [16#90004600#, 7, 0, 0]; begin
         if Length /= 2 then
            Value := Events.Decode (Data (1 .. Length), 0);
            pragma Assert (Value.Tag = Events.Malformed);
         end if;
      end;
   end loop;
   Value := Events.Decode ([16#90004600#, 65535], 0);
   pragma Assert (Value.Tag = Events.Malformed);
   Value := Events.Decode ([16#90004600#, Unsigned_32'Last], 0);
   pragma Assert (Value.Tag = Events.Malformed);
   Value := Events.Decode ([16#10004600#, 7], 0);
   pragma Assert (Value.Tag = Events.Malformed);
   for Bit in 16 .. 27 loop
      Value := Events.Decode ([16#90004600# or Shift_Left (Unsigned_32'(1), Bit), 7], 0);
      pragma Assert (Value.Tag = Events.Malformed);
   end loop;
   for Length in 0 .. 4 loop
      declare Data : constant Events.Words (1 .. 4) := [16#90001002#, 7, 1, 0]; begin
         if Length /= 3 then
            Value := Events.Decode (Data (1 .. Length), 0);
            pragma Assert (Value.Tag = Events.Malformed);
         end if;
      end;
   end loop;
   Value := Events.Decode ([16#90001002#, 65535, 1], 0);
   pragma Assert (Value.Tag = Events.Malformed);
   Value := Events.Decode ([16#90001002#, 7, 2], 0);
   pragma Assert (Value.Tag = Events.Malformed);
   Value := Events.Decode ([16#10001002#, 7, 1], 0);
   pragma Assert (Value.Tag = Events.Malformed);
   Value := Events.Decode ([16#90011002#, 7, 1], 0);
   pragma Assert (Value.Tag = Events.Malformed);
   Value := Events.Decode ([16#EABC1234#], 51);
   pragma Assert (Value.Tag = Events.Request_Failure and Value.Fence = 51 and
                  Value.Error_Code = 16#1234# and Value.Hint = 16#ABC#);
   Value := Events.Decode ([16#E0000001#, 0], 51);
   pragma Assert (Value.Tag = Events.Malformed);
   Value := Events.Decode ([16#F0000000#], 42);
   pragma Assert (Value.Tag = Events.Other_Message);
   Value := Events.Decode ([16#90008002#, 0], 0);
   pragma Assert (Value.Tag = Events.Other_Message);
end GuC_Context_Event_Tests;

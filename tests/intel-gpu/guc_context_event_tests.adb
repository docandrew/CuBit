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
   -- Unsolicited events (H5). STATE_CAPTURE is informational.
   Value := Events.Decode ([16#90008002#, 0], 0);
   pragma Assert (Value.Tag = Events.Notification and Value.Action = 16#8002#);
   for Action of Events.Words'(16#0000#, 16#7001#, 16#8002#, 16#8003#) loop
      Value := Events.Decode ([16#9000_0000# or Action], 9);
      pragma Assert (Value.Tag = Events.Notification and Value.Action = Action and
                     Value.Fence = 9);
   end loop;
   -- CONTEXT_RESET: exactly one guc_id. Device loss until reset recovery.
   for ID in Unsigned_32 range 0 .. 65534 loop
      Value := Events.Decode ([16#90001008#, ID], 31);
      pragma Assert (Value.Tag = Events.Context_Reset and Value.ID = ID and
                     Value.Tag in Events.Device_Loss and Value.Fence = 31);
   end loop;
   Value := Events.Decode ([16#90001008#, 65535], 0);
   pragma Assert (Value.Tag = Events.Malformed);
   Value := Events.Decode ([16#90001008#], 0);
   pragma Assert (Value.Tag = Events.Malformed);
   Value := Events.Decode ([16#90001008#, 7, 0], 0);
   pragma Assert (Value.Tag = Events.Malformed);
   Value := Events.Decode ([16#90011008#, 7], 0);
   pragma Assert (Value.Tag = Events.Malformed);
   -- ENGINE_FAILURE: class, instance, reason.
   Value := Events.Decode ([16#90001009#, 0, 1, 16#DEAD#], 0);
   pragma Assert (Value.Tag = Events.Engine_Failure and Value.Engine_Class = 0 and
                  Value.Engine_Instance = 1 and Value.Reason = 16#DEAD# and
                  Value.Tag in Events.Device_Loss);
   for Length in 1 .. 5 loop
      declare Data : constant Events.Words (1 .. 5) := [16#90001009#, 0, 1, 2, 3]; begin
         if Length /= 4 then
            Value := Events.Decode (Data (1 .. Length), 0);
            pragma Assert (Value.Tag = Events.Malformed);
         end if;
      end;
   end loop;
   -- Crash dump, exception and memory CAT error: the GuC is dead.
   for Action of Events.Words'(16#8004#, 16#8005#, 16#6000#) loop
      Value := Events.Decode ([16#9000_0000# or Action, 0], 0);
      pragma Assert (Value.Tag = Events.GuC_Failure and Value.Action = Action and
                     Value.Tag in Events.Device_Loss);
   end loop;
   -- Any other event stays Other_Message: retained, never dropped.
   Value := Events.Decode ([16#90001234#, 0], 0);
   pragma Assert (Value.Tag = Events.Other_Message);
end GuC_Context_Event_Tests;

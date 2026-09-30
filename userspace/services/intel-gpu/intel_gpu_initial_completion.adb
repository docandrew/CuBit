package body Intel_GPU_Initial_Completion is
   function State (Object : Attempt) return Phase is (Object.Value);
   function Last_Marker (Object : Attempt) return Unsigned_64 is (Object.Marker);
   function Marker_Reads (Object : Attempt) return Natural is (Object.Reads);
   procedure Fail (Object : in out Attempt) is
   begin Object.Value := Quarantined; end Fail;
   function Check (Object : in out Attempt; Status : out Result) return Boolean is
      Current : Unsigned_64;
   begin
      Status := Ownership_Lost;
      if not Owner_Ready then Fail (Object); return False; end if;
      Current := Now_Us;
      if not Owner_Ready then Fail (Object); return False; end if;
      if Current = Unsigned_64'Last or else Current < Object.Previous then
         Status := Invalid_Clock; Fail (Object); return False;
      end if;
      if Current - Object.Started >= 1_000_000 then
         Status := Timed_Out; Fail (Object); return False;
      end if;
      Object.Previous := Current; Status := Ready; return True;
   end Check;
   procedure Arm (Object : in out Attempt; Status : out Result;
                  Previous_Value : Unsigned_32 := 0;
                  Expected_Value : Unsigned_32 := 1) is
      Marker : Unsigned_64;
      OK : Boolean;
   begin
      Status := Rejected;
      if Object.Value /= Fresh then return; end if;
      Object.Value := Quarantined;
      if Previous_Value = Unsigned_32'Last or else
        Expected_Value /= Previous_Value + 1
      then
         return;
      end if;
      Object.Prior_Marker := Unsigned_64 (Previous_Value);
      Object.Target_Marker := Unsigned_64 (Expected_Value);
      Status := Ownership_Lost;
      if not Owner_Ready then return; end if;
      Object.Started := Now_Us; Object.Previous := Object.Started;
      if not Owner_Ready then return; end if;
      if Object.Started = Unsigned_64'Last then Status := Invalid_Clock; return; end if;
      Read_Marker (Marker, OK);
      Object.Reads := 1;
      if OK then Object.Marker := Marker; end if;
      if not Check (Object, Status) then return; end if;
      if not OK then Status := Read_Failed; return; end if;
      if Marker /= Object.Prior_Marker then Status := Unexpected_Marker; return; end if;
      Object.Value := Armed; Status := Ready;
   end Arm;
   procedure Wait (Object : in out Attempt; Poll_Limit : Positive;
                   Status : out Result) is
      Marker : Unsigned_64;
      OK : Boolean;
   begin
      Status := Rejected;
      if Object.Value /= Armed then return; end if;
      -- Latch the attempt before invoking callbacks; any partial failure
      -- keeps backing retained and makes retry impossible.
      Object.Value := Quarantined;
      for Index in 1 .. Poll_Limit loop
         if not Check (Object, Status) then return; end if;
         OK := Service_Events;
         if not Check (Object, Status) then return; end if;
         if not OK then Status := Event_Failed; return; end if;
         Read_Marker (Marker, OK);
         if Object.Reads < Natural'Last then Object.Reads := Object.Reads + 1; end if;
         if OK then Object.Marker := Marker; end if;
         if not Check (Object, Status) then return; end if;
         if not OK then Status := Read_Failed; return; end if;
         if Marker = Object.Target_Marker then
            Object.Value := Observed; Status := Complete; return;
         -- Context-relative barrier post-sync writes may temporarily clear
         -- the scratch marker. Neither zero nor the previous completion is
         -- evidence of this submission finishing.
         elsif Marker /= 0 and Marker /= Object.Prior_Marker then
            Status := Unexpected_Marker; return;
         end if;
         Pause;
      end loop;
      if not Check (Object, Status) then return; end if;
      Status := Timed_Out;
   end Wait;
end Intel_GPU_Initial_Completion;

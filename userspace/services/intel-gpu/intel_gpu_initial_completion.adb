package body Intel_GPU_Initial_Completion is
   package Timeline renames Intel_GPU_Timeline;
   use type Timeline.Value;
   use type Timeline.Observation;
   use type Timeline.Outcome;
   function State (Object : Attempt) return Phase is (Object.Value);
   function Last_Marker (Object : Attempt) return Unsigned_64 is (Object.Marker);
   function Marker_Reads (Object : Attempt) return Natural is (Object.Reads);
   procedure Fail (Object : in out Attempt) is
   begin
      Timeline.Abandon (Object.Timeline);
      Object.Value := Quarantined;
   end Fail;
   procedure Read (Object : in out Attempt; Value : out Unsigned_64; OK : out Boolean) is
   begin
      Read_Marker (Value, OK);
      if Object.Reads < Natural'Last then Object.Reads := Object.Reads + 1; end if;
      if OK then Object.Marker := Value; end if;
   end Read;
   procedure Arm (Object : in out Attempt; Budget_Us : Unsigned_64;
                  Status : out Result;
                  Previous_Value : Unsigned_32 := 0;
                  Expected_Value : Unsigned_32 := 1) is
      Marker, Started : Unsigned_64;
      OK, Accepted : Boolean;
      Prior : constant Timeline.Value := Timeline.Value (Previous_Value);
      Target : constant Timeline.Value := Timeline.Value (Expected_Value);
   begin
      Status := Rejected;
      if Object.Value not in Fresh | Observed then return; end if;
      Object.Value := Quarantined;
      if Previous_Value = Unsigned_32'Last or else
        Expected_Value /= Previous_Value + 1 or else Budget_Us = 0
      then
         return;
      end if;
      Status := Ownership_Lost;
      if not Owner_Ready then return; end if;
      Started := Now_Us;
      if not Owner_Ready then return; end if;
      if Started = Unsigned_64 (Timeline.Clock_Unavailable) then
         Status := Invalid_Clock; return;
      end if;
      Read (Object, Marker, OK);
      if not Owner_Ready then return; end if;
      if not OK then Status := Read_Failed; return; end if;
      -- Before publication the slot must still hold the previous value.
      if Timeline.Classify (Prior, Target, Timeline.Value (Marker)) /= Timeline.Unchanged then
         Status := Unexpected_Marker; return;
      end if;
      Timeline.Start (Object.Timeline, Prior, Target, Timeline.Microseconds (Started),
                      Timeline.Microseconds (Budget_Us), Accepted);
      if not Accepted then Status := Invalid_Clock; return; end if;
      Object.Value := Armed; Status := Ready;
   end Arm;
   procedure Observe (Object : in out Attempt; Gate_Open : Boolean;
                      Status : out Result) is
      Marker, Current : Unsigned_64;
      OK : Boolean;
      Outcome : Timeline.Outcome;
   begin
      Status := Rejected;
      if Object.Value /= Armed then return; end if;
      Status := Ownership_Lost;
      if not Owner_Ready then Fail (Object); return; end if;
      Read (Object, Marker, OK);
      if not Owner_Ready then Fail (Object); return; end if;
      Current := Now_Us;
      if not Owner_Ready then Fail (Object); return; end if;
      Timeline.Step (Object.Timeline, OK, Timeline.Value (Marker), Gate_Open,
                     Timeline.Microseconds (Current), Outcome);
      case Outcome is
         when Timeline.Pending => Status := Pending; return;
         when Timeline.Complete =>
            Object.Value := Observed; Status := Complete; return;
         when Timeline.Timeline_Fault => Status := Unexpected_Marker;
         when Timeline.Read_Fault => Status := Read_Failed;
         when Timeline.Clock_Fault => Status := Invalid_Clock;
         when Timeline.Deadline_Expired => Status := Timed_Out;
         when Timeline.Not_Waiting => Status := Rejected;
      end case;
      Fail (Object);
   end Observe;
   procedure Wait (Object : in out Attempt; Poll_Limit : Positive;
                   Status : out Result) is
   begin
      Status := Rejected;
      if Object.Value /= Armed then return; end if;
      for Index in 1 .. Poll_Limit loop
         Observe (Object, True, Status);
         if Status /= Pending then return; end if;
         if not Service_Events then
            Fail (Object); Status := Event_Failed; return;
         end if;
         Pause;
      end loop;
      Fail (Object);
      Status := Timed_Out;
   end Wait;
end Intel_GPU_Initial_Completion;

pragma Ada_2022;
with HID_Axis_Resolution;
package body HID_Input_Source with SPARK_Mode is
   use type Reports.Status;
   use type Reports.Compile_Status;
   use type Pointer.Status;
   use type Pointer_Pending.Button_State;
   use type Pointer_Pending.Append_Outcome;
   use type Reports.Coordinate_Status, Schema.Numeric_Value;
   use type HID_Axis_Resolution.Status;
   Primary_Mask : constant Pointer_Pending.Button_State := 1;
   Secondary_Mask : constant Pointer_Pending.Button_State := 2;
   Middle_Mask : constant Pointer_Pending.Button_State := 4;
   Side_Mask : constant Pointer_Pending.Button_State := 8;
   Extra_Mask : constant Pointer_Pending.Button_State := 16;
   function Mask (Target : Button_Target) return Pointer_Pending.Button_State is
     (case Target is when Ignore => 0, when Primary_Button => Primary_Mask,
      when Secondary_Button => Secondary_Mask, when Middle_Button => Middle_Mask,
      when Side_Button => Side_Mask, when Extra_Button => Extra_Mask);
   procedure Fill_Motion (S : in out State; Retention : out Pointer_Pending.Append_Outcome) is
      Data : Pointer_Pending.Report;
      Finished : Boolean;
      Stored : Pointer_Pending.Append_Outcome;
   begin
      Retention := Pointer_Pending.Appended;
      -- One bounded local batch. No hardware, callbacks, or consumer waits.
      for Turn in 1 .. Input_Pending.Capacity loop
         exit when not S.Motion_Pending
           or else Pointer_Pending.Count (S.Queue) = Input_Pending.Capacity;
         HID_Motion_Fragments.Take (S.Motion, S.Motion_Before, Data, Finished);
         Pointer_Pending.Append (S.Queue, Data, S.Motion_At, Stored);
         pragma Assert (Stored /= Pointer_Pending.Overflowed);
         Retention := Stored;
         S.Motion_Pending := not Finished;
      end loop;
   end Fill_Motion;
   procedure Acknowledge (S : in out State) is
      Ignored : Pointer_Pending.Append_Outcome;
   begin
      Pointer_Pending.Acknowledge (S.Queue);
      Fill_Motion (S, Ignored);
   end Acknowledge;
   procedure Retain (S : in out State; Data : Pointer_Pending.Report; Observed_Ms : Input_Pending.Word; Info : in out Report) is
   begin
      Pointer_Pending.Append (S.Queue, Data, Observed_Ms, Info.Retention);
      if Info.Retention = Pointer_Pending.Overflowed then Info.Recovery := True; Info.Dropped_Backlog := True; end if;
   end Retain;
   procedure Recover_Output (S : in out State; Observed_Ms : Input_Pending.Word; Preserve_Buttons : Boolean; Info : in out Report) is
      Decision : Pointer.Decision;
      Snapshot : Pointer_Pending.Report := (Buttons => 0, X => 0, Y => 0, Wheel => 0, Flags => 0);
   begin
      Info.Dropped_Backlog := Info.Dropped_Backlog or Count (S) > 0 or S.Motion_Pending;
      S.Motion_Pending := False;
      S.Motion := (others => <>);
      if Preserve_Buttons then
         Pointer.Rebase (S.Policy); Snapshot.Buttons := Pointer.Current_Buttons (S.Policy);
      else Pointer.Reset (S.Policy, Decision);
      end if;
      Pointer_Pending.Reset (S.Queue); Retain (S, Snapshot, Observed_Ms, Info); Info.Recovery := True;
   end Recover_Output;
   procedure Reset (S : in out State; Observed_Ms : Input_Pending.Word; Info : out Report) is
   begin
      Info := (others => <>); Reports.Reset (S.Raw); Recover_Output (S, Observed_Ms, False, Info); Info.Outcome := Source_Reset;
   end Reset;
   procedure Consumer_Changed (S : in out State; Observed_Ms : Input_Pending.Word; Info : out Report) is
   begin
      Info := (others => <>); Reports.Reset (S.Raw); Recover_Output (S, Observed_Ms, True, Info); Info.Outcome := Consumer_Reset;
   end Consumer_Changed;
   procedure Configure
     (S : in out State; Layout : Schema.Result; Fields : Schema.Field_Table;
      Usages : Schema.Usage_Table; Collections : Schema.Collection_Table;
      Touchpad : Schema.Count; Limit : Reports.Frames.Contact_Limit;
      Budget : Bindings.Work_Count; X_Range, Y_Range : Pointer.Axis_Range;
      X_Gain, Y_Gain : Pointer.Gain; X_Direction, Y_Direction : Pointer.Direction;
      Confidence : Pointer.Confidence_Policy; Buttons : Button_Map;
      Observed_Ms : Input_Pending.Word; Charged : out Bindings.Work_Count;
      Info : out Configure_Report; Coordinates : Coordinate_Policy)
   is
      Recovery : Report;
      Decision : Pointer.Decision;
      X, Y : Pointer.Axis_Range;
      GX, GY : Pointer.Gain;
      Scales_OK : Boolean := True;
      procedure Normalize (Limits : Pointer.Axis_Range;
         Measurement : Contacts.Axis_Measurement; Caller : Pointer.Gain;
         Value : out Pointer.Gain)
      is
         Logical_Length : constant Long_Long_Integer :=
           Long_Long_Integer (Limits.Maximum) - Long_Long_Integer (Limits.Minimum);
         Physical_Length : Long_Long_Integer :=
           Long_Long_Integer (Measurement.Maximum) - Long_Long_Integer (Measurement.Minimum);
         Resolved : HID_Axis_Resolution.Gain;
         Outcome : HID_Axis_Resolution.Status;
      begin
         Value := Caller;
         -- HID 6.2.2.7: both physical extents zero mean logical extents.
         if Measurement.Minimum = 0 and then Measurement.Maximum = 0 then
            Physical_Length := Logical_Length;
         end if;
         if Logical_Length not in HID_Axis_Resolution.Span or else
            Physical_Length not in HID_Axis_Resolution.Span then
            Scales_OK := False; return;
         end if;
         HID_Axis_Resolution.Resolve (Logical_Length, Physical_Length,
           Measurement.Unit, Measurement.Exponent, (Caller.Numerator, Caller.Denominator),
           Resolved, Outcome);
         if Outcome in HID_Axis_Resolution.Normalized | HID_Axis_Resolution.Caller_Units then
            Value := (Resolved.N, Resolved.D);
         else Scales_OK := False; end if;
      end Normalize;
   begin
      Info := (others => <>); Reset (S, Observed_Ms, Recovery);
      Reports.Configure (S.Raw, Layout, Fields, Usages, Collections, Touchpad, Limit, Budget, Charged, Info.Raw);
      if Info.Raw.Outcome /= Reports.Configured then return; end if;
      Info.Coordinates := Reports.Coordinates (S.Raw);
      X := X_Range; Y := Y_Range; GX := X_Gain; GY := Y_Gain;
      if Coordinates = Report_Bounds then
         if Info.Coordinates.Outcome /= Reports.Uniform_Coordinates or else
           Info.Coordinates.Bounds.X_Minimum >= Info.Coordinates.Bounds.X_Maximum or else
           Info.Coordinates.Bounds.Y_Minimum >= Info.Coordinates.Bounds.Y_Maximum
         then Reports.Invalidate (S.Raw); Info.Outcome := Coordinates_Refused; return; end if;
         X := (Pointer.Coordinate (Info.Coordinates.Bounds.X_Minimum), Pointer.Coordinate (Info.Coordinates.Bounds.X_Maximum));
         Y := (Pointer.Coordinate (Info.Coordinates.Bounds.Y_Minimum), Pointer.Coordinate (Info.Coordinates.Bounds.Y_Maximum));
         Normalize (X, Info.Coordinates.Measurements.X, X_Gain, GX);
         Normalize (Y, Info.Coordinates.Measurements.Y, Y_Gain, GY);
         if not Scales_OK then
            Reports.Invalidate (S.Raw); Info.Policy := Pointer.Scale_Refused;
            Info.Outcome := Pointer_Refused; return;
         end if;
      end if;
      Info.X_Gain := GX; Info.Y_Gain := GY;
      Pointer.Configure (S.Policy, X, Y, GX, GY, X_Direction, Y_Direction, Confidence, Decision);
      Info.Policy := Decision.Outcome;
      if not Pointer.Ready (S.Policy) then Reports.Invalidate (S.Raw); Info.Outcome := Pointer_Refused; return; end if;
      S.Buttons := Buttons; Info.Outcome := Source_Configured;
   end Configure;
   procedure Feed (S : in out State; Data : Schema.Bytes; Observed_Ms : Input_Pending.Word; Info : out Report) is
      Frame : Reports.Complete_Report;
      Known, Pressed, Bit_Value : Pointer_Pending.Button_State := 0;
      Decision : Pointer.Decision;
      Before_Buttons : Pointer_Pending.Button_State;
   begin
      Info := (others => <>);
      if not Ready (S) then return; end if;
      if not Input_Ready (S) then Info.Outcome := Input_Backpressured; return; end if;
      Reports.Feed (S.Raw, Data, Frame, Info.Raw);
      case Info.Raw.Outcome is
         when Reports.Ignored_Report => Info.Outcome := Report_Ignored;
         when Reports.Frame_Pending =>
            if Info.Raw.Discarded_Partial then Recover_Output (S, Observed_Ms, False, Info); end if;
            Info.Outcome := Frame_Waiting;
         when Reports.Frame_Ready =>
            if Info.Raw.Discarded_Partial then Recover_Output (S, Observed_Ms, False, Info); end if;
            for Button in Metadata.Button_Index loop
               if Frame.Buttons (Button).Present then
                  Bit_Value := Mask (S.Buttons (Button)); Known := Known or Bit_Value;
                  if Frame.Buttons (Button).Pressed then Pressed := Pressed or Bit_Value; end if;
               end if;
            end loop;
            Before_Buttons := Pointer.Current_Buttons (S.Policy);
            Pointer.Update (S.Policy,
              Pointer.Contact_Array (Frame.Contacts.Items (1 .. Natural (Frame.Contacts.Count))), Known, Pressed, Decision);
            Info.Policy := Decision.Outcome;
            if Decision.Outcome = Pointer.Invalid_Frame then
               Recover_Output (S, Observed_Ms, False, Info); Info.Outcome := Input_Rejected;
            elsif Decision.Emit then
               S.Motion := Decision.Data;
               S.Motion_Before := Before_Buttons;
               S.Motion_At := Observed_Ms;
               S.Motion_Pending := True;
               Fill_Motion (S, Info.Retention);
               Info.Outcome := Input_Updated;
            else Info.Outcome := Input_Unchanged;
            end if;
         when Reports.Metadata_Invalid | Reports.Contact_Invalid | Reports.Assembly_Refused =>
            Recover_Output (S, Observed_Ms, False, Info); Info.Outcome := Input_Rejected;
         when Reports.Not_Ready => Info.Outcome := Source_Not_Ready;
      end case;
   end Feed;
end HID_Input_Source;

pragma Ada_2022;
package body HID_Input_Source with SPARK_Mode is
   use type Reports.Status;
   use type Reports.Compile_Status;
   use type Pointer.Status;
   use type Pointer_Pending.Button_State;
   use type Pointer_Pending.Append_Outcome;
   use type Reports.Coordinate_Status, Schema.Numeric_Value;
   Primary_Mask : constant Pointer_Pending.Button_State := 1;
   Secondary_Mask : constant Pointer_Pending.Button_State := 2;
   Middle_Mask : constant Pointer_Pending.Button_State := 4;
   Side_Mask : constant Pointer_Pending.Button_State := 8;
   Extra_Mask : constant Pointer_Pending.Button_State := 16;
   function Mask (Target : Button_Target) return Pointer_Pending.Button_State is
     (case Target is when Ignore => 0, when Primary_Button => Primary_Mask,
      when Secondary_Button => Secondary_Mask, when Middle_Button => Middle_Mask,
      when Side_Button => Side_Mask, when Extra_Button => Extra_Mask);
   procedure Acknowledge (S : in out State) is
   begin Pointer_Pending.Acknowledge (S.Queue); end Acknowledge;
   procedure Retain (S : in out State; Data : Pointer_Pending.Report; Observed_Ms : Input_Pending.Word; Info : in out Report) is
   begin
      Pointer_Pending.Append (S.Queue, Data, Observed_Ms, Info.Retention);
      if Info.Retention = Pointer_Pending.Overflowed then Info.Recovery := True; Info.Dropped_Backlog := True; end if;
   end Retain;
   procedure Recover_Output (S : in out State; Observed_Ms : Input_Pending.Word; Preserve_Buttons : Boolean; Info : in out Report) is
      Decision : Pointer.Decision;
      Snapshot : Pointer_Pending.Report := (Buttons => 0, X => 0, Y => 0, Wheel => 0, Flags => 0);
   begin
      Info.Dropped_Backlog := Info.Dropped_Backlog or Count (S) > 0;
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
   begin
      Info := (others => <>); Reset (S, Observed_Ms, Recovery);
      Reports.Configure (S.Raw, Layout, Fields, Usages, Collections, Touchpad, Limit, Budget, Charged, Info.Raw);
      if Info.Raw.Outcome /= Reports.Configured then return; end if;
      Info.Coordinates := Reports.Coordinates (S.Raw);
      X := X_Range; Y := Y_Range;
      if Coordinates = Report_Bounds then
         if Info.Coordinates.Outcome /= Reports.Uniform_Coordinates or else
           Info.Coordinates.Bounds.X_Minimum >= Info.Coordinates.Bounds.X_Maximum or else
           Info.Coordinates.Bounds.Y_Minimum >= Info.Coordinates.Bounds.Y_Maximum
         then Reports.Invalidate (S.Raw); Info.Outcome := Coordinates_Refused; return; end if;
         X := (Pointer.Coordinate (Info.Coordinates.Bounds.X_Minimum), Pointer.Coordinate (Info.Coordinates.Bounds.X_Maximum));
         Y := (Pointer.Coordinate (Info.Coordinates.Bounds.Y_Minimum), Pointer.Coordinate (Info.Coordinates.Bounds.Y_Maximum));
      end if;
      Pointer.Configure (S.Policy, X, Y, X_Gain, Y_Gain, X_Direction, Y_Direction, Confidence, Decision);
      Info.Policy := Decision.Outcome;
      if not Pointer.Ready (S.Policy) then Reports.Invalidate (S.Raw); Info.Outcome := Pointer_Refused; return; end if;
      S.Buttons := Buttons; Info.Outcome := Source_Configured;
   end Configure;
   procedure Feed (S : in out State; Data : Schema.Bytes; Observed_Ms : Input_Pending.Word; Info : out Report) is
      Frame : Reports.Complete_Report;
      Known, Pressed, Bit_Value : Pointer_Pending.Button_State := 0;
      Decision : Pointer.Decision;
   begin
      Info := (others => <>);
      if not Ready (S) then return; end if;
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
            Pointer.Update (S.Policy,
              Pointer.Contact_Array (Frame.Contacts.Items (1 .. Natural (Frame.Contacts.Count))), Known, Pressed, Decision);
            Info.Policy := Decision.Outcome;
            if Decision.Outcome = Pointer.Invalid_Frame then
               Recover_Output (S, Observed_Ms, False, Info); Info.Outcome := Input_Rejected;
            elsif Decision.Emit then
               Retain (S, Decision.Data, Observed_Ms, Info); Info.Outcome := Input_Updated;
            else Info.Outcome := Input_Unchanged;
            end if;
         when Reports.Metadata_Invalid | Reports.Contact_Invalid | Reports.Assembly_Refused =>
            Recover_Output (S, Observed_Ms, False, Info); Info.Outcome := Input_Rejected;
         when Reports.Not_Ready => Info.Outcome := Source_Not_Ready;
      end case;
   end Feed;
end HID_Input_Source;

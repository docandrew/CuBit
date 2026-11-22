pragma Ada_2022;
with Interfaces;
with Input_Pending;
with Pointer_Pending;
with HID_Report_Schema;
with HID_Field_Usages;
with HID_Input_Bindings;
with HID_Input_Values;
with HID_Touchpad_Contact;
with HID_Report_Metadata;
with HID_Touchpad_Reports;
with HID_Touchpad_Pointer;
generic
   Maximum_Contacts, Maximum_Slots : Positive;
   with package Schema is new HID_Report_Schema (<>);
   with package Names is new HID_Field_Usages (Schema);
   with package Bindings is new HID_Input_Bindings (Schema, Names);
   with package Values is new HID_Input_Values (Schema);
   with package Contacts is new HID_Touchpad_Contact (Schema, Names, Bindings, Values);
   with package Metadata is new HID_Report_Metadata (Schema, Names, Bindings, Values);
package HID_Input_Source with SPARK_Mode is
   use type Contacts.Confidence_State;
   package Reports is new HID_Touchpad_Reports (Maximum_Contacts, Maximum_Slots, Schema, Names, Bindings, Values, Contacts, Metadata);
   function Identity (C : Contacts.Contact) return Interfaces.Unsigned_32 is (C.ID);
   function Active (C : Contacts.Contact) return Boolean is (C.Tip);
   function Positioned (C : Contacts.Contact) return Boolean is (C.Has_Position);
   function Confidence_Known (C : Contacts.Contact) return Boolean is (C.Confidence /= Contacts.Unknown);
   function Confident (C : Contacts.Contact) return Boolean is (C.Confidence = Contacts.Confident);
   function X_Value (C : Contacts.Contact) return Long_Long_Integer is (Long_Long_Integer (C.X));
   function Y_Value (C : Contacts.Contact) return Long_Long_Integer is (Long_Long_Integer (C.Y));
   package Pointer is new HID_Touchpad_Pointer (Maximum_Contacts, Contacts.Contact, Identity, Active, Positioned, Confidence_Known, Confident, X_Value, Y_Value);
   type Button_Target is (Ignore, Primary_Button, Secondary_Button, Middle_Button, Side_Button, Extra_Button);
   type Button_Map is array (Metadata.Button_Index) of Button_Target;
   type Coordinate_Policy is (Caller_Bounds, Report_Bounds);
   type Configure_Status is (Source_Configured, Reports_Refused, Pointer_Refused, Coordinates_Refused);
   type Configure_Report is record
      Outcome : Configure_Status := Reports_Refused;
      Raw : Reports.Compile_Report;
      Policy : Pointer.Status := Pointer.Policy_Not_Configured;
      Coordinates : Reports.Coordinate_Report;
   end record;
   type Status is (Input_Updated, Input_Unchanged, Frame_Waiting, Report_Ignored,
                   Input_Rejected, Source_Not_Ready, Source_Reset, Consumer_Reset);
   type Report is record
      Outcome : Status := Source_Not_Ready;
      Raw : Reports.Report;
      Policy : Pointer.Status := Pointer.Policy_Not_Configured;
      Retention : Pointer_Pending.Append_Outcome := Pointer_Pending.Appended;
      Recovery, Dropped_Backlog : Boolean := False;
   end record;
   type State is limited private;
   function Ready (S : State) return Boolean;
   function Tracking (S : State) return Boolean;
   function Pending (S : State) return Boolean;
   function Count (S : State) return Input_Pending.Count_Type;
   function Head (S : State) return Input_Pending.Item with Pre => Count (S) > 0;
   procedure Acknowledge (S : in out State) with Pre => Count (S) > 0;
   function Wake_Deadline (S : State; Now, Otherwise : Input_Pending.Word) return Input_Pending.Word;
   procedure Configure
     (S : in out State; Layout : Schema.Result; Fields : Schema.Field_Table;
      Usages : Schema.Usage_Table; Collections : Schema.Collection_Table;
      Touchpad : Schema.Count; Limit : Reports.Frames.Contact_Limit;
      Budget : Bindings.Work_Count; X_Range, Y_Range : Pointer.Axis_Range;
      X_Gain, Y_Gain : Pointer.Gain; X_Direction, Y_Direction : Pointer.Direction;
      Confidence : Pointer.Confidence_Policy; Buttons : Button_Map;
      Observed_Ms : Input_Pending.Word; Charged : out Bindings.Work_Count;
      Info : out Configure_Report; Coordinates : Coordinate_Policy);
   procedure Feed (S : in out State; Data : Schema.Bytes; Observed_Ms : Input_Pending.Word; Info : out Report);
   procedure Reset (S : in out State; Observed_Ms : Input_Pending.Word; Info : out Report);
   procedure Consumer_Changed (S : in out State; Observed_Ms : Input_Pending.Word; Info : out Report);
   -- Native owner peeks Head, tries a nonblocking publication, and acknowledges
   -- only acceptance. Loss/reset explicitly drops unpublished backlog and queues
   -- a recovery snapshot; consumer replacement snapshots known physical buttons
   -- without delivering the old consumer's history. Button mapping/calibration
   -- are explicit caller admissions. Report_Bounds derives matching logical
   -- bounds from compiled slots; unknown/inconsistent/empty ranges fail before
   -- pointer readiness. Caller_Bounds retains explicit caller admission.
   -- This component performs no IPC or IO.
private
   type State is limited record
      Raw : Reports.State;
      Policy : Pointer.State;
      Queue : Pointer_Pending.State;
      Buttons : Button_Map := [others => Ignore];
   end record;
   function Ready (S : State) return Boolean is (Reports.Ready (S.Raw) and then Pointer.Ready (S.Policy));
   function Tracking (S : State) return Boolean is (Pointer.Tracking (S.Policy));
   function Pending (S : State) return Boolean is (Reports.Pending (S.Raw));
   function Count (S : State) return Input_Pending.Count_Type is (Pointer_Pending.Count (S.Queue));
   function Head (S : State) return Input_Pending.Item is (Pointer_Pending.Element (S.Queue, 0));
   function Wake_Deadline (S : State; Now, Otherwise : Input_Pending.Word) return Input_Pending.Word is
     (Pointer_Pending.Wake_Deadline (S.Queue, Now, Otherwise));
end HID_Input_Source;

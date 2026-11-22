pragma Ada_2022;
with Interfaces;
with HID_Report_Schema;
with HID_Field_Usages;
with HID_Input_Bindings;
with HID_Input_Values;
with HID_Touchpad_Contact;
with HID_Report_Metadata;
with HID_Contact_Frames;
generic
   Maximum_Contacts, Maximum_Slots : Positive;
   with package Schema is new HID_Report_Schema (<>);
   with package Names is new HID_Field_Usages (Schema);
   with package Bindings is new HID_Input_Bindings (Schema, Names);
   with package Values is new HID_Input_Values (Schema);
   with package Contacts is new HID_Touchpad_Contact (Schema, Names, Bindings, Values);
   with package Metadata is new HID_Report_Metadata (Schema, Names, Bindings, Values);
package HID_Touchpad_Reports with SPARK_Mode is
   function Identity (Item : Contacts.Contact) return Interfaces.Unsigned_32 is (Item.ID);
   Empty_Contact : constant Contacts.Contact :=
     (ID => 0, Tip => False, Has_Position => False, Confidence => Contacts.Unknown, X => 0, Y => 0);
   package Frames is new HID_Contact_Frames (Maximum_Contacts, Contacts.Contact, Empty_Contact, Identity);
   type Slot_Count is new Natural range 0 .. Maximum_Slots;
   type Compile_Status is (Configured, Invalid_Schema, Metadata_Refused,
      Contact_Refused, No_Slots, Too_Many_Slots, Split_Report, Invalid_Tables,
      Budget_Exhausted);
   type Compile_Report is record
      Outcome : Compile_Status := Invalid_Schema;
      Metadata_Reason : Metadata.Compile_Status := Metadata.Invalid_Schema;
      Contact_Reason : Contacts.Compile_Status := Contacts.Invalid_Schema;
      Collection_ID : Schema.Count := 0;
   end record;
   type Status is (Frame_Ready, Frame_Pending, Not_Ready, Ignored_Report,
                   Metadata_Invalid, Contact_Invalid, Assembly_Refused);
   type Report is record
      Outcome : Status := Not_Ready;
      Metadata_Reason : Metadata.Decode_Status := Metadata.Unbound;
      Contact_Reason : Contacts.Decode_Status := Contacts.Unbound;
      Assembly : Frames.Report;
      Bad_Slot : Slot_Count := 0;
      Discarded_Partial : Boolean := False;
   end record;
   type Complete_Report is record
      Contacts : Frames.Frame;
      Buttons : Metadata.Button_Array := [others => (False, False)];
   end record;
   type State is limited private;
   type Coordinate_Status is (Coordinates_Unavailable, Uniform_Coordinates, Inconsistent_Coordinates);
   type Coordinate_Report (Outcome : Coordinate_Status := Coordinates_Unavailable) is record
      case Outcome is
         when Uniform_Coordinates => Bounds : Contacts.Coordinate_Bounds;
         when others => null;
      end case;
   end record;
   function Coordinates (S : State) return Coordinate_Report;
   function Ready (S : State) return Boolean;
   function Slots (S : State) return Slot_Count;
   function Pending (S : State) return Boolean;
   procedure Reset (S : in out State);
   procedure Invalidate (S : in out State);
   procedure Configure
     (S : in out State; Layout : Schema.Result; Fields : Schema.Field_Table;
      Usages : Schema.Usage_Table; Collections : Schema.Collection_Table;
      Touchpad : Schema.Count; Limit : Frames.Contact_Limit;
      Budget : Bindings.Work_Count; Charged : out Bindings.Work_Count;
      Info : out Compile_Report);
   -- Discover input finger slots in descriptor field order under this exact
   -- application collection. Compile metadata and all slots once. No partial
   -- configuration becomes Ready. Limit is separately admitted from capabilities.
   procedure Feed
     (S : in out State; Data : Schema.Bytes;
      Complete : out Complete_Report; Info : out Report);
   -- Data excludes the HID-I2C length prefix. Decode metadata before contacts;
   -- unused final contact slots are never decoded. Unrelated report IDs leave
   -- partial input intact; target-report errors discard it. Buttons come from
   -- the packet completing a frame. This owns no hardware/transport capability.
private
   type Slot_Table is array (Positive range 1 .. Maximum_Slots) of Contacts.Contact_Layout;
   type Collection_Table is array (Positive range 1 .. Maximum_Slots) of Schema.Count;
   type State is limited record
      Is_Ready, Has_Report_IDs : Boolean := False;
      Used : Slot_Count := 0;
      Limit : Frames.Contact_Count := 0;
      Meta : Metadata.Report_Layout;
      Layouts : Slot_Table;
      Frames_State : Frames.State;
      Geometry : Coordinate_Report;
   end record;
   function Ready (S : State) return Boolean is (S.Is_Ready);
   function Slots (S : State) return Slot_Count is (S.Used);
   function Pending (S : State) return Boolean is (Frames.Pending (S.Frames_State));
   function Coordinates (S : State) return Coordinate_Report is
     (if S.Is_Ready then S.Geometry else (Outcome => Coordinates_Unavailable));
end HID_Touchpad_Reports;

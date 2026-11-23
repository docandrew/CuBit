with HID_Report_Schema;
with HID_Field_Usages;
with HID_Input_Values;
with Pointer_Pending;
generic
   with package Schema is new HID_Report_Schema (<>);
   with package Names is new HID_Field_Usages (Schema);
   with package Values is new HID_Input_Values (Schema);
package HID_Mouse_Fallback with SPARK_Mode is
   type Slot is record
      Present : Boolean := False;
      Field : Schema.Field_Span;
      Element : Schema.Bit_Count := 0;
   end record;
   type Button_Slots is array (Positive range 1 .. 5) of Slot;
   type Model is record
      Ready : Boolean := False;
      ID : Schema.Report_ID := 0;
      X, Y, Wheel : Slot;
      Buttons : Button_Slots;
   end record;
   procedure Compile
     (Layout : Schema.Result; Fields : Schema.Field_Table; Usages : Schema.Usage_Table;
      Collections : Schema.Collection_Table; Result : out Model);
   procedure Decode
     (Plan : Model; Layout : Schema.Result; Data : Schema.Bytes;
      Matched, Accepted : out Boolean; Report : out Pointer_Pending.Report);
end HID_Mouse_Fallback;

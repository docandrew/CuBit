pragma Ada_2022;
with HID_Report_Schema;
with HID_Field_Usages;
with HID_Input_Bindings;
with HID_Input_Values;
generic
   with package Schema is new HID_Report_Schema (<>);
   with package Names is new HID_Field_Usages (Schema);
   with package Bindings is new HID_Input_Bindings (Schema, Names);
   with package Values is new HID_Input_Values (Schema);
package HID_Touchpad_Contact with SPARK_Mode is
   type Compile_Status is (Bound, Invalid_Schema, Unsupported_Collection,
      Missing_Field, Ambiguous_Field, Unsupported_Field, Split_Report,
      Budget_Exhausted, Invalid_Tables);
   type Decode_Status is (Sampled, Unbound, Wrong_Report, Invalid_Data);
   type Confidence_State is (Unknown, Confident, Not_Confident);
   type Contact is record
      ID : Schema.Word := 0;
      Tip, Has_Position : Boolean := False;
      Confidence : Confidence_State := Unknown;
      X, Y : Schema.Numeric_Value := 0;
   end record;
   type Contact_Layout is private;
   function Ready (Item : Contact_Layout) return Boolean;
   function Report_Identity (Item : Contact_Layout) return Schema.Report_ID
     with Pre => Ready (Item);
   type Coordinate_Bounds is record
      X_Minimum, X_Maximum, Y_Minimum, Y_Maximum : Schema.Numeric_Value := 0;
   end record;
   function Coordinates (Item : Contact_Layout) return Coordinate_Bounds
      with Pre => Ready (Item);
   procedure Compile
     (Layout : Schema.Result; Fields : Schema.Field_Table;
      Usages : Schema.Usage_Table; Collections : Schema.Collection_Table;
      Finger : Schema.Count; Budget : Bindings.Work_Count;
      Item : out Contact_Layout; Charged : out Bindings.Work_Count;
      Outcome : out Compile_Status);
   -- Compile once per descriptor/device lifetime. Requires a Logical Finger
   -- under an Application Touch Pad collection. Tip/contact-ID/X/Y must each
   -- bind uniquely to absolute scalar fields in one report; confidence may be
   -- absent and remains Unknown. A compiled layout owns copies of its fields
   -- and report geometry, so Decode never rescans tables or depends on them.
   procedure Decode
     (Item : Contact_Layout; Data : Schema.Bytes;
      Sample : out Contact; Outcome : out Decode_Status);
   -- Data excludes the HID-I2C length prefix, includes the report ID if used.
   -- Inactive contacts retain a valid ID but have no position; coordinates and
   -- confidence are not read for them. No partial sample escapes on failure.
   -- This produces one contact, not a complete parallel/hybrid frame and not
   -- a desktop pointer event. Caller owns framing, transport and device identity.
private
   type Part_Kind is (Tip_Part, ID_Part, X_Part, Y_Part, Confidence_Part);
   type Part is record
      Field : Schema.Field_Span;
      Element : Schema.Bit_Count := 0;
   end record;
   type Part_Table is array (Part_Kind) of Part;
   type Contact_Layout is record
      Valid, Has_Confidence : Boolean := False;
      Report : Schema.Result;
      Parts : Part_Table;
   end record;
   function Ready (Item : Contact_Layout) return Boolean is (Item.Valid);
   function Report_Identity (Item : Contact_Layout) return Schema.Report_ID is
     (Item.Parts (Tip_Part).Field.ID);
   function Coordinates (Item : Contact_Layout) return Coordinate_Bounds is
     ((Item.Parts (X_Part).Field.Logical_Minimum, Item.Parts (X_Part).Field.Logical_Maximum,
       Item.Parts (Y_Part).Field.Logical_Minimum, Item.Parts (Y_Part).Field.Logical_Maximum));
end HID_Touchpad_Contact;

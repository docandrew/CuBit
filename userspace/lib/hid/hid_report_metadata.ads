pragma Ada_2022;
with Interfaces;
with HID_Report_Schema;
with HID_Field_Usages;
with HID_Input_Bindings;
with HID_Input_Values;
generic
   with package Schema is new HID_Report_Schema (<>);
   with package Names is new HID_Field_Usages (Schema);
   with package Bindings is new HID_Input_Bindings (Schema, Names);
   with package Values is new HID_Input_Values (Schema);
package HID_Report_Metadata with SPARK_Mode is
   type Compile_Status is (Bound, Invalid_Schema, Unsupported_Collection,
      Missing_Field, Ambiguous_Field, Unsupported_Field, Split_Report,
      Budget_Exhausted, Invalid_Tables);
   type Decode_Status is (Sampled, Unbound, Wrong_Report, Invalid_Data);
   type Button_Index is range 1 .. 3;
   type Button_Sample is record
      Present, Pressed : Boolean := False;
   end record;
   type Button_Array is array (Button_Index) of Button_Sample;
   type Metadata is record
      Contact_Count : Interfaces.Unsigned_32 := 0;
      Scan_Time : Interfaces.Unsigned_16 := 0;
      Buttons : Button_Array := [others => (False, False)];
   end record;
   type Report_Layout is private;
   function Ready (Item : Report_Layout) return Boolean;
   function Report_Identity (Item : Report_Layout) return Schema.Report_ID
     with Pre => Ready (Item);
   procedure Compile
     (Layout : Schema.Result; Fields : Schema.Field_Table;
      Usages : Schema.Usage_Table; Collections : Schema.Collection_Table;
      Touchpad : Schema.Count; Budget : Bindings.Work_Count;
      Item : out Report_Layout; Charged : out Bindings.Work_Count;
      Outcome : out Compile_Status);
   -- Count/time and optional button usages belong to the touchpad report, not
   -- a nested finger/application. Cache absolute scalar fields from one report.
   -- Scan time must declare SI seconds at exponent -4 and a zero-based extent
   -- no greater than 65535, with matching logical/physical extents (100us ticks).
   -- Button presence is distinct from a released button; no button mapping or
   -- device capability is guessed. Caller separately admits device/contact max.
   procedure Decode
     (Item : Report_Layout; Data : Schema.Bytes;
      Sample : out Metadata; Outcome : out Decode_Status);
   -- Same byte framing as scalar/contact decoding. No partial metadata on error.
private
   type Part_Kind is (Count_Part, Time_Part, Button_1, Button_2, Button_3);
   type Part is record
      Present : Boolean := False;
      Field : Schema.Field_Span;
      Element : Schema.Bit_Count := 0;
   end record;
   type Part_Table is array (Part_Kind) of Part;
   type Report_Layout is record
      Valid : Boolean := False;
      Report : Schema.Result;
      Parts : Part_Table;
   end record;
   function Ready (Item : Report_Layout) return Boolean is (Item.Valid);
   function Report_Identity (Item : Report_Layout) return Schema.Report_ID is
     (Item.Parts (Count_Part).Field.ID);
end HID_Report_Metadata;

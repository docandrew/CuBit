with HID_Report_Schema;
with HID_Field_Usages;
with HID_Feature_Commands;
generic
   with package Schema is new HID_Report_Schema (<>);
   with package Names is new HID_Field_Usages (Schema);
package HID_Touchpad_Features with SPARK_Mode is
   package Wire renames HID_Feature_Commands;
   Maximum_Commands : constant := 3;
   type Command_List is array (Positive range 1 .. Maximum_Commands) of Wire.Command;
   type Plan is record
      Commands : Command_List;
      Count : Natural range 0 .. Maximum_Commands := 0;
      Mode_Advertised, Mode_Supported, Refused : Boolean := False;
   end record;
   procedure Build
     (Layout : Schema.Result; Fields : Schema.Field_Table; Usages : Schema.Usage_Table;
      Command_Register, Data_Register : Wire.D.Register_Number; Result : out Plan);
end HID_Touchpad_Features;

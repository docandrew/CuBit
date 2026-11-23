with HID_I2C_Descriptors;
package HID_Feature_Commands with SPARK_Mode, Pure is
   package D renames HID_I2C_Descriptors;
   Maximum_Payload : constant := 64;
   Maximum_Wire : constant := Maximum_Payload + 12;
   type Command is record
      Data : D.Bytes (1 .. Maximum_Wire) := [others => 0];
      Length : Natural range 0 .. Maximum_Wire := 0;
      ID : D.Byte := 0;
      Mode, Surface, Buttons : Boolean := False;
   end record;
   function None (Index : Positive) return Command;
   function Encode
     (Command_Register, Data_Register : D.Register_Number; ID : D.Byte;
      Payload : D.Bytes; Mode, Surface, Buttons : Boolean) return Command
     with Pre => Payload'Length <= Maximum_Payload,
       Post => Encode'Result.Length in 8 .. Maximum_Wire;
end HID_Feature_Commands;

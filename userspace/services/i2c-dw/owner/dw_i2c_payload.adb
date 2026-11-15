pragma Ada_2022;
package body DW_I2C_Payload with SPARK_Mode is
   procedure Read_Refused
     (Index : DW_I2C_Transfer.Count; Value : out DW_I2C_Transfer.Byte;
      Accepted : out Boolean) is
      pragma Unreferenced (Index);
   begin Value := 0; Accepted := False; end Read_Refused;
   procedure Write_Refused
     (Index : DW_I2C_Transfer.Count; Value : DW_I2C_Transfer.Byte;
      Accepted : out Boolean) is
      pragma Unreferenced (Index, Value);
   begin Accepted := False; end Write_Refused;
end DW_I2C_Payload;

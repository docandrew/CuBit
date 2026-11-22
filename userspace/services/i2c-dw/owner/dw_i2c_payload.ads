pragma Ada_2022;
with DW_I2C_Transfer;
package DW_I2C_Payload with SPARK_Mode is
   -- Default providers deny access. A borrowed owner must supply bounded,
   -- non-raising providers bound to the validated channel's live mappings.
   procedure Read_Refused
     (Index : DW_I2C_Transfer.Count; Value : out DW_I2C_Transfer.Byte;
      Accepted : out Boolean);
   procedure Write_Refused
     (Index : DW_I2C_Transfer.Count; Value : DW_I2C_Transfer.Byte;
      Accepted : out Boolean);
end DW_I2C_Payload;

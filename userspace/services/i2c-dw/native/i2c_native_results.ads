pragma Ada_2022;
with DW_I2C_Request_Types;
with I2C_Transfer_Wire;
package I2C_Native_Results with SPARK_Mode is
   function To_Wire (Item : DW_I2C_Request_Types.Response)
      return I2C_Transfer_Wire.Completion;
end I2C_Native_Results;

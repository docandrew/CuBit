pragma Ada_2022;
with DW_I2C_Transfer;
package DW_I2C_Request_Types with SPARK_Mode is
   type Copy_Result is (Copied, Refused, Uncertain);
   type Reason is (Completed, Unauthorized, Invalid_Channel, Unsupported_Channel, Busy, Invalid_Size,
      Input_Refused, Input_Uncertain, Admission_Refused,
      Output_Refused, Output_Uncertain, Internal_Mismatch);
   type Response is record
      Status : Reason := Busy;
      ID : DW_I2C_Transfer.Ticket := DW_I2C_Transfer.No_Ticket;
      Hardware_Succeeded, Effects_Possible : Boolean := False;
      Write_Count, Read_Count : DW_I2C_Transfer.Count := 0;
   end record;
end DW_I2C_Request_Types;

pragma Ada_2022;
package body I2C_Native_Results with SPARK_Mode is
   package Q renames DW_I2C_Request_Types;
   package W renames I2C_Transfer_Wire;
   function To_Wire (Item : Q.Response) return W.Completion is
     (Status => (case Item.Status is
        when Q.Completed => W.Completed,
        when Q.Unauthorized => W.Unauthorized,
        when Q.Invalid_Channel => W.Invalid_Channel,
        when Q.Unsupported_Channel => W.Unsupported_Channel,
        when Q.Busy => W.Busy,
        when Q.Invalid_Size => W.Invalid_Size,
        when Q.Input_Refused => W.Input_Refused,
        when Q.Input_Uncertain => W.Input_Uncertain,
        when Q.Admission_Refused => W.Admission_Refused,
        when Q.Output_Refused => W.Output_Refused,
        when Q.Output_Uncertain => W.Output_Uncertain,
        when Q.Internal_Mismatch => W.Internal_Mismatch),
      Ticket => W.Word (Item.ID),
      Hardware_Succeeded => Item.Hardware_Succeeded,
      Effects_Possible => Item.Effects_Possible,
      Requested_Writes => W.Count (Item.Write_Count),
      Delivered_Reads => W.Count (Item.Read_Count));
end I2C_Native_Results;

function Intel_GPU_GMBUS_Outcome (Status : DDC.Status; Checksum_Good : Boolean)
   return Intel_GPU_Port_Detection.EDID_Outcome with SPARK_Mode
is
   package PD renames Intel_GPU_Port_Detection;
begin
   return (case Status is
             when DDC.OK => (if Checksum_Good then PD.Read_Valid else PD.Bad_Checksum),
             when DDC.NAK => PD.No_Answer,
             when DDC.Not_Writable => PD.Not_Permitted,
             when DDC.Busy_Before_Start | DDC.Timeout_Ready | DDC.Timeout_Wait_Phase | DDC.Timeout_Idle =>
               PD.Timed_Out);
end Intel_GPU_GMBUS_Outcome;

with Intel_GPU_GMBUS;
with Intel_GPU_Port_Detection;
--  A GMBUS read's status as the detection's typed EDID outcome.
generic
   with package DDC is new Intel_GPU_GMBUS (<>);
function Intel_GPU_GMBUS_Outcome (Status : DDC.Status; Checksum_Good : Boolean)
   return Intel_GPU_Port_Detection.EDID_Outcome with SPARK_Mode;

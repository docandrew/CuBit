package body Intel_GPU_Broker_Request with SPARK_Mode is
   function Decode
     (Expected_Launcher, Sender, Stamped_Tag : Unsigned_64;
      Request_Label : Unsigned_32; Length, Flags : Unsigned_8;
      Reserved : Unsigned_16; Request : Words) return Decoded
   is
   begin
      if Expected_Launcher = 0 or else
        Sender /= Expected_Launcher or else Stamped_Tag /= Authority_Tag or else
        Request_Label /= Label or else Length /= 4 or else
        Flags /= Request_Flags or else
        Reserved /= 0 or else Request (0) /= Version or else
        Request (1) = 0 or else Request (2) /= 0 or else Request (3) = 0
      then
         return (Valid => False);
      end if;
      return (True, Request (1), Request (3));
   end Decode;
end Intel_GPU_Broker_Request;

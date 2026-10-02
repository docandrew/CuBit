package body Intel_GPU_Broker_Request with SPARK_Mode is
   function Decode
     (Expected_Launcher, Sender, Stamped_Tag : Unsigned_64;
      Request_Label : Unsigned_32; Length, Flags : Unsigned_8;
      Reserved : Unsigned_16; Request : Words) return Decoded
   is
   begin
      if Expected_Launcher = 0 or else
        Expected_Launcher > Unsigned_64 (Unsigned_32'Last) or else
        Sender /= Expected_Launcher or else Stamped_Tag /= Authority_Tag or else
        Request_Label /= Label or else Length /= 4 or else Flags /= 0 or else
        Reserved /= 0 or else Request (0) /= Version or else
        Request (1) < Unsigned_64 (Source_Slot'First) or else
        Request (1) > Unsigned_64 (Source_Slot'Last) or else
        Request (2) > Unsigned_64 (Destination_Slot'Last) or else Request (3) = 0
      then
         return (Valid => False);
      end if;
      return (True, Source_Slot (Request (1)), Destination_Slot (Request (2)),
              Request (3));
   end Decode;
end Intel_GPU_Broker_Request;

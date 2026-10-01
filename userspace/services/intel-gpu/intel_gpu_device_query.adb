package body Intel_GPU_Device_Query with SPARK_Mode is
   function Respond
     (Data : Snapshot; Request_Label : Unsigned_32;
      Length, Flags : Unsigned_8; Reserved : Unsigned_16;
      Request : Words; Timestamp_Hz : Unsigned_32 := 0) return Words
   is
   begin
      if Request_Label /= Label then
         return [Unsupported, Version, 0, 0];
      end if;
      if Length /= 4 or Flags /= 0 or Reserved /= 0 or
        Request (0) /= Version or Request (2) /= 0 or Request (3) /= 0 or
        Request (1) > Timestamp
      then
         return [Bad_Request, Version, 0, 0];
      end if;
      -- Only the measured ADL-N path is admitted today; never infer support
      -- from a PCI-table default or from a successful firmware upload.
      if Data.Device /= 16#46D2# then
         return [Unavailable, Version, 0, 0];
      end if;
      if Request (1) = Identity then
         return [OK, Version,
           16#8086# or Shift_Left (Unsigned_64 (Data.Device), 16) or
           Shift_Left (Unsigned_64 (Data.Revision), 32), 0];
      end if;
      if Request (1) = Timestamp then
         if Timestamp_Hz = 0 or Timestamp_Hz > 1_025_000_000 then
            return [Unavailable, Version, 0, 0];
         end if;
         return [OK, Version, Unsigned_64 (Timestamp_Hz), 0];
      end if;
      if not Data.Topology_Observed or Data.DSS_Mask = 0 or
        (Data.DSS_Mask and 16#C0#) /= 0 or Data.EU_Mask = 0 or
        (Data.EU_Mask and 16#5555#) /=
          (Shift_Right (Data.EU_Mask, 1) and 16#5555#)
      then
         return [Unavailable, Version, 0, 0];
      end if;
      return [OK, Version, Unsigned_64 (Data.DSS_Mask),
              Unsigned_64 (Data.EU_Mask)];
   end Respond;
end Intel_GPU_Device_Query;

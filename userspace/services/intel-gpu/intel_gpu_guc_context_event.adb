package body Intel_GPU_GuC_Context_Event with SPARK_Mode is
   function Decode (Payload : Words; Fence : Unsigned_16) return Event is
      Result : Event;
      Header, Message_Type : Unsigned_32;
   begin
      if Payload'Length = 0 or Payload'Length > 255 then return Result; end if;
      Header := Payload (Payload'First);
      if (Header and 16#80000000#) = 0 then return Result; end if;
      Message_Type := Shift_Right (Header, 28) and 7;
      if Message_Type = 6 then
         if Payload'Length /= 1 then return Result; end if;
         return (Request_Failure, Fence, 0, 0,
                 Header and 16#FFFF#, Shift_Right (Header, 16) and 16#FFF#);
      elsif Message_Type = 1 and then (Header and 16#FFFF#) = 16#1002# then
         -- Pinned single-context event: header + context ID + runnable state.
         if Header /= 16#90001002# or else Payload'Length /= 3 or else
           Payload (Payload'First + 1) >= 65535 or else
           Payload (Payload'First + 2) > 1 then return Result; end if;
         return (Scheduling_Done, Fence, Payload (Payload'First + 1),
                 Payload (Payload'First + 2), 0, 0);
      elsif Message_Type = 1 and then (Header and 16#FFFF#) = 16#4600# then
         -- Linux v6.16 guc_actions_abi.h / intel_guc_fwif.h and
         -- intel_guc_deregister_done_process_msg: HXG + one context ID.
         -- Pinned ABI: reject extra data/reserved bits, not a success prefix.
         if Header /= 16#90004600# or else Payload'Length /= 2 or else
           Payload (Payload'First + 1) >= 65535 then return Result; end if;
         return (Deregister_Done, Fence, Payload (Payload'First + 1), 0, 0, 0);
      elsif Message_Type in 1 | 3 | 5 | 7 then
         Result.Tag := Other_Message; Result.Fence := Fence;
      end if;
      return Result;
   end Decode;
end Intel_GPU_GuC_Context_Event;

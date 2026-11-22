with Intel_GPU_GuC_Actions;
package body Intel_GPU_GuC_Context_Event with SPARK_Mode is
   package Actions renames Intel_GPU_GuC_Actions;
   function Decode (Payload : Words; Fence : Unsigned_16) return Event is
      Result : Event;
      Header, Message_Type : Unsigned_32;
   begin
      if Payload'Length = 0 or Payload'Length > 255 then return Result; end if;
      Header := Payload (Payload'First);
      if (Header and Actions.Origin_GuC) = 0 then return Result; end if;
      Message_Type := Actions.Message_Type (Header);
      if Message_Type = Actions.Type_Response_Failure then
         if Payload'Length /= 1 then return Result; end if;
         return (Tag => Request_Failure, Fence => Fence,
                 Error_Code => Header and 16#FFFF#,
                 Hint => Shift_Right (Header, 16) and 16#FFF#, others => 0);
      elsif Message_Type = Actions.Type_Event and then
        Actions.Action_Of (Header) = Actions.Sched_Context_Mode_Done
      then
         -- Pinned single-context event: header + context ID + runnable state.
         if Header /= Actions.Event_Header (Actions.Sched_Context_Mode_Done) or else Payload'Length /= 3 or else
           Payload (Payload'First + 1) >= 65535 or else
           Payload (Payload'First + 2) > 1 then return Result; end if;
         return (Tag => Scheduling_Done, Fence => Fence,
                 ID => Payload (Payload'First + 1),
                 Runnable => Payload (Payload'First + 2), others => 0);
      elsif Message_Type = Actions.Type_Event and then
        Actions.Action_Of (Header) = Actions.Deregister_Context_Done
      then
         -- Linux v6.16 guc_actions_abi.h / intel_guc_fwif.h and
         -- intel_guc_deregister_done_process_msg: HXG + one context ID.
         -- Pinned ABI: reject extra data/reserved bits, not a success prefix.
         if Header /= Actions.Event_Header (Actions.Deregister_Context_Done) or else Payload'Length /= 2 or else
           Payload (Payload'First + 1) >= 65535 then return Result; end if;
         return (Tag => Deregister_Done, Fence => Fence,
                 ID => Payload (Payload'First + 1), others => 0);
      elsif Message_Type = Actions.Type_Event and then
        Actions.Action_Of (Header) = Actions.Context_Reset_Notification
      then
         -- Linux v6.16 intel_guc_context_reset_process_msg: one guc_id.
         if Header /= Actions.Event_Header (Actions.Context_Reset_Notification) or else
           Payload'Length /= 1 + Actions.Context_Reset_Data_Words or else
           Payload (Payload'First + 1) >= 65535 then return Result; end if;
         return (Tag => Context_Reset, Fence => Fence,
                 ID => Payload (Payload'First + 1), others => 0);
      elsif Message_Type = Actions.Type_Event and then
        Actions.Action_Of (Header) = Actions.Engine_Failure_Notification
      then
         -- Linux v6.16 intel_guc_engine_failure_process_msg: class,
         -- instance, reason.
         if Header /= Actions.Event_Header (Actions.Engine_Failure_Notification) or else
           Payload'Length /= 1 + Actions.Engine_Failure_Data_Words then return Result; end if;
         return (Tag => Engine_Failure, Fence => Fence,
                 Engine_Class => Payload (Payload'First + 1),
                 Engine_Instance => Payload (Payload'First + 2),
                 Reason => Payload (Payload'First + 3), others => 0);
      elsif Message_Type = Actions.Type_Event and then
        Actions.Action_Of (Header) in Actions.Notify_Crash_Dump_Posted |
          Actions.Notify_Exception | Actions.Notify_Memory_Cat_Error
      then
         return (Tag => GuC_Failure, Fence => Fence,
                 Action => Actions.Action_Of (Header), others => 0);
      elsif Message_Type = Actions.Type_Event and then
        Actions.Action_Of (Header) in Actions.Default_Notification |
          Actions.State_Capture_Notification |
          Actions.Notify_Flush_Log_Buffer_To_File | Actions.TLB_Invalidation_Done
      then
         return (Tag => Notification, Fence => Fence,
                 Action => Actions.Action_Of (Header), others => 0);
      elsif Message_Type in Actions.Type_Event | Actions.Type_No_Response_Busy |
        Actions.Type_No_Response_Retry | Actions.Type_Response_Success
      then
         Result.Tag := Other_Message; Result.Fence := Fence;
      end if;
      return Result;
   end Decode;
end Intel_GPU_GuC_Context_Event;

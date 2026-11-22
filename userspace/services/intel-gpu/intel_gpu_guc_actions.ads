with Interfaces; use Interfaces;
-- GuC v70 Host<->GuC (HXG) message ABI constants used by context lifecycle
-- and submission. Values are the Intel-authored Linux ABI headers, v6.16:
--   drivers/gpu/drm/i915/gt/uc/abi/guc_messages_abi.h   (HXG header fields)
--   drivers/gpu/drm/i915/gt/uc/abi/guc_actions_abi.h    (action codes)
-- The xe driver carries identical values in drivers/gpu/drm/xe/abi/.
-- Encoding constants only: no value here proves firmware acceptance.
package Intel_GPU_GuC_Actions with SPARK_Mode, Pure is
   subtype HXG_Word is Unsigned_32;
   subtype Action_Code is Unsigned_32 range 0 .. 16#FFFF#;

   -- guc_messages_abi.h: GUC_HXG_MSG_0_ORIGIN (bit 31), GUC_HXG_MSG_0_TYPE
   -- (bits 30:28), GUC_HXG_REQUEST_MSG_0_DATA0 (27:16), ACTION (15:0).
   Origin_GuC : constant HXG_Word := 16#8000_0000#;
   Type_Shift : constant := 28;
   Type_Mask : constant HXG_Word := 7;
   Action_Mask : constant HXG_Word := 16#FFFF#;
   Type_Request : constant HXG_Word := 0;          -- GUC_HXG_TYPE_REQUEST
   Type_Event : constant HXG_Word := 1;            -- GUC_HXG_TYPE_EVENT
   Type_Fast_Request : constant HXG_Word := 2;     -- GUC_HXG_TYPE_FAST_REQUEST
   Type_No_Response_Busy : constant HXG_Word := 3; -- GUC_HXG_TYPE_NO_RESPONSE_BUSY
   Type_No_Response_Retry : constant HXG_Word := 5;-- GUC_HXG_TYPE_NO_RESPONSE_RETRY
   Type_Response_Failure : constant HXG_Word := 6; -- GUC_HXG_TYPE_RESPONSE_FAILURE
   Type_Response_Success : constant HXG_Word := 7; -- GUC_HXG_TYPE_RESPONSE_SUCCESS

   -- guc_actions_abi.h enum intel_guc_action.
   Sched_Context : constant Action_Code := 16#1000#;
   --  INTEL_GUC_ACTION_SCHED_CONTEXT: re-read an enabled context's LRC tail.
   --  FAST request, no response, no G2H credit (intel_guc_submission.c
   --  __guc_add_request; xe_guc_submit.c submit_exec_queue).
   Sched_Context_Mode_Set : constant Action_Code := 16#1001#;
   --  INTEL_GUC_ACTION_SCHED_CONTEXT_MODE_SET: enable/disable scheduling.
   Sched_Context_Mode_Done : constant Action_Code := 16#1002#;
   --  INTEL_GUC_ACTION_SCHED_CONTEXT_MODE_DONE: G2H event for the above.
   Host2GuC_Update_Context_Policies : constant Action_Code := 16#100B#;
   Register_Context : constant Action_Code := 16#4502#;
   Deregister_Context : constant Action_Code := 16#4503#;
   Deregister_Context_Done : constant Action_Code := 16#4600#;

   -- Unsolicited G2H notifications (guc_actions_abi.h; dispatch in i915
   -- intel_guc_ct.c ct_process_request and xe_guc_ct.c).
   Default_Notification : constant Action_Code := 16#0000#;
   --  INTEL_GUC_ACTION_DEFAULT: legacy message bitmask in DATA.
   Context_Reset_Notification : constant Action_Code := 16#1008#;
   --  The GuC reset an engine under this guc_id (one payload DWORD).
   Engine_Failure_Notification : constant Action_Code := 16#1009#;
   --  The GuC's own engine reset failed: class, instance, reason. GT reset.
   Notify_Memory_Cat_Error : constant Action_Code := 16#6000#;
   --  xe only (xe/abi/guc_actions_abi.h): catastrophic memory error.
   TLB_Invalidation_Done : constant Action_Code := 16#7001#;
   State_Capture_Notification : constant Action_Code := 16#8002#;
   Notify_Flush_Log_Buffer_To_File : constant Action_Code := 16#8003#;
   Notify_Crash_Dump_Posted : constant Action_Code := 16#8004#;
   Notify_Exception : constant Action_Code := 16#8005#;
   --  Crash dump and exception: i915 intel_guc_crash_process_msg declares
   --  the GuC dead and schedules a GT reset.

   -- Payload DWORDs after the HXG header, where Linux pins them.
   Context_Reset_Data_Words : constant := 1;
   Engine_Failure_Data_Words : constant := 3;

   -- GUC_CONTEXT_ENABLE / GUC_CONTEXT_DISABLE (intel_guc_fwif.h).
   Context_Disable : constant HXG_Word := 0;
   Context_Enable : constant HXG_Word := 1;

   function Fast_Request_Header (Action : Action_Code) return HXG_Word is
     (Shift_Left (Type_Fast_Request, Type_Shift) or Action);
   function Event_Header (Action : Action_Code) return HXG_Word is
     (Origin_GuC or Shift_Left (Type_Event, Type_Shift) or Action);
   function Message_Type (Header : HXG_Word) return HXG_Word is
     (Shift_Right (Header, Type_Shift) and Type_Mask);
   function Action_Of (Header : HXG_Word) return Action_Code is
     (Header and Action_Mask);
end Intel_GPU_GuC_Actions;

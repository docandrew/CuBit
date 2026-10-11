------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  GuC SLPC (Single Loop Power Control) start-up, as Linux v6.16 i915 does
--  it with GuC submission (gt/uc/intel_guc_slpc.c, abi/guc_actions_slpc_abi.h,
--  docs/gpu-async-submission.md "GuC SLPC"). Pure encoding and sequencing:
--  - the two-page shared-data layout and its initial image (size, GTPERF on,
--    balancer and DCC off for IP < 12.70);
--  - the H2G SLPC request (action 0x3003, event id and argument count);
--  - the start-up sequence: reset, wait for RUNNING, query, unmask the ARAT
--    timer interrupt, max = RP0, use the efficient frequency, min = RPe,
--    query again; any failure ends in Failed;
--  - the frequency limits from RP_STATE_CAP and FREQ_INFO_REC (RPe);
--  - the fallback: a GEN6_RPNSWREQ software request for RP0.
--  Nothing here proves firmware acceptance; the driver logs the outcome.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Intel_GPU_GT_Frequency;

package Intel_GPU_SLPC with Pure, SPARK_Mode is
   package Freq renames Intel_GPU_GT_Frequency;
   use type Freq.MHz;

   ---------------------------------------------------------------------------
   --  H2G request (guc_actions_slpc_abi.h "SLPC H2G MESSAGE FORMAT")
   ---------------------------------------------------------------------------
   --  HXG header: origin host, type request, DATA0 zero, ACTION 0x3003.
   Request_Header : constant Unsigned_32 := 16#3003#;

   type Event is (Reset, Shutdown, Platform_Info_Change, Display_Mode_Change,
                  Flip_Complete, Query_Task_State, Parameter_Set,
                  Parameter_Unset);
   --  enum slpc_event_id.
   function Event_Code (Item : Event) return Unsigned_32 is
     (case Item is
         when Reset => 0, when Shutdown => 1, when Platform_Info_Change => 2,
         when Display_Mode_Change => 3, when Flip_Complete => 4,
         when Query_Task_State => 5, when Parameter_Set => 6,
         when Parameter_Unset => 7);

   Event_Id_Shift : constant := 8;           --  bits 31:8 (i915 uses 15:8)
   Event_Field_Mask : constant Unsigned_32 := 16#FF#;
   subtype Argument_Count is Unsigned_32 range 0 .. 9;
   --  HOST2GUC_PC_SLPC_EVENT_MAX_INPUT_ARGS.

   function Event_Word (Item : Event; Count : Argument_Count) return Unsigned_32 is
     (Shift_Left (Event_Code (Item), Event_Id_Shift) or Count)
     with Post =>
       (Shift_Right (Event_Word'Result, Event_Id_Shift) and Event_Field_Mask) =
         Event_Code (Item) and then
       (Event_Word'Result and Event_Field_Mask) = Count;

   --  Waitboost's parameter sets are FAST requests (HXG type 2, bits
   --  30:28): no response, as i915's intel_guc_send_nb.
   Fast_Request_Header : constant Unsigned_32 := 16#2000_3003#;
   --  Every request this driver sends carries two arguments.
   Request_Arguments : constant Argument_Count := 2;
   subtype Request_Index is Natural range 0 .. 3;
   type Request is array (Request_Index) of Unsigned_32;

   ---------------------------------------------------------------------------
   --  Override parameters (enum slpc_param_id)
   ---------------------------------------------------------------------------
   Parameter_Limit : constant := 32;          --  SLPC_MAX_PARAM
   subtype Parameter is Unsigned_32 range 0 .. Parameter_Limit - 1;
   Task_Enable_GTPerf         : constant Parameter := 0;
   Task_Disable_GTPerf        : constant Parameter := 1;
   Task_Enable_Balancer       : constant Parameter := 2;
   Task_Disable_Balancer      : constant Parameter := 3;
   Task_Enable_DCC            : constant Parameter := 4;
   Task_Disable_DCC           : constant Parameter := 5;
   Global_Min_Unslice_MHz     : constant Parameter := 6;
   Global_Max_Unslice_MHz     : constant Parameter := 7;
   Ignore_Efficient_Frequency : constant Parameter := 28;
   Use_Efficient_Frequency    : constant Unsigned_32 := 0;

   ---------------------------------------------------------------------------
   --  Shared data (struct slpc_shared_data, packed, two pages)
   ---------------------------------------------------------------------------
   Page_Bytes                 : constant := 4096;
   Cache_Line_Bytes           : constant := 64;
   Header_Bytes               : constant := Cache_Line_Bytes;
   Platform_Info_Bytes        : constant := Cache_Line_Bytes;
   Task_State_Bytes           : constant := Cache_Line_Bytes;
   Max_Override_Parameters    : constant := 256;
   Override_Bitfield_Words    : constant := Max_Override_Parameters / 32;
   Override_Bytes             : constant :=
     ((Max_Override_Parameters * 4 + Override_Bitfield_Words * 4 +
       Cache_Line_Bytes - 1) / Cache_Line_Bytes) * Cache_Line_Bytes;
   Mode_Table_Bytes           : constant := Page_Bytes;
   Shared_Data_Bytes          : constant := 2 * Page_Bytes;
   Other_Bytes                : constant := Shared_Data_Bytes -
     (Header_Bytes + Platform_Info_Bytes + Task_State_Bytes + Override_Bytes +
      Mode_Table_Bytes);
   --  sizeof (struct slpc_shared_data): the header's size field, and the
   --  page-aligned allocation.
   Structure_Bytes            : constant :=
     Header_Bytes + Platform_Info_Bytes + Task_State_Bytes + Override_Bytes +
     Other_Bytes + Page_Bytes;
   pragma Compile_Time_Error (Override_Bytes /= 1088, "SLPC override block size");
   pragma Compile_Time_Error (Structure_Bytes /= Shared_Data_Bytes,
                              "SLPC shared data must be exactly two pages");

   Size_Offset             : constant := 0;
   Global_State_Offset     : constant := 4;
   Task_State_Offset       : constant := Header_Bytes + Platform_Info_Bytes;
   Task_Status_Offset      : constant := Task_State_Offset;
   Task_Frequency_Offset   : constant := Task_State_Offset + 4;
   Override_Offset         : constant := Task_State_Offset + Task_State_Bytes;
   Override_Values_Offset  : constant := Override_Offset + Override_Bitfield_Words * 4;

   Word_Count : constant := Shared_Data_Bytes / 4;
   subtype Word_Index is Natural range 0 .. Word_Count - 1;
   Size_Word         : constant Word_Index := Size_Offset / 4;
   Global_State_Word : constant Word_Index := Global_State_Offset / 4;
   Task_Status_Word  : constant Word_Index := Task_Status_Offset / 4;
   Task_Frequency_Word : constant Word_Index := Task_Frequency_Offset / 4;

   function Bit_Word (Id : Parameter) return Word_Index is
     (Override_Offset / 4 + Natural (Id / 32))
     with Post => Bit_Word'Result in
       Override_Offset / 4 .. Override_Values_Offset / 4 - 1;
   function Value_Word (Id : Parameter) return Word_Index is
     (Override_Values_Offset / 4 + Natural (Id))
     with Post => Value_Word'Result >= Override_Values_Offset / 4 and then
       Value_Word'Result < (Override_Offset + Override_Bytes) / 4;
   function Parameter_Bit (Id : Parameter) return Unsigned_32 is
     (Shift_Left (1, Natural (Id mod 32)));

   --  The image the driver writes before publishing the page, word by word
   --  (slpc_shared_data_reset): size; GTPERF enabled; balancer and DCC
   --  disabled (graphics IP 12.0 < 12.70); everything else zero.
   --  Bits 0 .. 5: the GTPERF, balancer and DCC enable/disable pairs.
   Initial_Bits : constant Unsigned_32 := 2#11_1111#;
   function Initial_Word (Index : Word_Index) return Unsigned_32 is
     (if Index = Size_Word then Shared_Data_Bytes
      elsif Index = Bit_Word (Task_Enable_GTPerf) then Initial_Bits
      elsif Index = Value_Word (Task_Enable_GTPerf) then 1
      elsif Index = Value_Word (Task_Disable_Balancer) then 1
      elsif Index = Value_Word (Task_Disable_DCC) then 1
      else 0)
     with Post =>
       (if Index = Bit_Word (Task_Enable_GTPerf) then
          (for all Id in Task_Enable_GTPerf .. Task_Disable_DCC =>
             Bit_Word (Id) = Index and then
             (Initial_Word'Result and Parameter_Bit (Id)) /= 0)) and then
       (if Index = Value_Word (Task_Disable_GTPerf) or else
           Index = Value_Word (Task_Enable_Balancer) or else
           Index = Value_Word (Task_Enable_DCC) or else
           Index = Global_State_Word or else Index = Task_Status_Word or else
           Index = Task_Frequency_Word
        then Initial_Word'Result = 0);

   --  How long the driver waits for RUNNING after the reset event is
   --  acknowledged. i915 waits 5 ms (SLPC_RESET_TIMEOUT_MS); this runs once
   --  at start-up, where a slow first reset must not force the fallback.
   Reset_Deadline_Us : constant := 100_000;

   type Global_State is (Not_Running, Initializing, Resetting, Running,
                         Shutting_Down, Error, Unknown);
   --  enum slpc_global_state; anything else is Unknown.
   function Decode_State (Raw : Unsigned_32) return Global_State is
     (case Raw is
         when 0 => Not_Running, when 1 => Initializing, when 2 => Resetting,
         when 3 => Running, when 4 => Shutting_Down, when 5 => Error,
         when others => Unknown);

   --  Task state frequencies: unslice max bits 7:0, min 15:8, 50/3 MHz.
   Task_Min_Shift : constant := 8;
   function Task_Max_MHz (Frequency : Unsigned_32) return Freq.MHz is
     (Freq.Ratio_MHz (Frequency and Event_Field_Mask));
   function Task_Min_MHz (Frequency : Unsigned_32) return Freq.MHz is
     (Freq.Ratio_MHz (Shift_Right (Frequency, Task_Min_Shift) and Event_Field_Mask));

   ---------------------------------------------------------------------------
   --  Frequency limits
   ---------------------------------------------------------------------------
   type Limits is record
      Valid : Boolean := False;
      RP0, RPe, RPn : Freq.MHz := 0;
   end record;
   function Consistent (Plan : Limits) return Boolean is
     (Plan.Valid and then Plan.RPn > 0 and then Plan.RPn <= Plan.RPe and then
      Plan.RPe <= Plan.RP0);
   --  RPe is clamped into [RPn, RP0] as gen6_rps_init does.
   function Plan_Limits (Cap, Info : Unsigned_32) return Limits is
     ((Valid => Freq.RPn_MHz (Cap) > 0 and then Freq.RP0_MHz (Cap) >= Freq.RPn_MHz (Cap),
       RP0 => Freq.RP0_MHz (Cap), RPn => Freq.RPn_MHz (Cap),
       RPe => Freq.MHz'Max (Freq.RPn_MHz (Cap),
                Freq.MHz'Min (Freq.RPe_MHz (Info), Freq.RP0_MHz (Cap)))))
     with Post => (if Plan_Limits'Result.Valid then
       Consistent (Plan_Limits'Result) and then
       Plan_Limits'Result.RP0 = Freq.RP0_MHz (Cap));

   ---------------------------------------------------------------------------
   --  Start-up sequence
   ---------------------------------------------------------------------------
   type Step is (Send_Reset, Wait_Running, Query_State, Unmask_Timer, Set_Max,
                 Set_Efficient, Set_Min, Verify, Started, Failed);
   subtype Pending is Step range Send_Reset .. Verify;
   subtype Request_Step is Step with Static_Predicate =>
     Request_Step in Send_Reset | Query_State | Set_Max | Set_Efficient |
                     Set_Min | Verify;
   --  Limits are requested only after SLPC reported RUNNING.
   subtype Limit_Step is Step range Set_Max .. Set_Min;

   function Next (Current : Pending; Succeeded : Boolean) return Step is
     (if Succeeded then Step'Succ (Current) else Failed)
     with Post => (if not Succeeded then Next'Result = Failed
                   else Next'Result /= Failed and then
                        Step'Pos (Next'Result) = Step'Pos (Current) + 1);

   --  Distinct CT fences, none of them zero or the start-up probe's 42.
   Probe_Fence : constant Unsigned_16 := 42;
   function Fence_For (Item : Request_Step) return Unsigned_16 is
     (Probe_Fence + 1 + Unsigned_16 (Step'Pos (Item)))
     with Post => Fence_For'Result /= 0 and then Fence_For'Result /= Probe_Fence;

   function Set_Request (Id : Parameter; Value : Unsigned_32) return Request is
     [Request_Header, Event_Word (Parameter_Set, Request_Arguments), Id, Value];
   --  slpc_force_min_freq: SLPC's minimum, non-blocking.
   function Fast_Min_Request (Value : Unsigned_32) return Request is
     [Fast_Request_Header, Event_Word (Parameter_Set, Request_Arguments),
      Global_Min_Unslice_MHz, Value];
   function Request_For (Item : Request_Step; Shared_GGTT : Unsigned_32;
                         Plan : Limits) return Request is
     (case Item is
         when Send_Reset =>
           [Request_Header, Event_Word (Reset, Request_Arguments), Shared_GGTT, 0],
         when Query_State | Verify =>
           [Request_Header, Event_Word (Query_Task_State, Request_Arguments),
            Shared_GGTT, 0],
         when Set_Max => Set_Request (Global_Max_Unslice_MHz, Plan.RP0),
         when Set_Efficient =>
           Set_Request (Ignore_Efficient_Frequency, Use_Efficient_Frequency),
         when Set_Min => Set_Request (Global_Min_Unslice_MHz, Plan.RPe))
     with Pre => Consistent (Plan),
          Post => Request_For'Result (0) = Request_Header and then
            (if Item in Limit_Step then Request_For'Result (1) =
               Event_Word (Parameter_Set, Request_Arguments)) and then
            (if Item = Set_Max then Request_For'Result (3) = Plan.RP0) and then
            (if Item = Set_Min then Request_For'Result (3) = Plan.RPe and then
                                    Request_For'Result (3) <= Plan.RP0);

   ---------------------------------------------------------------------------
   --  ARAT timer interrupt and the fallback software request
   ---------------------------------------------------------------------------
   --  GEN6_PMINTRMSK (0xA168) bit 9 ARAT_EXPIRED_INTRMSK: cleared so the GuC
   --  receives the ARAT timer (intel_guc_pm_intrmsk_enable).
   PM_Interrupt_Mask_Offset : constant Unsigned_32 := 16#A168#;
   ARAT_Expired_Mask_Bit : constant Unsigned_32 := 16#200#;
   function Unmask_ARAT (Mask : Unsigned_32) return Unsigned_32 is
     (Mask and not ARAT_Expired_Mask_Bit)
     with Post => (Unmask_ARAT'Result and ARAT_Expired_Mask_Bit) = 0 and then
       (Unmask_ARAT'Result or ARAT_Expired_Mask_Bit) = (Mask or ARAT_Expired_Mask_Bit);

   type Software_Request is record
      Valid : Boolean := False;
      Value : Unsigned_32 := 0;
   end record;
   --  GEN6_RPNSWREQ = GEN9_FREQUENCY (RP0 ratio); the capability field is in
   --  50 MHz units, the request in 50/3 MHz units.
   function RP0_Request (Cap : Unsigned_32) return Software_Request
     with Post => (if RP0_Request'Result.Valid then
       Freq.Requested_MHz (RP0_Request'Result.Value) = Freq.RP0_MHz (Cap) and then
       Freq.RP0_MHz (Cap) > 0);

end Intel_GPU_SLPC;

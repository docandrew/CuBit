with Intel_GPU_GuC_Actions;
package body Intel_GPU_GuC_Context_Request with SPARK_Mode is
   package Actions renames Intel_GPU_GuC_Actions;
   function Build
     (ID : Unsigned_32; Context_GPU, Pin_Bias : Unsigned_64) return Request
   is
      Result : Request;
   begin
      if not Admissible (ID, Context_GPU, Pin_Bias) then return Result; end if;
      Result.Words (0) := Actions.Fast_Request_Header (Actions.Register_Context);
      Result.Words (1) := 1; -- KMD
      Result.Words (2) := ID;
      Result.Words (3) := 0; -- GuC render class
      Result.Words (4) := 1; -- logical RCS0 mask
      -- Workqueue descriptor/base/size remain zero for a non-parent context.
      Result.Words (10) := Intel_GPU_ADLN_LRC_Descriptor.Encode
        (Context_GPU, 65536, Intel_GPU_ADLN_LRC_Descriptor.Normal);
      -- Descriptor is currently 32-bit; high word remains zero.
      Result.Valid := True;
      return Result;
   end Build;

   function Policy
     (ID, Quantum_Us, Preemption_Us : Unsigned_32;
      Preempt_To_Idle : Boolean) return Policy_Request
   is
      Result : Policy_Request;
   begin
      if ID >= 65535 or Quantum_Us = 0 or Preemption_Us = 0 then
         return Result;
      end if;
      Result.Words :=
        [Actions.Fast_Request_Header (Actions.Host2GuC_Update_Context_Policies), ID, 16#20030001#, 2,
         16#20010001#, Quantum_Us, 16#20020001#, Preemption_Us,
         16#20050001#, 0, 0, 0];
      Result.Length := 10;
      if Preempt_To_Idle then
         Result.Words (10) := 16#20040001#;
         Result.Words (11) := 1;
         Result.Length := 12;
      end if;
      return Result;
   end Policy;

   function Scheduling_Mode (ID : Unsigned_32; Enable : Boolean) return Mode_Words is
   begin
      if ID >= 65535 then return [others => 0]; end if;
      return [Actions.Fast_Request_Header (Actions.Sched_Context_Mode_Set), ID,
              (if Enable then Actions.Context_Enable else Actions.Context_Disable)];
   end Scheduling_Mode;
   function Schedule (ID : Unsigned_32) return Schedule_Words is
   begin
      if ID >= 65535 then return [others => 0]; end if;
      return [Actions.Fast_Request_Header (Actions.Sched_Context), ID];
   end Schedule;
   function Deregister (ID : Unsigned_32) return Schedule_Words is
   begin
      if ID >= 65535 then return [others => 0]; end if;
      return [Actions.Fast_Request_Header (Actions.Deregister_Context), ID];
   end Deregister;
end Intel_GPU_GuC_Context_Request;

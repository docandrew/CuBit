with Interfaces; use Interfaces;
with Intel_GPU_ADLN_LRC_Descriptor;
with Intel_GPU_GuC_Actions;
package Intel_GPU_GuC_Context_Request with SPARK_Mode is
   type Request_Words is array (Natural range 0 .. 11) of Unsigned_32;
   type Request is record
      Valid : Boolean := False;
      Words : Request_Words := [others => 0];
   end record;
   -- GuC70 single ADL-N RCS context. Includes HXG FAST_REQUEST header, NOT
   -- the CT transport header/fence. No synchronous reply is expected.
   -- Caller must establish retained mapping, initialized/visible context,
   -- unique owned ID and authenticated firmware. Numeric validity grants none
   -- of those authorities. Scheduling/policy setup are separate requests.
   function Admissible
     (ID : Unsigned_32; Context_GPU, Pin_Bias : Unsigned_64) return Boolean is
     (ID < 65535 and then Pin_Bias /= 0 and then Pin_Bias mod 4096 = 0 and then
      Context_GPU >= Pin_Bias and then
      Intel_GPU_ADLN_LRC_Descriptor.Admissible (Context_GPU, 65536));
   function Build
     (ID : Unsigned_32; Context_GPU, Pin_Bias : Unsigned_64) return Request
     with Post =>
       Build'Result.Valid = Admissible (ID, Context_GPU, Pin_Bias) and then
       (if not Build'Result.Valid then
         (for all Word of Build'Result.Words => Word = 0));

   type Policy_Request is record
      Length : Natural range 0 .. 12 := 0;
      Words : Request_Words := [others => 0];
   end record;
   -- Normal KMD priority (2), no SLPC frequency request. Time values are
   -- microseconds, not milliseconds. CuBit deliberately rejects zero (which
   -- disables the corresponding firmware limit). Forced preemption is a
   -- platform/engine decision; do not guess it from request contents.
   function Policy
     (ID, Quantum_Us, Preemption_Us : Unsigned_32;
      Preempt_To_Idle : Boolean) return Policy_Request
     with Post =>
       Policy'Result.Length =
         (if ID >= 65535 or Quantum_Us = 0 or Preemption_Us = 0 then 0
          elsif Preempt_To_Idle then 12 else 10) and then
       (if Policy'Result.Length = 0 then
          (for all Word of Policy'Result.Words => Word = 0));

   type Mode_Words is array (Natural range 0 .. 2) of Unsigned_32;
   -- FAST_REQUEST scheduling enable/disable. Unlike registration/policy,
   -- this expects the separate SCHED_CONTEXT_MODE_DONE event: reserve receive
   -- credits and record pending state BEFORE publishing the request.
   function Scheduling_Mode (ID : Unsigned_32; Enable : Boolean) return Mode_Words
     with Post => (if ID >= 65535 then
       (for all Word of Scheduling_Mode'Result => Word = 0));
   type Schedule_Words is array (Natural range 0 .. 1) of Unsigned_32;
   -- GuC70 SCHED_CONTEXT, Linux v6.16 guc_actions_abi.h and
   -- intel_guc_submission.c __guc_add_request. Only for an enabled context
   -- whose updated LRC tail is already GPU-visible; no completion response.
   function Schedule (ID : Unsigned_32) return Schedule_Words
     with Post => (if ID >= 65535 then
       (for all Word of Schedule'Result => Word = 0));
   -- GuC single-context deregistration: action4503 + owned context ID.
   -- Linux __guc_action_deregister_context reserves an asynchronous G2H
   -- completion. This encoder neither sends nor acknowledges destruction.
   -- Caller must close admission, drain work, reserve completion credits and
   -- record pending state before publication. Scheduling-disable alone does
   -- not permit IDs, context pages, PPGTT or application backing to be reused.
   function Deregister (ID : Unsigned_32) return Schedule_Words
     with Post => Deregister'Result =
       (if ID < 65535 then Schedule_Words'
          [Intel_GPU_GuC_Actions.Fast_Request_Header
             (Intel_GPU_GuC_Actions.Deregister_Context), ID]
        else Schedule_Words'[0, 0]);
end Intel_GPU_GuC_Context_Request;

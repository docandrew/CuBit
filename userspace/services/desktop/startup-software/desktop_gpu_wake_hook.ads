with Interfaces;
with CuBit.Messages;
--  The GPU completion event and blocking-call evidence of the Mesa glue
--  (Native_GPU_Queue, anv_cubit_sync.c), for Desktop's event loop
--  (Desktop_GPU_Wake). startup-software has the same spec without a GPU.
package Desktop_GPU_Wake_Hook is
   type Arm_Result is (Armed, Idle, Unavailable);
   --  Armed: a completion with Token arrives when the GPU work submitted so
   --  far completes. Idle: it has. Unavailable: no event (use a timer).
   function Arm (Token : Interfaces.Unsigned_64) return Arm_Result;
   --  True: Receipt answered the armed wake (now disarmed).
   function Collect (Receipt : CuBit.Messages.CompletionEntry) return Boolean;
   --  No GPU queue: no hint to send.
   function Boost_Hint return Boolean;
   --  The glue's blocking calls since the last report, by call site.
   procedure Blocking_Report (Text : out String; Last : out Natural);
   --  Desktop's GPU jobs since the last report, by kinds: counts, sizes and
   --  submit-to-seen times (vulkan_submission_native.c).
   procedure Job_Report (Text : out String; Last : out Natural);
end Desktop_GPU_Wake_Hook;

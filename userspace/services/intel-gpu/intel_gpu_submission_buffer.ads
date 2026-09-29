with Interfaces;
package Intel_GPU_Submission_Buffer is
   -- Single serialized startup caller, before device publication. Never retry
   -- after a partial write or flush failure; backing remains retained.
   -- GGTT_Start/Bytes must be the reserved context+ring extent provided by
   -- the existing publication callback, not an arbitrary caller-chosen VA.
   procedure Initialize
     (GGTT_Start, Bytes : Interfaces.Unsigned_64; Success : out Boolean);
   function Initialized_GPU_Start return Interfaces.Unsigned_64;
end Intel_GPU_Submission_Buffer;

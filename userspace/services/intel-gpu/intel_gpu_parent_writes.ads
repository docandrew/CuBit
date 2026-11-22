with Interfaces; use Interfaces;
with Intel_GPU_Display_Topology; use Intel_GPU_Display_Topology;
package Intel_GPU_Parent_Writes with SPARK_Mode is
   -- Software restriction, not register-granular capability enforcement.
   -- Caller supplies a fresh serialized MMIO baseline and owns the transition;
   -- this predicate supplies neither ownership nor coherence.
   function Allowed
     (Item : Request_Well;
      Register_Offset, Fresh_Before, Proposed : Unsigned_32) return Boolean is
     (Fresh_Before /= Unsigned_32'Last and then
      (case Item is
         when PW1 =>
           (case Register_Offset is
              when 16#45404# => Proposed = (Fresh_Before or 2),
              when 16#46430# => Proposed = (Fresh_Before or 16#8000#),
              when others => False),
         when PW2 => Register_Offset = 16#45404# and then
           Proposed = (Fresh_Before or 8),
         when others => False));
end Intel_GPU_Parent_Writes;

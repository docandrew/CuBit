with System;
-- Where a registry's slots lie in memory, for committing its storage as a
-- prefix (Intel_GPU_Slot_Storage). Address arithmetic only: no slot is read.
package Intel_GPU_Render_Sessions.Layout with SPARK_Mode => Off is
   -- Bytes from Base to the end of slot Count's record (the end of the
   -- registry's own fields when Count is zero), where Object lies at or
   -- after Base.
   function Bytes_Through
     (Object : Registry; Base : System.Address; Count : Slot_Count)
      return Interfaces.Unsigned_64;
end Intel_GPU_Render_Sessions.Layout;

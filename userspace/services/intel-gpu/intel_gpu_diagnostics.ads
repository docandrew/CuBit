with Interfaces;
with CuBit.Log_Records;
--  intel-gpu's diagnostic lines, in its logstore ring (CuBit.Console_Route,
--  ABI-002). intel-gpu starts before logstore: devmgr introduces logstore
--  once procmgr offers it, and lines written before then wait in the
--  program's queue. Bring-up may hang or stop: a line is a copy into the
--  ring (or the queue), never a wait.
package Intel_GPU_Diagnostics is
   procedure Capture (Text : String;
     Level : CuBit.Log_Records.Severity := CuBit.Log_Records.Information);
   --  Before a stop: wait up to Limit_Ms for logstore to be introduced (the
   --  driver may stop before logstore runs), then briefly for it to take
   --  what was written. False: the lines never reached logstore.
   function Flush (Limit_Ms : Interfaces.Unsigned_64) return Boolean;
end Intel_GPU_Diagnostics;

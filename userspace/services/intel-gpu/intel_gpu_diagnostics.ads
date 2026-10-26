with Interfaces;
with System;
with CuBit.Log_Records;
--  intel-gpu's diagnostic lines: every line goes to serial at once, and to
--  logstore as soon as devmgr has granted the publisher (Intel_GPU_Log_Grant
--  says when to ask and what an answer means). Bring-up runs before
--  logstore, and may hang or stop: the grant is asked for and its reply
--  taken from inside bring-up waits (Tick, from Capture and from
--  Intel_GPU_Wait_Report.Pulse), never only from the request loop.
package Intel_GPU_Diagnostics is
   --  Serial now; logstore now when attached, else when the grant arrives.
   --  Runs Tick.
   procedure Capture (Text : String;
     Level : CuBit.Log_Records.Severity := CuBit.Log_Records.Information);
   --  Bounded nonblocking work: ask for the grant when due, take its reply
   --  if it came (other completions are kept, in order, for Poll_Driver),
   --  publish what is captured once attached.
   procedure Tick;
   --  Sole raw completion consumer: completions Tick kept come first, the
   --  log grant's reply is handled here, and the first other completion is
   --  returned unchanged to the caller. (Publications complete in the
   --  publisher's ring, not on the completion queue.)
   function Poll_Driver (Result : System.Address) return Interfaces.Unsigned_64;
   --  Captured records still wait for publication and a stopping driver
   --  should wait (boundedly) for them: attached, or the logstore deadline
   --  has not passed.
   function Pending return Boolean;
end Intel_GPU_Diagnostics;

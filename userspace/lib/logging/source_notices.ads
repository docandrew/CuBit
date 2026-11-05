pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Handles;
with CuBit.Log_Sources;
with CuBit.Process_IDs;

--  A launcher's notices to logstore of who each process it starts is, and
--  of its exit (LOG-003, CuBit.Log_Sources), so its records keep their
--  name after it exits. devmgr and procmgr start every process: only they
--  can say what a process is, and logstore takes names only from them.
--
--  Notices are kept, in order, until logstore runs (devmgr and procmgr
--  start before it): growable, sized by the processes started meanwhile.
--  Once logstore is known, Pump opens this launcher's registrar channel to
--  it (Collector badged with the registrar tag) and puts what was kept,
--  then each later notice at once. The channel is lossless: what does not
--  fit stays here for the next Pump. Never waits for logstore longer than
--  one bounded open.
generic
   --  This launcher's registrar issuance (CuBit.Log_Sources).
   Issuance : CuBit.Log_Sources.Registrar_Issuance;
   --  An untagged endpoint to logstore with Grant, or the null handle while
   --  none is known (as for Deferred_Warnings).
   with function Collector return CuBit.Handles.Endpoint;
package Source_Notices is
   --  Process started: its package identity (from its image) and its
   --  program's file name. Nothing is noted when both are empty.
   procedure Started (Process : CuBit.Process_IDs.Process_ID; Identity, Name : String);
   --  Process exited (or a launch was withdrawn).
   procedure Exited (Process : CuBit.Process_IDs.Process_ID);
   --  Call from the launcher's loop; cheap when nothing is pending.
   procedure Pump;
   --  Notices wait for logstore or for room: the caller should wake within
   --  Retry_Interval_Ms to Pump.
   function Pending return Boolean;
   Retry_Interval_Ms : constant := 1_000;
   --  Notices given up for want of memory.
   function Lost return Unsigned_64;
end Source_Notices;

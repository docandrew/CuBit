with CuBit.Messages;
with Interfaces;

--  Private Config/procmgr startup control. Only the registered process
--  manager may call this; the call offers Config an endpoint to the worker
--  procmgr started (CuBit.Handles.IPC.Call_Offering), which Config takes
--  with Accept_Offered while answering it. No binary self-nominates.
package Config_Worker_Startup with SPARK_Mode is
   type Operation is (Attach_Worker);
   for Operation use (Attach_Worker => 16#0616#);
   function Valid_Attachment (Request : CuBit.Messages.Message) return Boolean;
   -- procmgr offers Config a copy of the worker's endpoint badged with
   -- Worker_Request_Tag (CAP-002: an untagged copy arrives with tag zero,
   -- the kernel stamps no identity). Only that copy carries the badge, and
   -- the kernel-stamped sender must also be the registered Config. Inputs
   -- must come from RECEIVE and the trusted registry; this grants nothing.
   Worker_Request_Tag : constant Interfaces.Unsigned_64 := 16#4346_4757_524B_0001#;
   function Authorized_Config_Request
     (Sender : CuBit.Messages.Process_ID; Tag : Interfaces.Unsigned_64;
      Registered_Config : CuBit.Messages.Process_ID) return Boolean;
end Config_Worker_Startup;

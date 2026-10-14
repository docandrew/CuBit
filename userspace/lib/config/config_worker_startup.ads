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
   -- The offered endpoint is an untagged copy. The kernel then stamps its
   -- holder's identity as authority tag, NOT zero. Inputs must come from
   -- RECEIVE and the trusted registry; this predicate grants nothing.
   function Authorized_Config_Request
     (Sender : CuBit.Messages.Process_ID; Tag : Interfaces.Unsigned_64;
      Registered_Config : CuBit.Messages.Process_ID) return Boolean;
end Config_Worker_Startup;

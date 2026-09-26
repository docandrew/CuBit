with CuBit.Messages;
with Interfaces;

--  Private Config/procmgr startup control. Only the registered process
--  manager may call this after minting the reserved endpoint into Config.
--  Slot 61 is not chosen by the request, and no binary self-nominates.
package Config_Worker_Startup with SPARK_Mode is
   type Operation is (Attach_Worker);
   for Operation use (Attach_Worker => 16#0616#);
   Worker_Endpoint : constant CuBit.Messages.CapabilitySlot := 61;
   function Valid_Attachment (Request : CuBit.Messages.Message) return Boolean;
   -- The private endpoint is minted with object.param = 0. The kernel then
   -- stamps its holder's PID as authority tag, NOT zero. Inputs must come
   -- from RECEIVE and the trusted registry; this predicate grants nothing.
   function Authorized_Config_Request
     (Sender : CuBit.Messages.ProcessID; Tag, Registered_Config : Interfaces.Unsigned_64)
      return Boolean;
end Config_Worker_Startup;

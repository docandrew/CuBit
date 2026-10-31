pragma Ada_2022;
with CuBit.Handles;
with CuBit.Messages;
with GPIO_Request_Dispatch;
with GPIO_Native_Request_IO;
generic
   with package Dispatch is new GPIO_Request_Dispatch
     (Reply_Authority => CuBit.Handles.Reply,
      Save => GPIO_Native_Request_IO.Save,
      Answer_Saved => GPIO_Native_Request_IO.Answer_Saved,
      Answer_Current => GPIO_Native_Request_IO.Answer_Current, others => <>);
package GPIO_Native_Dispatch is
   procedure Handle (Caller : CuBit.Messages.Process_ID; Message : CuBit.Messages.Message;
      Result : out Dispatch.Report);
   -- Feed only Poll_Service_Request output. Never Poll_Any_Ipc, events or
   -- completions. Kernel-stamped authorityTag is the badge; caller is separate
   -- inbox metadata. Reject all reserved/IPC flag fields and oversized frames.
end GPIO_Native_Dispatch;

pragma Ada_2022;
with CuBit.Handles;
with CuBit.Handles.IPC;
with CuBit.Messages;
with GPIO_Admin_Owner;
with GPIO_Native_Transport;
generic
   with package Admin is new GPIO_Admin_Owner
     (Reply_Authority => CuBit.Handles.Reply, Event_Sink => GPIO_Native_Transport.Event_Sink,
      Offer_Receipt => CuBit.Handles.IPC.Carried, others => <>);
package GPIO_Native_Admin_Dispatch is
   procedure Handle (S : in out Admin.State; Caller : CuBit.Messages.Process_ID;
      Message : CuBit.Messages.Message; Setup_Deadline, Cleanup_Deadline : Admin.I.Tick);
   -- Feed only service inbox entries; admin owner authenticates kernel sender.
   -- Only the offered-capability IPC flag is accepted, never transferred/refused
   -- or reply-capability flags; kernel receipt acceptance remains authoritative.
end GPIO_Native_Admin_Dispatch;

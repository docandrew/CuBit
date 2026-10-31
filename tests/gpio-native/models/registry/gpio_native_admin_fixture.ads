pragma Ada_2022;
with CuBit.Handles;
with CuBit.Handles.IPC;
with GPIO_Admin_Owner;
with GPIO_Native_Admin_IO;
with GPIO_Native_Admin_Dispatch;
with GPIO_Native_Request_Fixture;
with GPIO_Native_Transport;
package GPIO_Native_Admin_Fixture is
   -- Semantic-only: No_Process grants no launcher authority. Actual main must
   -- pass its generated typed Startup.launcher binding.
   package IO is new GPIO_Native_Admin_IO (CuBit.Handles.No_Process);
   package Admin is new GPIO_Admin_Owner
     (CuBit.Handles.Reply, GPIO_Native_Transport.Event_Sink, CuBit.Handles.IPC.Carried,
      GPIO_Native_Request_Fixture.Registry, IO.Authorized, IO.Take_Offer, IO.Inspect,
      IO.Discard, IO.Save, IO.Answer_Saved, IO.Answer_Current);
   package Native is new GPIO_Native_Admin_Dispatch (Admin);
end GPIO_Native_Admin_Fixture;

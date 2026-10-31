pragma Ada_2022;
with CuBit.Handles;
with CuBit.Handles.IPC;
with GPIO_Controller_Resources;
with GPIO_Lease_Control;
with Intel_GPIO_IRQ;
with Intel_GPIO_Controller;
with GPIO_Pin_Leases;
with GPIO_Channel_Owner;
with GPIO_Service_Loop;
with GPIO_Native_Clock;
with GPIO_Native_Communities;
with GPIO_Native_Transport;
with GPIO_Request_Dispatch;
with GPIO_Native_Request_IO;
with GPIO_Native_Dispatch;
with GPIO_Admin_Owner;
with GPIO_Native_Admin_IO;
with GPIO_Native_Admin_Dispatch;
generic
   Capacity : GPIO_Controller_Resources.Pin_Limit;
   Operation_Budget : Intel_GPIO_IRQ.Limit;
   Launcher : CuBit.Handles.Process;
   with function Startup_Parked return Boolean;
package GPIO_Native_Driver is
   pragma Elaborate_Body;
   package Controller is new Intel_GPIO_Controller
     (Capacity, GPIO_Native_Clock.Read, GPIO_Native_Communities.Read, GPIO_Native_Communities.Write);
   procedure Next_Lease (Lease : out GPIO_Lease_Control.Lease_ID; Available : out Boolean);
   -- Internal process-lifetime allocator, no wrap/reuse and no wire authority.
   package Leases is new GPIO_Pin_Leases (Controller, GPIO_Native_Clock.Read, Next_Lease);
   package Channels is new GPIO_Channel_Owner
     (Leases, Operation_Budget, CuBit.Handles.Reply, GPIO_Native_Transport.Event_Sink,
      GPIO_Native_Transport.Answer, GPIO_Native_Transport.Try_Event);
   package Registry is new GPIO_Service_Loop
     (CuBit.Handles.Reply, GPIO_Native_Transport.Event_Sink, Channels, Capacity, GPIO_Native_Transport.Close, Startup_Parked, GPIO_Native_Communities.Admit_Pin);
   package Client is new GPIO_Request_Dispatch
     (CuBit.Handles.Reply, Registry.Authorized, GPIO_Native_Request_IO.Save,
      GPIO_Native_Request_IO.Answer_Saved, GPIO_Native_Request_IO.Answer_Current, Registry.Offer);
   package Native_Client is new GPIO_Native_Dispatch (Client);
   package Admin_IO is new GPIO_Native_Admin_IO (Launcher);
   package Admin is new GPIO_Admin_Owner
     (CuBit.Handles.Reply, GPIO_Native_Transport.Event_Sink, CuBit.Handles.IPC.Carried,
      Registry, Admin_IO.Authorized, Admin_IO.Take_Offer, Admin_IO.Inspect, Admin_IO.Discard,
      Admin_IO.Save, Admin_IO.Answer_Saved, Admin_IO.Answer_Current);
   package Native_Admin is new GPIO_Native_Admin_Dispatch (Admin);
   function Needs_Step (S : Admin.State) return Boolean;
   -- Actual native composition, not a permissive fixture. Main supplies its
   -- typed Startup.launcher, explicit budget/capacity and admitted resources.
   -- Startup parks host-owned GPIOs and freezes community geometry before
   -- readiness. Authenticated runtime admission supplies each exact pin/mode.
   -- No IRQ binding/enabling is performed here; parent stays disabled pending
   -- ONESHOT and exact event-epoch acknowledgement integration.
end GPIO_Native_Driver;

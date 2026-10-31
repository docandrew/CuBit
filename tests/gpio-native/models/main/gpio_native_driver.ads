pragma Ada_2022;
with CuBit.Handles;
with GPIO_Controller_Resources;
with GPIO_Lease_Control;
with Intel_GPIO_IRQ;
with Intel_GPIO_Controller;
with GPIO_Pin_Leases;
with GPIO_Channel_Owner;
with GPIO_Service_Loop;
with GPIO_Admin_Owner;
with GPIO_Native_Clock;
with GPIO_Native_Communities;
with Main_Model;
-- Host boundary composition: actual controller/lease/channel/registry/admin
-- owners; symbolic handle transfer/answers in Main_Model replace native IPC.
generic
   Capacity : GPIO_Controller_Resources.Pin_Limit;
   Operation_Budget : Intel_GPIO_IRQ.Limit;
   Launcher : CuBit.Handles.Process;
   with function Startup_Parked return Boolean;
package GPIO_Native_Driver is
   pragma Elaborate_Body;
   use type CuBit.Handles.Process;
   function Authorized (Caller : Main_Model.P.Process_ID) return Boolean is
     (Launcher = 1 and then Main_Model.Authorized (Caller));
   package Controller is new Intel_GPIO_Controller
     (Capacity, GPIO_Native_Clock.Read, GPIO_Native_Communities.Read, GPIO_Native_Communities.Write);
   procedure Next_Lease (Lease : out GPIO_Lease_Control.Lease_ID; Available : out Boolean);
   package Leases is new GPIO_Pin_Leases (Controller, GPIO_Native_Clock.Read, Next_Lease);
   package Channels is new GPIO_Channel_Owner
     (Leases, Operation_Budget, CuBit.Handles.Reply, Natural, Main_Model.Answer_Control, Main_Model.Try_Event);
   package Registry is new GPIO_Service_Loop
     (CuBit.Handles.Reply, Natural, Channels, Capacity, Main_Model.Close_Sink, Startup_Parked,
      GPIO_Native_Communities.Admit_Pin);
   package Admin is new GPIO_Admin_Owner
     (CuBit.Handles.Reply, Natural, Natural, Registry, Authorized,
      Main_Model.Take_Offer, Main_Model.Inspect, Main_Model.Discard, Main_Model.Save,
      Main_Model.Answer_Saved, Main_Model.Answer_Current);
   function Needs_Step (S : Admin.State) return Boolean;
end GPIO_Native_Driver;

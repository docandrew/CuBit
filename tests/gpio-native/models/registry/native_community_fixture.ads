with Intel_GPIO_Controller;
with GPIO_Native_Clock;
with GPIO_Native_Communities;
with GPIO_Pin_Leases;
with GPIO_Lease_Control;
package Native_Community_Fixture is
   package Driver is new Intel_GPIO_Controller
     (2, GPIO_Native_Clock.Read, GPIO_Native_Communities.Read, GPIO_Native_Communities.Write);
   procedure Next_Lease (Lease : out GPIO_Lease_Control.Lease_ID; Available : out Boolean);
   package Leases is new GPIO_Pin_Leases (Driver, GPIO_Native_Clock.Read, Next_Lease);
end Native_Community_Fixture;

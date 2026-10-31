pragma Ada_2022;
package body GPIO_Native_Driver is
   use type GPIO_Lease_Control.Lease_ID;
   Last_Lease : GPIO_Lease_Control.Lease_ID := GPIO_Lease_Control.No_Lease;
   procedure Next_Lease (Lease : out GPIO_Lease_Control.Lease_ID; Available : out Boolean) is
   begin
      Lease := GPIO_Lease_Control.No_Lease; Available := False;
      if Last_Lease = GPIO_Lease_Control.Lease_ID'Last then return; end if;
      Last_Lease := Last_Lease + 1; Lease := Last_Lease; Available := True;
   end Next_Lease;
   function Needs_Step (S : Admin.State) return Boolean is
     (Registry.Needs_Step or else Admin.Observe (S).At_Phase in
        Admin.Await_Admission | Admin.Await_Retirement | Admin.Drain_Lost_Admission);
end GPIO_Native_Driver;

package body Native_Community_Fixture is
   use type GPIO_Lease_Control.Lease_ID;
   Last : GPIO_Lease_Control.Lease_ID := GPIO_Lease_Control.No_Lease;
   procedure Next_Lease (Lease : out GPIO_Lease_Control.Lease_ID; Available : out Boolean) is
   begin
      Lease := GPIO_Lease_Control.No_Lease; Available := False;
      if Last = GPIO_Lease_Control.Lease_ID'Last then return; end if;
      Last := Last + 1; Lease := Last; Available := True;
   end Next_Lease;
end Native_Community_Fixture;

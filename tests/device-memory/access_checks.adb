with Ada.Text_IO;
with Interfaces; use Interfaces;
with Capabilities; use Capabilities;
with Device_Memory_Admission;
with Device_Memory_Authority;
with Physical_Ranges;
procedure Access_Checks is
   --  The one capability a mapping call names (CAP-002: by handle).
   Table : Capability := NULL_CAPABILITY;
   Allowed : Boolean;
   -- Nothing described: every page is bus address space, so these checks
   -- isolate the capability-table half of the decision.
   All_Device : Physical_Ranges.Map := Physical_Ranges.Empty_Map;
   procedure Check (Base, Size : Unsigned_64; Write, Expected : Boolean) is
   begin
      Allowed := Device_Memory_Authority.Authorize
        (Table, All_Device, Base, Size,
         (if Write then Device_Memory_Admission.Read_Write
          else Device_Memory_Admission.Read_Only));
      pragma Assert (Allowed = Expected);
   end Check;
begin
   Physical_Ranges.Seal (All_Device);
   Check (16#1000#, 4096, False, False);
   Table.capType := CAP_DEVICE_MEM;
   Table.object.ref := 16#1000#;
   Table.object.param := 8192;
   for Read_Right in Boolean loop
      for Write_Right in Boolean loop
         Table.rights := NO_RIGHTS;
         Table.rights (RIGHT_READ) := Read_Right;
         Table.rights (RIGHT_WRITE) := Write_Right;
         for Write_Request in Boolean loop
            Check (16#1000#, 8192, Write_Request,
                   Read_Right and (not Write_Request or Write_Right));
            Check (16#2000#, 4096, Write_Request,
                   Read_Right and (not Write_Request or Write_Right));
            Check (16#0FFF#, 1, Write_Request, False);
            Check (16#3000#, 1, Write_Request, False);
            Check (16#2000#, 4097, Write_Request, False);
            Check (16#1000#, 0, Write_Request, False);
            Check (Unsigned_64'Last - 1, 4, Write_Request, False);
         end loop;
      end loop;
   end loop;
   Table.rights := ALL_RIGHTS;
   Table.object.ref := Unsigned_64'Last - 4095;
   Table.object.param := 8192;
   Check (Unsigned_64'Last - 4095, 1, False, False);
   Table.object.ref := 16#1000#;
   Table.object.param := 0;
   Check (16#1000#, 1, False, False);
   Ada.Text_IO.Put_Line ("device memory rights/range PASS");
end Access_Checks;

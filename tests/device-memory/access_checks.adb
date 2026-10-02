with Ada.Text_IO;
with Interfaces; use Interfaces;
with Capabilities; use Capabilities;
with Capabilities.Operations;
procedure Access_Checks is
   Table : CapabilityTable := EMPTY_TABLE;
   Allowed : Boolean;
   procedure Check (Base, Size : Unsigned_64; Write, Expected : Boolean) is
   begin
      Capabilities.Operations.checkDeviceMemAccess
        (Table, Base, Size, Allowed, requireWrite => Write);
      pragma Assert (Allowed = Expected);
   end Check;
begin
   Check (16#1000#, 4096, False, False);
   Table (0).capType := CAP_DEVICE_MEM;
   Table (0).object.ref := 16#1000#;
   Table (0).object.param := 8192;
   for Read_Right in Boolean loop
      for Write_Right in Boolean loop
         Table (0).rights := NO_RIGHTS;
         Table (0).rights (RIGHT_READ) := Read_Right;
         Table (0).rights (RIGHT_WRITE) := Write_Right;
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
   Table (0).rights := ALL_RIGHTS;
   Table (0).object.ref := Unsigned_64'Last - 4095;
   Table (0).object.param := 8192;
   Check (Unsigned_64'Last - 4095, 1, False, False);
   Table (0).object.ref := 16#1000#;
   Table (0).object.param := 0;
   Check (16#1000#, 1, False, False);
   Ada.Text_IO.Put_Line ("device memory rights/range PASS");
end Access_Checks;

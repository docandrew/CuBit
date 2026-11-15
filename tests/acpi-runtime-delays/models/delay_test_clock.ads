with Interfaces; package Delay_Test_Clock is
 Now : Interfaces.Unsigned_64 := 10; Available : Boolean := True;
 Reads, Sleeps, Yields : Natural := 0;
 Reenter, Close_In_Sleep : Boolean := False;
end Delay_Test_Clock;

package Admission_Probe is
 type Gate_Array is array (Positive range 1 .. 4) of Boolean;
 Gates : Gate_Array := [others => False];
 Seen : Gate_Array := [others => False];
 Handle_At : Natural := 0;
 Quiet : Boolean := True;
 Blocked : Boolean := False;
 Diagnostics, Warnings, Button_Stops, Entry_Stops : Natural := 0;
 Cached_Power_Allowed, Cached_Battery_Allowed : Boolean := False;
end Admission_Probe;
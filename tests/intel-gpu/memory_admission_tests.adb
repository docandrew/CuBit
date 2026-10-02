with Interfaces; use Interfaces;
with Ada.Text_IO;
with Intel_GPU_Device_Query; use Intel_GPU_Device_Query;
with Intel_GPU_Memory_Admission; use Intel_GPU_Memory_Admission;
procedure Memory_Admission_Tests is
   State : Evidence;
begin
   pragma Assert (Policy (State) = Not_Admitted);
   for Bits in Unsigned_32 range 0 .. 63 loop
      State := (Device => 16#46D2#,
        Runtime_Admitted => (Bits and 1) /= 0,
        Owner_Held => (Bits and 2) /= 0,
        Session_Healthy => (Bits and 4) /= 0,
        Faulted => (Bits and 8) /= 0,
        CPU_To_GPU_Checked => (Bits and 16) /= 0,
        GPU_To_CPU_Checked => (Bits and 32) /= 0);
      pragma Assert (Policy (State) =
        (if (Bits and 15) /= 7 then Not_Admitted
         elsif (Bits and 48) = 48 then Owned_WB_Coherent
         else Owned_WB_Explicit_Maintenance));
   end loop;
   State := (16#46D2#, True, True, True, False, True, True);
   for Device in Unsigned_16 loop
      State.Device := Device;
      pragma Assert (Policy (State) =
        (if Device = 16#46D2# then Owned_WB_Coherent else Not_Admitted));
   end loop;
   State := (16#46D2#, True, True, True, False, True, True);
   -- A historical probe success cannot survive current ownership/health loss.
   State.Owner_Held := False;
   pragma Assert (Policy (State) = Not_Admitted);
   State.Owner_Held := True; State.Session_Healthy := False;
   pragma Assert (Policy (State) = Not_Admitted);
   State.Session_Healthy := True; State.Faulted := True;
   pragma Assert (Policy (State) = Not_Admitted);
   Ada.Text_IO.Put_Line
     ("Memory admission PASS: 64 flag states, 65536 device IDs; NOT hardware coherence");
end Memory_Admission_Tests;

pragma Ada_2022;
with Ada.Text_IO;
with Launch_Configuration;
with Root_Export_Admission;
procedure Root_Capacity_Tests is
   use Root_Export_Admission;
   use type Launch_Configuration.Root_Count;
   Limit : constant Root_Demand :=
     Root_Demand (Launch_Configuration.Maximum_Roots);
   Checks : Natural := 0;
   procedure Check (Device, Firmware : Root_Demand; Allowed : Boolean) is
      Pick : constant Selection := Choose (Device, Firmware);
   begin
      if Fits (Device, Firmware) /= Allowed
        or else (Pick.Status = Admitted) /= Allowed
        or else (if Allowed then
                   Root_Demand (Pick.Device) /= Device
                   or else Root_Demand (Pick.Firmware) /= Firmware
                 else Pick.Device /= 0 or else Pick.Firmware /= 0)
      then
         raise Program_Error with "root inventory admission mismatch";
      end if;
      Checks := Checks + 1;
   end Check;
begin
   Check (0, 0, True);
   --  Every partition at the ceiling, and one above it; no prefix allowed.
   for Device in Root_Demand range 0 .. Limit loop
      Check (Device, Limit - Device, True);
      Check (Device, Limit - Device + 1, False);
   end loop;
   Check (Limit + 1, 0, False);
   Check (Root_Demand'Last, 0, False);
   Check (0, Root_Demand'Last, False);
   Check (Root_Demand'Last, Root_Demand'Last, False);
   Ada.Text_IO.Put_Line ("root capacity checks:" & Checks'Image);
end Root_Capacity_Tests;

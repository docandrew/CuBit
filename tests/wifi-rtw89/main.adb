--  Hosted tests for the RTL8852BE driver's SPARK parsers. Run by run.sh;
--  exits nonzero when any check fails.
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Test_Support; use Test_Support;
with Efuse_Tests;
with Firmware_Tests;
with Descriptor_Tests;

procedure Main is
begin
   Efuse_Tests;
   Firmware_Tests;
   Descriptor_Tests;
   Put_Line ("rtw89 tests:" & Natural'Image (Checks) & " checks," &
             Natural'Image (Failed) & " failed");
   if Failed /= 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Main;

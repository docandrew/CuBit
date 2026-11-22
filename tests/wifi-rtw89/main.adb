--  Hosted tests for the RTL8852BE driver's SPARK units. Run by run.sh;
--  exits nonzero when any check fails. With arguments it is the Ada side
--  of the differential tests (Corpus): frames|rx <in> <out>, tables <out>.
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Test_Support; use Test_Support;
with Efuse_Tests;
with Firmware_Tests;
with Descriptor_Tests;
with Scan_Tests;
with Beacon_Tests;
with RX_Tests;
with Corpus;

procedure Main is
begin
   if Argument_Count = 3 and then Argument (1) = "frames" then
      Corpus.Frames (Argument (2), Argument (3));
      return;
   elsif Argument_Count = 3 and then Argument (1) = "rx" then
      Corpus.RX (Argument (2), Argument (3));
      return;
   elsif Argument_Count = 2 and then Argument (1) = "tables" then
      Corpus.Tables (Argument (2));
      return;
   end if;
   Efuse_Tests;
   Firmware_Tests;
   Descriptor_Tests;
   Scan_Tests;
   Beacon_Tests;
   RX_Tests;
   Put_Line ("rtw89 tests:" & Natural'Image (Checks) & " checks," &
             Natural'Image (Failed) & " failed");
   if Failed /= 0 then
      Set_Exit_Status (Failure);
   end if;
end Main;

with Ada.Text_IO;
with HID_Channel_Policy_Tests;
procedure Policy_Main is
   Checks : Natural;
begin
   HID_Channel_Policy_Tests (Checks);
   Ada.Text_IO.Put_Line ("channel policy checks=" & Checks'Image);
end Policy_Main;

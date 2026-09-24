with Ada.Text_IO;
with CuBit.Config_Inspection;
procedure Main is
   use CuBit.Config_Inspection;
   Offset_Scope : constant String (4 .. 16) := "cubit.display";
   Offset_Key : constant String (8 .. 25) := "cubit.display.mode";
   Last_Index : constant String (Positive'Last .. Positive'Last) := "x";
begin
   pragma Assert (Contains ("", ""));
   pragma Assert (Contains ("", "any.key"));
   pragma Assert (not Contains ("cubit", ""));
   pragma Assert (Contains ("cubit.display", "cubit.display"));
   pragma Assert (Contains ("cubit.display.", "cubit.display.mode"));
   pragma Assert (Contains (Offset_Scope, Offset_Key));
   pragma Assert (Contains (Last_Index, Last_Index));
   pragma Assert (Contains ("x", Last_Index));
   pragma Assert (not Contains (Last_Index, "y"));
   pragma Assert (not Contains ("cubit.display", "cubit.displaymalware"));
   pragma Assert (not Contains ("cubit.display", "cubit.displa"));
   pragma Assert (not Contains (".", "anything"));
   for Ch in Character loop
      pragma Assert
        (Contains ("app", "app" & Ch & "setting") = (Ch = '.'));
   end loop;
   Ada.Text_IO.Put_Line ("PASS Config namespace component boundaries and non-1 bounds");
end Main;

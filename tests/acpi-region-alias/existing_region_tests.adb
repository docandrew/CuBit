with Ada.Text_IO; use Ada.Text_IO;
with Region_Initialization_Tests;
procedure Existing_Region_Tests is
   Checks : constant Natural := Region_Initialization_Tests;
begin
   Put_Line ("Existing region checks" & Checks'Image);
end Existing_Region_Tests;

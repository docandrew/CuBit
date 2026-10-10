with Ada.Text_IO;
with AML_Namespace;
procedure Owner_Snapshot_Tests is
   package N is new AML_Namespace (16, Max_Frame_Roots => 2);
   Checks : Natural;
begin
   N.Owned.Test_Snapshots (Checks);
   Ada.Text_IO.Put_Line ("OWNER SNAPSHOTS" & Checks'Image);
end Owner_Snapshot_Tests;

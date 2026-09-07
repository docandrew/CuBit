with Ada.Text_IO;
with CCL_Workspace_Names; use CCL_Workspace_Names;
procedure Main is
   Number : Revision;
   Pending : Boolean;
   procedure Reject (Name : String) is
   begin
      Decode (Name, Number, Pending);
      pragma Assert (Number = 0);
   end Reject;
begin
   for Value in Saved_Revision loop
      for Temporary in Boolean loop
         declare
            Name : constant String := Filename (Value, Temporary);
            Offset_Name : constant String (17 .. 16 + Name'Length) := Name;
         begin
            Decode (Offset_Name, Number, Pending);
            pragma Assert (Number = Value and Pending = Temporary);
         end;
      end loop;
   end loop;
   Reject ("");
   Reject ("ccl-0000.ccl");
   Reject ("ccl-0001.exe");
   Reject ("ccl-0001.pending/");
   Reject ("../ccl-0001.ccl");
   Reject ("ccl-00x1.ccl");
   Reject ("xcl-0001.ccl");
   Ada.Text_IO.Put_Line ("workspace names: all 9999 revisions, pending/final, non-1 bounds PASS");
end Main;

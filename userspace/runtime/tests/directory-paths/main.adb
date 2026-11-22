with Ada.Text_IO; use Ada.Text_IO;
with CuBit.Directory_Paths; use CuBit.Directory_Paths;

procedure Main is
   Root, Child, Nested : Path;
   OK : Boolean;

   procedure Reject (Name : String) is
   begin
      Append_Child (Root, Name, Child, OK);
      pragma Assert (not OK and then Value (Child) = Value (Root));
   end Reject;
begin
   Set_Root ("@mem:0/", Root, OK);
   pragma Assert (OK);
   Append_Child (Root, "work", Child, OK);
   pragma Assert (OK and then Value (Child) = "@mem:0/work");
   Append_Child (Child, "project", Nested, OK);
   pragma Assert (OK and then Value (Nested) = "@mem:0/work/project");
   Reject ("");
   Reject (".");
   Reject ("..");
   Reject ("../secret");
   Reject ("a/b");
   Reject ("/absolute");
   Reject ("@nvme:0");
   Reject ("a" & Character'Val (0) & "b");
   Reject ([1 .. Maximum_Bytes => 'x']);
   declare
      Shifted : constant String (91 .. 94) := "work";
   begin
      Append_Child (Root, Shifted, Child, OK);
      pragma Assert (OK and then Value (Child) = "@mem:0/work");
   end;
   for Length in 0 .. Maximum_Bytes loop
      Set_Root ([1 .. Length => 'x'], Root, OK);
      pragma Assert (OK);
      Append_Child (Root, "a", Child, OK);
      pragma Assert (OK = (Length <= Maximum_Bytes - 2));
      if not OK then
         pragma Assert (Value (Child) = Value (Root));
      end if;
   end loop;
   Set_Root ([1 .. Maximum_Bytes - 1 => 'x'] & "/", Root, OK);
   pragma Assert (OK);
   Reject ("a");
   Set_Root ([1 .. Maximum_Bytes => 'x'] & "x", Root, OK);
   pragma Assert (not OK and then Value (Root) = "");
   Put_Line ("directory paths: component rejection and all capacity boundaries PASS");
end Main;

with Ada.Text_IO;
with Compositor_Workspace;
procedure Workspace_Tests is
   package W renames Compositor_Workspace;
   Boundary : Natural;
begin
   pragma Assert (not W.Valid (0, 1) and not W.Valid (1, 0));
   pragma Assert (not W.Valid (65_536, 1) and not W.Valid (1, 65_536));
   -- More than the old 16MiB scene-image cap is valid logical space.
   pragma Assert (W.Valid (7680, 2160));
   for Height in 1 .. W.Maximum_Extent loop
      Boundary := Natural'Min (W.Maximum_Extent, (Natural'Last / 4) / Height);
      pragma Assert (W.Valid (Boundary, Height));
      pragma Assert (not W.Valid (Boundary + 1, Height));
      pragma Assert (Long_Long_Integer (W.Pitch (Boundary)) * Long_Long_Integer (Height) <= Long_Long_Integer (Natural'Last));
   end loop;
   Ada.Text_IO.Put_Line ("PASS logical workspace: all height boundaries, zero/extent rejection and storage-independent admission");
end Workspace_Tests;

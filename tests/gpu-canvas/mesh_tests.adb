------------------------------------------------------------------------------
--  The tessellated teapot, hosted: Newell's control points against
--  FreeGLUT's copy, vertex count, unit normals, bounds and symmetry.
--  Usage: mesh_tests PATH/teapot-control-points.h
------------------------------------------------------------------------------
with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Teapot_Patches;
with Teapot_Mesh;

procedure Mesh_Tests is
   package P renames Teapot_Patches;
   Failures : Natural := 0;
   Tolerance : constant := 1.0e-4;
   Normal_Tolerance : constant := 1.0e-3;

   procedure Check (Condition : Boolean; What : String) is
   begin
      if Condition then
         Put_Line ("PASS " & What);
      else
         Put_Line ("FAIL " & What);
         Failures := Failures + 1;
      end if;
   end Check;

   --  Read every "{ x, y, z}" float triple of cpdata_teapot.
   type Float_List is array (Natural range <>) of Float;
   function Header_Values (Path : String) return Float_List is
      F : File_Type;
      Values : Float_List (0 .. P.Point_Count * 3 - 1) := [others => 0.0];
      Count : Natural := 0;
      In_Points : Boolean := False;
   begin
      Open (F, In_File, Path);
      while not End_Of_File (F) loop
         declare
            Line : constant String := Get_Line (F);
            I : Natural := Line'First;
            J : Natural;
         begin
            if not In_Points then
               for K in Line'Range loop
                  if K + 12 <= Line'Last and then Line (K .. K + 12) = "cpdata_teapot" then
                     In_Points := True;
                  end if;
               end loop;
            else
               while I <= Line'Last loop
                  if Line (I) = '-' or else Line (I) in '0' .. '9' then
                     J := I;
                     while J <= Line'Last and then Line (J) in '-' | '.' | '0' .. '9' loop
                        J := J + 1;
                     end loop;
                     if Count <= Values'Last then
                        Values (Count) := Float'Value (Line (I .. J - 1));
                     end if;
                     Count := Count + 1;
                     I := J + 1;   --  skip the 'f'
                  else
                     I := I + 1;
                  end if;
               end loop;
            end if;
         end;
      end loop;
      Close (F);
      Check (Count = Values'Length, "header has" & Natural'Image (Count) & " coordinates");
      return Values;
   end Header_Values;

   Mesh : Teapot_Mesh.Vertex_Array;
   Lid_Apex_Offsets : constant := 2;   --  FreeGLUT's two 0.002 lid nudges
begin
   declare
      Values : constant Float_List := Header_Values (Ada.Command_Line.Argument (1));
      Differences : Natural := 0;
   begin
      for I in P.Point_Index loop
         if abs (P.Points (I).X - Values (3 * I)) > Tolerance or else
           abs (P.Points (I).Y - Values (3 * I + 1)) > Tolerance or else
           abs (P.Points (I).Z - Values (3 * I + 2)) > Tolerance
         then
            Differences := Differences + 1;
            Put_Line ("  differs at point" & Natural'Image (I));
         end if;
      end loop;
      Check (Differences = Lid_Apex_Offsets,
             "control points equal FreeGLUT's except its two lid-apex nudges");
   end;

   Teapot_Mesh.Build (Mesh);
   declare
      Min_X, Min_Y, Min_Z : Float := Float'Last;
      Max_X, Max_Y, Max_Z : Float := Float'First;
      Bad_Normals : Natural := 0;
      Length : Float;
   begin
      for V of Mesh loop
         Min_X := Float'Min (Min_X, V.PX); Max_X := Float'Max (Max_X, V.PX);
         Min_Y := Float'Min (Min_Y, V.PY); Max_Y := Float'Max (Max_Y, V.PY);
         Min_Z := Float'Min (Min_Z, V.PZ); Max_Z := Float'Max (Max_Z, V.PZ);
         Length := Sqrt (V.NX * V.NX + V.NY * V.NY + V.NZ * V.NZ);
         if abs (Length - 1.0) > Normal_Tolerance then
            Bad_Normals := Bad_Normals + 1;
         end if;
      end loop;
      Check (Mesh'Length = 32 * 12 * 12 * 2 * 3, "vertex count 32 patches x 12 x 12 x 2 x 3");
      Check (Bad_Normals = 0, "every normal has unit length (degenerate corners too)");
      Check (abs (Min_X + 3.0) < 0.05 and then abs (Max_X - 3.43) < 0.1,
             "X spans handle (-3) to spout tip");
      Check (abs (Min_Y + Max_Y) < Tolerance, "symmetric across Y = 0");
      Check (abs (Min_Z) < Tolerance and then abs (Max_Z - 3.15) < Tolerance, "Z spans base 0 to knob 3.15");
      Put_Line ("  bounds X" & Float'Image (Min_X) & Float'Image (Max_X) &
                " Y" & Float'Image (Min_Y) & Float'Image (Max_Y) &
                " Z" & Float'Image (Min_Z) & Float'Image (Max_Z));
   end;
   if Failures = 0 then
      Put_Line ("mesh: ALL PASS");
   else
      Put_Line ("mesh: FAILURES" & Natural'Image (Failures));
      Ada.Command_Line.Set_Exit_Status (1);
   end if;
end Mesh_Tests;

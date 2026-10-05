with Ada.Text_IO;
with Interfaces;
with System.Storage_Elements;
with Compositor_Source_Region;
with Vulkan_Affine_Binding.Regions;
procedure Region_Binding_Tests is
   package R renames Compositor_Source_Region;
   package B renames Vulkan_Affine_Binding;
   package G renames B.G;
   use type R.Word, B.Outcome, G.Logical_Coordinate;
   procedure Reset (Status : R.Word) with Import, Convention => C, External_Name => "region_mock_reset";
   function Calls return R.Word with Import, Convention => C, External_Name => "region_mock_calls";
   function Field (Index : R.Word) return R.Word with Import, Convention => C, External_Name => "region_mock_field";
   Screen : constant G.Output := (32, 24, G.Orientation'First, (5, 4), -20, 10);
   Context : constant System.Address := System.Storage_Elements.To_Address (1);
   Window : constant R.Rectangle := (2, 3, 7, 9, 32, 24);
   Result : B.Outcome;
   Count : Natural := 0;
begin
   for W in R.Word range 1 .. 16 loop
      for H in R.Word range 1 .. 16 loop
         for X in R.Word range 0 .. W loop
            for Y in R.Word range 0 .. H loop
               for Extent in R.Word range 0 .. 3 loop
                  pragma Assert (R.Valid ((X, Y, Extent, Extent, W, H)) =
                    (Extent > 0 and X + Extent <= W and Y + Extent <= H));
                  Count := Count + 1;
               end loop;
            end loop;
         end loop;
      end loop;
   end loop;
   Reset (0);
   B.Regions.Draw_Output (Context, Screen, (-18, 12, -10, 19), (0, 0, 32, 24), Window, True, True, Result);
   pragma Assert (Result = B.Recorded and Calls = 1);
   pragma Assert (Field (0) = 2 and Field (1) = 3 and Field (2) = 7 and Field (3) = 9 and
     Field (4) = 32 and Field (5) = 24 and Field (6) = 32 and Field (7) = 24 and Field (8) = 2);
   B.Regions.Draw_Output (Context, Screen, (-18, 12, -10, 19), (0, 0, 0, 0), Window, True, True, Result);
   pragma Assert (Result = B.Empty and Calls = 1);
   B.Regions.Draw_Output (Context, Screen, (-18, 12, -10, 19), (0, 0, 32, 24), Window, False, True, Result);
   pragma Assert (Result = B.Rejected and Calls = 1);
   for Fault in 0 .. 10 loop
      declare
         Bad : R.Rectangle := Window;
      begin
         case Fault is
            when 0 => Bad.Width := 0;
            when 1 => Bad.Height := 0;
            when 2 => Bad.X := 32;
            when 3 => Bad.Y := 24;
            when 4 => Bad.Width := R.Word'Last;
            when 5 => Bad.Height := R.Word'Last;
            when 6 => Bad.Image_Width := 65536;
            when 7 => Bad.Image_Height := 65536;
            when 8 => Bad.Image_Width := 0;
            when 9 => Bad.Image_Height := 0;
            when others => Bad.X := R.Word'Last;
         end case;
         B.Regions.Draw_Output (Context, Screen, (-18, 12, -10, 19), (0, 0, 0, 0), Bad, True, True, Result);
         pragma Assert (Result = B.Rejected and Calls = 1);
      end;
   end loop;
   for Status in 1 .. 2 loop
      Reset (if Status = 1 then 1 else R.Word'Last);
      B.Regions.Draw_Output (Context, Screen, (-18, 12, -10, 19), (0, 0, 32, 24), Window, True, False, Result);
      pragma Assert (Result = B.Rejected and Calls = 1);
   end loop;
   Ada.Text_IO.Put_Line ("PASS region binding: exact six-word ABI, clipped/empty draw, invalid windows/blend, nonzero native statuses; geometry cases" & Count'Image);
end Region_Binding_Tests;

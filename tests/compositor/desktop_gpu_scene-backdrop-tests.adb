with Ada.Text_IO; with System; with System.Storage_Elements;
with Interfaces; with Vulkan_Device_Mock;
procedure Desktop_GPU_Scene.Backdrop.Tests is
   use CuBit.Appearance;
   use type Interfaces.Unsigned_32, V.Layer_Kind, V.A.G.Logical_Rectangle,
     V.A.Word, V.A.G.Logical_Coordinate, D.Source_Result, System.Address;
   procedure Reset with Import, Convention => C, External_Name => "submission_mock_reset";
   procedure Context_Set (Create, Release : Interfaces.Unsigned_32) with Import, Convention => C, External_Name => "context_mock_set";
   procedure Pipeline_Set (Create, Close : Interfaces.Unsigned_32) with Import, Convention => C, External_Name => "pipeline_mock_set";
   procedure Image_Set (Bytes : Interfaces.Unsigned_64; Mask, Prep, Bind, Release : Interfaces.Unsigned_32) with Import, Convention => C, External_Name => "image_mock";
   function Calls (Index : Interfaces.Unsigned_32) return Interfaces.Unsigned_32 with Import, Convention => C, External_Name => "submission_mock_calls";
   S : State; OK : Boolean; Result : Outcome;
   Source : Vulkan_Submission.Source_Ticket;
   Imported : D.Source_Result; Released : System.Address;
   Address : constant System.Address := System.Storage_Elements.To_Address (55);
   Cases : Natural := 0;
begin
   Reset; Context_Set (0, 0); Pipeline_Set (0, 0);
   Vulkan_Device_Mock.Set (True, True, 0); Image_Set (4096, 1, 0, 0, 0);
   D.Initialize (25); D.Configure_Targets (32, 24, 1, 16384, OK); pragma Assert (OK);
   D.Prepare_Pipeline (OK); pragma Assert (OK);
   D.Import_Owned_Source (128, Address, Source, Imported); pragma Assert (Imported = D.Source_Accepted);
   for Scheme in Color_Scheme loop
      for Background in CuBit.Appearance.Background loop
         for Position in Placement loop
            for Rotation in V.A.G.Orientation loop
               for Scale in 4 .. 6 loop
                  Begin_Frame (S, (32, 24, Rotation, (V.A.G.Scale_Component (Scale), 4), -20, 10), 0, OK); pragma Assert (OK);
                  Capture (S, (Scheme, Background, Position), Source, OK); pragma Assert (OK);
                  pragma Assert (V.Item (S.Scene, 1).Kind = V.Physical_Solid);
                  pragma Assert (V.Item (S.Scene, 1).Surface = (0, 0, 32, 24));
                  pragma Assert (V.Item (S.Scene, 1).Tint =
                    (if Background = Ocean then 16#FF204058# elsif Scheme = Alloy_Dark then 16#FF20282E# else 16#FF545D63#));
                  if Background in Wallpaper | Cubie then
                     pragma Assert (Layer_Count (S) = 2);
                     pragma Assert (V.Item (S.Scene, 2).Surface = (0, 0, 2048, (if Background = Cubie then 1152 else 576)));
                     pragma Assert (V.Item (S.Scene, 2).Kind = (case Position is when Fill => V.Backdrop_Fill, when Fit => V.Backdrop_Fit, when Center => V.Backdrop_Center));
                  else pragma Assert (Layer_Count (S) = 1); end if;
                  Discard (S, Result); pragma Assert (Result = Retry); Cases := Cases + 1;
               end loop;
            end loop;
         end loop;
      end loop;
   end loop;
   -- A missing image must not submit the already appended background fill.
   Begin_Frame (S, (32, 24, V.A.G.Unrotated, (1, 1), 0, 0), 0, OK); pragma Assert (OK);
   Capture (S, Default, Vulkan_Submission.No_Source, OK); pragma Assert (not OK);
   Finish (S, Result); pragma Assert (Result = Rejected and Calls (2) = 0);
   -- Exhausting the layer bound between the fill and image rejects all work.
   Begin_Frame (S, (32, 24, V.A.G.Unrotated, (1, 1), 0, 0), 0, OK); pragma Assert (OK);
   for I in 1 .. V.Maximum_Layers - 1 loop
      Append (S, (Kind => V.Solid, Surface => (0, 0, 1, 1), others => <>), OK); pragma Assert (OK);
   end loop;
   Capture (S, Default, Source, OK); pragma Assert (not OK);
   Finish (S, Result); pragma Assert (Result = Rejected and Calls (2) = 0);
   D.Release_Source (Source, Released); pragma Assert (Released = Address);
   Close (S, OK); pragma Assert (OK); D.Stop;
   Ada.Text_IO.Put_Line ("PASS wallpaper capture:" & Natural'Image (Cases) & " style/DPI/rotation cases; missing source and overflow never submit a partial scene");
end Desktop_GPU_Scene.Backdrop.Tests;

with Ada.Text_IO; with System; with System.Storage_Elements; with Interfaces;
with Vulkan_Device_Mock;
procedure Desktop_GPU_Scene.Drawing.Mixed_Tests is
   subtype U32 is Interfaces.Unsigned_32; subtype U64 is Interfaces.Unsigned_64;
   use type V.A.G.Logical_Coordinate, U32, D.Source_Result, System.Address, V.Layer_Kind, V.A.G.Logical_Rectangle;
   procedure Reset with Import, Convention => C, External_Name => "submission_mock_reset";
   procedure Context_Set (Create, Release : U32) with Import, Convention => C, External_Name => "context_mock_set";
   procedure Image_Set (Bytes : U64; Mask, Prep, Bind, Release : U32) with Import, Convention => C, External_Name => "image_mock";
   procedure Pipeline_Set (Create, Close : U32) with Import, Convention => C, External_Name => "pipeline_mock_set";
   S : State; OK : Boolean; Result : Outcome;
   Source : Vulkan_Submission.Source_Ticket; Imported : D.Source_Result; Released : System.Address;
   Address : constant System.Address := System.Storage_Elements.To_Address (55);
   Screen : constant V.A.G.Output := (32, 24, V.A.G.Unrotated, (5, 4), -20, 10);
   Items : Compositor_Text.Glyphs := (others => (65, (100, 100, 132, 117)));
   procedure Start is
   begin Begin_Frame (S, Screen, 0, OK); pragma Assert (OK); end Start;
   procedure Drop is
   begin Discard (S, Result); pragma Assert (Result = Retry and Image_Reader_Count (S) = 0); end Drop;
begin
   Reset; Context_Set (0, 0); Pipeline_Set (0, 0);
   Vulkan_Device_Mock.Set (True, True, 0); Image_Set (4096, 1, 0, 0, 0);
   D.Initialize (25); D.Configure_Targets (32, 24, 1, 16384, OK); pragma Assert (OK);
   D.Prepare_Pipeline (OK); pragma Assert (OK);
   D.Import_Owned_Source (128, Address, Source, Imported); pragma Assert (Imported = D.Source_Accepted);
   Start;
   Fill (S, (30, 20, 65535, 65535), 16#123456#, OK); pragma Assert (OK);
   pragma Assert (Layer_Count (S) = 2 and V.Item (S.Scene, 2).Kind = V.Physical_Solid and
     V.Item (S.Scene, 2).Surface = (30, 20, 32, 24));
   Fill (S, (8, 8, 2, 2), 0, OK); pragma Assert (OK and Layer_Count (S) = 2);
   Text (S, Items, 32, (0, 0, 32, 24), 16#FFFFFFFF#, OK);
   pragma Assert (OK and Layer_Count (S) = 2 and Reader_Count (S) = 0);
   Text (S, Items, 0, (0, 0, 32, 24), 16#FFFFFFFF#, OK); pragma Assert (OK and Layer_Count (S) = 2);
   Image (S, Vulkan_Submission.No_Source, (100, 100, 120, 120), (0, 0, 32, 24), OK);
   pragma Assert (OK and Image_Reader_Count (S) = 0 and Layer_Count (S) = 2);
   Image (S, Source, (-20, 10, 12, 34), (3, 5, 27, 21), OK, Over => True, Straight_Alpha => True);
   pragma Assert (OK and Layer_Count (S) = 4 and Image_Reader_Count (S) = 1 and
     V.Item (S.Scene, 3).Kind = V.Set_Physical_Clip and V.Item (S.Scene, 3).Surface = (3, 5, 27, 21) and
     V.Item (S.Scene, 4).Kind = V.Straight_Textured);
   D.Release_Source (Source, Released); pragma Assert (Released = System.Null_Address);
   Drop;
   Start; Image (S, Source, (-20, 10, 12, 34), (0, 0, 32, 24), OK, Straight_Alpha => True);
   pragma Assert (not OK); Finish (S, Result); pragma Assert (Result = Rejected and Layer_Count (S) = 0);
   Start; Fill (S, (0, 0, 32, 24), 0, OK); pragma Assert (OK);
   Image (S, Vulkan_Submission.No_Source, (-20, 10, 12, 34), (0, 0, 32, 24), OK); pragma Assert (not OK);
   Finish (S, Result); pragma Assert (Result = Rejected and Layer_Count (S) = 0);
   Start;
   for I in 1 .. V.Maximum_Layers loop Fill (S, (0, 0, 1, 1), 0, OK); end loop;
   pragma Assert (not OK); Finish (S, Result); pragma Assert (Result = Rejected);
   -- Independent inverse-pixel oracle: a physical pixel is covered when its
   -- unrotated unit square overlaps the scaled logical rectangle and damage.
   -- This does not use Damage or the adapter's forward edge transform.
   for Rotation in V.A.G.Orientation loop
      for N in V.A.G.Scale_Component loop
         for Den in V.A.G.Scale_Component loop
            for Origin in -1 .. 1 loop
               declare
                  O : constant V.A.G.Output :=
                    (32, 24, Rotation, (N, Den),
                     V.A.G.Output_Origin (Origin * 100),
                     V.A.G.Output_Origin (-Origin * 100));
                  A : constant V.A.G.Logical_Rectangle :=
                    (O.X + 1, O.Y + 2, O.X + 9, O.Y + 7);
                  Covered : Natural := 0;
               begin
                  Begin_Frame (S, O, 0, OK); pragma Assert (OK);
                  -- A preceding clip must not leak into the fill request.
                  Set_Clip (S, (0, 0, 1, 1), OK); pragma Assert (OK);
                  Logical_Fill (S, A, (3, 4, 29, 21), 16#FF123456#, OK);
                  pragma Assert (OK and Image_Reader_Count (S) = 0 and Reader_Count (S) = 0);
                  for Y in 0 .. 23 loop
                     for X in 0 .. 31 loop
                        declare
                           PX, PY : Integer;
                           Expected, Actual : Boolean := False;
                        begin
                           case Rotation is
                              when V.A.G.Unrotated => PX := X; PY := Y;
                              when V.A.G.Clockwise_90 => PX := Y; PY := 31 - X;
                              when V.A.G.Clockwise_180 => PX := 31 - X; PY := 23 - Y;
                              when V.A.G.Clockwise_270 => PX := 23 - Y; PY := X;
                           end case;
                           Expected := X >= 3 and X < 29 and Y >= 4 and Y < 21
                             and PX * Integer (Den) < 9 * Integer (N)
                             and (PX + 1) * Integer (Den) > Integer (N)
                             and PY * Integer (Den) < 7 * Integer (N)
                             and (PY + 1) * Integer (Den) > 2 * Integer (N);
                           if Layer_Count (S) = 3 then
                              declare L : constant V.Layer := V.Item (S.Scene, 3); begin
                                 pragma Assert (L.Kind = V.Physical_Solid and L.Tint = 16#FF123456#);
                                 Actual := V.A.G.Logical_Coordinate (X) >= L.Surface.Left
                                   and V.A.G.Logical_Coordinate (X) < L.Surface.Right
                                   and V.A.G.Logical_Coordinate (Y) >= L.Surface.Top
                                   and V.A.G.Logical_Coordinate (Y) < L.Surface.Bottom;
                              end;
                           end if;
                           pragma Assert (Actual = Expected);
                           if Expected then Covered := Covered + 1; end if;
                        end;
                     end loop;
                  end loop;
                  pragma Assert ((Covered = 0 and Layer_Count (S) = 1) or
                    (Covered > 0 and Layer_Count (S) = 3));
                  declare Before : constant V.Length := Layer_Count (S); begin
                     Logical_Fill (S, (O.X + 9, O.Y, O.X, O.Y + 7), (0, 0, 32, 24), 0, OK);
                     pragma Assert (OK and Layer_Count (S) = Before);
                     Logical_Fill (S, A, (9, 9, 1, 1), 0, OK);
                     pragma Assert (OK and Layer_Count (S) = Before);
                  end;
                  Drop;
               end;
            end loop;
         end loop;
      end loop;
   end loop;
   Logical_Fill (S, (0, 0, 1, 1), (0, 0, 32, 24), 0, OK);
   pragma Assert (not OK and Current (S) = Idle);
   Ada.Text_IO.Put_Line ("PASS logical fill: 3072 output configurations, 2359296 inverse-pixel checks, empty damage and capture admission");
   Start;
   Shadow (S, (-18, 12, -10, 19), (0, 0, 32, 24), 16#2468AC#, OK);
   pragma Assert (OK and Layer_Count (S) = 3 and Reader_Count (S) = 0 and Image_Reader_Count (S) = 0);
   pragma Assert (V.Item (S.Scene, 2).Kind = V.Checker_Grid and
     V.Item (S.Scene, 2).Surface = (-10, 15, -7, 22) and
     V.Item (S.Scene, 3).Surface = (-15, 19, -10, 22));
   Finish (S, Result); pragma Assert (Result = Pending);
   Discard (S, Result); pragma Assert (Result = Pending);
   Close (S, OK); pragma Assert (not OK);
   Poll (S, Result); pragma Assert (Result = Complete and Current (S) = Idle);
   Start; Shadow (S, (100, 100, 120, 120), (0, 0, 32, 24), 0, OK);
   pragma Assert (OK and Layer_Count (S) = 0); Drop;
   Start;
   for I in 1 .. V.Maximum_Layers - 1 loop
      Append (S, (Kind => V.Solid, Surface => (-20, 10, -19, 11), others => <>), OK);
      pragma Assert (OK);
   end loop;
   Shadow (S, (-18, 12, -10, 19), (0, 0, 32, 24), 0, OK); pragma Assert (not OK);
   Finish (S, Result); pragma Assert (Result = Rejected and Layer_Count (S) = 0);
   Start;
   Shadow (S, (0, 0, V.A.G.Logical_Coordinate'Last, 20), (0, 0, 32, 24), 0, OK);
   pragma Assert (not OK); Finish (S, Result); pragma Assert (Result = Rejected);
   Ada.Text_IO.Put_Line ("PASS bounded shadow capture: three layers, no readers, pending retirement, invisible elision, overflow and partial-capture rejection");
   Start;
   for I in 1 .. 100 loop
      Image_Region (S, Source, (-18, 12, -10, 19), (0, 0, 32, 24),
        (2, 3, 7, 9, 32, 24), OK, Over => True, Straight_Alpha => True);
      pragma Assert (OK and Image_Reader_Count (S) = 1 and Layer_Count (S) = I * 2);
   end loop;
   D.Release_Source (Source, Released); pragma Assert (Released = System.Null_Address);
   Finish (S, Result); pragma Assert (Result = Pending);
   Discard (S, Result); pragma Assert (Result = Pending and Image_Reader_Count (S) = 1);
   Close (S, OK); pragma Assert (not OK);
   Poll (S, Result); pragma Assert (Result = Complete and Image_Reader_Count (S) = 0);
   Start;
   Image_Region (S, Source, (100, 100, 120, 120), (0, 0, 32, 24),
     (0, 0, 0, 9, 32, 24), OK, Over => True);
   pragma Assert (not OK); Finish (S, Result); pragma Assert (Result = Rejected);
   Start;
   for I in 1 .. V.Maximum_Layers - 1 loop
      Append (S, (Kind => V.Solid, Surface => (-20, 10, -19, 11), others => <>), OK);
      pragma Assert (OK);
   end loop;
   Image_Region (S, Source, (-18, 12, -10, 19), (0, 0, 32, 24),
     (2, 3, 7, 9, 32, 24), OK, Over => True);
   pragma Assert (not OK and Image_Reader_Count (S) = 1);
   Finish (S, Result); pragma Assert (Result = Rejected and Image_Reader_Count (S) = 0);
   Ada.Text_IO.Put_Line ("PASS region drawing: one atlas reader for 100 windows, held completion, invalid/offscreen rejection and partial capture cleanup");
   D.Release_Source (Source, Released); pragma Assert (Released = Address);
   Close (S, OK); pragma Assert (OK); D.Stop;
   Ada.Text_IO.Put_Line ("PASS drawing adapter: physical fills, empty/offscreen batches, independent clips, straight-alpha image pin, invalid source/blend and whole-frame overflow");
end Desktop_GPU_Scene.Drawing.Mixed_Tests;

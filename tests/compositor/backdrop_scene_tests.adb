with Ada.Text_IO;
with Interfaces;
with System.Storage_Elements;
with Vulkan_Scene;
with Compositor_Image_Sampling;
procedure Backdrop_Scene_Tests is
   package C renames Vulkan_Scene;
   package V renames C.V;
   package D renames C.D;
   package G renames C.A.G;
   package I renames Compositor_Image_Sampling;
   use type G.Logical_Coordinate, C.Phase, C.Layer_Kind, V.Source_Ticket, Interfaces.Unsigned_32, Interfaces.Unsigned_64;
   subtype Word is Interfaces.Unsigned_32;
   subtype Wide is Interfaces.Unsigned_64;
   procedure Reset with Import, Convention => C, External_Name => "submission_mock_reset";
   procedure Reset_Draw (Value : Word) with Import, Convention => C, External_Name => "backdrop_submission_reset";
   function Calls (Index : Word) return Word with Import, Convention => C, External_Name => "submission_mock_calls";
   function Draws return Word with Import, Convention => C, External_Name => "backdrop_submission_calls";
   function Geometry (Draw, Field : Word) return Wide with Import, Convention => C, External_Name => "backdrop_submission_geometry";
   Context : constant System.Address := System.Storage_Elements.To_Address (1);
   Screen : G.Output := (Width => 32, Height => 24, others => <>);
   Scene : C.State;
   Submission : V.State;
   Damage : D.State;
   Source, Old : V.Source_Ticket;
   Released : System.Address;
   OK : Boolean;
   procedure Fresh is
   begin
      Reset; Reset_Draw (0); Scene := C.Open (Screen);
      Submission := V.Open (Context);
      V.Install_Source (Submission, 0, Context, Old);
      V.Remove_Source (Submission, Old, Released);
      V.Install_Source (Submission, 0, Context, Source);
      V.Begin_Record (Submission, OK); pragma Assert (OK);
      V.Begin_Scene (Submission, Context, 32, 24, OK); pragma Assert (OK);
      Damage := D.Open (32, 24); D.Begin_Paint (Damage, 1);
   end Fresh;
   procedure Replay is
   begin
      C.Seal (Scene, OK); pragma Assert (OK);
      C.Replay (Scene, Submission, Damage, OK);
   end Replay;
begin
   for Mode in I.Placement loop
      Fresh;
      C.Append_Backdrop (Scene, Source, 17, 5, Mode, OK); pragma Assert (OK);
      Replay;
      pragma Assert (OK and Draws = 1 and V.Draws (Submission) = 2);
      pragma Assert (Geometry (0, 8) = 17 and Geometry (0, 9) = 5 and Geometry (0, 10) = 1);
      pragma Assert (Geometry (0, 4) = 0 and Geometry (0, 5) = 0 and
        Geometry (0, 6) = 32 and Geometry (0, 7) = 24);
      case Mode is
         when I.Fill => pragma Assert (Geometry (0, 2) = 82 and Geometry (0, 3) = 24);
         when I.Fit => pragma Assert (Geometry (0, 2) = 32 and Geometry (0, 3) = 10);
         when I.Center => pragma Assert (Geometry (0, 2) = 17 and Geometry (0, 3) = 5);
      end case;
   end loop;
   Fresh;
   D.Finish (Damage, D.Completed);
   D.Change (Damage, (1, 2, 7, 8)); D.Change (Damage, (20, 16, 29, 22)); D.Begin_Paint (Damage, 1);
   C.Append (Scene, (Kind => C.Set_Clip, Surface => (3, 4, 25, 20), others => <>), OK); pragma Assert (OK);
   C.Append_Backdrop (Scene, Source, 17, 5, I.Fit, OK); pragma Assert (OK);
   Replay;
   pragma Assert (OK and Draws = 2 and V.Draws (Submission) = 4);
   pragma Assert (Geometry (0, 4) = 3 and Geometry (0, 5) = 4 and Geometry (0, 6) = 4 and Geometry (0, 7) = 4);
   pragma Assert (Geometry (1, 4) = 20 and Geometry (1, 5) = 16 and Geometry (1, 6) = 5 and Geometry (1, 7) = 4);
   pragma Assert (Geometry (0, 10) = 2 and Geometry (1, 10) = 2);
   Fresh;
   C.Append (Scene, (Kind => C.Set_Clip, Surface => (0, 0, 0, 0), others => <>), OK);
   C.Append_Backdrop (Scene, Source, 17, 5, I.Fill, OK);
   C.Append (Scene, (Kind => C.Reset_Clip, others => <>), OK);
   C.Append_Backdrop (Scene, Source, 17, 5, I.Center, OK); Replay;
   pragma Assert (OK and Draws = 1 and V.Draws (Submission) = 3 and Geometry (0, 2) = 17);
   for Rotation in G.Orientation loop
      for Scale in 4 .. 8 loop
         Screen := (32, 24, Rotation, (G.Scale_Component (Scale), 4), -20, 10);
         Fresh;
         C.Append_Physical_Clip (Scene, (3, 5, 27, 21), OK); pragma Assert (OK);
         C.Append_Backdrop (Scene, Source, 17, 5, I.Fill, OK); pragma Assert (OK); Replay;
         pragma Assert (OK and Draws = 1 and Geometry (0, 4) = 3 and Geometry (0, 5) = 5 and
           Geometry (0, 6) = 24 and Geometry (0, 7) = 16);
         Fresh;
         C.Append_Physical_Clip (Scene, (30, 20, 65535, 65535), OK); pragma Assert (OK);
         C.Append_Backdrop (Scene, Source, 17, 5, I.Fill, OK); Replay;
         pragma Assert (OK and Draws = 1 and Geometry (0, 4) = 30 and Geometry (0, 5) = 20 and
           Geometry (0, 6) = 2 and Geometry (0, 7) = 4);
         Fresh;
         C.Append_Physical_Clip (Scene, (7, 5, 3, 2), OK); pragma Assert (OK);
         C.Append_Backdrop (Scene, Source, 17, 5, I.Fill, OK);
         C.Append (Scene, (Kind => C.Reset_Clip, others => <>), OK);
         C.Append_Backdrop (Scene, Source, 17, 5, I.Fill, OK); Replay;
         pragma Assert (OK and Draws = 1 and Geometry (0, 4) = 0 and Geometry (0, 6) = 32);
      end loop;
   end loop;
   for Invalid in 1 .. 5 loop
      Fresh;
      C.Append (Scene, (Kind => C.Set_Physical_Clip,
        Surface => ((if Invalid = 1 then -1 else 0), 0, 32, 24),
        Source => (if Invalid = 2 then Source else V.No_Source),
        Over => Invalid = 3, Mask => Invalid = 4, Tint => (if Invalid = 5 then 1 else 0)), OK);
      pragma Assert (not OK and C.Current (Scene) = C.Rejected and C.Count (Scene) = 0);
   end loop;
   Fresh; C.Append_Backdrop (Scene, Old, 17, 5, I.Fill, OK); pragma Assert (OK); Replay;
   pragma Assert (not OK and V.Draws (Submission) = 0 and Calls (13) = 0 and Draws = 0);
   Fresh; C.Append_Backdrop (Scene, V.No_Source, 17, 5, I.Fill, OK);
   pragma Assert (not OK and C.Current (Scene) = C.Rejected);
   for Invalid in 1 .. 4 loop
      Fresh;
      C.Append (Scene, (Source => Source, Kind => C.Backdrop_Fill,
        Surface => (0, 0, (if Invalid = 1 then 0 else 17), 5),
        Over => Invalid = 2, Mask => Invalid = 3, Tint => (if Invalid = 4 then 1 else 0)), OK);
      pragma Assert (not OK and C.Current (Scene) = C.Rejected and C.Count (Scene) = 0);
   end loop;
   Fresh;
   for N in 1 .. C.Maximum_Layers loop
      C.Append_Backdrop (Scene, Source, 17, 5, I.Fit, OK); pragma Assert (OK);
   end loop;
   C.Append_Backdrop (Scene, Source, 17, 5, I.Fit, OK);
   pragma Assert (not OK and C.Current (Scene) = C.Rejected and C.Count (Scene) = C.Maximum_Layers);
   Fresh; C.Append_Backdrop (Scene, Source, 17, 5, I.Fill, OK); Reset_Draw (1); Replay;
   pragma Assert (not OK and Draws = 1 and not V.Complete_Frame (Submission));
   Fresh; D.Finish (Damage, D.Completed); Damage := D.Open (31, 24); D.Begin_Paint (Damage, 1);
   C.Append_Backdrop (Scene, Source, 17, 5, I.Fill, OK); Replay;
   pragma Assert (not OK and Draws = 0 and Calls (13) = 0);
   Ada.Text_IO.Put_Line ("BACKDROP SCENE: PASS 60 physical clip/DPI/rotation cases and 5 malformed clips; modes, sparse damage/clip/reset, background ordering, stale preflight, malformed layers, cap and faults");
end Backdrop_Scene_Tests;

with Vulkan_Glyph_Sources;
with Compositor_Glyph_Placement;
with Compositor_Text;
with Ada.Text_IO;
with Ada.Assertions;
with Interfaces;
with System.Storage_Elements;
with Vulkan_Submission;
with Vulkan_Frame;
with Vulkan_Scene;
with Vulkan_Target_Owner;
with Compositor_Pool;
with Compositor_Target_Damage;
with Compositor_Damage;
with Vulkan_Affine_Binding;
procedure Vulkan_Submission_Mock_Tests is
   package V renames Vulkan_Submission;
   package B renames Vulkan_Affine_Binding;
   package F renames Vulkan_Frame;
   package P renames Compositor_Pool;
   package O renames Vulkan_Target_Owner;
   use type O.Phase;
   use type F.Admission, F.Completion, P.Ticket;
   Pool : P.State;
   Frame_Result : F.Admission;
   Completed : F.Completion;
   Frame, Offered, Front : P.Ticket;
   Bindings : constant F.Targets :=
      (System.Storage_Elements.To_Address (101), System.Storage_Elements.To_Address (102), System.Storage_Elements.To_Address (103));
   function Geometry (Index : Interfaces.Unsigned_32) return Interfaces.Unsigned_32 with Import, Convention => C,
     External_Name => "submission_mock_geometry";
   function Last_Pass return System.Address with Import, Convention => C, External_Name => "submission_mock_last_pass";
   use type System.Storage_Elements.Integer_Address, V.Source_Slot, V.Source_Ticket, System.Address, V.Phase, V.Observation, B.Outcome, Interfaces.Unsigned_32;
   subtype Word is Interfaces.Unsigned_32;
   type Codes is array (Positive range <>) of Word;
   Import_Faults : constant Codes := [1, 2, Word'Last, 42];
   Release_Faults : constant Codes := [1, 2, Word'Last];
   Frame_Faults : constant Codes := [0, 7, 8, 1, 2, 3, 4];
   Target_Faults : constant Codes := [1, 2, Word'Last, 42, 43];
   procedure Reset with Import, Convention => C, External_Name => "submission_mock_reset";
   procedure Set (Index, Value : Word) with Import, Convention => C, External_Name => "submission_mock_set";
   function Calls (Index : Word) return Word with Import, Convention => C, External_Name => "submission_mock_calls";
   Context : constant System.Address := System.Storage_Elements.To_Address (1);
   S : V.State;
   Source, Other, Old_Source : V.Source_Ticket;
   Released : System.Address;
   OK : Boolean;
   procedure Fresh is
   begin
      S := V.Open (Context); pragma Assert (V.Can_Destroy (S));
      V.Install_Source (S, 0, Context, Source);
      pragma Assert (Source /= V.No_Source and V.Source_Valid (S, Source) and not V.Can_Destroy (S));
   end Fresh;
   procedure Retained is
      Before : constant V.State := S;
      Ignored : V.Source_Ticket;
      Returned : System.Address;
   begin
      pragma Assert (not V.Quiescent (S));
      V.Remove_Source (S, Source, Returned);
      pragma Assert (Returned = System.Null_Address and V.Same_Sources (S, Before));
      V.Install_Source (S, 1, System.Storage_Elements.To_Address (2), Ignored);
      pragma Assert (Ignored = V.No_Source and V.Same_Sources (S, Before));
      pragma Assert (V.Source_Valid (S, Source));
   end Retained;
   R : V.Observation;
   Drawn : B.Outcome;
   Screen : constant B.G.Output := (Width => 8, Height => 6, others => <>);
   procedure Begin_Scene is
   begin
      V.Begin_Scene (S, Context, 8, 6, OK);
      pragma Assert (OK and V.Pass_Active (S) and not V.Quiescent (S));
   end Begin_Scene;
   procedure End_Scene is
   begin
      V.End_Scene (S, OK);
      pragma Assert (OK and V.Pass_Finished (S) and not V.Pass_Active (S) and not V.Quiescent (S));
   end End_Scene;
   procedure Draw (Empty : Boolean := False) is
   begin
      V.Draw_Output (S, Source, Screen, (0, 0, 8, 6),
        (0, 0, (if Empty then 0 else 8), 6), False, False, 0, Drawn);
   end Draw;
   procedure Check_Gradients is
      package C renames Vulkan_Scene;
      use type C.Phase, C.Layer_Kind, C.Layer, B.G.Logical_Coordinate;
      Scene : C.State;
      Accepted : Boolean;
      Band : C.Layer;
      Next_Row, Row, Alpha, Expected, Actual : Natural;
      Top : constant Word := 16#0012_3456#;
      Bottom : constant Word := 16#00FE_DCBA#;
      Heights : constant array (Positive range <>) of Positive := (1, 2, 255, 256, 257, 2160, 4096);
      Prefix : constant C.Layer := (Surface => (0, 0, 1, 1), Kind => C.Solid, others => <>);
   begin
      for Height of Heights loop
         Scene := C.Open (Screen);
         C.Append_Gradient (Scene, (-3, -100, 20, B.G.Logical_Coordinate (Height - 100)), Top, Bottom, Accepted);
         pragma Assert (Accepted and C.Count (Scene) <= 256);
         Next_Row := 0;
         for I in 1 .. C.Count (Scene) loop
            Band := C.Item (Scene, I);
            pragma Assert (Band.Kind = C.Solid and Band.Source = V.No_Source and not Band.Over and not Band.Mask);
            pragma Assert (Band.Surface.Top = B.G.Logical_Coordinate (Next_Row) - 100);
            pragma Assert (Band.Surface.Left = -3 and Band.Surface.Right = 20 and Band.Surface.Bottom > Band.Surface.Top);
            for Y in Integer (Band.Surface.Top) .. Integer (Band.Surface.Bottom) - 1 loop
               Row := Natural (Y + 100);
               Alpha := (if Height = 1 then 0 else Row * 255 / (Height - 1));
               for K in 0 .. 2 loop
                  declare
                     A : constant Integer := Integer (Interfaces.Shift_Right (Top, K * 8) and 255);
                     Z : constant Integer := Integer (Interfaces.Shift_Right (Bottom, K * 8) and 255);
                  begin
                     Expected := (A * 255 + (Z - A) * Alpha + 127) / 255;
                     Actual := Natural (Interfaces.Shift_Right (Band.Tint, K * 8) and 255);
                     pragma Assert (Actual = Expected);
                  end;
               end loop;
            end loop;
            Next_Row := Natural (Band.Surface.Bottom + 100);
         end loop;
         pragma Assert (Next_Row = Height);
      end loop;
      -- Equal final RGB bands consume less bounded scene capacity.
      Scene := C.Open (Screen);
      for I in 1 .. C.Maximum_Layers - 1 loop C.Append (Scene, Prefix, Accepted); end loop;
      C.Append_Gradient (Scene, (0, 0, 4, 2160), 0, 0, Accepted);
      pragma Assert (Accepted and C.Count (Scene) = C.Maximum_Layers);
      pragma Assert (C.Item (Scene, C.Maximum_Layers).Surface.Bottom = 2160);
      Scene := C.Open (Screen);
      for I in 1 .. C.Maximum_Layers - 17 loop C.Append (Scene, Prefix, Accepted); end loop;
      C.Append_Gradient (Scene, (0, 0, 4, 2160), 16#0020_2020#, 16#0030_3030#, Accepted);
      pragma Assert (Accepted and C.Count (Scene) = C.Maximum_Layers);
      Scene := C.Open (Screen);
      for I in 1 .. C.Maximum_Layers - 1 loop C.Append (Scene, Prefix, Accepted); end loop;
      C.Append_Gradient (Scene, (0, 0, 4, 2), Top, Bottom, Accepted);
      pragma Assert (not Accepted and C.Current (Scene) = C.Rejected and C.Count (Scene) = C.Maximum_Layers);
      for I in 1 .. C.Maximum_Layers - 1 loop pragma Assert (C.Item (Scene, I) = Prefix); end loop;
      Scene := C.Open (Screen);
      C.Append_Gradient (Scene, (0, -2 ** 30, 4, 2 ** 30), Top, Bottom, Accepted);
      pragma Assert (not Accepted and C.Current (Scene) = C.Rejected and C.Count (Scene) = 0);
      Scene := C.Open (Screen);
      C.Append_Gradient (Scene, (0, 0, 0, 10), Top, Bottom, Accepted);
      pragma Assert (Accepted and C.Count (Scene) = 0);
      C.Seal (Scene, Accepted);
      C.Append_Gradient (Scene, (0, 0, 1, 1), Top, Bottom, Accepted);
      pragma Assert (not Accepted and C.Current (Scene) = C.Rejected);
      Ada.Text_IO.Put_Line ("VULKAN-GRADIENT: PASS exact bands/rows, clipping origin, capacity prefix, extreme and sealed rejection");
   end Check_Gradients;
   procedure Check_Fills is
      Area : B.G.Physical_Rectangle;
      Accepted : Boolean;
   begin
      for Case_Id in 1 .. 7 loop
         Reset; Fresh; V.Begin_Record (S, Accepted); Begin_Scene;
         Area := (0, 0, 8, 6);
         case Case_Id is
            when 2 => Area.Right := 0;
            when 3 => Area.Right := 9;
            when 4 => Set (13, 2);
            when 5 =>
               for I in 1 .. V.Maximum_Draws loop V.Admit_Draw (S, Accepted); pragma Assert (Accepted); end loop;
            when 6 => V.Reject_Frame (S);
            when 7 => Area.Left := 7; Area.Right := 3;
            when others => null;
         end case;
         V.Fill_Output (S, Area, 16#204060#, Accepted);
         pragma Assert (Accepted = (Case_Id in 1 | 2 | 7));
         pragma Assert (Calls (13) = (if Case_Id in 1 | 4 then 1 else 0));
         pragma Assert (V.Draws (S) = (if Case_Id = 5 then V.Maximum_Draws elsif Case_Id = 6 then 0 else 1));
         pragma Assert (V.Source_Valid (S, Source));
         if not Accepted then
            declare Before : constant Word := Calls (13); begin
               V.Fill_Output (S, (0, 0, 8, 6), 0, Accepted);
               pragma Assert (not Accepted and Calls (13) = Before);
            end;
         end if;
         V.Cancel (S, Accepted); pragma Assert (Accepted);
      end loop;
      Ada.Text_IO.Put_Line ("VULKAN-FILLS: PASS opaque/empty/inverted/out-of-bounds, foreign failure, exhausted budget and no replay after rejection");
   end Check_Fills;
   procedure Check_Solid_Layers is
      use type B.G.Logical_Coordinate;
      package C renames Vulkan_Scene;
      package T renames Compositor_Target_Damage;
      Scene : C.State;
      Damage : T.State;
      Value : C.Layer;
      Accepted : Boolean;
   begin
      for Case_Id in 1 .. 7 loop
         Reset; Fresh;
         Damage := T.Open (8, 6); T.Begin_Paint (Damage, 1);
         Scene := C.Open (Screen);
         Value := (V.No_Source, (1, 1, 7, 5), False, False, 16#305080#, C.Solid);
         case Case_Id is
            when 2 => Value.Over := True;
            when 3 => Value.Mask := True;
            when 4 => Value.Source := Source;
            when 5 => Value.Surface := (-8, -6, -1, -1);
            when 6 => Set (13, 2);
            when others => null;
         end case;
         C.Append (Scene, Value, Accepted);
         pragma Assert (Accepted = (Case_Id not in 2 .. 4));
         C.Seal (Scene, Accepted);
         pragma Assert (Accepted = (Case_Id not in 2 .. 4));
         V.Begin_Record (S, Accepted); Begin_Scene;
         if Case_Id = 7 then
            for I in 1 .. V.Maximum_Draws - 1 loop V.Admit_Draw (S, Accepted); pragma Assert (Accepted); end loop;
         end if;
         C.Replay (Scene, S, Damage, Accepted);
         pragma Assert (Accepted = (Case_Id in 1 | 5));
         pragma Assert (Calls (6) = 0 and Calls (13) =
           (if Case_Id = 1 then 2 elsif Case_Id in 5 .. 7 then 1 else 0));
         pragma Assert (V.Source_Valid (S, Source));
         V.Cancel (S, Accepted); pragma Assert (Accepted);
      end loop;
      Ada.Text_IO.Put_Line ("VULKAN-SOLIDS: PASS source-free ordered fill, invalid blend/mask/source rejection, empty clips, foreign failure and shared-budget exhaustion");
   end Check_Solid_Layers;
   procedure Check_Physical_Fills is
      package C renames Vulkan_Scene;
      package T renames Compositor_Target_Damage;
      package G renames B.G;
      use type G.Logical_Coordinate;
      Scene : C.State; Damage : T.State; Accepted : Boolean;
      Display : G.Output := Screen;
      Scales : constant array (1 .. 6) of G.UI_Scale :=
        ((1, 1), (5, 4), (3, 2), (7, 4), (2, 1), (3, 1));
   begin
      Display.X := -3; Display.Y := 7;
      for Scale of Scales loop
         Display.Scale := Scale;
         for Rotation in G.Orientation loop
            Display.Rotation := Rotation;
            for Case_Id in 1 .. 3 loop
               Reset; Fresh; Scene := C.Open (Display);
               Damage := T.Open (8, 6); T.Begin_Paint (Damage, 1);
               C.Append_Physical_Fill (Scene,
                 (if Case_Id = 1 then (1, 1, 7, 5)
                  elsif Case_Id = 2 then (6, 4, G.Pixel_Edge'Last, G.Pixel_Edge'Last)
                  else (8, 6, 8, 6)), 16#305080#, Accepted);
               pragma Assert (Accepted);
               C.Seal (Scene, Accepted); pragma Assert (Accepted);
               V.Begin_Record (S, Accepted); Begin_Scene;
               C.Replay (Scene, S, Damage, Accepted); pragma Assert (Accepted);
               pragma Assert (Calls (13) = (if Case_Id = 3 then 1 else 2));
               if Case_Id /= 3 then
                  pragma Assert (Geometry (0) = (if Case_Id = 1 then 1 else 6));
                  pragma Assert (Geometry (1) = (if Case_Id = 1 then 1 else 4));
                  pragma Assert (Geometry (2) = (if Case_Id = 1 then 7 else 8));
                  pragma Assert (Geometry (3) = (if Case_Id = 1 then 5 else 6));
                  pragma Assert (Geometry (10) = 16#305080#);
               end if;
               V.Cancel (S, Accepted); pragma Assert (Accepted);
            end loop;
         end loop;
      end loop;
      for Invalid in 1 .. 2 loop
         Scene := C.Open (Screen);
         C.Append (Scene, (V.No_Source,
           (if Invalid = 1 then (-1, 0, 1, 1) else (0, 0, 65536, 1)),
           False, False, 0, C.Physical_Solid), Accepted);
         pragma Assert (not Accepted and C.Count (Scene) = 0);
      end loop;
      Ada.Text_IO.Put_Line ("VULKAN-PHYSICAL-FILLS: PASS 72 scale/rotation/extent/empty cases and malformed coordinate rejection; no second transform");
   end Check_Physical_Fills;
   procedure Check_Clips is
      package C renames Vulkan_Scene;
      package T renames Compositor_Target_Damage;
      Scene : C.State;
      Damage : T.State;
      Command : C.Layer;
      Accepted : Boolean;
      L, U, R, Dn : Word;
   begin
      for Case_Id in 1 .. 10 loop
         Reset; Fresh; Scene := C.Open (Screen);
         Damage := T.Open (8, 6); T.Begin_Paint (Damage, 1);
         Command := (V.No_Source, (2, 1, 6, 5), False, False, 0, C.Set_Clip);
         case Case_Id is
            when 2 => Command.Surface := (0, 0, 0, 0);
            when 3 => Command.Surface := (6, 5, 2, 1);
            when 4 => Command.Surface := (9, 7, 12, 9);
            when 6 => Command.Source := Source;
            when 7 => Command.Mask := True;
            when 8 => Command.Tint := 1;
            when others => null;
         end case;
         C.Append (Scene, Command, Accepted);
         pragma Assert (Accepted = (Case_Id not in 6 .. 8));
         if Case_Id = 5 then
            C.Append (Scene, (V.No_Source, (0, 0, 0, 0), False, False, 0, C.Reset_Clip), Accepted);
            pragma Assert (Accepted);
         elsif Case_Id = 9 then
            C.Append (Scene, (V.No_Source, (3, 2, 5, 4), False, False, 0, C.Set_Clip), Accepted);
            pragma Assert (Accepted);
         end if;
         C.Append (Scene, (Source, (0, 0, 8, 6), True, False, 0, C.Textured), Accepted);
         if Case_Id = 10 then
            C.Append_Gradient (Scene, (0, 0, 8, 6), 0, 16#FFFFFF#, Accepted);
         else
            C.Append (Scene, (V.No_Source, (0, 0, 8, 6), False, False, 0, C.Solid), Accepted);
         end if;
         C.Seal (Scene, Accepted);
         V.Begin_Record (S, Accepted); Begin_Scene;
         C.Replay (Scene, S, Damage, Accepted);
         pragma Assert (Accepted = (Case_Id not in 6 .. 8));
         if Case_Id in 1 | 5 | 9 | 10 then
            L := (if Case_Id = 5 then 0 elsif Case_Id = 9 then 3 else 2);
            U := (if Case_Id = 5 then 0 elsif Case_Id = 9 then 2 else 1);
            R := (if Case_Id = 5 then 8 elsif Case_Id = 9 then 5 else 6);
            Dn := (if Case_Id = 5 then 6 elsif Case_Id = 9 then 4 else 5);
            pragma Assert (Calls (6) = 1);
            pragma Assert (Geometry (4) = L and Geometry (5) = U and
              Geometry (6) = R - L and Geometry (7) = Dn - U);
            pragma Assert (Geometry (8) = 8 and Geometry (9) = 6);
            pragma Assert (Geometry (0) = L and Geometry (2) = R and Geometry (3) = Dn);
            pragma Assert (Geometry (1) = (if Case_Id = 10 then 4 else U));
            if Case_Id = 10 then pragma Assert (Geometry (10) = 16#CCCCCC#); end if;
         else
            pragma Assert (Calls (6) = 0);
            pragma Assert (Calls (13) = (if Case_Id in 2 .. 4 then 1 else 0));
         end if;
         pragma Assert (V.Source_Valid (S, Source));
         V.Cancel (S, Accepted); pragma Assert (Accepted);
      end loop;
      Ada.Text_IO.Put_Line ("VULKAN-CLIPS: PASS clipped texture/solid/gradient, original sampling, empty/inverted/offscreen, reset/replacement, invalid metadata");
   end Check_Clips;
   procedure Check_Glyph_Scenes is
      package C renames Vulkan_Scene;
      package T renames Compositor_Target_Damage;
      package G renames B.G;
      package A renames B.A;
      package GP renames Compositor_Glyph_Placement;
      use type A.Signed, G.Logical_Coordinate;
      function Origin (Index : Word) return Interfaces.Integer_64 with Import,
        Convention => C, External_Name => "submission_mock_origin";
      Scene : C.State;
      Damage : T.State;
      Display : G.Output := (80, 72, G.Unrotated, (1, 1), -3, 7);
      Scales : constant array (1 .. 6) of G.UI_Scale :=
        ((1, 1), (5, 4), (3, 2), (7, 4), (2, 1), (3, 1));
      Cell, Clip : G.Logical_Rectangle;
      Command : C.Layer;
      Full, Expected : A.Result;
      Area : G.Physical_Rectangle;
      Accepted : Boolean;
   begin
      for Scale of Scales loop
         Display.Scale := Scale;
         for Rotation in G.Orientation loop
            Display.Rotation := Rotation;
            for Case_Id in 1 .. 10 loop
               Reset; Fresh; Scene := C.Open (Display);
               Damage := T.Open (80, 72); T.Begin_Paint (Damage, 1);
               Cell := (3, 11, 10, 28); Clip := (-100, -100, 100, 100);
               if Case_Id = 2 then Clip := (4, 12, 9, 26);
               elsif Case_Id = 3 then Clip := (0, 0, 0, 0);
               elsif Case_Id = 9 then Cell.Right := Cell.Left;
               elsif Case_Id = 10 then Cell := (-5, 6, 4, 23);
               end if;
               C.Append (Scene, (V.No_Source, Clip, False, False, 0, C.Set_Clip), Accepted);
               pragma Assert (Accepted);
               Command := (Source, Cell, True, True, 16#FF2468AC#, C.Glyph_Mask);
               if Case_Id = 4 then Command.Mask := False;
               elsif Case_Id = 5 then Command.Over := False;
               elsif Case_Id = 6 then Command.Source := V.No_Source;
               end if;
               C.Append (Scene, Command, Accepted);
               pragma Assert (Accepted = (Case_Id not in 4 .. 6));
               C.Seal (Scene, Accepted);
               if Case_Id = 7 then
                  Old_Source := Source;
                  V.Remove_Source (S, Source, Released);
                  V.Install_Source (S, 0, Context, Source);
                  pragma Assert (not V.Source_Valid (S, Old_Source));
               elsif Case_Id = 8 then Set (6, 1);
               end if;
               V.Begin_Record (S, Accepted); pragma Assert (Accepted);
               V.Begin_Scene (S, Context, 80, 72, Accepted); pragma Assert (Accepted);
               C.Replay (Scene, S, Damage, Accepted);
               pragma Assert (Accepted = (Case_Id not in 4 .. 8));
               Full := GP.Plan (Display, (Cell.Left, Cell.Top));
               Area := Compositor_Text.Clip (Display, Cell, G.Damage (Display, Clip));
               Expected := (if Full.Visible then A.Clip (Full.Value, 80, 72, Area)
                            else (Visible => False));
               if Case_Id in 4 .. 7 then
                  pragma Assert (Calls (6) = 0 and Calls (13) = 0);
               elsif Expected.Visible then
                  pragma Assert (Calls (6) = 1);
                  pragma Assert (Geometry (4) = Expected.Value.Clip_X and
                    Geometry (5) = Expected.Value.Clip_Y and Geometry (6) = Expected.Value.Clip_W and
                    Geometry (7) = Expected.Value.Clip_H);
                  pragma Assert (Geometry (8) = Expected.Value.Logical_W and
                    Geometry (9) = Expected.Value.Logical_H);
                  pragma Assert (Geometry (11) = 1 and Geometry (12) = 1 and
                    Geometry (13) = 1 and Geometry (14) = Command.Tint and
                    Geometry (15) = Word (G.Orientation'Pos (Rotation)) and Geometry (16) = 1);
                  pragma Assert (Origin (0) = -GP.Snap (Cell.Left, Display.X, Scale) and
                    Origin (1) = -GP.Snap (Cell.Top, Display.Y, Scale));
               else pragma Assert (Calls (6) = 0);
               end if;
               pragma Assert (V.Source_Valid (S, Source));
               Retained;
               V.Cancel (S, Accepted); pragma Assert (Accepted);
            end loop;
         end loop;
      end loop;
      Ada.Text_IO.Put_Line ("VULKAN-GLYPHS: PASS 240 density/rotation/cell/clip cases, unit raster sampling, retained tickets, stale-source/foreign/metadata faults");
   end Check_Glyph_Scenes;
   procedure Check_Glyph_Associations is
      package G renames Vulkan_Glyph_Sources;
      package C renames Vulkan_Scene;
      use type G.State;
      Map, Before, Empty : G.State;
      K : G.Key := (0, 65, (5, 4));
      K2 : constant G.Key := (1, 66, (5, 4));
      Raster : G.L.Layout;
      Accepted : Boolean;
      Scene : C.State;
      Display : B.G.Output := (80, 72, B.G.Unrotated, (5, 4), 0, 0);
   begin
      Reset; Fresh;
      pragma Assert (G.Resolve (Map, S, K) = V.No_Source);
      G.Bind (Map, S, 1, K, V.No_Source, G.L.Plan (K.Scale), Accepted);
      pragma Assert (not Accepted);
      G.Bind (Map, S, 1, K, Source, G.L.Plan ((3, 2)), Accepted);
      pragma Assert (not Accepted);
      Raster := G.L.Plan (K.Scale); Raster.Bytes := Raster.Bytes - 1;
      G.Bind (Map, S, 1, K, Source, Raster, Accepted); pragma Assert (not Accepted);
      G.Bind (Map, S, 1, K, Source, G.L.Plan (K.Scale), Accepted); pragma Assert (Accepted);
      Before := Map;
      pragma Assert (G.Resolve (Map, S, (0, 65, (10, 8))) = Source);
      pragma Assert (G.Resolve (Map, S, (1, 65, (5, 4))) = V.No_Source);
      pragma Assert (G.Resolve (Map, S, (0, 66, (5, 4))) = V.No_Source);
      pragma Assert (G.Resolve (Map, S, (0, 65, (3, 2))) = V.No_Source);
      G.Bind (Map, S, 2, K2, Source, G.L.Plan (K2.Scale), Accepted);
      pragma Assert (not Accepted and Map = Before);
      V.Install_Source (S, 1, System.Storage_Elements.To_Address (2), Other);
      G.Bind (Map, S, 1, K2, Other, G.L.Plan (K2.Scale), Accepted);
      pragma Assert (not Accepted and Map = Before);
      G.Bind (Map, S, 2, K, Other, G.L.Plan (K.Scale), Accepted);
      pragma Assert (not Accepted and Map = Before);
      G.Bind (Map, S, 2, K2, Other, G.L.Plan (K2.Scale), Accepted);
      pragma Assert (Accepted and G.Resolve (Map, S, K2) = Other);
      Before := Map;
      G.Forget (Map, S, 1, Accepted); pragma Assert (not Accepted and Map = Before);
      Scene := C.Open (Display);
      C.Append_Glyph (Scene, S, Map, K, (0, 0, 8, 17), 16#FFFFFFFF#, Accepted);
      pragma Assert (Accepted and C.Count (Scene) = 1);
      Display.Scale := (3, 2); Scene := C.Open (Display);
      C.Append_Glyph (Scene, S, Map, K, (0, 0, 8, 17), 16#FFFFFFFF#, Accepted);
      pragma Assert (not Accepted and C.Count (Scene) = 0);
      V.Begin_Record (S, Accepted); Begin_Scene;
      V.End_Scene (S, Accepted); pragma Assert (Accepted);
      for Phase in 1 .. 3 loop
         G.Bind (Map, S, 3, K, Source, G.L.Plan (K.Scale), Accepted);
         pragma Assert (not Accepted and Map = Before);
         G.Forget (Map, S, 3, Accepted); pragma Assert (not Accepted and Map = Before);
         pragma Assert (G.Resolve (Map, S, K) = Source);
         if Phase = 1 then V.Seal (S, Accepted); pragma Assert (Accepted);
         elsif Phase = 2 then V.Submit (S, Accepted); pragma Assert (Accepted);
         end if;
      end loop;
      Set (3, 1); V.Poll (S, R); pragma Assert (R = V.Still_Pending);
      G.Forget (Map, S, 1, Accepted); pragma Assert (not Accepted and Map = Before);
      Set (3, 0); V.Poll (S, R); pragma Assert (R = V.Finished);
      Old_Source := Source; V.Remove_Source (S, Source, Released);
      pragma Assert (G.Resolve (Map, S, K) = V.No_Source);
      V.Install_Source (S, 0, Context, Source);
      pragma Assert (Source /= Old_Source and G.Resolve (Map, S, K) = V.No_Source);
      Display.Scale := (5, 4); Scene := C.Open (Display);
      C.Append_Glyph (Scene, S, Map, K, (0, 0, 8, 17), 0, Accepted);
      pragma Assert (not Accepted and C.Count (Scene) = 0);
      G.Bind (Map, S, 1, K, Source, G.L.Plan (K.Scale), Accepted);
      pragma Assert (Accepted and G.Resolve (Map, S, K) = Source and G.Resolve (Map, S, K2) = Other);
      V.Remove_Source (S, Source, Released); G.Forget (Map, S, 1, Accepted);
      pragma Assert (Accepted and G.At_Slot (Map, 1) = V.No_Source);
      Reset; Fresh; Map := Empty;
      for I in G.Slot loop
         K := ((I - 1) / 95, 32 + (I - 1) mod 95, (1, 1));
         if I > 1 then V.Install_Source (S, V.Source_Slot (I - 1),
           System.Storage_Elements.To_Address (System.Storage_Elements.Integer_Address (I)), Source); end if;
         G.Bind (Map, S, I, K, Source, G.L.Plan (K.Scale), Accepted);
         pragma Assert (Accepted and G.Resolve (Map, S, K) = Source);
      end loop;
      Before := Map;
      V.Install_Source (S, V.Source_Slot'Last, System.Storage_Elements.To_Address (999), Source);
      pragma Assert (Source /= V.No_Source);
      G.Bind (Map, S, 1, (1, 126, (1, 1)), Source, G.L.Plan ((1, 1)), Accepted);
      pragma Assert (not Accepted and Map = Before);
      V.Begin_Record (S, Accepted); Set (4, 2); V.Cancel (S, Accepted);
      pragma Assert (not Accepted and V.Current (S) = V.Quarantined);
      G.Forget (Map, S, 1, Accepted); pragma Assert (not Accepted and Map = Before);
      Ada.Text_IO.Put_Line ("VULKAN-GLYPH-KEYS: PASS rational density/face/code, layout validation, duplicate/live rejection, 128 slots, stale generations, command/fence/unknown retention");
      Ada.Text_IO.Put_Line ("VULKAN-GLYPH-KEYS metadata bytes:" & Integer'Image (Map'Size / 8));
   end Check_Glyph_Associations;
   procedure Check_Scene_Snapshot is
      package C renames Vulkan_Scene;
      package T renames Compositor_Target_Damage;
      use type C.State, C.Phase;
      Scene : C.State;
      Damage : T.State;
      Value : C.Layer;
      Success : Boolean;
   begin
      for Case_Id in 1 .. 12 loop
         Reset; Fresh;
         Damage := T.Open (8, 6); T.Begin_Paint (Damage, 1);
         Scene := C.Open (Screen);
         Value := (Source, (0, 0, 8, 6), True, False, 0, C.Textured);
         if Case_Id = 4 then
            V.Install_Source (S, 1, System.Storage_Elements.To_Address (2), Other);
         end if;
         if Case_Id /= 9 then
            for I in 1 .. (if Case_Id in 1 | 2 then C.Maximum_Layers else 2) loop
               if Case_Id = 4 and I = 2 then Value.Source := Other; end if;
               C.Append (Scene, Value, Success); pragma Assert (Success);
               Value.Surface := (0, 0, 8, 6);
            end loop;
         end if;
         if Case_Id /= 7 then C.Seal (Scene, Success); pragma Assert (Success); end if;
         case Case_Id is
            when 2 | 3 =>
               C.Append (Scene, Value, Success); pragma Assert (not Success and C.Current (Scene) = C.Rejected);
            when 4 =>
               V.Remove_Source (S, Other, Released);
               V.Install_Source (S, 1, System.Storage_Elements.To_Address (2), Old_Source);
               pragma Assert (Old_Source /= Other);
            when 5 => Set (6, 2);
            when 6 => C.Seal (Scene, Success); pragma Assert (not Success);
            when 8 =>
               Scene := C.Open (Screen); Value.Source := V.No_Source;
               C.Append (Scene, Value, Success); pragma Assert (not Success);
            when 10 => Damage := T.Open (9, 6); T.Begin_Paint (Damage, 1);
            when 11 => Set (13, 2);
            when others => null;
         end case;
         V.Begin_Record (S, Success); Begin_Scene;
         if Case_Id = 12 then
            for I in 1 .. V.Maximum_Draws loop V.Admit_Draw (S, Success); pragma Assert (Success); end loop;
         end if;
         declare Before : constant C.State := Scene; begin
            C.Replay (Scene, S, Damage, Success);
            pragma Assert (Scene = Before);
         end;
         pragma Assert (Success = (Case_Id in 1 | 9));
         pragma Assert (Calls (13) = (if Case_Id in 1 | 5 | 9 | 11 then 1 else 0));
         pragma Assert (Calls (6) = (if Case_Id = 1 then Word (C.Maximum_Layers) elsif Case_Id = 5 then 1 else 0));
         if Case_Id = 2 then pragma Assert (C.Count (Scene) = C.Maximum_Layers); end if;
         V.Cancel (S, Success); pragma Assert (Success);
      end loop;
      -- Overflow while collecting must retain the complete prefix, never seal it.
      Reset; Fresh; Scene := C.Open (Screen);
      Value := (Source, (0, 0, 8, 6), False, False, 0, C.Textured);
      for I in 1 .. C.Maximum_Layers loop C.Append (Scene, Value, Success); pragma Assert (Success); end loop;
      C.Append (Scene, Value, Success); pragma Assert (not Success and C.Count (Scene) = C.Maximum_Layers);
      C.Seal (Scene, Success); pragma Assert (not Success);
      Ada.Text_IO.Put_Line ("VULKAN-SCENE: PASS 512-layer capacity, immutable snapshot, stale-generation preflight, late edits, overflow, invalid/unsealed/empty scenes and foreign failure");
      Ada.Text_IO.Put_Line ("VULKAN-SCENE state bytes:" & Integer'Image (C.State'Size / 8));
   end Check_Scene_Snapshot;
   procedure Check_Layer_Replay is
      package T renames Compositor_Target_Damage;
      package D renames Compositor_Damage;
      use type T.State;
      Damage : T.State;
      Output : B.G.Output;
      Surface : B.G.Logical_Rectangle;
      Ticket : V.Source_Ticket;
      Success : Boolean;
      Before_Calls : Word;
   begin
      for Case_Id in 1 .. 8 loop
         Reset; Fresh; V.Begin_Record (S, Success); Begin_Scene;
         Damage := T.Open (8, 6);
         T.Begin_Paint (Damage, 1); T.Finish (Damage, T.Completed);
         if Case_Id /= 7 then
            for I in 0 .. 7 loop
               declare Col : constant Natural := (I mod 4) * 2;
                  Row : constant Natural := (I / 4) * 2;
               begin T.Change (Damage, (Col, Row, Col + 1, Row + 1)); end;
            end loop;
         end if;
         T.Begin_Paint (Damage, 1);
         Output := Screen; Surface := (0, 0, 8, 6); Ticket := Source;
         case Case_Id is
            when 2 => Output.Width := 9;
            when 3 => Ticket := V.No_Source;
            when 4 =>
               for I in 1 .. V.Maximum_Draws - 4 loop
                  V.Admit_Draw (S, Success); pragma Assert (Success);
               end loop;
            when 5 => Set (6, 2);
            when 6 => Surface := (0, 0, 0, 0);
            when 8 => V.Reject_Frame (S);
            when others => null;
         end case;
         declare Before : constant T.State := Damage; begin
            F.Replay_Layer (S, Damage, Ticket, Output, Surface, True, False, 0, Success);
            pragma Assert (Damage = Before);
         end;
         pragma Assert (Success = (Case_Id in 1 | 6 | 7));
         pragma Assert (V.Source_Valid (S, Source));
         case Case_Id is
            when 1 => pragma Assert (V.Draws (S) = 8 and Calls (6) = 8);
            when 2 | 7 | 8 => pragma Assert (V.Draws (S) = 0 and Calls (6) = 0);
            when 3 => pragma Assert (V.Draws (S) = 1 and Calls (6) = 0);
            when 4 => pragma Assert (V.Draws (S) = V.Maximum_Draws and Calls (6) = 4);
            when 5 => pragma Assert (V.Draws (S) = 1 and Calls (6) = 1);
            when 6 => pragma Assert (V.Draws (S) = 8 and Calls (6) = 0);
            when others => null;
         end case;
         if not Success then
            Before_Calls := Calls (6);
            F.Replay_Layer (S, Damage, Source, Screen, (0, 0, 8, 6), True, False, 0, Success);
            pragma Assert (not Success and Calls (6) = Before_Calls);
         end if;
         V.Cancel (S, Success); pragma Assert (Success);
         T.Finish (Damage, T.Cancelled);
         pragma Assert (D.Count (T.Pending (Damage, 1)) = (if Case_Id = 7 then 0 else 8));
      end loop;
      Ada.Text_IO.Put_Line ("VULKAN-LAYERS: PASS eight-region replay, output/source rejection, frame-budget exhaustion, foreign failure, empty clips/plans and no replay after rejection");
   end Check_Layer_Replay;
   procedure Check_Repaint_History is
      package T renames Compositor_Target_Damage;
      package D renames Compositor_Damage;
      use type D.State, T.State, P.Slot;
      subtype X is Natural range 0 .. 19;
      subtype Y is Natural range 0 .. 11;
      type Pixels is array (X, Y) of Natural;
      type Targets is array (P.Live_Slot) of Pixels;
      Scene : Pixels := (others => (others => 1));
      Images : Targets := (others => (others => (others => 0)));
      Damage : T.State := T.Open (20, 12);
      function Covered (Regions : D.State; Col : X; Row : Y) return Boolean is
        (D.Covers (Regions, (Col, Row, Col + 1, Row + 1)));
      procedure Check is
      begin
         pragma Assert (T.Valid (Damage));
         for Slot in P.Live_Slot loop
            for Col in X loop
               for Row in Y loop
                  pragma Assert (Images (Slot) (Col, Row) = Scene (Col, Row) or else
                    Covered (T.Pending (Damage, Slot), Col, Row));
               end loop;
            end loop;
         end loop;
      end Check;
      procedure Change (Col : X; Row : Y) is
      begin
         Scene (Col, Row) := Scene (Col, Row) + 1;
         T.Change (Damage, (Col, Row, Col + 1, Row + 1));
         Check;
      end Change;
   begin
      Check;
      for Cycle in 1 .. 600 loop
         -- More than eight disjoint changes exercise conservative saturation.
         for I in 0 .. 10 loop
            Change ((2 * I + Cycle) mod 20, (2 * I + Cycle / 20) mod 12);
         end loop;
         declare
            Slot : constant P.Live_Slot := P.Live_Slot (1 + Cycle mod 3);
            Snapshot : constant Pixels := Scene;
         begin
            T.Begin_Paint (Damage, Slot);
            declare Plan : constant D.State := T.Painting (Damage); begin
               Change ((Cycle + 7) mod 20, (Cycle + 3) mod 12);
               pragma Assert (T.Painting (Damage) = Plan);
               if Cycle mod 7 = 0 then
                  T.Finish (Damage, T.Cancelled);
               else
                  for Col in X loop
                     for Row in Y loop
                        if Covered (Plan, Col, Row) then
                           Images (Slot) (Col, Row) := Snapshot (Col, Row);
                        end if;
                     end loop;
                  end loop;
                  T.Finish (Damage, T.Completed);
               end if;
            end;
         end;
         Check;
      end loop;
      T.Begin_Paint (Damage, 1);
      declare Before : constant T.State := Damage; begin
         T.Finish (Damage, T.Unknown);
         pragma Assert (T.Faulted (Damage) and T.Active (Damage) = 1 and
           T.Painting (Damage) = T.Painting (Before));
         for Slot in P.Live_Slot loop
            pragma Assert (T.Pending (Damage, Slot) = T.Pending (Before, Slot));
         end loop;
      end;
      Change (19, 11);
      pragma Assert (T.Faulted (Damage));
      Ada.Text_IO.Put_Line ("VULKAN-REPAINT: PASS 600 independent pixel-history cycles, saturation, in-flight changes, cancellation and unknown retention");
   end Check_Repaint_History;
begin
   Check_Gradients;
   Check_Clips;
   Check_Glyph_Scenes;
   Check_Glyph_Associations;
   Check_Solid_Layers;
   Check_Physical_Fills;
   Check_Fills;
   Check_Scene_Snapshot;
   Check_Layer_Replay;
   Check_Repaint_History;
   for Cycle in 1 .. 1000 loop
      Reset; Fresh; V.Begin_Record (S, OK);
      pragma Assert (OK and not V.Quiescent (S));
      Begin_Scene; Retained;
      for I in 1 .. 3 loop V.Admit_Draw (S, OK); pragma Assert (OK); end loop;
      End_Scene;
      V.Seal (S, OK); pragma Assert (OK and not V.Quiescent (S));
      Retained; V.Submit (S, OK); pragma Assert (OK and not V.Quiescent (S));
      Set (3, 1);
      for I in 1 .. 10 loop
         V.Poll (S, R); Retained;
         pragma Assert (R = V.Still_Pending and V.Current (S) = V.Pending and not V.Quiescent (S));
         pragma Assert (Calls (0) = 1 and Calls (1) = 1 and Calls (2) = 1 and Calls (4) = 0);
      end loop;
      Set (3, 0); V.Poll (S, R);
      pragma Assert (R = V.Finished and V.Quiescent (S));
   end loop;
   Reset; Fresh; V.Begin_Record (S, OK);
   Begin_Scene;
   for I in 1 .. V.Maximum_Draws loop V.Admit_Draw (S, OK); pragma Assert (OK); end loop;
   V.Admit_Draw (S, OK);
   pragma Assert (not OK and V.Draws (S) = 4096 and not V.Complete_Frame (S));
   Draw; pragma Assert (Drawn = B.Rejected and Calls (5) = 0 and Calls (6) = 0);
   V.Cancel (S, OK); pragma Assert (OK and V.Quiescent (S) and Calls (2) = 0);
   for Fault in Word range 0 .. 4 loop
      for Code in Word range 1 .. 3 loop
         Reset; Fresh;
         Set (Fault, (if Code = 3 then Word'Last else Code));
         V.Begin_Record (S, OK);
         if Fault /= 0 then
            pragma Assert (OK); Begin_Scene;
            if Fault = 4 then V.Cancel (S, OK);
            else
               End_Scene;
               V.Seal (S, OK);
               if Fault /= 1 then
                  pragma Assert (OK); V.Submit (S, OK);
                  if Fault = 3 then
                     pragma Assert (OK); V.Poll (S, R);
                  end if;
               end if;
            end if;
         end if;
         if Fault = 3 and Code = 1 then
            pragma Assert (V.Current (S) = V.Pending);
         else
            pragma Assert (V.Current (S) = V.Quarantined);
         end if;
         pragma Assert (not V.Quiescent (S)); Retained;
      end loop;
   end loop;
   for Fault in Word range 5 .. 6 loop
      Reset; Fresh; V.Begin_Record (S, OK); Begin_Scene; Set (Fault, 2);
      Draw; pragma Assert (Drawn = B.Rejected and not V.Complete_Frame (S));
      pragma Assert (Calls (6) = (if Fault = 5 then 0 else 1));
      Draw; pragma Assert (V.Draws (S) = 1 and Calls (5) = 1);
      V.Cancel (S, OK); pragma Assert (OK and V.Quiescent (S) and Calls (2) = 0);
   end loop;
   Reset; Fresh; V.Begin_Record (S, OK); Begin_Scene; Draw (True);
   pragma Assert (Drawn = B.Empty and V.Complete_Frame (S) and V.Draws (S) = 1 and Calls (6) = 0);
   V.Cancel (S, OK); pragma Assert (OK);
   for Fault in Word range 7 .. 8 loop
      for Code in Word range 1 .. 3 loop
         Reset; Fresh; V.Begin_Record (S, OK);
         Set (Fault, (if Code = 3 then Word'Last else Code));
         V.Begin_Scene (S, Context, 8, 6, OK);
         if Fault = 8 then pragma Assert (OK); V.End_Scene (S, OK); end if;
         pragma Assert (not OK and V.Current (S) = V.Quarantined and not V.Quiescent (S));
      end loop;
   end loop;
   for Bad in 1 .. 7 loop
      Reset; Fresh; V.Begin_Record (S, OK);
      if Bad in 3 | 6 | 7 then Begin_Scene; end if;
      if Bad in 3 | 6 then End_Scene; end if;
      if Bad = 6 then V.Seal (S, OK); V.Submit (S, OK); end if;
      declare
         Caught : Boolean := False;
      begin
         begin
            case Bad is
               when 1 => Draw;
               when 2 | 7 => V.Seal (S, OK);
               when 3 => V.Begin_Scene (S, Context, 8, 6, OK);
               when 4 => V.End_Scene (S, OK);
               when 5 => V.Submit (S, OK);
               when others => V.Cancel (S, OK);
            end case;
         exception when Ada.Assertions.Assertion_Error => Caught := True;
         end;
         pragma Assert (Caught and not V.Quiescent (S) and Calls (6) = 0 and Calls (4) = 0);
         pragma Assert (Calls (1) = (if Bad = 6 then 1 else 0));
         pragma Assert (Calls (2) = (if Bad = 6 then 1 else 0));
         pragma Assert (Calls (7) = (if Bad in 3 | 6 | 7 then 1 else 0));
         pragma Assert (Calls (8) = (if Bad in 3 | 6 then 1 else 0));
      end;
      if Bad = 6 then V.Poll (S, R); else V.Cancel (S, OK); end if;
      pragma Assert (V.Quiescent (S));
   end loop;
   -- Duplicate registration, null input, full capacity and slot ABA.
   Reset; Fresh;
   V.Install_Source (S, 1, Context, Other); pragma Assert (Other = V.No_Source);
   V.Install_Source (S, 1, System.Null_Address, Other); pragma Assert (Other = V.No_Source);
   for I in V.Source_Slot range 1 .. V.Source_Slot'Last loop
      V.Install_Source (S, I, System.Storage_Elements.To_Address (System.Storage_Elements.Integer_Address (I) + 10), Other);
      pragma Assert (Other /= V.No_Source and V.Source_Valid (S, Other));
   end loop;
   for I in V.Source_Slot loop
      V.Install_Source (S, I, System.Storage_Elements.To_Address (1000), Other);
      pragma Assert (Other = V.No_Source and V.Source_Present (S, I));
   end loop;
   Old_Source := Source;
   V.Remove_Source (S, Source, Released); pragma Assert (Released = Context);
   V.Install_Source (S, 0, Context, Source);
   pragma Assert (Source /= Old_Source and V.Source_Valid (S, Source) and not V.Source_Valid (S, Old_Source));
   V.Remove_Source (S, Old_Source, Released); pragma Assert (Released = System.Null_Address and V.Source_Valid (S, Source));
   V.Begin_Record (S, OK); Begin_Scene;
   V.Draw_Output (S, Old_Source, Screen, (0, 0, 8, 6), (0, 0, 8, 6), False, False, 0, Drawn);
   pragma Assert (Drawn = B.Rejected and not V.Complete_Frame (S) and Calls (5) = 0 and Calls (6) = 0);
   V.Cancel (S, OK); pragma Assert (OK);
   for I in V.Source_Slot loop
      V.Remove_Source (S, V.Source_At (S, I), Released);
      pragma Assert (Released /= System.Null_Address and not V.Source_Present (S, I));
   end loop;
   pragma Assert (V.Can_Destroy (S));
   -- The managed provider is never invoked while GPU references may exist.
   for Cycle in 1 .. 100 loop
      Reset; S := V.Open (Context);
      V.Import_Source (S, 0, Context, Source);
      pragma Assert (V.Source_Valid (S, Source) and Calls (9) = 1);
      V.Remove_Source (S, Source, Released);
      pragma Assert (Released = System.Null_Address and V.Source_Valid (S, Source));
      V.Begin_Record (S, OK); Begin_Scene;
      V.Import_Source (S, 1, Context, Other);
      V.Release_Source (S, Source, Released);
      pragma Assert (Other = V.No_Source and Released = System.Null_Address and Calls (9) = 1 and Calls (10) = 0);
      Draw; End_Scene; V.Seal (S, OK); V.Submit (S, OK);
      Set (3, 1); V.Poll (S, R); V.Release_Source (S, Source, Released);
      pragma Assert (Released = System.Null_Address and Calls (10) = 0 and V.Source_Valid (S, Source));
      Set (3, 0); V.Poll (S, R); V.Release_Source (S, Source, Released);
      pragma Assert (Released = Context and Calls (10) = 1 and V.Can_Destroy (S));
   end loop;
   for Code of Import_Faults loop
      Reset; S := V.Open (Context); Set (9, Code);
      V.Import_Source (S, 0, Context, Source);
      pragma Assert (Source = V.No_Source and not V.Source_Present (S, 0));
      pragma Assert (V.Current (S) = (if Code = 1 then V.Idle else V.Quarantined));
   end loop;
   for Code of Release_Faults loop
      Reset; S := V.Open (Context); V.Import_Source (S, 0, Context, Source); Set (10, Code);
      V.Release_Source (S, Source, Released);
      pragma Assert (Released = System.Null_Address and V.Current (S) = V.Quarantined and V.Source_Valid (S, Source));
      V.Release_Source (S, Source, Released); V.Import_Source (S, 1, Context, Other);
      pragma Assert (Calls (10) = 1 and Calls (9) = 1 and Released = System.Null_Address and Other = V.No_Source);
   end loop;
   Ada.Text_IO.Put_Line ("VULKAN-PROVIDER policy: PASS 100 managed lifecycles, 4 import faults, 3 release faults, no busy calls or unknown replay");
   -- Actual frame adapter, including three-target saturation/newest-ready
   -- replacement while a visible front and pending display remain retained.
   Reset; S := V.Open (Context); Pool := P.Open (1);
   for Cycle in 1 .. 1000 loop
      F.Begin_Record (S, Pool, Frame_Result, Replace_Ready => True);
      pragma Assert (Frame_Result = F.Started);
      Frame := P.Writer (Pool);
      pragma Assert (Frame.Buffer /= P.Front (Pool).Buffer and Frame.Buffer /= P.Displayed (Pool).Buffer);
      F.Begin_Scene (S, Pool, Bindings, 8, 6, OK);
      pragma Assert (OK and Last_Pass = Bindings (Frame.Buffer));
      F.End_Scene (S, Pool, OK); pragma Assert (OK);
      F.Submit (S, Pool, OK); pragma Assert (OK);
      Set (3, 1); F.Poll (S, Pool, Completed);
      pragma Assert (Completed = F.Still_Pending and P.Writer (Pool) = Frame and not P.Writable (Pool, Frame));
      Set (3, 0); F.Poll (S, Pool, Completed);
      pragma Assert (Completed = F.Ready and P.Ready (Pool) = Frame);
      if Cycle = 1 then
         P.Present (Pool, Offered); P.Latch_Display (Pool, Offered, P.None, True);
      elsif Cycle = 2 then
         P.Present (Pool, Offered);
      else
         -- With front+pending+ready, ordinary admission leaves everything held.
         F.Begin_Record (S, Pool, Frame_Result);
         pragma Assert (Frame_Result = F.Deferred and P.Ready (Pool) = Frame);
      end if;
   end loop;
   Front := P.Front (Pool); Offered := P.Displayed (Pool);
   P.Latch_Display (Pool, Offered, Front, True);
   P.Present (Pool, Offered); Front := P.Front (Pool);
   P.Latch_Display (Pool, Offered, Front, True); P.Retire_Front (Pool, Offered, True);
   pragma Assert (not P.Faulted (Pool) and P.Front (Pool) = P.None);
   -- All command faults quarantine both lifetimes and retain the exact writer.
   for Fault of Frame_Faults loop
      Reset; S := V.Open (Context); Pool := P.Open (2); Set (Fault, 2);
      F.Begin_Record (S, Pool, Frame_Result); Frame := P.Writer (Pool);
      if Fault /= 0 then
         pragma Assert (Frame_Result = F.Started);
         F.Begin_Scene (S, Pool, Bindings, 8, 6, OK);
         if Fault /= 7 then
            pragma Assert (OK);
            if Fault = 4 then F.Cancel (S, Pool, OK);
            else
               F.End_Scene (S, Pool, OK);
               if Fault /= 8 then
                  pragma Assert (OK); F.Submit (S, Pool, OK);
                  if Fault = 3 then pragma Assert (OK); F.Poll (S, Pool, Completed); end if;
               end if;
            end if;
         end if;
      end if;
      pragma Assert (P.Faulted (Pool) and P.Writer (Pool) = Frame and Frame /= P.None);
      pragma Assert (V.Current (S) = V.Quarantined and P.Ready (Pool) = P.None);
   end loop;
   Reset; S := V.Open (Context); Pool := P.Open (3);
   F.Begin_Record (S, Pool, Frame_Result); F.Cancel (S, Pool, OK);
   pragma Assert (OK and P.Writer (Pool) = P.None and P.Ready (Pool) = P.None);
   -- A display protocol fault while GPU work exists must prevent reuse even
   -- if the GPU later completes or command cancellation succeeds.
   for Pending_GPU in Boolean loop
      Reset; S := V.Open (Context); Pool := P.Open (4);
      F.Begin_Record (S, Pool, Frame_Result); Frame := P.Writer (Pool);
      if Pending_GPU then
         F.Begin_Scene (S, Pool, Bindings, 8, 6, OK); F.End_Scene (S, Pool, OK); F.Submit (S, Pool, OK);
      end if;
      P.Retire_Display (Pool, P.None, False); pragma Assert (P.Faulted (Pool));
      if Pending_GPU then
         F.Poll (S, Pool, Completed); pragma Assert (Completed = F.Uncertain);
      else
         F.Cancel (S, Pool, OK); pragma Assert (not OK);
      end if;
      pragma Assert (V.Quiescent (S) and P.Faulted (Pool) and P.Writer (Pool) = Frame);
      F.Begin_Record (S, Pool, Frame_Result);
      pragma Assert (Frame_Result = F.Failed and Calls (0) = 1 and P.Writer (Pool) = Frame);
   end loop;
   Ada.Text_IO.Put_Line ("VULKAN-FRAME: PASS 1000 frames/998 saturation deferrals/newest-ready replacements, three target selection, 7 command faults/2 overlapping display faults and cancellation");
   for Cycle in 1 .. 100 loop
      declare
         Owner : O.State;
         Closed : Boolean;
         procedure Held is
         begin
            O.Close (Owner, S, Pool, Closed);
            pragma Assert (not Closed and O.Current (Owner) = O.Live and Calls (12) = 0);
         end Held;
      begin
         Reset; S := V.Open (Context); Pool := P.Open (1);
         O.Initialize (Owner, Context, 1, OK); pragma Assert (OK);
         F.Begin_Record (S, Pool, Frame_Result); Held;
         F.Begin_Scene (S, Pool, O.Bindings (Owner), 8, 6, OK);
         F.End_Scene (S, Pool, OK); F.Submit (S, Pool, OK); Held;
         Set (3, 1); F.Poll (S, Pool, Completed); Held;
         Set (3, 0); F.Poll (S, Pool, Completed); Held;
         P.Present (Pool, Offered); Held;
         P.Latch_Display (Pool, Offered, P.None, True); Held;
         -- Keep the front while a newer GPU-completed candidate is discarded.
         F.Begin_Record (S, Pool, Frame_Result);
         F.Begin_Scene (S, Pool, O.Bindings (Owner), 8, 6, OK);
         F.End_Scene (S, Pool, OK); F.Submit (S, Pool, OK); F.Poll (S, Pool, Completed);
         P.Discard_Ready (Pool, P.Ready (Pool)); Held;
         pragma Assert (P.Front (Pool) = Offered and P.Ready (Pool) = P.None);
         P.Retire_Front (Pool, Offered, True);
         O.Close (Owner, S, Pool, Closed);
         pragma Assert (Closed and O.Current (Owner) = O.Closed and Calls (12) = 1);
         O.Close (Owner, S, Pool, Closed); pragma Assert (not Closed and Calls (12) = 1);
      end;
   end loop;
   for Code of Target_Faults loop
      declare Owner : O.State; Closed : Boolean; begin
         Reset; S := V.Open (Context); Pool := P.Open (1); Set (11, Code);
         O.Initialize (Owner, Context, 1, OK);
         pragma Assert (not OK and O.Current (Owner) = (if Code = 1 then O.Closed else O.Quarantined));
         O.Close (Owner, S, Pool, Closed); pragma Assert (not Closed and Calls (12) = 0);
      end;
   end loop;
   for Code of Release_Faults loop
      declare Owner : O.State; Closed : Boolean; begin
         Reset; S := V.Open (Context); Pool := P.Open (1);
         O.Initialize (Owner, Context, 1, OK); Set (12, Code);
         O.Close (Owner, S, Pool, Closed);
         pragma Assert (not Closed and O.Current (Owner) = O.Quarantined and Calls (12) = 1);
         Set (12, 0); O.Close (Owner, S, Pool, Closed);
         pragma Assert (not Closed and Calls (12) = 1);
      end;
   end loop;
   declare Owner : O.State; Closed : Boolean; begin
      Reset; S := V.Open (Context); Pool := P.Open (1); O.Initialize (Owner, Context, 2, OK);
      O.Close (Owner, S, Pool, Closed); pragma Assert (not Closed and Calls (12) = 0);
      Pool := P.Open (2); V.Install_Source (S, 0, Context, Source);
      O.Close (Owner, S, Pool, Closed); pragma Assert (not Closed and Calls (12) = 0);
      V.Remove_Source (S, Source, Released);
      O.Close (Owner, S, Pool, Closed); pragma Assert (Closed and Calls (12) = 1);
   end;
   -- Discarding a stale/incorrect candidate preserves every occupied role.
   Reset; S := V.Open (Context); Pool := P.Open (3);
   F.Begin_Record (S, Pool, Frame_Result); Frame := P.Writer (Pool);
   F.Begin_Scene (S, Pool, Bindings, 8, 6, OK); F.End_Scene (S, Pool, OK);
   F.Submit (S, Pool, OK); F.Poll (S, Pool, Completed);
   P.Discard_Ready (Pool, P.None);
   pragma Assert (P.Faulted (Pool) and P.Ready (Pool) = Frame);
   Ada.Text_IO.Put_Line ("VULKAN-TARGETS policy: PASS 100 teardown lifecycles/5 initialization faults/3 close faults/epoch and source gates/ready discard retention");
   Ada.Text_IO.Put_Line ("VULKAN-SOURCE state bytes:" & Integer'Image (V.State'Size / 8));
   Ada.Text_IO.Put_Line ("VULKAN-SOURCES: PASS 140 slots/duplicate/null/full/stale handles; retained during recording/sealed/pending/quarantine; released after completion/cancel");
   Ada.Text_IO.Put_Line ("VULKAN-SUBMISSION policy: PASS 1000 lifecycles/10000 pending polls/4096 cap/21 status faults/7 forbidden orders/rejected-context and draw retention");
end Vulkan_Submission_Mock_Tests;

with Vulkan_Frame;
with Vulkan_Submission.Checkers;
with Vulkan_Submission.Regions;
with Vulkan_Submission.Backdrops;
with Vulkan_Submission.Previews;
with Compositor_Gradient;
with Compositor_Damage;
package body Vulkan_Scene with SPARK_Mode is
   use type A.Word, A.G.Logical_Coordinate;
   function Physical_Coordinates (R : A.G.Logical_Rectangle) return Boolean is
     (R.Left in 0 .. A.G.Logical_Coordinate (A.G.Pixel_Edge'Last) and
      R.Top in 0 .. A.G.Logical_Coordinate (A.G.Pixel_Edge'Last) and
      R.Right in 0 .. A.G.Logical_Coordinate (A.G.Pixel_Edge'Last) and
      R.Bottom in 0 .. A.G.Logical_Coordinate (A.G.Pixel_Edge'Last));
   function Backdrop_Dimensions (R : A.G.Logical_Rectangle) return Boolean is
     (R.Left = 0 and R.Top = 0 and
      R.Right in 1 .. A.G.Logical_Coordinate (A.G.Pixel_Edge'Last) and
      R.Bottom in 1 .. A.G.Logical_Coordinate (A.G.Pixel_Edge'Last));
   function Open (Screen : A.G.Output; Background : A.Word := 0) return State is
      S : State;
   begin
      S.Screen := Screen; S.Background := Background;
      return S;
   end Open;
   procedure Append (S : in out State; Value : Layer; Accepted : out Boolean) is
   begin
      Accepted := False;
      if S.Status /= Collecting or S.Used = Maximum_Layers or
        Value.Kind in Region_Textured | Straight_Region | Preview or
        (Value.Kind in Physical_Solid | Set_Physical_Clip and then not Physical_Coordinates (Value.Surface)) or
        (if Value.Kind in Textured | Straight_Textured | Glyph_Mask | Backdrop_Fill | Backdrop_Fit | Backdrop_Center then
           Value.Source = V.No_Source or
             (Value.Kind = Straight_Textured and then (not Value.Over or Value.Mask)) or
             (Value.Kind = Glyph_Mask and then (not Value.Over or not Value.Mask)) or
             (Value.Kind in Backdrop_Fill | Backdrop_Fit | Backdrop_Center and then
                (not Backdrop_Dimensions (Value.Surface) or Value.Over or Value.Mask or Value.Tint /= 0))
         else Value.Source /= V.No_Source or Value.Over or Value.Mask or
           (Value.Kind in Set_Clip | Reset_Clip | Set_Physical_Clip and then Value.Tint /= 0)) then
         S.Status := Rejected; return;
      end if;
      S.Used := S.Used + 1; S.Entries (S.Used) := Value; Accepted := True;
   end Append;
   procedure Append_Preview
     (S : in out State; Source : V.Source_Ticket; Bounds : A.G.Logical_Rectangle;
      Description : Preview_Description; Accepted : out Boolean) is
   begin
      Append (S, (Source, Bounds, False, False, 0, Textured), Accepted);
      if Accepted then
         S.Previews (S.Used) := Description;
         S.Entries (S.Used).Kind := Preview;
      end if;
   end Append_Preview;
   procedure Append_Region (S : in out State; Source : V.Source_Ticket;
      Surface : A.G.Logical_Rectangle; Region : R.Rectangle;
      Over, Straight_Alpha : Boolean; Accepted : out Boolean) is
   begin
      Accepted := False;
      if not R.Valid (Region) or (Straight_Alpha and not Over) then
         S.Status := Rejected; return;
      end if;
      Append (S, (Source, Surface, Over, False, 0,
        (if Straight_Alpha then Straight_Textured else Textured)), Accepted);
      if Accepted then
         S.Regions (S.Used) := Region;
         S.Entries (S.Used).Kind := (if Straight_Alpha then Straight_Region else Region_Textured);
      end if;
   end Append_Region;
   procedure Append_Physical_Fill
     (S : in out State; Area : A.G.Physical_Rectangle; RGB : A.Word; Accepted : out Boolean) is
   begin
      Append (S, (V.No_Source,
        (A.G.Logical_Coordinate (Area.Left), A.G.Logical_Coordinate (Area.Top),
         A.G.Logical_Coordinate (Area.Right), A.G.Logical_Coordinate (Area.Bottom)),
        False, False, RGB, Physical_Solid), Accepted);
   end Append_Physical_Fill;
   procedure Append_Physical_Clip
     (S : in out State; Area : A.G.Physical_Rectangle; Accepted : out Boolean) is
   begin
      Append (S, (V.No_Source,
        (A.G.Logical_Coordinate (Area.Left), A.G.Logical_Coordinate (Area.Top),
         A.G.Logical_Coordinate (Area.Right), A.G.Logical_Coordinate (Area.Bottom)),
        False, False, 0, Set_Physical_Clip), Accepted);
   end Append_Physical_Clip;
   procedure Append_Backdrop
     (S : in out State; Source : V.Source_Ticket;
      Source_W, Source_H : A.G.Physical_Extent;
      Mode : Compositor_Image_Sampling.Placement; Accepted : out Boolean) is
      Kind : constant Layer_Kind :=
        (case Mode is when Compositor_Image_Sampling.Fill => Backdrop_Fill,
         when Compositor_Image_Sampling.Fit => Backdrop_Fit,
         when Compositor_Image_Sampling.Center => Backdrop_Center);
   begin
      Append (S, (Source, (0, 0, A.G.Logical_Coordinate (Source_W), A.G.Logical_Coordinate (Source_H)),
        False, False, 0, Kind), Accepted);
   end Append_Backdrop;
   procedure Append_Glyph
     (S : in out State; Submission : V.State; Sources : Vulkan_Glyph_Sources.State;
      Key : Vulkan_Glyph_Sources.Key; Cell : A.G.Logical_Rectangle;
      Tint : A.Word; Accepted : out Boolean) is
      package R renames Vulkan_Glyph_Sources;
      Source : constant V.Source_Ticket := R.Resolve (Sources, Submission, Key);
   begin
      if not R.Keys.Same (Key, (Key.Face, Key.Code, S.Screen.Scale)) or else Source = V.No_Source then
         S.Status := Rejected; Accepted := False; return;
      end if;
      Append (S, (Source, Cell, True, True, Tint, Glyph_Mask), Accepted);
   end Append_Glyph;
   procedure Append_Gradient
     (S : in out State; Surface : A.G.Logical_Rectangle;
      Top, Bottom : A.Word; Accepted : out Boolean) is
      use type A.G.Logical_Coordinate;
      Span : constant Long_Long_Integer :=
        Long_Long_Integer (Surface.Bottom) - Long_Long_Integer (Surface.Top);
      Height : Positive;
      Row, Last : Natural := 0;
      Original : constant Length := S.Used;
   begin
      Accepted := False;
      if S.Status /= Collecting then S.Status := Rejected; return; end if;
      if Surface.Left >= Surface.Right or Surface.Top >= Surface.Bottom then
         Accepted := True; return;
      end if;
      if Span > Long_Long_Integer (Positive'Last) then
         S.Status := Rejected; return;
      end if;
      Height := Positive (Span);
      for Band in 1 .. 256 loop
         Last := Compositor_Gradient.Color_Run_Last (Top, Bottom, Row, Height);
         Append (S,
           (Source => V.No_Source,
            Surface => (Surface.Left,
              A.G.Logical_Coordinate (Long_Long_Integer (Surface.Top) + Long_Long_Integer (Row)),
              Surface.Right,
              A.G.Logical_Coordinate (Long_Long_Integer (Surface.Top) + Long_Long_Integer (Last) + 1)),
            Over => False, Mask => False,
            Tint => Compositor_Gradient.At_Row (Top, Bottom, Row, Height), Kind => Solid), Accepted);
         if not Accepted then return; end if;
         Row := Last + 1;
         if Row = Height then return; end if;
         pragma Loop_Invariant (Row < Height);
         pragma Loop_Invariant (S.Status = Collecting);
         pragma Loop_Invariant (S.Screen = S.Screen'Loop_Entry);
         pragma Loop_Invariant (S.Used >= Original and S.Used <= Original + Band);
         pragma Loop_Invariant
           (for all I in 1 .. Original => S.Entries (I) = S.Entries'Loop_Entry (I));
         pragma Loop_Invariant
           (for all I in 1 .. Original => S.Regions (I) = S.Regions'Loop_Entry (I));
      end loop;
      -- Defensive bound: even a future grouping change cannot grow work.
      S.Status := Rejected; Accepted := False;
   end Append_Gradient;
   procedure Seal (S : in out State; Accepted : out Boolean) is
   begin
      Accepted := S.Status = Collecting;
      S.Status := (if Accepted then Sealed else Rejected);
   end Seal;
   procedure Replay
     (S : State; Submission : in out V.State; Damage : D.State; Accepted : out Boolean) is
      use type Compositor_Damage.Box;
      Clip : A.G.Physical_Rectangle := (0, 0, S.Screen.Width, S.Screen.Height);
      Physical : A.G.Logical_Rectangle;
   begin
      Accepted := False;
      if S.Status /= Sealed or not V.Complete_Frame (Submission) then
         V.Reject_Frame (Submission); return;
      end if;
      if D.Bounds (Damage) /= Compositor_Damage.Box'(0, 0, Natural (S.Screen.Width), Natural (S.Screen.Height)) then
         V.Reject_Frame (Submission); return;
      end if;
      if not Sources_Ready (S, Submission) then
         V.Reject_Frame (Submission); return;
      end if;
      declare
         Plan : constant Compositor_Damage.State := D.Painting (Damage);
         Area : Compositor_Damage.Box;
      begin
         for I in 1 .. Compositor_Damage.Count (Plan) loop
            Area := Compositor_Damage.Item (Plan, I);
            if Area.Left > Natural (S.Screen.Width) or Area.Right > Natural (S.Screen.Width) or
              Area.Top > Natural (S.Screen.Height) or Area.Bottom > Natural (S.Screen.Height)
            then V.Reject_Frame (Submission); return; end if;
            V.Fill_Output (Submission,
              (A.G.Pixel_Edge (Area.Left), A.G.Pixel_Edge (Area.Top), A.G.Pixel_Edge (Area.Right), A.G.Pixel_Edge (Area.Bottom)),
              S.Background, Accepted);
            if not Accepted then return; end if;
            pragma Loop_Invariant (V.Current (Submission) = V.Recording and V.Pass_Active (Submission));
            pragma Loop_Invariant (V.Complete_Frame (Submission));
            pragma Loop_Invariant (V.Same_Sources (Submission, Submission'Loop_Entry));
         end loop;
      end;
      for I in 1 .. S.Used loop
         if S.Entries (I).Kind = Set_Clip then
            Clip := A.G.Damage (S.Screen, S.Entries (I).Surface);
            Accepted := True;
         elsif S.Entries (I).Kind = Set_Physical_Clip then
            Physical := S.Entries (I).Surface;
            if not Physical_Coordinates (Physical) then
               V.Reject_Frame (Submission); Accepted := False; return;
            end if;
            Clip := (A.G.Pixel_Edge (Physical.Left), A.G.Pixel_Edge (Physical.Top),
              A.G.Pixel_Edge (Physical.Right), A.G.Pixel_Edge (Physical.Bottom));
            Accepted := True;
         elsif S.Entries (I).Kind = Reset_Clip then
            Clip := (0, 0, S.Screen.Width, S.Screen.Height);
            Accepted := True;
         elsif S.Entries (I).Kind in Backdrop_Fill | Backdrop_Fit | Backdrop_Center then
            Physical := S.Entries (I).Surface;
            if not Backdrop_Dimensions (Physical) then
               V.Reject_Frame (Submission); Accepted := False; return;
            end if;
            Vulkan_Submission.Backdrops.Replay
              (Submission, Damage, S.Entries (I).Source, S.Screen.Width, S.Screen.Height,
               A.G.Physical_Extent (Physical.Right), A.G.Physical_Extent (Physical.Bottom),
               (case S.Entries (I).Kind is
                  when Backdrop_Fill => Compositor_Image_Sampling.Fill,
                  when Backdrop_Fit => Compositor_Image_Sampling.Fit,
                  when others => Compositor_Image_Sampling.Center), Clip, Accepted);
         elsif S.Entries (I).Kind = Preview then
            Vulkan_Submission.Previews.Replay
              (Submission, Damage, S.Entries (I).Source, S.Screen,
               S.Entries (I).Surface, S.Previews (I).Width, S.Previews (I).Height,
               S.Previews (I).Mode, Clip, Accepted);
         elsif S.Entries (I).Kind = Physical_Solid then
            Physical := S.Entries (I).Surface;
            if not Physical_Coordinates (Physical) then
               V.Reject_Frame (Submission); Accepted := False; return;
            end if;
            Vulkan_Frame.Replay_Physical_Fill
              (Submission, Damage, S.Screen,
               (A.G.Pixel_Edge (Physical.Left), A.G.Pixel_Edge (Physical.Top), A.G.Pixel_Edge (Physical.Right), A.G.Pixel_Edge (Physical.Bottom)),
               S.Entries (I).Tint, Accepted, Clip);
         elsif S.Entries (I).Kind in Region_Textured | Straight_Region then
            Vulkan_Submission.Regions.Replay
              (Submission, Damage, S.Entries (I).Source, S.Screen,
               S.Entries (I).Surface, Clip, S.Regions (I), S.Entries (I).Over,
               S.Entries (I).Kind = Straight_Region, Accepted);
         elsif S.Entries (I).Kind = Checker_Grid then
            Vulkan_Submission.Checkers.Replay
              (Submission, Damage, S.Screen, S.Entries (I).Surface, Clip,
               S.Entries (I).Tint, Accepted);
         elsif S.Entries (I).Kind = Solid then
            Vulkan_Frame.Replay_Fill
              (Submission, Damage, S.Screen, S.Entries (I).Surface, S.Entries (I).Tint, Accepted, Clip);
         else
            Vulkan_Frame.Replay_Layer
              (Submission, Damage, S.Entries (I).Source, S.Screen, S.Entries (I).Surface,
               S.Entries (I).Over, S.Entries (I).Mask, S.Entries (I).Tint, Accepted, Clip,
               S.Entries (I).Kind = Glyph_Mask, S.Entries (I).Kind = Straight_Textured);
         end if;
         if not Accepted then return; end if;
         pragma Loop_Invariant (V.Current (Submission) = V.Recording and V.Pass_Active (Submission));
         pragma Loop_Invariant (V.Complete_Frame (Submission));
         pragma Loop_Invariant (V.Same_Sources (Submission, Submission'Loop_Entry));
      end loop;
      Accepted := True;
   end Replay;
end Vulkan_Scene;

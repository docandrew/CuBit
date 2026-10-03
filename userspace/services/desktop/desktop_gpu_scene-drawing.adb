package body Desktop_GPU_Scene.Drawing with SPARK_Mode is
   use type V.A.G.Pixel_Edge;
   function Empty (Area : V.A.G.Physical_Rectangle) return Boolean is
     (Area.Left >= Area.Right or Area.Top >= Area.Bottom);
   function Intersect (Left, Right : V.A.G.Physical_Rectangle) return V.A.G.Physical_Rectangle is
     (V.A.G.Pixel_Edge'Max (Left.Left, Right.Left), V.A.G.Pixel_Edge'Max (Left.Top, Right.Top),
      V.A.G.Pixel_Edge'Min (Left.Right, Right.Right), V.A.G.Pixel_Edge'Min (Left.Bottom, Right.Bottom));
   function Capturable (S : State) return Boolean is
     (S.Status = Capturing and not S.Cold and not S.Invalid);
   procedure Shadow (S : in out State; Window : V.A.G.Logical_Rectangle;
      Damage : V.A.G.Physical_Rectangle; Color : V.A.Word;
      Accepted : out Boolean; Depth : Compositor_Shadow.Depth := 3) is
      P : constant Compositor_Shadow.Plan := Compositor_Shadow.Build (Window, Depth);
      Screen : constant V.A.G.Output := V.Output (S.Scene);
      Clip : constant V.A.G.Physical_Rectangle := Intersect (Damage, (0, 0, Screen.Width, Screen.Height));
      Started : Boolean := False;
      Initial : constant Natural := Natural (Layer_Count (S));
   begin
      Accepted := False;
      if not Capturable (S) then return; end if;
      if not P.Valid then S.Invalid := True; return; end if;
      Accepted := True;
      for I in P.Areas'Range loop
         if not Empty (Intersect (V.A.G.Damage (Screen, P.Areas (I)), Clip)) then
            if not Started then
               Set_Clip (S, Clip, Accepted); if not Accepted then return; end if;
               Started := True;
            end if;
            V.Append (S.Scene, (Vulkan_Submission.No_Source, P.Areas (I), False, False, Color, V.Checker_Grid), Accepted);
            if not Accepted then S.Invalid := True; return; end if;
         end if;
         pragma Loop_Invariant (Valid (S));
         pragma Loop_Invariant (Reader_Count (S) = Reader_Count (S)'Loop_Entry);
         pragma Loop_Invariant (Image_Reader_Count (S) = Image_Reader_Count (S)'Loop_Entry);
         pragma Loop_Invariant (Natural (Layer_Count (S)) <= Initial + 1 + I);
         pragma Loop_Invariant (if not Started then Natural (Layer_Count (S)) = Initial);
      end loop;
   end Shadow;
   procedure Fill (S : in out State; Area : V.A.G.Physical_Rectangle;
      Color : V.A.Word; Accepted : out Boolean) is
      Screen : constant V.A.G.Output := V.Output (S.Scene);
      Bounds : constant V.A.G.Physical_Rectangle := (0, 0, Screen.Width, Screen.Height);
      Clipped : constant V.A.G.Physical_Rectangle := Intersect (Area, Bounds);
   begin
      Accepted := False;
      if not Capturable (S) then return; end if;
      if Empty (Clipped) then Accepted := True; return; end if;
      Set_Clip (S, Bounds, Accepted); if not Accepted then return; end if;
      V.Append_Physical_Fill (S.Scene, Clipped, Color, Accepted);
      if not Accepted then S.Invalid := True; end if;
   end Fill;
   procedure Logical_Fill (S : in out State;
      Area : V.A.G.Logical_Rectangle; Damage : V.A.G.Physical_Rectangle;
      Color : V.A.Word; Accepted : out Boolean) is
   begin
      Fill (S, Intersect (V.A.G.Damage (V.Output (S.Scene), Area), Damage),
        Color, Accepted);
   end Logical_Fill;
   procedure Image (S : in out State; Source : Vulkan_Submission.Source_Ticket;
      Surface : V.A.G.Logical_Rectangle; Damage : V.A.G.Physical_Rectangle;
      Accepted : out Boolean; Over : Boolean := False; Straight_Alpha : Boolean := False) is
      Screen : constant V.A.G.Output := V.Output (S.Scene);
      Clipped : constant V.A.G.Physical_Rectangle := Intersect (V.A.G.Damage (Screen, Surface), Damage);
   begin
      Accepted := False;
      if not Capturable (S) then return; end if;
      if Straight_Alpha and not Over then S.Invalid := True; return; end if;
      if Empty (Clipped) then Accepted := True; return; end if;
      Set_Clip (S, Clipped, Accepted); if not Accepted then return; end if;
      Append (S, (Source, Surface, Over, False, 0,
        (if Straight_Alpha then V.Straight_Textured else V.Textured)), Accepted);
   end Image;
   procedure Image_Region (S : in out State; Source : Vulkan_Submission.Source_Ticket;
      Surface : V.A.G.Logical_Rectangle; Damage : V.A.G.Physical_Rectangle;
      Region : V.R.Rectangle; Accepted : out Boolean;
      Over : Boolean := False; Straight_Alpha : Boolean := False) is
      Clipped : constant V.A.G.Physical_Rectangle :=
        Intersect (V.A.G.Damage (V.Output (S.Scene), Surface), Damage);
   begin
      Accepted := False;
      if not Capturable (S) then return; end if;
      if not V.R.Valid (Region) or (Straight_Alpha and not Over) then
         S.Invalid := True; return;
      end if;
      if Empty (Clipped) then Accepted := True; return; end if;
      Set_Clip (S, Clipped, Accepted); if not Accepted then return; end if;
      Pin_Image (S, Source, Accepted); if not Accepted then return; end if;
      V.Append_Region (S.Scene, Source, Surface, Region, Over, Straight_Alpha, Accepted);
      if not Accepted then S.Invalid := True; end if;
   end Image_Region;
   procedure Text (S : in out State; Items : Compositor_Text.Glyphs;
      Length : Compositor_Text.Count; Damage : V.A.G.Physical_Rectangle;
      Tint : V.A.Word; Accepted : out Boolean; Face : Face_ID := 0) is
      Screen : constant V.A.G.Output := V.Output (S.Scene);
      Bounds : constant V.A.G.Physical_Rectangle := (0, 0, Screen.Width, Screen.Height);
      Clipped : constant V.A.G.Physical_Rectangle := Intersect (Damage, Bounds);
      Clip_Started : Boolean := False;
   begin
      Accepted := False;
      if not Capturable (S) then return; end if;
      Accepted := True;
      if Empty (Clipped) then return; end if;
      for I in 1 .. Length loop
         if not Empty (Compositor_Text.Clip (Screen, Items (I).Cell, Clipped)) then
            if not Clip_Started then
               Set_Clip (S, Clipped, Accepted); if not Accepted then return; end if;
               Clip_Started := True;
            end if;
            Add_Glyph (S, (Face, Items (I).Code, Screen.Scale), Items (I).Cell, Tint, Accepted);
            if not Accepted then return; end if;
         end if;
         pragma Loop_Invariant (Valid (S));
         pragma Loop_Invariant (D.Valid);
      end loop;
   end Text;
end Desktop_GPU_Scene.Drawing;

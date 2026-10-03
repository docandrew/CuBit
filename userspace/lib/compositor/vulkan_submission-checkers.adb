with Vulkan_Checker_FFI;
package body Vulkan_Submission.Checkers with SPARK_Mode is
   package F renames Vulkan_Checker_FFI;
   use type G.Pixel_Edge, F.Word;
   procedure Draw_Output
     (S : in out State; Screen : G.Output; Area : G.Logical_Rectangle;
      Damage : G.Physical_Rectangle; RGB : Compositor_Affine.Word;
      Accepted : out Boolean) is
      Bounds : constant G.Physical_Rectangle := G.Damage (Screen, Area);
      Clip : constant G.Physical_Rectangle :=
        (G.Pixel_Edge'Max (Bounds.Left, Damage.Left),
         G.Pixel_Edge'Max (Bounds.Top, Damage.Top),
         G.Pixel_Edge'Min (Bounds.Right, Damage.Right),
         G.Pixel_Edge'Min (Bounds.Bottom, Damage.Bottom));
      Status : F.Word;
   begin
      Admit_Draw (S, Accepted);
      if not Accepted then return; end if;
      if Screen.Width /= S.Target_Width or Screen.Height /= S.Target_Height then
         Reject_Frame (S); Accepted := False; return;
      end if;
      if Clip.Left >= Clip.Right or Clip.Top >= Clip.Bottom then return; end if;
      F.Record_Draw (S.Context,
        (F.Signed (Area.Left), F.Signed (Area.Top), F.Signed (Area.Right), F.Signed (Area.Bottom),
         F.Signed (Screen.X), F.Signed (Screen.Y), F.Word (Screen.Scale.Numerator), F.Word (Screen.Scale.Denominator),
         F.Word (Screen.Width), F.Word (Screen.Height), F.Word (G.Orientation'Pos (Screen.Rotation)),
         F.Word (Clip.Left), F.Word (Clip.Top), F.Word (Clip.Right - Clip.Left), F.Word (Clip.Bottom - Clip.Top), RGB), Status);
      Accepted := Status = 0;
      if not Accepted then Reject_Frame (S); end if;
   end Draw_Output;
   procedure Replay
     (S : in out State; Damage : D.State; Screen : G.Output;
      Area : G.Logical_Rectangle; Clip : G.Physical_Rectangle;
      RGB : Compositor_Affine.Word; Accepted : out Boolean) is
      package R renames D.D;
      use type R.Box;
      Plan : constant R.State := D.Painting (Damage);
      Initial : constant Draw_Count := Draws (S);
      Part : R.Box;
   begin
      Accepted := False;
      if not Complete_Frame (S) then return; end if;
      if D.Bounds (Damage) /= R.Box'(0, 0, Natural (Screen.Width), Natural (Screen.Height)) then
         Reject_Frame (S); return;
      end if;
      for I in 1 .. R.Count (Plan) loop
         Part := R.Item (Plan, I);
         if Part.Left > Natural (Screen.Width) or Part.Right > Natural (Screen.Width) or
           Part.Top > Natural (Screen.Height) or Part.Bottom > Natural (Screen.Height)
         then Reject_Frame (S); return; end if;
         Draw_Output (S, Screen, Area,
           (G.Pixel_Edge'Max (G.Pixel_Edge (Part.Left), Clip.Left),
            G.Pixel_Edge'Max (G.Pixel_Edge (Part.Top), Clip.Top),
            G.Pixel_Edge'Min (G.Pixel_Edge (Part.Right), Clip.Right),
            G.Pixel_Edge'Min (G.Pixel_Edge (Part.Bottom), Clip.Bottom)), RGB, Accepted);
         if not Accepted then return; end if;
         pragma Loop_Invariant (Current (S) = Recording and Pass_Active (S));
         pragma Loop_Invariant (Complete_Frame (S));
         pragma Loop_Invariant (Same_Sources (S, S'Loop_Entry));
         pragma Loop_Invariant (Draws (S) >= Initial and Draws (S) <= Initial + I);
      end loop;
      Accepted := True;
   end Replay;
end Vulkan_Submission.Checkers;

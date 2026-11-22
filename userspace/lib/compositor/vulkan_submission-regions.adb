with Vulkan_Submission_FFI;
with Vulkan_Affine_Binding.Regions;
package body Vulkan_Submission.Regions with SPARK_Mode is
   procedure Draw_Output (S : in out State; Source : Source_Ticket;
      Screen : G.Output; Surface : G.Logical_Rectangle; Damage : G.Physical_Rectangle;
      Region : Compositor_Source_Region.Rectangle; Over, Straight_Alpha : Boolean;
      Result : out Vulkan_Affine_Binding.Outcome) is
      package F renames Vulkan_Submission_FFI;
      use type F.Code, G.Pixel_Edge;
      OK : Boolean;
      Match : F.Code;
   begin
      Admit_Draw (S, OK);
      Result := Vulkan_Affine_Binding.Rejected;
      if not OK then return; end if;
      if not Source_Valid (S, Source) then Reject_Frame (S); return; end if;
      if Screen.Width /= S.Target_Width or else Screen.Height /= S.Target_Height then
         Reject_Frame (S); return;
      end if;
      F.Matches (S.Context, S.Sources (Source.Index).Context, Match);
      if Match = 0 then
         Vulkan_Affine_Binding.Regions.Draw_Output
           (S.Sources (Source.Index).Context, Screen, Surface, Damage, Region, Over, Straight_Alpha, Result);
      end if;
      if Result = Vulkan_Affine_Binding.Rejected then Reject_Frame (S); end if;
   end Draw_Output;
   procedure Replay
     (S : in out State; Damage : D.State; Source : Source_Ticket;
      Screen : G.Output; Surface : G.Logical_Rectangle; Clip : G.Physical_Rectangle;
      Region : Compositor_Source_Region.Rectangle; Over, Straight_Alpha : Boolean;
      Accepted : out Boolean) is
      package R renames D.D;
      use type R.Box, G.Pixel_Edge;
      Result : Vulkan_Affine_Binding.Outcome;
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
         Draw_Output (S, Source, Screen, Surface,
           (G.Pixel_Edge'Max (G.Pixel_Edge (Part.Left), Clip.Left),
            G.Pixel_Edge'Max (G.Pixel_Edge (Part.Top), Clip.Top),
            G.Pixel_Edge'Min (G.Pixel_Edge (Part.Right), Clip.Right),
            G.Pixel_Edge'Min (G.Pixel_Edge (Part.Bottom), Clip.Bottom)), Region, Over, Straight_Alpha, Result);
         Accepted := Result /= Vulkan_Affine_Binding.Rejected;
         if not Accepted then return; end if;
         pragma Loop_Invariant (Current (S) = Recording and Pass_Active (S));
         pragma Loop_Invariant (Complete_Frame (S));
         pragma Loop_Invariant (Same_Sources (S, S'Loop_Entry));
         pragma Loop_Invariant (Draws (S) >= Initial and Draws (S) <= Initial + I);
      end loop;
      Accepted := True;
   end Replay;
end Vulkan_Submission.Regions;

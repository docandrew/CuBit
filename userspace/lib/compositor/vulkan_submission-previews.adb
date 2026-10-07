with Vulkan_Submission_FFI;
package body Vulkan_Submission.Previews with SPARK_Mode is
   package F renames Vulkan_Submission_FFI;
   use type F.Code, B.P.G.Pixel_Edge;
   procedure Draw_Output
     (S : in out State; Source : Source_Ticket;
      Screen : B.P.G.Output; Bounds : B.P.G.Logical_Rectangle;
      Source_W, Source_H : B.P.S.Extent;
      Mode : B.P.S.Placement; Damage : B.P.G.Physical_Rectangle;
      Result : out B.Outcome) is
      OK : Boolean;
      Match : F.Code;
   begin
      Admit_Draw (S, OK);
      Result := B.Rejected;
      if not OK then return; end if;
      if not Source_Valid (S, Source) or else Screen.Width /= S.Target_Width or else Screen.Height /= S.Target_Height then
         Reject_Frame (S); return;
      end if;
      F.Matches (S.Context, S.Sources (Source.Index).Context, Match);
      if Match = 0 then
         B.Draw_Output (S.Sources (Source.Index).Context, Screen, Bounds, Damage,
           Source_W, Source_H, Mode, Result);
      end if;
      if Result = B.Rejected then Reject_Frame (S); end if;
   end Draw_Output;
   procedure Replay
     (S : in out State; Damage : D.State; Source : Source_Ticket;
      Screen : B.P.G.Output; Bounds : B.P.G.Logical_Rectangle;
      Source_W, Source_H : B.P.S.Extent;
      Mode : B.P.S.Placement; Clip : B.P.G.Physical_Rectangle;
      Accepted : out Boolean) is
      package R renames D.D;
      package G renames B.P.G;
      use type R.Box;
      Plan : constant R.State := D.Painting (Damage);
      Initial : constant Draw_Count := Draws (S);
      Area : R.Box;
      Result : B.Outcome;
   begin
      Accepted := False;
      if not Complete_Frame (S) then return; end if;
      if D.Bounds (Damage) /= R.Box'(0, 0, Natural (Screen.Width), Natural (Screen.Height)) then
         Reject_Frame (S); return;
      end if;
      for I in 1 .. R.Count (Plan) loop
         Area := R.Item (Plan, I);
         if Area.Left > Natural (Screen.Width) or Area.Right > Natural (Screen.Width) or
           Area.Top > Natural (Screen.Height) or Area.Bottom > Natural (Screen.Height)
         then Reject_Frame (S); return; end if;
         Draw_Output (S, Source, Screen, Bounds, Source_W, Source_H, Mode,
           (G.Pixel_Edge'Max (G.Pixel_Edge (Area.Left), Clip.Left),
            G.Pixel_Edge'Max (G.Pixel_Edge (Area.Top), Clip.Top),
            G.Pixel_Edge'Min (G.Pixel_Edge (Area.Right), Clip.Right),
            G.Pixel_Edge'Min (G.Pixel_Edge (Area.Bottom), Clip.Bottom)), Result);
         if Result = B.Rejected then return; end if;
         pragma Loop_Invariant (Current (S) = Recording and Pass_Active (S));
         pragma Loop_Invariant (Complete_Frame (S));
         pragma Loop_Invariant (Same_Sources (S, S'Loop_Entry));
         pragma Loop_Invariant (Draws (S) = Initial + I);
      end loop;
      Accepted := True;
   end Replay;
end Vulkan_Submission.Previews;

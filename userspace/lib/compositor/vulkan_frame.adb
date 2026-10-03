with Compositor_Damage;
with Vulkan_Affine_Binding;
package body Vulkan_Frame with SPARK_Mode is
   use type V.Observation;
   procedure Replay_Layer
     (Submission : in out V.State; Damage : D.State; Source : V.Source_Ticket;
      Screen : Compositor_Affine.G.Output;
      Surface : Compositor_Affine.G.Logical_Rectangle;
      Over, Mask : Boolean; Tint : Compositor_Affine.Word; Accepted : out Boolean;
      Clip : Compositor_Affine.G.Physical_Rectangle :=
        (0, 0, Compositor_Affine.G.Pixel_Edge'Last, Compositor_Affine.G.Pixel_Edge'Last);
      Raster_Glyph : Boolean := False; Straight_Alpha : Boolean := False) is
      package R renames Compositor_Damage;
      package G renames Compositor_Affine.G;
      use type Vulkan_Affine_Binding.Outcome, R.Box;
      Plan : constant R.State := D.Painting (Damage);
      Result : Vulkan_Affine_Binding.Outcome;
      Initial : constant V.Draw_Count := V.Draws (Submission);
      Area : R.Box;
   begin
      Accepted := False;
      if not V.Complete_Frame (Submission) then return; end if;
      if D.Bounds (Damage) /= R.Box'(0, 0, Natural (Screen.Width), Natural (Screen.Height)) then
         V.Reject_Frame (Submission); return;
      end if;
      for I in 1 .. R.Count (Plan) loop
         Area := R.Item (Plan, I);
         if Area.Left > Natural (Screen.Width) or Area.Right > Natural (Screen.Width) or
           Area.Top > Natural (Screen.Height) or Area.Bottom > Natural (Screen.Height)
         then V.Reject_Frame (Submission); return; end if;
         V.Draw_Output (Submission, Source, Screen, Surface,
           (G.Pixel_Edge'Max (G.Pixel_Edge (Area.Left), Clip.Left),
            G.Pixel_Edge'Max (G.Pixel_Edge (Area.Top), Clip.Top),
            G.Pixel_Edge'Min (G.Pixel_Edge (Area.Right), Clip.Right),
            G.Pixel_Edge'Min (G.Pixel_Edge (Area.Bottom), Clip.Bottom)),
           Over, Mask, Tint, Result, Raster_Glyph, Straight_Alpha);
         if Result = Vulkan_Affine_Binding.Rejected then return; end if;
         pragma Loop_Invariant (V.Current (Submission) = V.Recording and V.Pass_Active (Submission));
         pragma Loop_Invariant (V.Complete_Frame (Submission));
         pragma Loop_Invariant (V.Same_Sources (Submission, Submission'Loop_Entry));
         pragma Loop_Invariant (V.Draws (Submission) = Initial + I);
      end loop;
      Accepted := True;
   end Replay_Layer;
   procedure Replay_Fill
     (Submission : in out V.State; Damage : D.State;
      Screen : Compositor_Affine.G.Output; Surface : Compositor_Affine.G.Logical_Rectangle;
      RGB : Compositor_Affine.Word; Accepted : out Boolean;
      Clip : Compositor_Affine.G.Physical_Rectangle :=
        (0, 0, Compositor_Affine.G.Pixel_Edge'Last, Compositor_Affine.G.Pixel_Edge'Last)) is
   begin
      Replay_Physical_Fill (Submission, Damage, Screen,
        Compositor_Affine.G.Damage (Screen, Surface), RGB, Accepted, Clip);
   end Replay_Fill;
   procedure Replay_Physical_Fill
     (Submission : in out V.State; Damage : D.State;
      Screen : Compositor_Affine.G.Output; Surface : Compositor_Affine.G.Physical_Rectangle;
      RGB : Compositor_Affine.Word; Accepted : out Boolean;
      Clip : Compositor_Affine.G.Physical_Rectangle :=
        (0, 0, Compositor_Affine.G.Pixel_Edge'Last, Compositor_Affine.G.Pixel_Edge'Last)) is
      package G renames Compositor_Affine.G;
      package R renames Compositor_Damage;
      use type R.Box;
      Plan : constant R.State := D.Painting (Damage);
      Mapped : constant G.Physical_Rectangle := Surface;
      Area : R.Box;
      Left, Top, Right, Bottom : Natural;
      Initial : constant V.Draw_Count := V.Draws (Submission);
   begin
      Accepted := False;
      if not V.Complete_Frame (Submission) then return; end if;
      if D.Bounds (Damage) /= R.Box'(0, 0, Natural (Screen.Width), Natural (Screen.Height)) then
         V.Reject_Frame (Submission); return;
      end if;
      for I in 1 .. R.Count (Plan) loop
         Area := R.Item (Plan, I);
         Left := Natural'Max (Natural'Max (Area.Left, Natural (Mapped.Left)), Natural (Clip.Left));
         Top := Natural'Max (Natural'Max (Area.Top, Natural (Mapped.Top)), Natural (Clip.Top));
         Right := Natural'Min (Natural'Min (Area.Right, Natural (Mapped.Right)), Natural (Clip.Right));
         Bottom := Natural'Min (Natural'Min (Area.Bottom, Natural (Mapped.Bottom)), Natural (Clip.Bottom));
         if Left < Right and Top < Bottom then
            V.Fill_Output (Submission,
              (G.Pixel_Edge (Left), G.Pixel_Edge (Top), G.Pixel_Edge (Right), G.Pixel_Edge (Bottom)), RGB, Accepted);
            if not Accepted then return; end if;
         end if;
         pragma Loop_Invariant (V.Current (Submission) = V.Recording and V.Pass_Active (Submission));
         pragma Loop_Invariant (V.Complete_Frame (Submission));
         pragma Loop_Invariant (V.Same_Sources (Submission, Submission'Loop_Entry));
         pragma Loop_Invariant (V.Draws (Submission) >= Initial and V.Draws (Submission) <= Initial + I);
      end loop;
      Accepted := True;
   end Replay_Physical_Fill;
   procedure Begin_Record
     (Submission : in out V.State; Pool : in out P.State;
      Result : out Admission; Replace_Ready : Boolean := False) is
      Ticket : P.Ticket;
      Accepted : Boolean;
   begin
      if P.Faulted (Pool) then Result := Failed; return; end if;
      -- An existing writer belongs to another caller's unfinished admission.
      if P.Writer (Pool) /= P.None then Result := Deferred; return; end if;
      P.Acquire (Pool, Ticket, Replace_Ready);
      if Ticket = P.None then
         Result := (if P.Faulted (Pool) then Failed else Deferred); return;
      end if;
      P.Start_Render (Pool, Ticket);
      V.Begin_Record (Submission, Accepted);
      if Accepted then Result := Started;
      else P.Finish_Render (Pool, Ticket, P.Unknown); Result := Failed;
      end if;
   end Begin_Record;
   procedure Begin_Scene
     (Submission : in out V.State; Pool : in out P.State; Bindings : Targets;
      Width, Height : Compositor_Affine.G.Physical_Extent; Accepted : out Boolean) is
   begin
      V.Begin_Scene (Submission, Bindings (P.Writer (Pool).Buffer), Width, Height, Accepted);
      if not Accepted then P.Finish_Render (Pool, P.Writer (Pool), P.Unknown); end if;
   end Begin_Scene;
   procedure End_Scene
     (Submission : in out V.State; Pool : in out P.State; Accepted : out Boolean) is
   begin
      V.End_Scene (Submission, Accepted);
      if not Accepted then P.Finish_Render (Pool, P.Writer (Pool), P.Unknown); end if;
   end End_Scene;
   procedure Submit
     (Submission : in out V.State; Pool : in out P.State; Accepted : out Boolean) is
   begin
      V.Seal (Submission, Accepted);
      if Accepted then V.Submit (Submission, Accepted); end if;
      if not Accepted then P.Finish_Render (Pool, P.Writer (Pool), P.Unknown); end if;
   end Submit;
   procedure Cancel
     (Submission : in out V.State; Pool : in out P.State; Released : out Boolean) is
   begin
      V.Cancel (Submission, Released);
      P.Finish_Render (Pool, P.Writer (Pool), (if Released then P.Failed_Quiescent else P.Unknown));
      Released := Released and not P.Faulted (Pool);
   end Cancel;
   procedure Poll
     (Submission : in out V.State; Pool : in out P.State; Result : out Completion) is
      Observed : V.Observation;
   begin
      V.Poll (Submission, Observed);
      if Observed = V.Still_Pending then Result := Still_Pending; return; end if;
      P.Finish_Render (Pool, P.Writer (Pool), (if Observed = V.Finished then P.Completed else P.Unknown));
      Result := (if not P.Faulted (Pool) then Ready else Uncertain);
   end Poll;
   procedure Begin_Record
     (Submission : in out V.State; Pool : in out P.State; Damage : in out D.State;
      Result : out Admission; Replace_Ready : Boolean := False) is
   begin
      Begin_Record (Submission, Pool, Result, Replace_Ready);
      if Result = Started then D.Begin_Paint (Damage, P.Writer (Pool).Buffer); end if;
   end Begin_Record;
   procedure Cancel
     (Submission : in out V.State; Pool : in out P.State; Damage : in out D.State; Released : out Boolean) is
   begin
      Cancel (Submission, Pool, Released);
      D.Finish (Damage, (if Released then D.Cancelled else D.Unknown));
   end Cancel;
   procedure Poll
     (Submission : in out V.State; Pool : in out P.State; Damage : in out D.State; Result : out Completion) is
   begin
      Poll (Submission, Pool, Result);
      if Result /= Still_Pending then D.Finish (Damage, (if Result = Ready then D.Completed else D.Unknown)); end if;
   end Poll;
end Vulkan_Frame;

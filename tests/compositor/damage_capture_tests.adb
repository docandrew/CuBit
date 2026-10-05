with Ada.Text_IO;
with Compositor_Damage; with Compositor_Pool;
procedure Damage_Capture_Tests is
   package D renames Compositor_Damage;
   package P renames Compositor_Pool;
   use type D.State, D.Box, P.Ticket;
   Pending, Frame, Saved : D.State;
   Pool : P.State := P.Open (1);
   Writer, Displayed : P.Ticket;
   Accepted : Boolean;
begin
   D.Capture (Pending, Frame, Accepted);
   pragma Assert (not Accepted and D.Count (Frame) = 0);
   for Cycle in 1 .. 1000 loop
      D.Add (Pending, (0, 0, 2, 2));
      D.Add (Pending, (62, 62, 64, 64));
      Saved := Pending;
      D.Capture (Pending, Frame, Accepted);
      pragma Assert (Accepted and Frame = Saved and D.Count (Pending) = 0);
      P.Acquire (Pool, Writer); P.Start_Render (Pool, Writer);
      -- Simulate fresh input, sparse-overload collapse and a layout repaint
      -- while the renderer still owns the old frame. No capture may replace it.
      for Event in 1 .. 32 loop
         D.Add (Pending, (Event, Event, Event + 1, Event + 1));
         D.Capture (Pending, Frame, Accepted);
         pragma Assert (not Accepted and Frame = Saved and D.Count (Pending) > 0);
         pragma Assert (P.Rendering (Pool) and not P.Writable (Pool, Writer));
      end loop;
      if Cycle mod 2 = 0 then
         D.Clear (Pending); D.Add (Pending, (0, 0, 128, 128));
      end if;
      P.Finish_Render (Pool, Writer, P.Completed);
      P.Present (Pool, Displayed);
      pragma Assert (Displayed = Writer);
      D.Clear (Frame); -- exactly the submitted snapshot, never live damage
      pragma Assert (D.Covers (Pending, (32, 32, 33, 33)));
      if Cycle mod 2 = 0 then pragma Assert (D.Bounds (Pending) = (0, 0, 128, 128)); end if;
      P.Retire_Display (Pool, Displayed, True);
      Saved := Pending;
      D.Capture (Pending, Frame, Accepted);
      pragma Assert (Accepted and Frame = Saved and D.Count (Pending) = 0);
      D.Clear (Frame);
   end loop;
   Ada.Text_IO.Put_Line ("DAMAGE-CAPTURE: PASS 1000 delayed frames/32000 fresh input updates, held snapshots, overload and layout replacement");
end Damage_Capture_Tests;

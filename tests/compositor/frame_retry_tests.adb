with Ada.Text_IO;
with Compositor_Damage; with Compositor_Pool; with Compositor_Repaint;
procedure Frame_Retry_Tests is
   package D renames Compositor_Damage;
   package P renames Compositor_Pool;
   package R renames Compositor_Repaint;
   use type D.State, D.Box, P.Ticket;
   Pending, Frame, Saved, Repair : D.State;
   Pool : P.State := P.Open (1);
   Repaint : R.State := R.Open ((0, 0, 128, 128));
   Writer, Got, Front : P.Ticket;
   Accepted : Boolean;
begin
   D.Restore (Pending, Frame);
   pragma Assert (D.Count (Pending) = 0 and D.Count (Frame) = 0);
   P.Acquire (Pool, Front); P.Start_Render (Pool, Front);
   P.Finish_Render (Pool, Front, P.Completed); P.Present (Pool, Got);
   P.Latch_Display (Pool, Got, P.None, True);
   for Cycle in 1 .. 1000 loop
      D.Add (Pending, (0, 0, 2, 2));
      D.Add (Pending, (62, 62, 64, 64));
      D.Capture (Pending, Frame, Accepted);
      pragma Assert (Accepted);
      Saved := Frame;
      P.Acquire (Pool, Writer);
      pragma Assert (Writer /= P.None and Writer.Buffer /= Front.Buffer);
      R.Take (Repaint, Writer.Buffer, Repair);
      P.Start_Render (Pool, Writer);
      for Event in 1 .. 32 loop
         D.Add (Pending, (Event, Event, Event + 1, Event + 1));
      end loop;
      if Cycle mod 2 = 0 then D.Add (Pending, (0, 0, 128, 128)); end if;
      -- The actual retry branch's ordering: retire only after quiescence,
      -- invalidate partial writer pixels, restore damage, acquire fresh ticket.
      P.Finish_Render (Pool, Writer, P.Failed_Quiescent);
      R.Failed_Render (Repaint, Writer.Buffer);
      D.Restore (Pending, Frame);
      pragma Assert (D.Count (Frame) = 0 and not P.Faulted (Pool));
      pragma Assert (P.Ready (Pool) = P.None and P.Front (Pool) = Front);
      pragma Assert (D.Covers (R.Pending (Repaint, Writer.Buffer), (0, 0, 128, 128)));
      for I in 1 .. D.Count (Saved) loop
         pragma Assert (D.Covers (Pending, D.Item (Saved, I)));
      end loop;
      for Event in 1 .. 32 loop
         pragma Assert (D.Covers (Pending, (Event, Event, Event + 1, Event + 1)));
      end loop;
      Saved := Pending;
      D.Restore (Pending, Frame);
      pragma Assert (Pending = Saved); -- duplicate empty restore cannot erase work
      P.Acquire (Pool, Got);
      pragma Assert (Got /= P.None and Got /= Writer and Got.Buffer /= Front.Buffer);
      P.Present (Pool, Writer);
      pragma Assert (Writer = P.None); -- never publish the failed frame
      D.Capture (Pending, Frame, Accepted);
      pragma Assert (Accepted and Frame = Saved);
      R.Take (Repaint, Got.Buffer, Repair);
      P.Start_Render (Pool, Got); P.Finish_Render (Pool, Got, P.Completed);
      P.Present (Pool, Writer);
      P.Latch_Display (Pool, Writer, Front, True); Front := Writer;
      D.Clear (Frame);
   end loop;
   Ada.Text_IO.Put_Line ("FRAME-RETRY: PASS 1000 retries/32000 fresh updates, repair, held fronts, no failed publication");
end Frame_Retry_Tests;

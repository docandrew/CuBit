with Ada.Text_IO;
with Compositor_Frame_Replacement; use Compositor_Frame_Replacement;
procedure Frame_Replacement_Tests is
   use type BP.Ticket, CP.ID, D.State;
   Transfer : CP.State;
   Pool : BP.State;
   Pending, Frame : D.State;
   Held, Writer : BP.Ticket;
   OK : Boolean;
   procedure Setup is
   begin
      Transfer := CP.Open (1); Pool := BP.Open (1);
      D.Clear (Pending); D.Clear (Frame);
      D.Add (Pending, (0, 0, 8, 8));
      D.Capture (Pending, Frame, OK);
      BP.Acquire (Pool, Writer); BP.Start_Render (Pool, Writer);
      BP.Finish_Render (Pool, Writer, BP.Completed); BP.Present (Pool, Held);
      BP.Acquire (Pool, Writer);
      CP.Prepare (Transfer, 1, OK); pragma Assert (OK);
      CP.Submitted (Transfer, False, 100);
   end Setup;
begin
   for Cycle in 1 .. 1000 loop
      Setup;
      Replace (Transfer, Pool, Pending, Frame, 101, OK);
      pragma Assert (not OK); -- no fresher scene
      for I in 1 .. 32 loop
         D.Add (Pending, (I, I, I + 2, I + 2));
      end loop;
      Replace (Transfer, Pool, Pending, Frame, 100, OK);
      pragma Assert (not OK and BP.Displayed (Pool) = Held);
      Replace (Transfer, Pool, Pending, Frame, CP.ID'Last, OK);
      pragma Assert (not OK);
      Replace (Transfer, Pool, Pending, Frame, 101, OK);
      pragma Assert (OK and CP.Writable (Transfer) and BP.Displayed (Pool) = BP.None);
      pragma Assert (BP.Writable (Pool, Writer) and D.Count (Frame) = 0);
      pragma Assert (D.Covers (Pending, (0, 0, 8, 8)) and D.Covers (Pending, (32, 32, 34, 34)));
      D.Capture (Pending, Frame, OK); pragma Assert (OK);
      BP.Start_Render (Pool, Writer); BP.Finish_Render (Pool, Writer, BP.Completed);
      BP.Present (Pool, Held); BP.Acquire (Pool, Writer);
      CP.Prepare (Transfer, 2, OK); pragma Assert (OK);
      CP.Submitted (Transfer, True, 101);
      D.Add (Pending, (40, 40, 42, 42));
      Replace (Transfer, Pool, Pending, Frame, 102, OK);
      pragma Assert (not OK and BP.Displayed (Pool) = Held); -- accepted frame immutable
   end loop;
   Ada.Text_IO.Put_Line ("FRESH-PRESENT: PASS 1000 replacements, 32000 updates, deadline and in-flight guards");
end Frame_Replacement_Tests;

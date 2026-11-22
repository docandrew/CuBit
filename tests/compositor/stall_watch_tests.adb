with Ada.Text_IO; with Interfaces;
with Compositor_Stall_Watch;
-- Scenarios from the NUC run: serialized cold uploads with frames
-- interleaved must never count as a stall; idle time never counts; only a
-- failing run with no upload progress for the whole deadline does.
procedure Stall_Watch_Tests is
   package W renames Compositor_Stall_Watch;
   use type Interfaces.Unsigned_64;
   Deadline : constant := 2_000;
   S : W.State; Stalled : Boolean; Uploads : Interfaces.Unsigned_64 := 0;
   Checks : Natural := 0;
   procedure Check (C : Boolean; What : String) is
   begin
      if not C then Ada.Text_IO.Put_Line ("FAIL " & What); raise Program_Error; end if;
      Checks := Checks + 1;
   end Check;
begin
   -- 1. Thirty glyph warm-up retries in one second, each with an upload: never.
   for N in 1 .. 30 loop
      Uploads := Uploads + 1;
      W.Retried (S, 1_000 + 33 * Interfaces.Unsigned_64 (N), Uploads, Deadline, Stalled);
      Check (not Stalled, "cold uploads are progress");
   end loop;
   W.Completed (S);
   -- 2. Eight retries without uploads, but a frame completes after them,
   --    repeatedly for 10 s (the old 8-frame rule fired here): never.
   for Second in 0 .. 9 loop
      for N in 1 .. 8 loop
         W.Retried (S, 5_000 + 1_000 * Interfaces.Unsigned_64 (Second) + Interfaces.Unsigned_64 (N), Uploads, Deadline, Stalled);
         Check (not Stalled, "retries between published frames");
      end loop;
      W.Completed (S);
   end loop;
   -- 3. Long idle, then a single failing capture: the window opens now.
   W.Retried (S, 600_000, Uploads, Deadline, Stalled);
   Check (not Stalled, "idle time is not a stall");
   -- 4. Failing for 1999 ms: not yet; at 2000 ms with no upload: stalled.
   W.Retried (S, 601_999, Uploads, Deadline, Stalled); Check (not Stalled, "before deadline");
   W.Retried (S, 602_000, Uploads, Deadline, Stalled); Check (Stalled, "deadline passed without progress");
   -- 5. An upload at the last moment restarts the window.
   W.Completed (S);
   W.Retried (S, 700_000, Uploads, Deadline, Stalled);
   Uploads := Uploads + 1;
   W.Retried (S, 701_999, Uploads, Deadline, Stalled); Check (not Stalled, "upload progress");
   W.Retried (S, 703_998, Uploads, Deadline, Stalled); Check (not Stalled, "window restarted at progress");
   W.Retried (S, 703_999, Uploads, Deadline, Stalled); Check (Stalled, "deadline from last progress");
   -- 6. A clock that goes backwards restarts instead of underflowing.
   W.Retried (S, 10, Uploads, Deadline, Stalled); Check (not Stalled, "clock regression");
   -- 7. Failures 2 s apart are not a continuous failing run.
   W.Completed (S);
   for N in 1 .. 10 loop
      W.Retried (S, 900_000 + 2_000 * Interfaces.Unsigned_64 (N), Uploads, Deadline, Stalled);
      Check (not Stalled, "sparse failures");
   end loop;
   -- 8. Continuous failures every 16 ms for 2 s without uploads: stalled.
   W.Completed (S);
   for N in 0 .. 125 loop
      W.Retried (S, 950_000 + 16 * Interfaces.Unsigned_64 (N), Uploads, Deadline, Stalled);
      Check (Stalled = (16 * N >= Deadline), "continuous failures");
   end loop;
   Ada.Text_IO.Put_Line ("PASS stall watch:" & Checks'Image &
     " checks; warm-up uploads, interleaved frames and idle time never stall; 2000 ms without progress does");
end Stall_Watch_Tests;

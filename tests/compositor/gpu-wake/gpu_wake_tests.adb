with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Interfaces; use Interfaces;
with CuBit.Messages;
with Desktop_GPU_Wake;
with Desktop_GPU_Wake_Hook;
with Desktop_Logs;
--  GPU-001 step 3 follow-up, Linux-hosted: Desktop's GPU completion wait
--  (Desktop_GPU_Wake, the production unit) against a scripted glue hook.
--  It must sleep on the event when armed, never look again twice in a row
--  (no spin when the pending work is not the GPU's), keep the timer when no
--  event exists, hand fresh increasing tokens, consume only its own answer,
--  and report once per second.
procedure GPU_Wake_Tests is
   package W renames Desktop_GPU_Wake;
   package H renames Desktop_GPU_Wake_Hook;
   use type W.Wait_Mode;
   Failures, Checks : Natural := 0;
   Sequence : Unsigned_64 := 100;
   Now : Unsigned_64 := 5_000;
   Mode : W.Wait_Mode;
   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;
   function Answer (Token : Unsigned_64) return CuBit.Messages.CompletionEntry is
     ((token => Token, valid => True));
   Spins : Natural := 0;
   Previous : W.Wait_Mode := W.Timer;
begin
   W.Prepare_Wait (False, Sequence, Now, Mode);
   Check (Mode = W.Timer and then H.Arms = 0, "nothing rendering: timer, no arm");

   H.Next_Answer := H.Armed;
   W.Prepare_Wait (True, Sequence, Now, Mode);
   Check (Mode = W.Event and then H.Arms = 1, "rendering: arm and sleep on the event");
   W.Prepare_Wait (True, Sequence, Now, Mode);
   Check (Mode = W.Event and then H.Arms = 1, "armed already: no second arm");
   Check (not W.Matches (H.Armed_Token + 1), "a foreign token is not the wake's");
   W.Collect (Answer (H.Armed_Token + 1), Now);
   Check (W.Matches (H.Last_Token), "a foreign completion leaves it armed");
   W.Collect (Answer (H.Last_Token), Now);
   Check (not W.Matches (H.Last_Token) and then H.Armed_Token = 0, "its answer disarms it");
   W.Prepare_Wait (True, Sequence, Now, Mode);
   Check (Mode = W.Event and then H.Arms = 2, "re-armed with a new token");
   W.Collect (Answer (H.Last_Token), Now);

   -- Done already: look once, then the timer; never two looks in a row,
   -- however long the pending work is not the GPU's.
   H.Next_Answer := H.Idle;
   for Turn in 1 .. 1_000 loop
      W.Prepare_Wait (True, Sequence, Now, Mode);
      if Mode = W.Look_Again and then Previous = W.Look_Again then
         Spins := Spins + 1;
      end if;
      Previous := Mode;
   end loop;
   Check (Spins = 0, "no two immediate looks in a row");
   W.Prepare_Wait (False, Sequence, Now, Mode);
   H.Next_Answer := H.Idle;
   W.Prepare_Wait (True, Sequence, Now, Mode);
   Check (Mode = W.Look_Again, "a fresh wait looks again once when the GPU is done");

   H.Next_Answer := H.Unavailable;
   W.Prepare_Wait (True, Sequence, Now, Mode);
   Check (Mode = W.Timer, "no event: the 1 ms timer");
   Check (H.Tokens_Increasing, "every token fresh and increasing");

   -- Evidence: one line per period while anything happened.
   W.Woke (W.Event, Now, Now + 3);
   H.Next_Answer := H.Armed;
   W.Prepare_Wait (True, Sequence, Now, Mode);
   W.Woke (W.Event, Now, Now + W.Event_Fallback_Ms);   --  outlasted: capped
   Now := Now + 1_500;
   W.Prepare_Wait (False, Sequence, Now, Mode);
   declare
      Line : constant String := Ada.Strings.Unbounded.To_String (Desktop_Logs.Last_Line);
   begin
      Check (Desktop_Logs.Lines = 1, "one report line");
      Check (
             Ada.Strings.Fixed.Index (Line, "armed=3") > 0 and then
             Ada.Strings.Fixed.Index (Line, "event_capped=1") > 0 and then
             Ada.Strings.Fixed.Index (Line, "mesa-blocking: create=1/9us/9us") > 0,
             "report carries arms, the longest event sleep and Mesa's blocking calls");
   end;
   Now := Now + 1_500;
   W.Prepare_Wait (False, Sequence, Now, Mode);
   Check (Desktop_Logs.Lines = 1, "a quiet period writes nothing");
   --  Input hints: one hook call each, never a wait; sent and coalesced
   --  are both counted in the next report.
   for I in 1 .. 4 loop W.Input_Hint; end loop;
   Check (H.Hints = 4, "each input gives one hint call");
   Now := Now + 1_500;
   W.Prepare_Wait (False, Sequence, Now, Mode);
   Check (Desktop_Logs.Lines = 2 and then
          Ada.Strings.Fixed.Index (Ada.Strings.Unbounded.To_String (Desktop_Logs.Last_Line), "boost_hints=2/2") > 0,
          "report carries sent/coalesced hints");

   --  Waitboost off: the policy goes out once, at the next rendering wait,
   --  and input hints stop.
   W.Set_Waitboost (False);
   W.Input_Hint;
   Check (H.Hints = 4, "no hints while waitboost is off");
   W.Prepare_Wait (False, Sequence, Now, Mode);
   Check (H.Policies = 0, "policy waits for an open queue (rendering)");
   W.Prepare_Wait (True, Sequence, Now, Mode);
   W.Prepare_Wait (True, Sequence, Now, Mode);
   Check (H.Policies = 1 and not H.Last_Policy, "policy sent once, off");

   --  Abandoned renderer startup: no call into the glue from here on, not
   --  even a report, a hint or an arm while rendering.
   W.Set_Waitboost (True);
   W.Disable;
   declare
      Arms_Before : constant Natural := H.Arms;
      Lines_Before : constant Natural := Desktop_Logs.Lines;
      Reports_Before : constant Natural := H.Reports;
   begin
      W.Input_Hint;
      Now := Now + 1_500;
      W.Prepare_Wait (True, Sequence, Now, Mode);
      Check (H.Hints = 4 and then H.Policies = 1 and then H.Arms = Arms_Before,
             "disabled: no hint, policy or arm");
      Check (Mode = W.Timer, "disabled: the loop keeps its timer");
      Check (Desktop_Logs.Lines = Lines_Before and then H.Reports = Reports_Before,
             "disabled: no report from the glue");
   end;

   if Failures = 0 then
      Put_Line ("Desktop GPU wake PASS:" & Checks'Image & " checks");
   else
      Put_Line ("Desktop GPU wake FAILED:" & Failures'Image & " of" & Checks'Image);
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end GPU_Wake_Tests;

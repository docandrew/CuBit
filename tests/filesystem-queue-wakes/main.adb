--  Hosted tests for Queue_Wakes (filesystem.svc's wake requests,
--  docs/filesystem-protocol-v2.md step 1): every held wake is answered
--  exactly once, a request with answers waiting is answered at once, and
--  random sequences agree with a counting reference.
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Queue_Wakes; use Queue_Wakes;

procedure Main is
   Failures : Natural := 0;
   Checks : Natural := 0;

   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;

   type Event is (Request_Idle, Request_Busy, Hold_Fails, Answer_Posted, Queue_Ends);
   package Random_Events is new Ada.Numerics.Discrete_Random (Event);
   Generator : Random_Events.Generator;

   S : State := Idle;
   Outcome : Arrival;
   Held_Answered : Boolean;
begin
   --  The fixed cases.
   Arrive (S, Work_Waiting => True, Outcome => Outcome);
   Check (Outcome.Answer_Now and then not Outcome.Answer_Held and then S = Idle,
          "answers waiting: answered at once");
   Arrive (S, Work_Waiting => False, Outcome => Outcome);
   Check (not Outcome.Answer_Now and then not Outcome.Answer_Held and then S = Held,
          "nothing waiting: held");
   Arrive (S, Work_Waiting => False, Outcome => Outcome);
   Check (Outcome.Answer_Held and then not Outcome.Answer_Now and then S = Held,
          "a newer wake supersedes the held one");
   Posted (S, Held_Answered);
   Check (Held_Answered and then S = Idle, "an answer wakes the held one");
   Posted (S, Held_Answered);
   Check (not Held_Answered, "an answer with nothing held wakes nobody");
   Arrive (S, Work_Waiting => False, Outcome => Outcome);
   Ended (S, Held_Answered);
   Check (Held_Answered and then S = Idle, "the queue's end answers a held wake");
   Arrive (S, Work_Waiting => False, Outcome => Outcome);
   Hold_Failed (S);
   Check (S = Idle, "a failed hold holds nothing");
   Ended (S, Held_Answered);
   Check (not Held_Answered, "nothing held after a failed hold");

   --  Random sequences: holds = answers of held wakes + (1 if one is held).
   Random_Events.Reset (Generator, 20261008);
   for Round in 1 .. 1_000 loop
      declare
         Holds, Held_Answers : Natural := 0;
      begin
         S := Idle;
         for Step in 1 .. 200 loop
            case Random_Events.Random (Generator) is
               when Request_Idle | Request_Busy =>
                  declare
                     Busy : constant Boolean := Random_Events.Random (Generator) = Request_Busy;
                  begin
                     Arrive (S, Busy, Outcome);
                     if Outcome.Answer_Held then
                        Held_Answers := Held_Answers + 1;
                     end if;
                     Check (Outcome.Answer_Now = Busy, "answered at once exactly when busy");
                     if not Outcome.Answer_Now then
                        Holds := Holds + 1;
                     end if;
                  end;
               when Hold_Fails =>
                  if S = Held then
                     Hold_Failed (S);
                     Holds := Holds - 1;   --  never held after all
                  end if;
               when Answer_Posted =>
                  Posted (S, Held_Answered);
                  if Held_Answered then
                     Held_Answers := Held_Answers + 1;
                  end if;
               when Queue_Ends =>
                  Ended (S, Held_Answered);
                  if Held_Answered then
                     Held_Answers := Held_Answers + 1;
                  end if;
            end case;
            if Holds /= Held_Answers + (if S = Held then 1 else 0) then
               Check (False, "each held wake is answered exactly once");
            end if;
         end loop;
      end;
   end loop;

   if Failures = 0 then
      Put_Line ("QUEUE-WAKES:" & Checks'Image & " checks PASS");
   else
      Put_Line ("QUEUE-WAKES:" & Failures'Image & " of" & Checks'Image & " checks FAIL");
   end if;
end Main;

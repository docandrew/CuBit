--  Hosted tests for Desktop_Launch_Notice: refusal, early-end and launch
--  problem notices, their words, and the bounded watch of launched
--  children; and for Desktop_Launch_Request: procmgr's answer shapes, the
--  bounded wait and the released tokens (BUG-008).
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.Child_Exits;
with CuBit.Launch_Arguments;
with CuBit.Process_IDs; use CuBit.Process_IDs;
with CuBit.Launch_Handles;
with Desktop_Launch_Notice; use Desktop_Launch_Notice;
with Desktop_Launch_Request;

procedure Main is
   package LA renames CuBit.Launch_Arguments;
   package CE renames CuBit.Child_Exits;
   package LH renames CuBit.Launch_Handles;
   package LQ renames Desktop_Launch_Request;
   use type LQ.Reply_Kind;
   Reply_OK  : constant := 16#F000#;
   Reply_Err : constant := 16#F001#;
   Stale : LQ.Stale_Tokens;
   Failures : Natural := 0;
   Checks   : Natural := 0;

   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;

   --  Distinct identities for the watch (PID in the low half).
   function Child (N : Positive) return Process_ID is (From_Word (Unsigned_64 (N)));

   N : Notice;
   T : Watch_Table;
   Shown : Boolean;
begin
   Show_Refusal (N, "Penny", LA.Startup_Table_Missing);
   Check (N.Why = Refused and then Title (N) = "Penny could not start", "refusal title");
   Check (Name_Of (N) = "Penny", "the app's name");
   Check (Detail (N) = "Reason: it has no startup table (rebuild it).", "refusal detail");

   --  Names longer than a menu label are cut, never overrun.
   Show_Refusal (N, [1 .. 40 => 'x'], LA.Executable_Not_Found);
   Check (N.Length = Maximum_Name, "long name cut to the label bound");

   --  A child that opens a window started: its later end is not a failure.
   Watch (T, Child (10), "Files");
   Window_Opened (T, Child (10));
   Ended (T, N, (Process => Child (10), Kind => CE.Stopped, Code => 0), Shown);
   Check (not Shown, "an end after a window is not a startup failure");

   --  Stopped before a window: crashed or killed while starting.
   Watch (T, Child (11), "Penny");
   Ended (T, N, (Process => Child (11), Kind => CE.Stopped, Code => 0), Shown);
   Check (Shown and then Title (N) = "Penny stopped while starting", "early stop title");
   Check (Detail (N) = "It was stopped (crashed or killed) before it opened a window.",
          "early stop detail");
   --  Its watch ended: a second report is not shown again.
   Ended (T, N, (Process => Child (11), Kind => CE.Stopped, Code => 0), Shown);
   Check (not Shown, "one report per watched child");

   --  Exit code 0 without a window is a normal end; nonzero is a failure.
   Watch (T, Child (12), "Tool");
   Ended (T, N, (Process => Child (12), Kind => CE.Exited, Code => 0), Shown);
   Check (not Shown, "a clean exit is not a failure");
   Watch (T, Child (13), "Tool");
   Ended (T, N, (Process => Child (13), Kind => CE.Exited, Code => 3), Shown);
   Check (Shown and then Detail (N) = "It exited with code 3 before it opened a window.",
          "nonzero exit detail");

   --  Unknown processes (not launched from the menu) are ignored.
   Ended (T, N, (Process => Child (99), Kind => CE.Stopped, Code => 0), Shown);
   Check (not Shown, "unwatched children are not reported");

   --  Bounded: past capacity the oldest watch is replaced, others kept.
   for K in 1 .. Watch_Capacity + 1 loop
      Watch (T, Child (100 + K), "App");
   end loop;
   Ended (T, N, (Process => Child (100 + Watch_Capacity + 1), Kind => CE.Stopped, Code => 0), Shown);
   Check (Shown, "the newest watch is kept when full");
   Ended (T, N, (Process => Child (102), Kind => CE.Stopped, Code => 0), Shown);
   Check (Shown, "other watches survive a replacement");

   --  Every reason has a detail line.
   for Reason in LA.Launch_Failure loop
      Show_Refusal (N, "App", Reason);
      Check (Detail (N)'Length > String'("Reason: .")'Length, "detail for " & Reason'Image);
   end loop;

   --  procmgr's answers to OP_LAUNCH, as procmgr builds them.
   --  BUG-008: the started answer, child + rights + issuance outcome (the
   --  handle Not_Accepted: Desktop's submission accepts no capability).
   --  Desktop read only one-word answers, so every launch looked
   --  uncertain and blocked the next one.
   Check (LQ.Classify (True, Reply_OK, LH.Launched_Reply_Words,
                       [To_Word (Child (39)), LH.Word_Of (LH.All_Rights),
                        LH.Outcome_Word (LH.Not_Accepted), 0]) = LQ.Started,
          "started, its handle not accepted");
   Check (LQ.Classify (True, Reply_OK, LH.Launched_Reply_Words,
                       [To_Word (Child (40)), LH.Word_Of (LH.No_Rights),
                        LH.Outcome_Word (LH.Capacity), 0]) = LQ.Started,
          "started, no handle issued (table at its ceiling)");
   Check (LQ.Classify (True, Reply_OK, 1, [To_Word (Child (41)), 0, 0, 0]) = LQ.Unreadable,
          "a one-word started answer is not procmgr's shape");
   Check (LQ.Classify (True, Reply_OK, LH.Launched_Reply_Words,
                       [0, LH.Word_Of (LH.All_Rights), LH.Outcome_Word (LH.Done), 0]) = LQ.Unreadable,
          "started without a child");
   Check (LQ.Classify (True, Reply_OK, LH.Launched_Reply_Words,
                       [To_Word (Child (42)), LH.Rights_Word_Limit, LH.Outcome_Word (LH.Done), 0])
          = LQ.Unreadable, "started with an invalid rights word");
   Check (LQ.Classify (True, Reply_OK, LH.Launched_Reply_Words,
                       [To_Word (Child (42)), 1, 0, 0]) = LQ.Unreadable,
          "started without an outcome word");
   Check (LQ.Classify (False, Reply_OK, LH.Launched_Reply_Words,
                       [To_Word (Child (43)), 1, LH.Outcome_Word (LH.Done), 0]) = LQ.Unreadable,
          "an undelivered completion");
   Check (LQ.Classify (True, Reply_Err, 1,
                       [LA.Launch_Failure'Enum_Rep (LA.Not_Granted), 0, 0, 0]) = LQ.Refused,
          "refused with a reason");
   Check (LQ.Classify (True, Reply_Err, 4,
                       [LA.Launch_Failure'Enum_Rep (LA.Request_Withheld), 16#41#, 0, 0]) = LQ.Refused,
          "refused: a request withheld, its label after it");
   Check (LQ.Classify (True, Reply_Err, 1, [999, 0, 0, 0]) = LQ.Unreadable,
          "refused with no known reason");
   Check (LQ.Classify (True, 16#1234#, 1, [0, 0, 0, 0]) = LQ.Unreadable, "another label");

   --  The bounded wait.
   Check (not LQ.Overdue (1_000, 1_000 + LQ.Answer_Limit_Ms - 1), "waiting within the bound");
   Check (LQ.Overdue (1_000, 1_000 + LQ.Answer_Limit_Ms), "overdue at the bound");
   Check (not LQ.Overdue (Unsigned_64'Last - 10, Unsigned_64'Last - 1), "no overflow near the end");

   --  Released launches: an answer that still comes is recognised once.
   Check (not LQ.Contains (Stale, 0), "token 0 is never stale");
   for K in 1 .. LQ.Stale_Capacity loop
      Check (not LQ.Full (Stale), "room before" & K'Image);
      LQ.Remember (Stale, Unsigned_64 (100 + K));
   end loop;
   Check (LQ.Full (Stale), "full at capacity");
   Check (LQ.Contains (Stale, 101) and then LQ.Contains (Stale, 104), "each remembered");
   LQ.Forget (Stale, 102);
   Check (not LQ.Contains (Stale, 102) and then LQ.Contains (Stale, 103) and then not LQ.Full (Stale),
          "forgotten once its answer came; the others kept");
   LQ.Forget (Stale, 999);
   Check (LQ.Contains (Stale, 101), "forgetting an unknown token changes nothing");

   --  Desktop's own launch problems: never a launch without a word.
   Show_Problem (N, "Dictionary", Still_Starting, "Bible");
   Check (Title (N) = "Dictionary did not start", "still-starting title");
   Check (Detail (N) = "Bible is still starting. Try again in a moment.", "still-starting detail");
   Show_Problem (N, "Bible", Unanswered);
   Check (Title (N) = "Bible may not have started", "unanswered title");
   Check (Detail (N) = "procmgr did not answer within 75 seconds. The Desktop stopped waiting;" &
          " you can launch apps again.", "unanswered detail");
   Show_Problem (N, "Bible", Unreadable);
   Check (Title (N) = "Bible may not have started", "unreadable title");
   Show_Problem (N, "Files", Not_Sent);
   Check (Title (N) = "Files could not start" and then
          Detail (N) = "Reason: the Desktop could not send the launch request to procmgr.",
          "not-sent notice");

   if Failures = 0 then
      Put_Line ("desktop-launch-notice:" & Checks'Image & " checks PASS");
   else
      Put_Line ("desktop-launch-notice:" & Failures'Image & " of" & Checks'Image &
                " checks FAILED");
      raise Program_Error;
   end if;
end Main;

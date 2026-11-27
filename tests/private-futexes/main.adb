--  Hosted tests of the private futex table (Private_Futexes) and of the
--  word protocol over it (Word_Protocol).
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Private_Futexes; use Private_Futexes;
with Word_Protocol;

procedure Main is
   Failures : Natural := 0;

   procedure Check (Condition : Boolean; What : String) is
   begin
      if Condition then
         Put_Line ("PASS " & What);
      else
         Put_Line ("FAIL " & What);
         Failures := Failures + 1;
      end if;
   end Check;

   T : Table;
   Taken : Batch;
   Count : Batch_Count;
   Found, Removed : Boolean;
   S : Slot;
begin
   Enqueue (T, 5, 16#100#, 50);
   Enqueue (T, 2, 16#200#, 20);
   Enqueue (T, 9, 16#100#, 90);
   Enqueue (T, 1, 16#100#, 10);
   Find_Oldest (T, 16#100#, Found, S);
   Check (Found and then S = 5, "the oldest waiter of a word, not the lowest slot");
   Find_Oldest (T, 16#300#, Found, S);
   Check (not Found, "no waiter of another word");
   Take_Batch (T, 16#100#, 1, False, Taken, Count);
   Check (Count = 1 and then Taken (1) = 50 and then not T.Used (5), "wake one: the oldest");
   Take_Batch (T, 16#100#, 1, False, Taken, Count);
   Check (Count = 1 and then Taken (1) = 90, "then the next oldest");
   Remove (T, 1, 11, Removed);
   Check (not Removed and then T.Used (1), "removal of another thread's entry changes nothing");
   Remove (T, 1, 10, Removed);
   Check (Removed and then not T.Used (1), "removal of its own entry");
   Take_Batch (T, 16#100#, Chunk, True, Taken, Count);
   Check (Count = 0 and then T.Used (2), "wake of a word with no waiter takes nobody else");

   --  Batches: 40 waiters on one word, three batches of at most Chunk.
   for I in Slot range 10 .. 49 loop
      Enqueue (T, I, 16#400#, Thread_Ref (I) + 100);
   end loop;
   declare
      Total, Rounds : Natural := 0;
   begin
      loop
         Take_Batch (T, 16#400#, Chunk, True, Taken, Count);
         Total := Total + Count;
         Rounds := Rounds + 1;
         exit when Count < Chunk;
      end loop;
      Check (Total = 40 and then Rounds = 3, "a wake of all in batches of Chunk");
   end;
   Check (T.Used (2), "the other word's waiter stays");

   --  The protocol over the table.
   declare
      use Word_Protocol;
      P : State := Initial (0);
      Slept, Woke : Boolean;
   begin
      Wait_Load (P, 1, 0);
      Wait_Commit (P, 1, Slept);
      Check (Slept, "protocol: a waiter that saw its value sleeps");
      Wait_Load (P, 2, 0);
      Store (P, 3, 1);
      Wait_Commit (P, 2, Slept);
      Check (Slept and then P.Pending, "protocol: a store between load and commit leaves a wake owed");
      Wake_One (P, Woke);
      Check (Woke and then P.T (1).P = Running and then P.T (2).P = Sleeping,
             "protocol: wake one wakes the oldest sleeper");
      Wake_All (P);
      Check (P.T (2).P = Running and then not P.Pending, "protocol: wake all leaves no sleeper");
      Wait_Load (P, 3, 0);
      Wait_Commit (P, 3, Slept);
      Check (not Slept, "protocol: a stale expected value does not sleep");
   end;

   if Failures = 0 then
      Put_Line ("private-futexes: PASS");
   else
      Put_Line ("private-futexes: FAIL" & Natural'Image (Failures));
   end if;
end Main;

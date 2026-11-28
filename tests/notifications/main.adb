--  Hosted tests of the kernel's notification rules (Notification_Objects)
--  and of the two-level fan-in model over them (Fan_In_Model).
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Notification_Objects; use Notification_Objects;
with Fan_In_Model;

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

   N, R : Notification;
   Result : Signal_Result;
   Outcome : Wait_Outcome;
   Taken : Unsigned_64;
   Bind : Bind_Outcome;
   Removed : Boolean;
   W, B : Thread_Ref;
begin
   --  Semantics: OR of badges, cleared by Wait.
   Create (N, 3);
   Signal (N, 2#0001#, First, Result);
   Check (Result.What = Nothing and then N.Bits = 2#0001#, "signal with no waiter keeps the badge");
   Signal (N, 2#0100#, First, Result);
   Check (N.Bits = 2#0101#, "badges OR");
   Signal (N, 2#0001#, First, Result);
   Check (N.Bits = 2#0101#, "the same badge twice is one bit");
   Start_Wait (N, 7, True, Outcome, Taken);
   Check (Outcome = Got and then Taken = 2#0101# and then N.Bits = 0,
          "wait takes every pending bit and clears them");
   Start_Wait (N, 7, False, Outcome, Taken);
   Check (Outcome = Empty_Now and then Taken = 0, "wait Now with nothing pending");

   --  The single waiter; a second wait is Busy and steals nothing.
   Start_Wait (N, 7, True, Outcome, Taken);
   Check (Outcome = Blocked and then N.Waiter = 7, "a wait blocks as the waiter");
   Start_Wait (N, 8, True, Outcome, Taken);
   Check (Outcome = Busy and then N.Waiter = 7, "a second waiter is refused Busy");
   Signal (N, 16#80#, First, Result);
   Check (Result.What = Wake_Waiter and then Result.Thread = 7 and then Result.Taken = 16#80#
            and then N.Bits = 0 and then N.Waiter = No_Thread,
          "a wake hands the bits to the waiter");
   Start_Wait (N, 8, True, Outcome, Taken);
   Cancel_Wait (N, 9, Removed);
   Check (not Removed and then N.Waiter = 8, "cancel of another thread changes nothing");
   Cancel_Wait (N, 8, Removed);
   Check (Removed and then N.Waiter = No_Thread, "cancel removes the waiter");

   --  Bound thread: a signal with no waiter rings it, bits stay.
   Create (R, 3);
   Bind_Thread (R, 11, Bind, Result);
   Check (Bind = Bound_Now and then R.Is_Root, "bind a root to a thread");
   Bind_Thread (R, 12, Bind, Result);
   Check (Bind = Already, "a root has one bound thread");
   Signal (R, 4, First, Result);
   Check (Result.What = Ring_Bound and then Result.Thread = 11 and then R.Bits = 4,
          "no waiter: the bound thread is rung, the bits wait");
   Start_Wait (R, 11, True, Outcome, Taken);
   Check (Outcome = Got and then Taken = 4, "the bound thread takes them");

   --  Leaf -> root.
   Create (N, 3);
   Signal (N, 1, First, Result);
   Bind_Leaf (N, R, 1, (Id => 2, Gen => 1), 16#100#, Bind, Result);
   Check (Bind = Bound_Now and then Is_Leaf (N) and then R.Bits = 16#100#
            and then Result.What = Ring_Bound,
          "bits pending at binding reach the root at once");
   Start_Wait (R, 11, True, Outcome, Taken);
   Start_Wait (R, 11, True, Outcome, Taken);
   Check (Outcome = Blocked, "the root's thread waits");
   Signal (N, 2, First, Result);
   Check (Result.What = Forward and then Result.To = (2, 1) and then Result.To_Badge = 16#100#
            and then N.Bits = 3,
          "a leaf keeps its bits and forwards its root badge");
   Signal (R, Result.To_Badge, Second, Result);
   Check (Result.What = Wake_Waiter and then Result.Thread = 11 and then Result.Taken = 16#100#,
          "the root's waiter is woken with the leaf's badge");
   Start_Wait (N, 5, True, Outcome, Taken);
   Check (Outcome = Got and then Taken = 3, "the leaf's bits are taken from the leaf");
   Start_Wait (N, 5, True, Outcome, Taken);
   Check (Outcome = Busy, "a bound leaf has no blocking waiter");

   --  Two levels only.
   declare
      Other : Notification;
   begin
      Create (Other, 4);
      Bind_Leaf (R, Other, 2, (Id => 3, Gen => 1), 1, Bind, Result);
      Check (Bind = Not_Allowed, "a root never becomes a leaf");
      Bind_Leaf (Other, N, 3, (Id => 1, Gen => 1), 1, Bind, Result);
      Check (Bind = Not_Allowed, "a leaf never becomes a root");
      Bind_Leaf (N, Other, 1, (Id => 3, Gen => 1), 1, Bind, Result);
      Check (Bind = Already, "a leaf has one root");
      Bind_Thread (N, 13, Bind, Result);
      Check (Bind = Not_Allowed, "a leaf is not bound to a thread");
      declare
         Same : Notification := Other;
      begin
         Bind_Leaf (Other, Same, 3, (Id => 3, Gen => 1), 1, Bind, Result);
      end;
      Check (Bind = Not_Allowed, "no object is its own root");
      Bind_Leaf (Other, R, 3, (Id => 2, Gen => 1), 0, Bind, Result);
      Check (Bind = Not_Allowed, "a zero badge is refused");
      Signal (N, 1, Second, Result);
      Check (Result.What /= Forward, "the second step never forwards");
   end;

   Unbind_Leaf (N);
   Check (not Is_Leaf (N) and then N.Bits = 1, "unbind keeps the leaf's bits");
   Finish (R, W, B);
   Check (R = Empty and then B = 11, "an end hands back its bound thread");

   --  The model: a wake through the table.
   declare
      use Fan_In_Model;
      S : State := Initial;
      Woken, Rung : Thread_Ref;
   begin
      Create_Object (S, 1, 2);
      Create_Object (S, 2, 3);
      Fan_In_Model.Bind (S, 1, 2, 16#10#, Bind);
      Check (Bind = Bound_Now, "model: bind leaf 1 to root 2");
      Fan_In_Model.Wait (S, 2, 21, True, Outcome, Taken);
      Fan_In_Model.Wake (S, 1, 4, Woken, Rung, Taken);
      Check (Woken = 21 and then Taken = 16#10#, "model: a leaf's wake wakes the root's waiter");
      End_Object (S, 2);
      Fan_In_Model.Wake (S, 1, 8, Woken, Rung, Taken);
      Check (Woken = No_Thread and then (S.O (1).Bits and 8) = 8,
             "model: a stale root: the bits wait in the leaf");
      Create_Object (S, 2, 3);
      Fan_In_Model.Wait (S, 2, 21, True, Outcome, Taken);
      Fan_In_Model.Wake (S, 1, 8, Woken, Rung, Taken);
      Check (Woken = No_Thread and then S.O (2).Waiter = 21,
             "model: a new object in the root's place is not reached by the old link");
      Fan_In_Model.Wait (S, 1, 22, True, Outcome, Taken);
      Check (Outcome = Got and then not Is_Leaf (S.O (1)),
             "model: a wait on a leaf whose root ended unbinds it");
   end;

   --  Revocation by generation: revoke while waiting, then reuse.
   declare
      use Fan_In_Model;
      S : State := Initial;
      Woken, Rung, W : Thread_Ref;
      Done : Boolean;
      Old_Gen : Unsigned_32;
   begin
      Create_Object (S, 3, 4);
      Old_Gen := S.G (3);
      Wait_With (S, 3, Old_Gen, 30, True, Done, Outcome, Taken);
      Check (Done and then Outcome = Blocked, "model: a current Wait right waits");
      Revoke (S, 3, W);
      Check (W = 30 and then S.O (3).Waiter = No_Thread, "model: revoke tells the waiter");
      Wake_With (S, 3, Old_Gen, 1, Done, Woken, Rung, Taken);
      Check (not Done and then Woken = No_Thread and then S.O (3).Bits = 0,
             "model: a revoked Wake right changes nothing");
      Wake_With (S, 3, S.G (3), 1, Done, Woken, Rung, Taken);
      Check (Done and then S.O (3).Bits = 1, "model: the re-stamped right wakes");
      End_Object (S, 3);
      Create_Object (S, 3, 5);
      Wake_With (S, 3, Old_Gen + 1, 2, Done, Woken, Rung, Taken);
      Check (not Done and then S.O (3).Bits = 0,
             "model: a right of the slot's earlier object does not reach the new one");
   end;

   --  Regression (review 4, the revoke bypass): a binding is only as good
   --  as the Wake right that made it. Process B binds its leaf 1 to A's
   --  root 2 with a Wake right; A revokes 2. B's leaf no longer reaches 2
   --  (its wake unbinds it, the bits stay in it), B's stale right cannot
   --  bind again, and a fresh right binds and reaches 2. A root's end
   --  likewise unbinds a leaf at the leaf's next wake, which can then bind
   --  to another root.
   declare
      use Fan_In_Model;
      S : State := Initial;
      Woken, Rung, W : Thread_Ref;
      Done : Boolean;
      Old_Gen : Unsigned_32;
   begin
      Create_Object (S, 1, 2);
      Create_Object (S, 2, 3);
      Create_Object (S, 4, 3);
      Old_Gen := S.G (2);
      Fan_In_Model.Bind (S, 1, 2, 16#10#, Bind);
      Revoke (S, 2, W);
      Fan_In_Model.Wait (S, 2, 21, True, Outcome, Taken);
      Fan_In_Model.Wake (S, 1, 4, Woken, Rung, Taken);
      Check (Woken = No_Thread and then S.O (2).Waiter = 21 and then S.O (2).Bits = 0,
             "model: after a root's Revoke, a leaf bound with an old right does not reach it");
      Check (not Is_Leaf (S.O (1)) and then (S.O (1).Bits and 4) = 4,
             "model: that leaf is unbound at its wake, bits kept");
      Bind_With (S, 1, 2, Old_Gen, 16#10#, Done, Bind);
      Check (not Done and then not Is_Leaf (S.O (1)), "model: the old right binds nothing");
      Bind_With (S, 1, 2, S.G (2), 16#10#, Done, Bind);
      Check (Done and then Bind = Bound_Now and then S.O (2).Waiter = No_Thread,
             "model: a fresh right binds again, and the pending bits reach the root");
      Fan_In_Model.Wait (S, 2, 21, True, Outcome, Taken);
      Fan_In_Model.Wake (S, 1, 4, Woken, Rung, Taken);
      Check (Woken = 21 and then Taken = 16#10#, "model: the rebound leaf reaches the root's waiter");
      End_Object (S, 2);
      Fan_In_Model.Wake (S, 1, 8, Woken, Rung, Taken);
      Check (not Is_Leaf (S.O (1)) and then (S.O (1).Bits and 8) = 8,
             "model: an ended root's leaf is unbound at its wake, bits kept");
      Fan_In_Model.Bind (S, 1, 4, 16#20#, Bind);
      Check (Bind = Bound_Now, "model: and binds to another root");
   end;

   if Failures = 0 then
      Put_Line ("notifications: PASS");
   else
      Put_Line ("notifications: FAIL" & Natural'Image (Failures));
   end if;
end Main;

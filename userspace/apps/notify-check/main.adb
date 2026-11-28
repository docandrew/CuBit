------------------------------------------------------------------------------
--  Notifications and private futexes (IPC revision 2, S1), live in the
--  kernel: badges OR, Wait takes and clears, one waiter (a second is Busy),
--  deadlines, fan-in leaf -> root (two levels only), a root bound to a
--  thread rings its receive (the bridge to the old receive), the end of a
--  notification (Ended), the per-process limit, private futex words; then
--  the wake-flood benchmark: a victim's round trips through a server's
--  notification, alone and while hostile threads spin Wake on it.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System.Storage_Elements;
with CuBit.Console_Route;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Handles;
with CuBit.Handles.Notifications;
with CuBit.Benchmark_Clock;
with CuBit.Timing_Histograms;
with CuBit.Waits;
with CuBit.Monotonic;
with Wait_ABI;
with Channel_Wake_ABI;
with Futex_ABI;
with Notify_Check_Support; use Notify_Check_Support;

procedure Main is
   package NA renames CuBit.Handles.Notifications.ABI;
   use type NA.Outcome;
   LF : constant Character := ASCII.LF;
   Passed : Boolean := True;

   procedure Check (Condition : Boolean; Name : String) is
   begin
      debugPrint ("notify-check: " & Name & (if Condition then " PASS" else " FAIL") & LF);
      Passed := Passed and Condition;
   end Check;

   procedure Pause (Us : Unsigned_64) is
   begin
      CuBit.Waits.Sleep_Until (Now_Us + Us);
   end Pause;

   Self : constant CuBit.Handles.Process := N.Own_Process;
   A, Root, Leaf, Other : N.Waitable;
   W1, W4, Root_W, Leaf_W, Plain : N.Waker;
   Bits, Started : Unsigned_64;
   Exit_1 : aliased Word := 0;
   Msg : aliased Message := NULL_MESSAGE;
   Ok : Boolean;
begin
   CuBit.Console_Route.Route_Named ("logstore");

   --  Create, mint, OR, take and clear.
   Check (N.Create (Self, A) = NA.Ok and then N.Valid (A), "create a notification");
   Check (N.Mint_Waker (A, 1, False, W1) = NA.Ok and then N.Mint_Waker (A, 4, True, W4) = NA.Ok,
          "mint two badged wakers");
   Check (N.Mint_Waker (A, 0, False, Plain) = NA.Not_Allowed, "a zero badge is refused");
   Check (N.Wait (A, N.Now) = 0, "Now with nothing pending");
   Check (N.Wake (W1) = NA.Signalled and then N.Wake (W4) = NA.Signalled
          and then N.Wake (W1) = NA.Signalled, "three wakes");
   Check (N.Wait (A, N.Now) = 5, "Wait takes the OR of the badges");
   Check (N.Wait (A, N.Now) = 0, "and clears them");
   Check (N.Wake (CuBit.Handles.Notifications.To_Waker (0)) = NA.Refused, "a null waker is refused");

   --  A deadline with nothing pending.
   Started := Now_Us;
   Bits := N.Wait (A, Started + 20_000);
   Check (Bits = 0 and then Now_Us >= Started + 20_000, "a deadline expires with nothing");

   --  One waiter; a second is Busy and steals nothing.
   Target := A;
   Ok := Spawn (Wait_Once'Access, 0, Exit_1'Access);
   Check (Ok, "waiter thread started");
   while not Waiting loop
      null;
   end loop;
   Pause (5_000);
   Check (N.Wait (A, Now_Us + 2_000) = NA.Busy, "a second waiter is refused Busy");
   Check (N.Wake (W4) = NA.Signalled, "wake the waiter");
   Join (Exit_1'Access);
   Check (Target_Result = 4, "the waiter takes the badge");

   --  A wake from another thread ends a blocked Wait.
   By := W1;
   Delay_Us := 3_000;
   Ok := Spawn (Wake_Later'Access, 0, Exit_1'Access);
   Bits := N.Wait (A, Now_Us + 1_000_000);
   Join (Exit_1'Access);
   Check (Ok and then Bits = 1, "a blocked wait woken by another thread");

   --  Fan-in: leaf -> root, two levels only.
   Check (N.Create (Self, Root) = NA.Ok and then N.Create (Self, Leaf) = NA.Ok
          and then N.Create (Self, Other) = NA.Ok, "create root, leaf and another");
   Check (N.Mint_Waker (Root, 16#100#, False, Root_W) = NA.Ok
          and then N.Mint_Waker (Leaf, 16#2#, False, Leaf_W) = NA.Ok, "mint root and leaf wakers");
   Check (N.Bind_Leaf (Leaf, Root_W) = NA.Ok, "bind the leaf to the root");
   Check (N.Wake (Leaf_W) = NA.Signalled, "wake the leaf");
   Check (N.Wait (Root, N.Now) = 16#100#, "the root gets the leaf's root badge");
   Check (N.Wait (Leaf, N.Now) = 2, "the leaf keeps its own bits");
   Check (N.Wait (Leaf, Now_Us + 1_000) = NA.Busy, "a bound leaf has no blocking waiter");
   declare
      Other_W : N.Waker;
   begin
      Check (N.Mint_Waker (Other, 8, False, Other_W) = NA.Ok, "mint another's waker");
      Check (N.Bind_Leaf (Root, Other_W) = NA.Not_Allowed, "a root never becomes a leaf");
      Check (N.Bind_Leaf (Other, Leaf_W) = NA.Not_Allowed, "a leaf never becomes a root");
      Check (N.Bind_Leaf (Leaf, Other_W) = NA.Already, "a leaf has one root");
      Check (N.Bind_Thread (Leaf) = NA.Not_Allowed, "a leaf is not bound to a thread");
   end;

   --  A waiter on the root is woken through the leaf.
   Target := Root;
   Waiting := False;
   Ok := Spawn (Wait_Once'Access, 0, Exit_1'Access);
   while not Waiting loop
      null;
   end loop;
   Pause (5_000);
   Check (N.Wake (Leaf_W) = NA.Signalled, "wake the leaf with a waiter on the root");
   Join (Exit_1'Access);
   Check (Target_Result = 16#100#, "the root's waiter is woken by the leaf");
   Check (N.Wait (Leaf, N.Now) = 2, "the leaf's bits wait for it");

   --  A root bound to this thread rings its receive (the old receive).
   Check (N.Unbind_Leaf (Leaf) = NA.Ok, "unbind the leaf");
   Check (N.Bind_Thread (Root) = NA.Ok, "bind the root to this thread");
   Check (N.Bind_Thread (Other) = NA.Already, "a thread has one bound notification");
   By := Root_W;
   Delay_Us := 3_000;
   Ok := Spawn (Wake_Later'Access, 0, Exit_1'Access);
   Msg := NULL_MESSAGE;
   Bits := CuBit.Waits.Wait
     (Wait_ABI.Messages_Sources, CuBit.Monotonic.Milliseconds + 1_000,
      Unsigned_64 (System.Storage_Elements.To_Integer (Msg'Address)));
   Join (Exit_1'Access);
   Check (Bits /= Wait_ABI.Nothing and then Msg.tag.label = Channel_Wake_ABI.Wake_Label,
          "the bound thread's receive returns a doorbell");
   Check (N.Wait (Root, N.Now) = 16#100#, "the bits wait in the root");
   Check (N.Unbind_Thread (Root) = NA.Ok, "unbind the thread");

   --  The end: closing the Wait handle wakes its waiter Ended, and every
   --  waker of it answers Ended.
   Target := Other;
   Waiting := False;
   Ok := Spawn (Wait_Once'Access, 0, Exit_1'Access);
   while not Waiting loop
      null;
   end loop;
   Pause (5_000);
   Check (N.Close (Other), "close a notification with a waiter");
   Join (Exit_1'Access);
   Check (Target_Result = NA.Ended, "its waiter returns Ended");
   Check (N.Close (Leaf) and then N.Wake (Leaf_W) = NA.Ended, "a waker of an ended notification");
   Check (N.Wake (Root_W) = NA.Signalled, "the root lives on");
   Check (N.Wait (Root, N.Now) = 16#100#, "and its bits are its own");

   --  Revocation by generation: while a thread waits with the old Wait
   --  right, then a Waker of the old generation, then the slot reused.
   declare
      Old_W, New_W : N.Waker;
      Before : constant N.Waitable := Root;
   begin
      Check (N.Mint_Waker (Root, 16#20#, False, Old_W) = NA.Ok, "revoke: mint a waker");
      Target := Root;
      Waiting := False;
      Ok := Spawn (Wait_Once'Access, 0, Exit_1'Access);
      while not Waiting loop
         null;
      end loop;
      Pause (5_000);
      Check (N.Revoke (Root) = NA.Ok, "revoke while a thread waits");
      Join (Exit_1'Access);
      Check (Target_Result = NA.Revoked, "the waiter with the old Wait right wakes Revoked");
      Check (N.Wake (Old_W) = NA.Revoked, "a waker of the old generation answers Revoked");
      Check (N.Wait (Before, N.Now) = NA.Refused, "the old Wait handle is gone");
      Check (N.Mint_Waker (Root, 16#40#, False, New_W) = NA.Ok and then N.Wake (New_W) = NA.Signalled
             and then N.Wait (Root, N.Now) = 16#40#, "the re-stamped handle mints and waits");
      Check (N.Close (Root) and then N.Create (Self, Root) = NA.Ok, "close it, create again");
      Check (N.Wake (Old_W) /= NA.Signalled and then N.Wake (New_W) /= NA.Signalled,
             "no waker of an earlier object reaches a new one");
   end;

   --  Regression (review 4, the revoke bypass): a binding is only as good
   --  as the Wake handle that made it. B's leaf is bound with a waker of
   --  A's root; A revokes the root. B's leaf wake answers Revoked and A's
   --  waiter does not wake; the old waker binds nothing; a fresh waker
   --  binds again. The root's end likewise unbinds the leaf at its wake
   --  (Ended), which may then bind to another root.
   declare
      R2, L2, R3 : N.Waitable;
      R2_W, R2_Fresh, L2_W, R3_W : N.Waker;
   begin
      Check (N.Create (Self, R2) = NA.Ok and then N.Create (Self, L2) = NA.Ok
             and then N.Create (Self, R3) = NA.Ok
             and then N.Mint_Waker (R2, 16#200#, False, R2_W) = NA.Ok
             and then N.Mint_Waker (L2, 16#4#, False, L2_W) = NA.Ok
             and then N.Mint_Waker (R3, 16#400#, False, R3_W) = NA.Ok
             and then N.Bind_Leaf (L2, R2_W) = NA.Ok, "rebind: a root, a leaf, another root");
      Check (N.Revoke (R2) = NA.Ok, "rebind: revoke the root");
      Target := R2;
      Target_Result := 0;
      Waiting := False;
      Ok := Spawn (Wait_Once'Access, 0, Exit_1'Access);
      while not Waiting loop
         null;
      end loop;
      Pause (5_000);
      Check (N.Wake (L2_W) = NA.Revoked,
             "after the root's Revoke, a leaf bound with its old waker answers Revoked");
      Pause (5_000);
      Check (Target_Result = 0, "and the revoked root's waiter does not wake");
      Check (N.Bind_Leaf (L2, R2_W) = NA.Revoked_Object, "the root's old waker binds nothing");
      Check (N.Mint_Waker (R2, 16#200#, False, R2_Fresh) = NA.Ok
             and then N.Bind_Leaf (L2, R2_Fresh) = NA.Ok, "a fresh waker of the root binds the leaf");
      Join (Exit_1'Access);
      Check (Target_Result = 16#200#, "and the leaf's pending wake reaches the root's waiter");
      Check (N.Wait (L2, N.Now) = 4, "rebind: the leaf's own bits");
      Check (N.Wake (L2_W) = NA.Signalled and then N.Wait (R2, N.Now) = 16#200#
             and then N.Wait (L2, N.Now) = 4, "the rebound leaf reaches the root");
      Check (N.Close (R2), "rebind: close the root");
      Check (N.Wake (L2_W) = NA.Ended and then N.Wait (L2, N.Now) = 4,
             "an ended root's leaf is unbound at its wake (Ended): its bits wait in it");
      Check (N.Bind_Leaf (L2, R3_W) = NA.Ok and then N.Wake (L2_W) = NA.Signalled
             and then N.Wait (R3, N.Now) = 16#400#, "the leaf binds to another root");
      Ok := N.Close (L2) and then N.Close (R3);
   end;

   --  Regression (review 1): a binding racing the end of both of its
   --  objects (two threads close them) never locks an ended record.
   declare
      Races : constant := 300;
      Bound_Or_Ended : Boolean := True;
      Outcome : NA.Outcome;
   begin
      for I in 1 .. Races loop
         if N.Create (Self, Close_A) = NA.Ok and then N.Create (Self, Close_B) = NA.Ok
           and then N.Mint_Waker (Close_B, 1, False, Close_B_W) = NA.Ok
         then
            Ok := Spawn (Close_Both'Access, 0, Exit_1'Access);
            Outcome := N.Bind_Leaf (Close_A, Close_B_W);
            Bound_Or_Ended := Bound_Or_Ended
              and then Outcome in NA.Ok | NA.Ended_Object | NA.Refused_Call | NA.Revoked_Object;
            Join (Exit_1'Access);
         end if;
      end loop;
      Check (Bound_Or_Ended, "a binding racing the end of both objects is refused or bound, no fault");
   end;

   --  A misaligned private word: refused at the call, counted as nobody.
   Check (N.Private_Wake (System.Storage_Elements.To_Address (3), 1) = NA.Bad_Argument,
          "a private wake of an unaligned word is refused");
   Check (CuBit.Waits.Futex_Wake (CuBit.Waits.Private_Word (System.Storage_Elements.To_Address (3)), 1)
            = Futex_ABI.Wake_Refused,
          "the library answers it Wake_Refused, not a count");

   --  The per-process limit, then every one given back.
   declare
      Held : array (1 .. NA.Per_Process_Limit) of N.Waitable;
      Made : Natural := 0;
      Extra : N.Waitable;
   begin
      for I in Held'Range loop
         exit when N.Create (Self, Held (I)) /= NA.Ok;
         Made := Made + 1;
      end loop;
      --  Two are held already (A and Root).
      Check (Made = NA.Per_Process_Limit - 2, "create up to the per-process limit");
      Check (N.Create (Self, Extra) = NA.Limit_Reached, "one more is refused");
      for I in 1 .. Made loop
         Ok := N.Close (Held (I));
      end loop;
      Check (N.Create (Self, Extra) = NA.Ok and then N.Close (Extra), "closing gives the charge back");
   end;

   --  Private futex words.
   Gate := 7;
   Check (N.Private_Wait (Gate'Address, 6, N.Now) = NA.Retry, "a private word holding another value");
   Check (N.Private_Wait (Gate'Address, 7, N.Now) = NA.Timed_Out, "a private word holding the value, Now");
   Started := Now_Us;
   Check (N.Private_Wait (Gate'Address, 7, Started + 10_000) = NA.Timed_Out
          and then Now_Us >= Started + 10_000, "a private word deadline expires");
   Check (N.Private_Wait (System.Storage_Elements.To_Address (3), 0, N.Now) = NA.Bad_Argument,
          "an unaligned word is refused");
   Gate := 0;
   Waiting := False;
   Ok := Spawn (Sleep_On_Gate'Access, 0, Exit_1'Access);
   while not Waiting loop
      null;
   end loop;
   Pause (5_000);
   Gate := 1;
   Check (N.Private_Wake (Gate'Address, 1) = 1, "a private wake wakes its sleeper");
   Join (Exit_1'Access);
   Check (Gate_Result = NA.Woken, "the sleeper was woken");
   Check (N.Private_Wake (Gate'Address, 1) = 0, "a private wake with no sleeper");

   --  The wake-flood benchmark.
   declare
      Ticks_Per_Ms : Unsigned_64;
      Alone, Flooded : CuBit.Timing_Histograms.Histogram;
      Failed_Alone, Failed_Flooded : Natural;
      Server_Exit, Hostile_1, Hostile_2 : aliased Word := 0;
      Ignore : Unsigned_64;
      Turns_Before : Unsigned_64;
      Rounds : constant := 20_000;
   begin
      CuBit.Benchmark_Clock.Calibrate (Ticks_Per_Ms);
      Check (N.Create (Self, Server_In) = NA.Ok and then N.Create (Self, Victim_In) = NA.Ok
             and then N.Mint_Waker (Server_In, Victim_Badge, False, Victim_Ping) = NA.Ok
             and then N.Mint_Waker (Server_In, Hostile_Badge, False, Hostile_Ping) = NA.Ok
             and then N.Mint_Waker (Server_In, Stop_Badge, False, Stop_Ping) = NA.Ok
             and then N.Mint_Waker (Victim_In, Victim_Badge, False, Server_Reply) = NA.Ok,
             "wake-flood: notifications and wakers");
      Check (Spawn (Serve'Access, 0, Server_Exit'Access), "wake-flood: server started");
      Ping_Pong (2_000, Alone, Failed_Alone);   --  warm up
      Ping_Pong (Rounds, Alone, Failed_Alone);
      CuBit.Benchmark_Clock.Report ("wake-flood-alone-round-trip", Alone);
      Flooding := True;
      Check (Spawn (Flood'Access, 0, Hostile_1'Access) and then Spawn (Flood'Access, 0, Hostile_2'Access),
             "wake-flood: two hostile threads started");
      Pause (20_000);
      Turns_Before := Server_Turns;
      Ping_Pong (Rounds, Flooded, Failed_Flooded);
      Flooding := False;
      Join (Hostile_1'Access);
      Join (Hostile_2'Access);
      CuBit.Benchmark_Clock.Report ("wake-flood-flooded-round-trip", Flooded);
      debugPrint ("BENCH: wake-flood ticks_per_ms=" & Unsigned_64'Image (Ticks_Per_Ms)
                  & " hostile_wakes=" & Unsigned_64'Image (Hostile_Wakes)
                  & " server_turns_while_flooded=" & Unsigned_64'Image (Server_Turns - Turns_Before)
                  & " victim_rounds=" & Natural'Image (Rounds) & LF);
      Ignore := N.Wake (Stop_Ping);
      Join (Server_Exit'Access);
      Check (Failed_Alone = 0 and then Failed_Flooded = 0, "wake-flood: every victim round trip answered");
      Check (Hostile_Wakes > 0, "wake-flood: the hostile threads woke");
   end;

   debugPrint ("notify-check: " & (if Passed then "ALL PASS" else "FAIL") & LF);
end Main;

--  Root port lifecycle (XHCI_Port_Machine) and the PORTSC change
--  acknowledgement (XHCI_Registers.Acknowledge_Changes).
with Ada.Text_IO;
with Interfaces; use Interfaces;
with XHCI_Registers; use XHCI_Registers;
with XHCI_Port_Machine; use XHCI_Port_Machine;

procedure Port_Machine_Tests is
   S : Port_State;
   A : Action;
   Checks : Natural := 0;

   function Status (Connected, Enabled : Boolean; Changes : Unsigned_32 := 0;
                    Resetting : Boolean := False; Speed : Port_Speed := SPEED_FULL) return Unsigned_32 is
     ((if Connected then PORTSC_CCS else 0) or (if Enabled then PORTSC_PED else 0) or
      (if Resetting then PORTSC_PR else 0) or PORTSC_PP or Changes or
      Shift_Left (Unsigned_32 (Speed), PORTSC_SPEED_SHIFT));

   procedure Feed (Value : Unsigned_32; Now : Milliseconds) is
   begin
      Step (S, Observe (Value), Now, A);
      Checks := Checks + 1;
   end Feed;

   procedure Expect (P : Phase; Act : Action; Message : String) is
   begin
      if S.Current /= P or else A /= Act then
         Ada.Text_IO.Put_Line ("port-machine-tests: FAIL " & Message & " got " &
           Phase'Image (S.Current) & " " & Action'Image (A));
         raise Program_Error;
      end if;
   end Expect;

   Seed : Unsigned_32 := 16#9E37_79B9#;
   function Random return Unsigned_32 is
   begin
      Seed := Seed xor Shift_Left (Seed, 13);
      Seed := Seed xor Shift_Right (Seed, 17);
      Seed := Seed xor Shift_Left (Seed, 5);
      return Seed;
   end Random;
begin
   --  USB2 hotplug: connect, debounce 100 ms, reset, 10 ms recovery, ready.
   S := (others => <>);
   Feed (Status (True, False, PORTSC_CSC), 1_000);
   Expect (Debouncing, No_Action, "connect starts debounce");
   pragma Assert (Next_Deadline (S) = 1_100);
   Feed (Status (True, False), 1_099);
   Expect (Debouncing, No_Action, "no reset before 100 ms");
   Feed (Status (True, False), 1_100);
   Expect (Resetting, Start_Reset, "reset after a stable 100 ms");
   Feed (Status (True, False, Resetting => True), 1_110);
   Expect (Resetting, No_Action, "reset in progress");
   Feed (Status (True, True, PORTSC_PRC), 1_160);
   Expect (Recovering, No_Action, "reset done: recovery");
   pragma Assert (Next_Deadline (S) = 1_170);
   Feed (Status (True, True), 1_169);
   Expect (Recovering, No_Action, "no enumeration inside TRSTRCY");
   Feed (Status (True, True), 1_170);
   Expect (Ready, No_Action, "ready after 10 ms recovery");
   Claim (S);
   Finished (S, True);
   pragma Assert (S.Current = Attached);
   --  Unplug: release the slots, then empty.
   Feed (Status (False, False, PORTSC_CSC or PORTSC_PEC), 5_000);
   Expect (Releasing, Release_Slots, "disconnect releases slots");
   Released (S, Connected => False, Now => 5_001);
   pragma Assert (S.Current = Empty);

   --  Bounce during debounce restarts the stable interval.
   S := (others => <>);
   Feed (Status (True, False, PORTSC_CSC), 0);
   Feed (Status (True, False, PORTSC_CSC), 90);
   Feed (Status (True, False), 150);
   Expect (Debouncing, No_Action, "bounce restarted the interval");
   Feed (Status (True, False), 190);
   Expect (Resetting, Start_Reset, "stable 100 ms after the last bounce");
   --  A port that bounces for longer than the limit is given up.
   S := (others => <>);
   Feed (Status (True, False, PORTSC_CSC), 0);
   for T in 1 .. 25 loop
      Feed (Status (True, False, PORTSC_CSC), Milliseconds (T * 90));
      exit when S.Current /= Debouncing;
      pragma Assert (Milliseconds (T * 90) < Debounce_Limit_Ms);
   end loop;
   Expect (Failed, Give_Up, "bouncing port given up");

   --  USB3: link training enables the port; no reset.
   S := (others => <>);
   Feed (Status (True, True, PORTSC_CSC, Speed => SPEED_SUPER), 0);
   Feed (Status (True, True, Speed => SPEED_SUPER), 100);
   Expect (Recovering, No_Action, "enabled USB3 port skips the reset");
   --  A reset already underway is waited for, never restarted.
   S := (others => <>);
   Feed (Status (True, False, PORTSC_CSC), 0);
   Feed (Status (True, False, Resetting => True), 100);
   Expect (Resetting, No_Action, "existing reset not restarted");
   --  Reset that never completes.
   Feed (Status (True, False, Resetting => True), 100 + Reset_Limit_Ms);
   Expect (Failed, Give_Up, "reset timeout gives up");
   Feed (Status (False, False, PORTSC_CSC), 2_000);
   Expect (Releasing, Release_Slots, "failed port releases on unplug");
   Released (S, False, 2_000);

   --  Disconnect in every pre-enumeration phase goes straight to Empty.
   for P in Debouncing .. Ready loop
      S := (Current => P, others => <>);
      Feed (Status (False, False, PORTSC_CSC), 10);
      Expect (Empty, No_Action, "disconnect while " & Phase'Image (P));
   end loop;
   --  Disconnect while enumerating cancels via release.
   S := (Current => Enumerating, others => <>);
   Feed (Status (False, False, PORTSC_CSC), 10);
   Expect (Releasing, Release_Slots, "disconnect during enumeration");
   Released (S, True, 20);
   pragma Assert (S.Current = Debouncing, "reconnected meanwhile: start over");
   --  The controller disabling an attached port (PEC, PED clear) re-enumerates.
   S := (Current => Attached, others => <>);
   Feed (Status (True, False, PORTSC_PEC), 10);
   Expect (Releasing, Release_Slots, "port disabled under an attached device");
   --  A fast replug (new CSC while attached) re-enumerates.
   S := (Current => Attached, others => <>);
   Feed (Status (True, True, PORTSC_CSC), 10);
   Expect (Releasing, Release_Slots, "replug while attached");

   --  Software replug (the recovery ladder's last rung): release, then a
   --  still-connected device debounces, resets and re-enumerates.
   S := (Current => Attached, others => <>);
   Recover (S);
   pragma Assert (S.Current = Releasing);
   Released (S, Connected => True, Now => 100);
   Feed (Status (True, False), 200);
   Expect (Resetting, Start_Reset, "replug resets the port");

   --  Boot hand-over.
   pragma Assert (Booted (True, True, 0).Current = Attached);
   pragma Assert (Booted (True, False, 0).Current = Debouncing,
                  "a connected device without a slot is retried as hotplug");
   pragma Assert (Booted (False, False, 0).Current = Empty);

   --  Acknowledge_Changes: exactly the change bits read, never PED/PR/WPR/LWS.
   for I in 1 .. 1_000_000 loop
      declare
         V : constant Unsigned_32 := Random;
         W : constant Unsigned_32 := Acknowledge_Changes (V);
      begin
         pragma Assert ((W and (PORTSC_PED or PORTSC_PR or PORTSC_WPR or PORTSC_LWS)) = 0);
         pragma Assert ((W and PORTSC_CHANGE_BITS) = (V and PORTSC_CHANGE_BITS));
         pragma Assert ((W and PORTSC_PRESERVE) = (V and PORTSC_PRESERVE));
         pragma Assert ((W and not (PORTSC_CHANGE_BITS or PORTSC_PRESERVE)) = 0);
      end;
   end loop;
   Ada.Text_IO.Put_Line ("port-machine-tests: PASS" & Natural'Image (Checks) & " steps");
end Port_Machine_Tests;

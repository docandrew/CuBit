--  SPDX-License-Identifier: GPL-3.0-or-later
--  bench-lock-mutual (IDIOT-001, tests/headless/init-mailbox-mutual.ccl):
--  two copies, started one after the other (adjacent process slots, the
--  same generation), RESUME each other Rounds times at once. Each holds no
--  authority, so every call is refused. A kernel that looked the caller's
--  authority up under the target's mailbox lock took the two mailbox locks
--  in opposite orders: the two CPUs deadlocked, spinning with interrupts
--  off. Each copy names both neighbouring slots, so it reaches the other
--  whichever was started first.
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Handles;
with CuBit.Handles.Control;
with CuBit.Process_IDs;
with CuBit.Waits;
procedure Lock_Mutual is
   package H renames CuBit.Handles;
   Rounds : constant := 4_000;
   --  Both copies running before the rounds (no partner yet: refused as
   --  an invalid identity, never locked).
   Settle_Ms : constant := 500;
   Self : constant Unsigned_64 := Own_Process_Word;
   --  Process_Identities: the slot is the identity word's low bits.
   Next_Slot : constant Unsigned_64 := 1;
   --  The two neighbouring slots: the other copy is one of them.
   type Neighbours is array (1 .. 2) of Unsigned_64;
   Partners : constant Neighbours := [Self - Next_Slot, Self + Next_Slot];
   Accepted : Natural := 0;
   Ignored : Unsigned_64;
begin
   Ignored := CuBit.Waits.Sleep_For (Settle_Ms);
   debugPrint ("MAILBOX-MUTUAL: START rounds=" & Natural'Image (Rounds) & ASCII.LF);
   for Round in 1 .. Rounds loop
      for Partner of Partners loop
         if H.Control.Resume (CuBit.Process_IDs.From_Word (Partner), H.No_Process,
                              Max_Frames => 0, CPU_Quota => 0)
         then
            Accepted := Accepted + 1;
         end if;
      end loop;
   end loop;
   debugPrint ("MAILBOX-MUTUAL: done " & (if Accepted = 0 then "PASS" else "FAIL") &
               " rounds=" & Natural'Image (Rounds) & " accepted=" & Natural'Image (Accepted) &
               ASCII.LF);
end Lock_Mutual;

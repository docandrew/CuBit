--  ABI-002 slice 2 / KERN-013: Wait_Policy.Decide against an independent
--  reference for every Sources word up to 2**12 (all six source bits, the
--  readiness bit, IPC-010's futex modifiers, and unknown bits) with
--  deadlines Now, Forever and a finite one, and B, C in 0 .. 3;
--  Readiness_Publication's counter; Futex_ABI's shared words.
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Wait_ABI;
with Futex_ABI;
with Wait_Policy;
with Readiness_Publication;

procedure Wait_Tests is
   package W renames Wait_ABI;
   package P renames Wait_Policy;
   package RP renames Readiness_Publication;
   package FA renames Futex_ABI;
   use type P.Form, RP.Count;
   Failures, Checks : Natural := 0;

   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         if Failures < 20 then Put_Line ("FAIL: " & What); end if;
      end if;
   end Check;

   Finite : constant Unsigned_64 := 12_345;
   Deadlines : constant array (1 .. 3) of Unsigned_64 := [W.Now, W.Forever, Finite];

   function Reference (Sources, Deadline, B, C : Unsigned_64) return P.Form is
      R  : constant Unsigned_64 := 1;   -- requests
      E  : constant Unsigned_64 := 2;   -- events
      CO : constant Unsigned_64 := 4;   -- completions
      F  : constant Unsigned_64 := 8;   -- futex word
      O  : constant Unsigned_64 := 16;  -- offered capability
      T  : constant Unsigned_64 := 32;  -- timer
      RO : constant Unsigned_64 := 256; -- readiness only
      WF : constant Unsigned_64 := 512;  -- wake first (IPC-010)
      SF : constant Unsigned_64 := 1024; -- the waited word is shared
      WS : constant Unsigned_64 := 2048; -- the woken word is shared
   begin
      if Sources = R + E then
         return (if Deadline = W.Forever then P.Messages_Forever
                 elsif Deadline = W.Now then P.Messages_Now else P.Messages_Until);
      elsif Sources = R and then Deadline = W.Now then return P.Requests_Now;
      elsif Sources = E and then Deadline = W.Now then return P.Events_Now;
      elsif Sources = CO and then B > 0 and then Deadline = W.Forever and then C <= B then
         return P.Completions_Wait;
      elsif Sources = CO and then B > 0 and then Deadline = W.Now and then C = 0 then
         return P.Completions_Now;
      elsif Sources = R + E + CO + RO then return P.Ready;
      elsif Sources in F | F + SF | F + WF | F + WF + WS | F + SF + WF | F + SF + WF + WS then
         return P.Futex;
      elsif Sources = O and then Deadline = W.Now then return P.Offered;
      elsif Sources = T and then Deadline = W.Forever then return P.Timer;
      else return P.Unsupported;
      end if;
   end Reference;

   Form_Seen : array (P.Form) of Boolean := [others => False];
   type Slot_List is array (Positive range <>) of FA.Grant_Slot_Number;
   type Offset_List is array (Positive range <>) of FA.Grant_Offset;
   Slots   : constant Slot_List := [1, 2, 16#F_FFFF#];
   Offsets : constant Offset_List := [0, 4, 4096, 16#3F_FFFF_FFFC#];
   C : RP.Count;
begin
   for S in Unsigned_64 range 0 .. 2 ** 12 - 1 loop
      for D of Deadlines loop
         for B in Unsigned_64 range 0 .. 3 loop
            for CC in Unsigned_64 range 0 .. 3 loop
               declare
                  Got : constant P.Form := P.Decide (S, D, B, CC);
               begin
                  Check (Got = Reference (S, D, B, CC),
                         "Decide" & S'Image & D'Image & B'Image & CC'Image);
                  Form_Seen (Got) := True;
               end;
            end loop;
         end loop;
      end loop;
   end loop;
   for F in P.Form loop
      Check (Form_Seen (F), "form reachable: " & F'Image);
   end loop;
   --  Bits and named words.
   for S in W.Source loop
      Check (W.Bit (S) = 2 ** W.Source'Pos (S), "bit of " & S'Image);
   end loop;
   Check (W.Messages_Sources = W.Bit (W.Requests) + W.Bit (W.Events), "messages word");
   Check (W.Nothing /= W.Unsupported_Form and then W.Nothing /= 0, "Nothing is distinct");
   --  The futex form's modifiers.
   Check (W.Waits_Shared (W.Futex_Sources + W.Shared_Futex) and then
          not W.Waits_Shared (W.Futex_Sources + W.Wake_First + W.Wake_Shared), "shared wait bit");
   Check (W.Wakes_First (W.Futex_Sources + W.Wake_First) and then
          not W.Wakes_First (W.Futex_Sources + W.Shared_Futex), "wake-first bit");
   Check (W.Wakes_Shared (W.Futex_Sources + W.Wake_First + W.Wake_Shared) and then
          not W.Wakes_Shared (W.Futex_Sources + W.Wake_First), "shared wake bit");
   --  Shared words round trip.
   for Slot of Slots loop
      for Off of Offsets loop
         Check (FA.Slot_Of (FA.Shared_Word (Slot, Off)) = Slot and then
                FA.Offset_Of (FA.Shared_Word (Slot, Off)) = Off and then
                FA.Well_Formed (FA.Shared_Word (Slot, Off)), "shared word round trip");
      end loop;
   end loop;
   Check (not FA.Well_Formed (FA.Shared_Word (1, 2)) and then not FA.Well_Formed (16#10_0000#),
          "shared word: unaligned offset or slot 0 is malformed");
   --  The counter only goes up, one at a time, and stays at its largest.
   C := 0;
   for I in 1 .. 1000 loop
      declare Before : constant RP.Count := C;
      begin
         C := RP.Next (C);
         Check (C = Before + 1, "counter steps by one");
      end;
   end loop;
   Check (RP.Next (RP.Count'Last) = RP.Count'Last, "counter saturates");
   Check (RP.Behind (5, 4) and then not RP.Behind (4, 4) and then not RP.Behind (3, 4),
          "behind exactly when produced exceeds consumed");
   if Failures = 0 then
      Put_Line ("wait policy: PASS checks=" & Checks'Image);
   else
      Put_Line ("wait policy: FAIL" & Failures'Image & " of" & Checks'Image);
      raise Program_Error;
   end if;
end Wait_Tests;

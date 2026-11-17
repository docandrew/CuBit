--  ABI-002 slice 2 / KERN-013: Wait_Policy.Decide against an independent
--  reference for every Sources word up to 2**10 (all six source bits, the
--  readiness bit, and unknown bits) with deadlines Now, Forever and a
--  finite one, and B, C in 0 .. 3; and Readiness_Publication's counter.
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Wait_ABI;
with Wait_Policy;
with Readiness_Publication;

procedure Wait_Tests is
   package W renames Wait_ABI;
   package P renames Wait_Policy;
   package RP renames Readiness_Publication;
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
      elsif Sources = F then return P.Futex;
      elsif Sources = O and then Deadline = W.Now then return P.Offered;
      elsif Sources = T and then Deadline = W.Forever then return P.Timer;
      else return P.Unsupported;
      end if;
   end Reference;

   Form_Seen : array (P.Form) of Boolean := [others => False];
   C : RP.Count;
begin
   for S in Unsigned_64 range 0 .. 2 ** 10 - 1 loop
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

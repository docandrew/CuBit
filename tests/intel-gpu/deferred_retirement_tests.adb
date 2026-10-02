with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Deferred_Retirement; use Intel_GPU_Deferred_Retirement;
with Intel_GPU_Buffer_Backing;
procedure Deferred_Retirement_Tests is
   Object : Queue;
   Calls : Natural := 0;
   Expected : Candidate;
   Expected_Index : Slot := 1;
   Decision : Outcome := Waiting;
   function Attempt (Index : Slot; Item : Candidate) return Outcome is
   begin
      Calls := Calls + 1;
      pragma Assert (Index = Expected_Index and Item = Expected);
      return Decision;
   end Attempt;
   procedure Poll is new Intel_GPU_Deferred_Retirement.Poll (Attempt);
   procedure Sweep is
   begin
      for I in Slot loop Poll (Object); end loop;
   end Sweep;
begin
   Sweep; pragma Assert (Calls = 0);
   for Generation in Unsigned_64 range 1 .. 128 loop
      for Index in Slot loop
         Expected_Index := Index;
         Expected := ((Generation - 1) * Intel_GPU_Buffer_Backing.Ticket_Stride + Unsigned_64 (Index),
                      123, 456, 789, Generation * 16 + Unsigned_64 (Index));
         Remember (Object, Index, Expected);
         -- Duplicate or stale notifications cannot replace saved authority.
         declare Forged : Candidate := Expected; begin
            Forged.Stamp := 999;
            Remember (Object, Index, Forged);
            if Generation > 1 then
               Forged.Ticket := Forged.Ticket - Intel_GPU_Buffer_Backing.Ticket_Stride;
               Remember (Object, Index, Forged);
            end if;
         end;
         Calls := 0; Decision := Waiting;
         Sweep; pragma Assert (Calls = 1);
         Sweep; pragma Assert (Calls = 2);
         Decision := (if Index mod 2 = 0 then Discarded else Submitted);
         Sweep; pragma Assert (Calls = 3);
         Sweep; pragma Assert (Calls = 3); -- no retry after submission/discard
      end loop;
   end loop;
   Calls := 0;
   for Field in 1 .. 6 loop
      declare Invalid : Candidate := (1, 123, 456, 789, 1); begin
         case Field is
            when 1 => Invalid.Ticket := 0;
            when 2 => Invalid.Session := 0;
            when 3 => Invalid.Sender := 0;
            when 4 => Invalid.Handle := 0;
            when 5 => Invalid.Ticket := 2; -- wrong slot
            when 6 => Invalid.Ticket := Unsigned_64'Last;
         end case;
         Remember (Object, 1, Invalid);
         Sweep;
         pragma Assert (Calls = 0);
      end;
   end loop;
   Ada.Text_IO.Put_Line ("Deferred retirement PASS:2048 candidates, saved identity, bounded polling, no submission replay, invalid admission");
end Deferred_Retirement_Tests;

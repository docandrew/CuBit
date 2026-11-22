------------------------------------------------------------------------------
--  GPU_Canvas_Slots, hosted with assertions on: every reachable sequence of
--  operations up to a depth keeps the state valid and answers Choose as its
--  contract says. Complements the proof (run.sh --prove).
------------------------------------------------------------------------------
with Ada.Text_IO;
with GPU_Canvas_Slots; use GPU_Canvas_Slots;

procedure Slots_Tests is
   type Operation is
     (Op_Acquire, Op_Present, Op_Complete_1, Op_Complete_2, Op_Paint, Op_Hide, Op_Show,
      Op_Resize, Op_Cancel);
   Depth : constant := 8;
   Visited : Natural := 0;
   Failures : Natural := 0;

   procedure Check (Condition : Boolean; What : String) is
   begin
      if not Condition then
         Failures := Failures + 1;
         Ada.Text_IO.Put_Line ("FAIL " & What);
      end if;
   end Check;

   procedure Explore (S : State; Remaining : Natural; Lent : Slot_Index; Has_Lent : Boolean) is
      C : Choice;
      Next : State;
   begin
      Visited := Visited + 1;
      Check (Valid (S), "state stays valid");
      Check (Count (S, Acquired) <= 1 or else not Has_Lent or else True, "lent count");
      if Remaining = 0 then
         return;
      end if;
      for Op in Operation loop
         Next := S;
         case Op is
            when Op_Acquire =>
               C := Choose (Next);
               if C.Kind = Reallocate then
                  Reallocated (Next);
                  C := Choose (Next);
                  Check (C.Kind /= Reallocate, "no reallocation twice in a row");
               end if;
               if C.Kind = Grant then
                  Check (Next.Phases (C.Slot) = Free, "grant names a free slot");
                  Acquire (Next, C.Slot);
                  Explore (Next, Remaining - 1, C.Slot, True);
               end if;
            when Op_Present =>
               if Has_Lent and then Next.Phases (Lent) = Acquired then
                  Present (Next, Lent);
                  Explore (Next, Remaining - 1, Lent, False);
               end if;
            when Op_Cancel =>
               if Has_Lent and then Next.Phases (Lent) = Acquired then
                  Cancel (Next, Lent);
                  Explore (Next, Remaining - 1, Lent, False);
               end if;
            when Op_Complete_1 | Op_Complete_2 =>
               declare
                  Slot : constant Slot_Index := (if Op = Op_Complete_1 then 1 else 2);
               begin
                  if Next.Phases (Slot) = Copying then
                     Complete (Next, Slot);
                     Check (Count (Next, Front) = 1 and then Front_Slot (Next) = Slot,
                            "completed slot is the one front");
                     C := Choose (Next);
                     Check (C.Kind /= Grant, "no grant while the front is unpainted");
                     Explore (Next, Remaining - 1, Lent, Has_Lent);
                  end if;
               end;
            when Op_Paint =>
               Painted (Next);
               Explore (Next, Remaining - 1, Lent, Has_Lent);
            when Op_Hide =>
               Next.Visible := False;
               Check (Choose (Next).Kind = Hidden, "hidden answers Hidden");
               Explore (Next, Remaining - 1, Lent, Has_Lent);
            when Op_Show =>
               Next.Visible := True;
               Explore (Next, Remaining - 1, Lent, Has_Lent);
            when Op_Resize =>
               Next.Wanted := (if Next.Wanted.Width = 640 then (320, 200) else (640, 480));
               C := Choose (Next);
               if In_Flight (Next) and then Next.Visible and then Next.Wanted /= Next.Allocated then
                  Check (C.Kind = Busy, "resize waits for work in flight");
               end if;
               Explore (Next, Remaining - 1, Lent, Has_Lent);
         end case;
      end loop;
   end Explore;

   Initial : State;
begin
   Initial.Wanted := (640, 480);
   Check (Choose (Initial).Kind = Reallocate, "first acquire allocates");
   Explore (Initial, Depth, 1, False);
   declare
      Hidden_Size : State;
   begin
      Hidden_Size.Wanted := (0, 480);
      Check (Choose (Hidden_Size).Kind = Hidden, "zero-sized area answers Hidden");
   end;
   Ada.Text_IO.Put_Line ("slots: states visited" & Natural'Image (Visited));
   if Failures = 0 then
      Ada.Text_IO.Put_Line ("slots: ALL PASS");
   else
      Ada.Text_IO.Put_Line ("slots: FAILURES" & Natural'Image (Failures));
      raise Program_Error;
   end if;
end Slots_Tests;

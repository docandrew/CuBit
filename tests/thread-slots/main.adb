------------------------------------------------------------------------------
--  Hosted simulation of the thread-slot model (tests/thread-slots): random
--  creates and exits with contracts checked at run time, the invariant and
--  the lowest-free rule asserted after every step.
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Readiness_Publication;
with Thread_Slots;
with Slot_Protocol; use Slot_Protocol;

procedure Main is
   use type Readiness_Publication.Thread_Slot;
   package Pick is new Ada.Numerics.Discrete_Random (Thread);
   G : Pick.Generator;
   S : State := Initial;
   Steps : constant := 1_000_000;
   Creates, Refusals, Exits : Natural := 0;
   Created : Boolean;
   T : Thread;
begin
   Pick.Reset (G, 2026);
   --  Fill first: every slot taken, the rest refused.
   for U in Thread loop
      if not S.Live (U) then
         Create (S, U, Created);
         if Created then Creates := Creates + 1; else Refusals := Refusals + 1; end if;
         pragma Assert (Inv (S));
      end if;
   end loop;
   pragma Assert (Refusals = Model_Threads - Readiness_Publication.Maximum_Threads);
   for Step in 1 .. Steps loop
      T := Pick.Random (G);
      if not S.Live (T) then
         declare
            Before : constant Thread_Slots.Slot_Set := S.Slots;
         begin
            Create (S, T, Created);
            if Created then
               Creates := Creates + 1;
               --  The lowest free slot.
               pragma Assert (not Before (S.Owner (T)) and then
                 (for all U in Thread_Slots.Slot'First .. S.Owner (T) =>
                    U = S.Owner (T) or else Before (U)));
            else
               Refusals := Refusals + 1;
               pragma Assert (for all U in Thread_Slots.Slot => Before (U));
            end if;
         end;
      elsif T /= Slot_Protocol.Main then
         Finish (S, T);
         Exits := Exits + 1;
      end if;
      pragma Assert (Inv (S));
      --  Slots taken = live threads (the kernel's limit agrees with them).
      declare
         Live_Threads : Natural := 0;
      begin
         for U in Thread loop
            if S.Live (U) then Live_Threads := Live_Threads + 1; end if;
         end loop;
         pragma Assert (Thread_Slots.Taken_Count (S.Slots) = Live_Threads);
      end;
   end loop;
   Ada.Text_IO.Put_Line ("thread-slots: PASS" & Steps'Image & " steps," & Creates'Image
                         & " creates," & Refusals'Image & " refused (all taken)," & Exits'Image & " exits");
end Main;

--  Hosted tests of the per-CPU deadline heap (Wait_Deadlines): random
--  inserts and removals against a plain array, popping in deadline order.
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Wait_Deadlines; use Wait_Deadlines;

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

   H : Heap;
   Model : array (Thread_Ref) of Unsigned_64 := [others => Unsigned_64'Last];
   Seed : Unsigned_64 := 16#9E37_79B9_7F4A_7C15#;
   T : Thread_Ref;
   Last_Key : Unsigned_64 := 0;
   In_Order, Exact : Boolean := True;

   function Next return Unsigned_64 is
   begin
      Seed := Seed xor Shift_Left (Seed, 13);
      Seed := Seed xor Shift_Right (Seed, 7);
      Seed := Seed xor Shift_Left (Seed, 17);
      return Seed;
   end Next;
begin
   for Step in 1 .. 200_000 loop
      T := Thread_Ref (1 + Next mod 1023);
      if Contains (H, T) then
         Remove (H, T);
         Model (T) := Unsigned_64'Last;
      else
         Model (T) := Next mod 1_000_000;
         Insert (H, T, Model (T));
      end if;
   end loop;
   for U in Thread_Ref range 1 .. Thread_Ref'Last loop
      if Contains (H, U) /= (Model (U) /= Unsigned_64'Last)
        or else (Contains (H, U) and then Key_Of (H, U) /= Model (U))
      then
         Exact := False;
      end if;
   end loop;
   Check (Exact, "membership and deadlines match after 200000 random steps");
   loop
      Pop_Due (H, Unsigned_64'Last, T);
      exit when T = No_Thread;
      if Model (T) < Last_Key then
         In_Order := False;
      end if;
      Last_Key := Model (T);
      Model (T) := Unsigned_64'Last;
   end loop;
   Check (In_Order and then H.Size = 0, "pops come in deadline order and empty the heap");
   Check (Earliest (H) = No_Thread and then Earliest_Key (H) = Unsigned_64'Last, "empty heap");
   Insert (H, 5, 100);
   Insert (H, 6, 50);
   Pop_Due (H, 49, T);
   Check (T = No_Thread, "nothing due before the earliest deadline");
   Pop_Due (H, 50, T);
   Check (T = 6, "the earliest is due at its deadline");

   if Failures = 0 then
      Put_Line ("wait-deadlines: PASS");
   else
      Put_Line ("wait-deadlines: FAIL" & Natural'Image (Failures));
   end if;
end Main;

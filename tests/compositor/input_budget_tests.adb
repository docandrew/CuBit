with Ada.Text_IO;
with Client_Input_Budget;
procedure Input_Budget_Tests is
   package B renames Client_Input_Budget;
   use type B.Tick;
   S : B.Batch;
   Starts : constant array (Positive range <>) of B.Tick :=
     (0, 1, 2, 1_000, B.Tick'Last - 1, B.Tick'Last);
   Count : Natural := 0;
begin
   for Start of Starts loop
      for Now of Starts loop
         S := B.Open (Start);
         for Poll in 0 .. B.Poll_Limit loop
            declare
               Expected : constant Boolean := Poll < B.Poll_Limit and then
                 (Poll = 0 or else Now = Start);
            begin
               if B.Can_Poll (S, Now) /= Expected then
                  raise Program_Error with "poll admission mismatch";
               end if;
               Count := Count + 1;
            end;
            if Poll < B.Poll_Limit then B.Charge (S); end if;
         end loop;
      end loop;
   end loop;
   -- Endless ready input at a frozen clock still reaches rendering in 32
   -- polls, repeatedly. Admission never depends on the queue becoming empty.
   for Frame in 1 .. 10_000 loop
      S := B.Open (42);
      while B.Can_Poll (S, 42) loop
         B.Charge (S);
      end loop;
      if B.Used (S) /= B.Poll_Limit then raise Program_Error; end if;
   end loop;
   Ada.Text_IO.Put_Line ("PASS input budget" & Count'Image &
     " admission cases; 10000 sustained-input batches");
end Input_Budget_Tests;

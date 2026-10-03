with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_Log_Completion; use Intel_GPU_Log_Completion;
procedure Log_Completion_Tests is
begin
   for Label in Unsigned_64 range 0 .. 65535 loop
      for Minimum in Unsigned_64 range 0 .. 6 loop
         pragma Assert
           (Recoverable (True, False, Label, 4, 0, 0, Minimum, True) =
              ((Label in 16#F000# | 16#F009# and Minimum <= 5) or
               (Label = 16#F008# and Minimum = 0)));
      end loop;
   end loop;
   pragma Assert (not Recoverable (False, False, 16#F008#, 4, 0, 0, 0, True));
   pragma Assert (not Recoverable (True, True, 16#F008#, 4, 0, 0, 0, True));
   pragma Assert (not Recoverable (True, False, 16#F008#, 0, 0, 0, 0, True));
   pragma Assert (not Recoverable (True, False, 16#F008#, 4, 1, 0, 0, True));
   pragma Assert (not Recoverable (True, False, 16#F008#, 4, 0, 1, 0, True));
   pragma Assert (not Recoverable (True, False, 16#F008#, 4, 0, 0, 0, False));
   pragma Assert (not Recoverable (True, False, 16#F009#, 4, 0, 0, Unsigned_64'Last, True));
   Ada.Text_IO.Put_Line ("GPU log completion PASS: acknowledged rate-limit is recoverable, ambiguity is not");
end Log_Completion_Tests;

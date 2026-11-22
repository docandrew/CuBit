------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
package body Intel_GPU_Wait_Heartbeat with SPARK_Mode is

   procedure Pulse (M : in out Monitor; Now : Microseconds; Line : out Line_Kind) is
      Same_Wait : constant Boolean :=
        M.Active and then Now >= M.Last_Pulse and then Now - M.Last_Pulse < Wait_Gap_Us;
   begin
      Line := None;
      if not Same_Wait then
         M := (Active => True, Started => Now, Last_Pulse => Now, Last_Line => Now,
               Lines => 0, Polls => 1);
         return;
      end if;
      M.Last_Pulse := Now;
      if M.Polls < Poll_Count'Last then
         M.Polls := M.Polls + 1;
      end if;
      if M.Lines = 0 then
         if Elapsed (M, Now) >= Slow_Threshold_Us then
            M.Lines := 1;
            M.Last_Line := Now;
            Line := Entry_Line;
         end if;
      elsif M.Lines < Lines_Per_Wait and then Now >= M.Last_Line
        and then Now - M.Last_Line >= Period_Us
      then
         M.Lines := M.Lines + 1;
         M.Last_Line := Now;
         Line := Progress_Line;
      end if;
   end Pulse;

end Intel_GPU_Wait_Heartbeat;

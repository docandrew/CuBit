with Interfaces; use Interfaces;
with Ada.Text_IO;
with Intel_GPU_DC_Write;
procedure DC_Write_Tests is
   procedure Periodic (Read_Limit, Write_Limit : Positive; Write_Exhaustion : Boolean) is
      Reads, Writes : Natural := 0;
      function Read return Unsigned_32 is
      begin
         Reads := Reads + 1;
         return (if Reads mod 7 = 0 then 2 else 0);
      end Read;
      procedure Write (Value : Unsigned_32; Success : out Boolean) is
      begin
         pragma Assert (Value = 0);
         Writes := Writes + 1; Success := True;
      end Write;
      procedure Pause is null;
      package DC is new Intel_GPU_DC_Write (Read, Write, Pause);
      use type DC.Outcome;
      Result : DC.Report;
   begin
      DC.Execute (True, 0, Read_Limit, Write_Limit, Result);
      if Write_Exhaustion then
         pragma Assert (Result.Status = DC.Write_Budget_Exhausted and Reads = 21 and Writes = 3);
      else
         pragma Assert (Result.Status = DC.Read_Budget_Exhausted and Reads = 20 and Writes = 3);
      end if;
      pragma Assert (Result.Status /= DC.Stable_Register);
   end Periodic;
   procedure Run (Glitch_At, Bad_At, Fail_Write, Read_Budget, Write_Budget : Natural) is
      Reads, Writes, Pauses : Natural := 0;
      Target : constant Unsigned_32 := 16#0030_0000#;
      function Read return Unsigned_32 is
      begin
         Reads := Reads + 1;
         if Reads = Bad_At then return Unsigned_32'Last; end if;
         return (if Reads = Glitch_At then Target or 2 else Target);
      end Read;
      procedure Write (Value : Unsigned_32; Success : out Boolean) is
      begin
         pragma Assert (Value = Target);
         Writes := Writes + 1;
         Success := Writes /= Fail_Write;
      end Write;
      procedure Pause is
      begin Pauses := Pauses + 1; end Pause;
      package DC is new Intel_GPU_DC_Write (Read, Write, Pause);
      use type DC.Outcome;
      Result : DC.Report;
      Expected_Reads, Expected_Writes, Matches : Natural := 0;
      Expected : DC.Outcome := DC.Read_Budget_Exhausted;
   begin
      DC.Execute (False, Target, Read_Budget, Write_Budget, Result);
      pragma Assert (Result.Status = DC.Rejected and Writes = 0 and Reads = 0);
      DC.Execute (True, Unsigned_32'Last, Read_Budget, Write_Budget, Result);
      pragma Assert (Result.Status = DC.Rejected and Writes = 0 and Reads = 0);
      -- Independent trace oracle: seven adjacent matches, reset by a glitch.
      Expected_Writes := 1;
      if Fail_Write = 1 then Expected := DC.Write_Failed;
      else
         for N in 1 .. Read_Budget loop
            Expected_Reads := N;
            if N = Bad_At then Expected := DC.Invalid_MMIO; exit; end if;
            if N = Glitch_At then
               Matches := 0;
               if Expected_Writes = Write_Budget then
                  Expected := DC.Write_Budget_Exhausted; exit;
               elsif N = Read_Budget then exit;
               end if;
               Expected_Writes := Expected_Writes + 1;
               if Expected_Writes = Fail_Write then Expected := DC.Write_Failed; exit; end if;
            else
               Matches := Matches + 1;
               if Matches = 7 then Expected := DC.Stable_Register; exit; end if;
            end if;
         end loop;
      end if;
      DC.Execute (True, Target, Read_Budget, Write_Budget, Result);
      pragma Assert (Result.Status = Expected);
      pragma Assert (Result.Reads = Expected_Reads and Reads = Expected_Reads);
      pragma Assert (Result.Writes = Expected_Writes and Writes = Expected_Writes);
      pragma Assert (Result.Consecutive = Matches);
      pragma Assert (Pauses <= Reads);
      DC.Execute (True, Target, Read_Budget, Write_Budget, Result);
      pragma Assert (Result.Status = DC.Rejected and Reads = Expected_Reads and Writes = Expected_Writes);
   end Run;
begin
   Periodic (20, 100, False);
   Periodic (100, 3, True);
   for Glitch in 0 .. 8 loop
      for Bad in 0 .. 8 loop
         for Fail in 0 .. 2 loop
            for Reads in 1 .. 15 loop
               for Writes in 1 .. 2 loop Run (Glitch, Bad, Fail, Reads, Writes); end loop;
            end loop;
         end loop;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("DC write PASS: 7290 combinations + 2 periodic-glitch budget cases");
end DC_Write_Tests;

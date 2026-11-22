with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_GGTT_Retire;
with Intel_GPU_GGTT_Reservations;
procedure GGTT_Retire_Tests is
   package R renames Intel_GPU_GGTT_Reservations;
   use type R.Result;
   Memory : array (Unsigned_64 range 0 .. 4) of Unsigned_64;
   Calls, Writes, Invalidations, Fail_At, Lose_At, Gates : Natural := 0;
   Bad_Read : Boolean := False;
   function Gate (First, Bytes : Unsigned_64) return Boolean is
   begin
      Gates := Gates + 1;
      return First = 4096 and Bytes = 3 * 4096 and
        (Lose_At = 0 or else Gates < Lose_At);
   end Gate;
   procedure Read_PTE (Index : Unsigned_64; Value : out Unsigned_64; OK : out Boolean) is
   begin
      pragma Assert (Index in 1 .. 3);
      Calls := Calls + 1;
      Value := Memory (Index); OK := Calls /= Fail_At;
      if Bad_Read and Writes > 0 then Value := 0; end if;
   end Read_PTE;
   procedure Write_PTE (Index, Value : Unsigned_64; OK : out Boolean) is
   begin
      pragma Assert (Index in 1 .. 3 and Value = 16#9001#);
      Calls := Calls + 1; Writes := Writes + 1;
      Memory (Index) := Value; -- failure may follow a posted write
      OK := Calls /= Fail_At;
   end Write_PTE;
   procedure Invalidate (OK : out Boolean) is
   begin
      pragma Assert (Writes = 3);
      Calls := Calls + 1; Invalidations := Invalidations + 1;
      OK := Calls /= Fail_At;
   end Invalidate;
   package Retire is new Intel_GPU_GGTT_Retire (Gate, Read_PTE, Write_PTE, Invalidate);
   use type Retire.Result;
   procedure Run (Failure, Lost : Natural; Malformed : Natural := 0) is
      Ledger : R.Ledger;
      Attempt : Retire.Attempt;
      OK : Boolean;
      Claim : R.Result;
      Status : Retire.Result;
      Count : Natural;
      First : Unsigned_64 := 4096;
      Bytes : Unsigned_64 := 3 * 4096;
      Scratch : Unsigned_64 := 16#9000#;
   begin
      Calls := 0; Writes := 0; Invalidations := 0; Gates := 0;
      Fail_At := Failure; Lose_At := Lost; Bad_Read := Malformed = 5;
      Memory := [16#DEAD#, 16#10001#, 16#11001#, 16#12001#, 16#BEEF#];
      R.Admit (Ledger, 4096, 4096, 16 * 4096, OK); pragma Assert (OK);
      R.Reserve (Ledger, 4096, 3 * 4096, Claim); pragma Assert (Claim = R.Reserved);
      pragma Assert (R.Has_Claim (Ledger, 4096, 3 * 4096));
      pragma Assert (not R.Has_Claim (Ledger, 4096, 4096));
      pragma Assert (not R.Has_Claim (Ledger, Unsigned_64'Last, 1));
      case Malformed is
         when 1 => Bytes := 4096;
         when 2 => Scratch := 16#11000#;
         when 3 => Memory (2) := 0;
         when 4 => First := 0;
         when 6 => Scratch := 0;
         when others => null;
      end case;
      Retire.Execute (Attempt, Ledger, First, 16#10000#, Bytes, Scratch, Status);
      if Failure = 0 and Lost = 0 and Malformed = 0 then
         pragma Assert (Status = Retire.Detached and Invalidations = 1 and Writes = 3);
         pragma Assert (Calls = 10);
      else
         pragma Assert (Status /= Retire.Detached);
         if Writes > 0 then pragma Assert (Status = Retire.Quarantined); end if;
         if Malformed in 1 .. 4 or Malformed = 6 then pragma Assert (Writes = 0); end if;
      end if;
      pragma Assert (Memory (0) = 16#DEAD# and Memory (4) = 16#BEEF#);
      pragma Assert (R.Count (Ledger) = 1 and R.Has_Claim (Ledger, 4096, 3 * 4096));
      pragma Assert (not R.Space_Free (Ledger, 4096, 3 * 4096));
      Count := Calls;
      Retire.Execute (Attempt, Ledger, First, 16#10000#, Bytes, Scratch, Status);
      pragma Assert (Status = Retire.Rejected and Calls = Count);
   end Run;
begin
   Run (0, 0);
   for Failure in 1 .. 10 loop Run (Failure, 0); end loop;
   -- 1 initial +2*3 preflight +2*3 writes +2*3 readback +1 final =20 gates.
   for Lost in 1 .. 20 loop Run (0, Lost); end loop;
   for Malformed in 1 .. 6 loop Run (0, 0, Malformed); end loop;
   Ada.Text_IO.Put_Line ("GGTT retirement PASS: exact claims, scratch alias rejection, all I/O/owner failures, retained claims, no replay (mock PTEs)");
end GGTT_Retire_Tests;

with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_GGTT_Reservations.Reclamation;
procedure GGTT_Reclamation_Tests is
   package R renames Intel_GPU_GGTT_Reservations;
   use type R.Result;
   Memory : array (Unsigned_64 range 0 .. 260) of Unsigned_64;
   Start : Unsigned_64 := 4;
   Calls, Writes, Invalidations, Gates, Fail_At, Lose_At : Natural := 0;
   Runs : Natural := 0;
   Bad_Read : Boolean := False;
   function Gate (First, Bytes : Unsigned_64) return Boolean is
   begin
      Gates := Gates + 1;
      return First = Start * 4096 and Bytes = 3 * 4096 and
        (Lose_At = 0 or else Gates < Lose_At);
   end Gate;
   procedure Read_PTE
     (Index : Unsigned_64; Value : out Unsigned_64; OK : out Boolean) is
   begin
      pragma Assert (Index in Start .. Start + 2);
      Calls := Calls + 1;
      Value := Memory (Index);
      OK := Calls /= Fail_At;
      if Bad_Read and Writes > 0 then Value := 0; end if;
   end Read_PTE;
   procedure Write_PTE (Index, Value : Unsigned_64; OK : out Boolean) is
   begin
      pragma Assert (Index in Start .. Start + 2 and Value = 16#9001#);
      Calls := Calls + 1;
      Writes := Writes + 1;
      Memory (Index) := Value; -- An unsuccessful posted write may take effect.
      OK := Calls /= Fail_At;
   end Write_PTE;
   procedure Invalidate (OK : out Boolean) is
   begin
      pragma Assert (Writes = 3);
      Calls := Calls + 1;
      Invalidations := Invalidations + 1;
      OK := Calls /= Fail_At;
   end Invalidate;
   package Reclaim is new R.Reclamation (Gate, Read_PTE, Write_PTE, Invalidate);
   use type Reclaim.Result;
   procedure Run
     (Total, Target : Positive; Failure, Lost : Natural;
      Malformed : Natural := 0)
   is
      Book : R.Ledger;
      Attempt : Reclaim.Attempt;
      Outcome : Reclaim.Result;
      Claim : R.Result;
      OK : Boolean;
      Old_Calls, Old_Gates : Natural;
      First : Unsigned_64 := Unsigned_64 (Target) * 4 * 4096;
      Bytes : Unsigned_64 := 3 * 4096;
      Scratch : Unsigned_64 := 16#9000#;
      Happy : constant Boolean := Failure = 0 and Lost = 0 and Malformed = 0;
   begin
      Runs := Runs + 1;
      Start := Unsigned_64 (Target) * 4;
      Calls := 0; Writes := 0; Invalidations := 0; Gates := 0;
      Fail_At := Failure; Lose_At := Lost; Bad_Read := Malformed = 5;
      Memory := [others => 16#DEADBEEF#];
      Memory (Start) := 16#10001#;
      Memory (Start + 1) := 16#11001#;
      Memory (Start + 2) := 16#12001#;
      R.Admit (Book, 4096, 4096, 259 * 4096, OK);
      pragma Assert (OK);
      for I in 1 .. Total loop
         R.Reserve (Book, Unsigned_64 (I) * 4 * 4096, 3 * 4096, Claim);
         pragma Assert (Claim = R.Reserved);
      end loop;
      case Malformed is
         when 1 => Bytes := 4096;
         when 2 => Scratch := 16#11000#;
         when 3 => Memory (Start + 1) := 0;
         when 4 => First := 0;
         when 6 => Scratch := 0;
         when 7 => Bytes := 0;
         when 8 => First := Unsigned_64'Last;
         when others => null;
      end case;
      Reclaim.Execute (Attempt, Book, First, 16#10000#, Bytes, Scratch, Outcome);
      if Happy then
         pragma Assert (Outcome = Reclaim.Released and Calls = 10 and Gates = 20);
         pragma Assert (Invalidations = 1 and Writes = 3);
         pragma Assert (R.Count (Book) = Total - 1);
         pragma Assert (R.Space_Free (Book, First, Bytes));
         pragma Assert (not R.Has_Claim (Book, First, Bytes));
      else
         pragma Assert (Outcome /= Reclaim.Released and R.Count (Book) = Total);
         if Writes > 0 then pragma Assert (Outcome = Reclaim.Quarantined); end if;
         pragma Assert (R.Has_Claim (Book, Start * 4096, 3 * 4096));
         pragma Assert (not R.Space_Free (Book, Start * 4096, 3 * 4096));
      end if;
      pragma Assert (R.Valid (Book));
      for I in 1 .. Total loop
         if I /= Target then
            pragma Assert (R.Has_Claim (Book, Unsigned_64 (I) * 4 * 4096, 3 * 4096));
         end if;
      end loop;
      for I in Memory'Range loop
         if I not in Start .. Start + 2 then
            pragma Assert (Memory (I) = 16#DEADBEEF#);
         end if;
      end loop;
      if Happy then
         -- Reallocate the same address. A stale attempt must not release it.
         R.Reserve (Book, First, Bytes, Claim);
         pragma Assert (Claim = R.Reserved and R.Count (Book) = Total);
      end if;
      Old_Calls := Calls; Old_Gates := Gates;
      Reclaim.Execute (Attempt, Book, First, 16#10000#, Bytes, Scratch, Outcome);
      pragma Assert (Outcome = Reclaim.Rejected and Calls = Old_Calls and Gates = Old_Gates);
      pragma Assert (R.Count (Book) = Total and R.Valid (Book));
      pragma Assert (R.Has_Claim (Book, Start * 4096, 3 * 4096));
   end Run;
begin
   for Total in 1 .. 64 loop
      for Target in 1 .. Total loop Run (Total, Target, 0, 0); end loop;
   end loop;
   -- Gate20 is AFTER successful invalidation: loss still retains the claim.
   for Target in 1 .. 64 loop
      for Failure in 1 .. 10 loop Run (64, Target, Failure, 0); end loop;
      for Lost in 1 .. 20 loop Run (64, Target, 0, Lost); end loop;
      for Malformed in 1 .. 8 loop Run (64, Target, 0, 0, Malformed); end loop;
   end loop;
   Ada.Text_IO.Put_Line ("GGTT reclamation PASS cases=" & Runs'Image &
     " exact deletion, preserved claims, fault retention, post-invalidation loss, ABA replay (mock PTEs)");
end GGTT_Reclamation_Tests;

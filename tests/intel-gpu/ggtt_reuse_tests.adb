with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_GGTT_Publish;
with Intel_GPU_GGTT_Reservations.Reclamation;
procedure GGTT_Reuse_Tests is
   package R renames Intel_GPU_GGTT_Reservations;
   Book : R.Ledger;
   Memory : array (Unsigned_64 range 1 .. 5) of Unsigned_64 :=
     [1 => 16#DEAD#, 5 => 16#BEEF#, others => 16#9001#];
   Prepared, Invalidated : Boolean := False;
   Calls : Natural := 0;
   function Gate (First, Bytes : Unsigned_64) return Boolean is
     (First >= 8192 and then First < 5 * 4096 and then Bytes > 0 and then
      Bytes <= 5 * 4096 - First);
   procedure Read_PTE (Index : Unsigned_64; Value : out Unsigned_64;
                       OK : out Boolean) is
   begin
      Calls := Calls + 1;
      pragma Assert (Index in 2 .. 4);
      Value := Memory (Index); OK := True;
   end Read_PTE;
   procedure Write_PTE (Index, Value : Unsigned_64; OK : out Boolean) is
   begin
      Calls := Calls + 1;
      pragma Assert (Index in 2 .. 4 and Prepared);
      Memory (Index) := Value; OK := True;
   end Write_PTE;
   procedure Prepare (First, Bytes : Unsigned_64; OK : out Boolean) is
   begin
      pragma Assert (First = 8192 and Bytes = 3 * 4096);
      Prepared := True; OK := True;
   end Prepare;
   procedure Invalidate (OK : out Boolean) is
   begin
      Calls := Calls + 1;
      Invalidated := True; OK := True;
   end Invalidate;
   package Publisher is new Intel_GPU_GGTT_Publish
     (Gate, Prepare, Read_PTE, Write_PTE, Invalidate);
   package Reclaimer is new R.Reclamation
     (Gate, Read_PTE, Write_PTE, Invalidate);
   use type R.Result;
   use type Publisher.Result;
   use type Reclaimer.Result;
   OK : Boolean;
   Claim : R.Result;
   Prior_Attempt : Reclaimer.Attempt;
   procedure Cycle (DMA : Unsigned_64; First_Cycle : Boolean) is
      Publish_Attempt : Publisher.Attempt;
      Retire_Attempt : Reclaimer.Attempt;
      Published : Publisher.Result;
      Retired : Reclaimer.Result;
      GPU : Unsigned_64;
      Old_Calls : Natural;
   begin
      Prepared := False; Invalidated := False;
      Publisher.Publish_Available
        (Publish_Attempt, Book, DMA, 3 * 4096, 4096, GPU, Published);
      pragma Assert (Published = Publisher.Published and GPU = 8192);
      pragma Assert (Prepared and Invalidated and R.Count (Book) = 3);
      for Page in Unsigned_64 range 0 .. 2 loop
         pragma Assert (Memory (2 + Page) = DMA + Page * 4096 + 1);
      end loop;
      if not First_Cycle then
         Old_Calls := Calls;
         Reclaimer.Execute (Prior_Attempt, Book, GPU, DMA, 3 * 4096, 16#9000#, Retired);
         pragma Assert (Retired = Reclaimer.Rejected and Calls = Old_Calls);
         pragma Assert (R.Has_Claim (Book, GPU, 3 * 4096));
      end if;
      Invalidated := False;
      if First_Cycle then
         Reclaimer.Execute (Prior_Attempt, Book, GPU, DMA, 3 * 4096, 16#9000#, Retired);
      else
         Reclaimer.Execute (Retire_Attempt, Book, GPU, DMA, 3 * 4096, 16#9000#, Retired);
      end if;
      pragma Assert (Retired = Reclaimer.Released and Invalidated);
      pragma Assert (R.Count (Book) = 2 and R.Valid (Book));
      pragma Assert (R.Has_Claim (Book, 4096, 4096) and R.Has_Claim (Book, 5 * 4096, 4096));
      pragma Assert (R.Space_Free (Book, GPU, 3 * 4096));
      pragma Assert (Memory (1) = 16#DEAD# and Memory (5) = 16#BEEF#);
      for Page in Unsigned_64 range 2 .. 4 loop
         pragma Assert (Memory (Page) = 16#9001#);
      end loop;
   end Cycle;
begin
   R.Admit (Book, 4096, 4096, 5 * 4096, OK); pragma Assert (OK);
   R.Reserve (Book, 4096, 4096, Claim); pragma Assert (Claim = R.Reserved);
   R.Reserve (Book, 5 * 4096, 4096, Claim); pragma Assert (Claim = R.Reserved);
   for I in 1 .. 1024 loop
      Cycle (16#100000# + Unsigned_64 (I) * 16#10000#, I = 1);
   end loop;
   Ada.Text_IO.Put_Line ("GGTT reuse PASS1024 publication/reclaim cycles, nonzero scratch overwrite, neighboring claims, stale attempt rejection (mock PTEs)");
end GGTT_Reuse_Tests;

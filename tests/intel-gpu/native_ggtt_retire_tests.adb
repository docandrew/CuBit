with Ada.Text_IO;
with Interfaces; use Interfaces;
with Interfaces.C;
with System;
with System.Storage_Elements; use System.Storage_Elements;
with Intel_GPU_Native_GGTT;
with Intel_GPU_Native_GuC_Invalidate;
with Intel_GPU_GGTT_Retire;
with Intel_GPU_GGTT_Reservations;
procedure Native_GGTT_Retire_Tests is
   package C renames Interfaces.C;
   package R renames Intel_GPU_GGTT_Reservations;
   use type System.Address;
   use type C.int;
   use type R.Result;
   function Mmap (Addr : System.Address; Length : C.size_t;
                  Prot, Flags, FD : C.int; Offset : C.long) return System.Address
     with Import, Convention => C, External_Name => "mmap";
   function Munmap (Addr : System.Address; Length : C.size_t) return C.int
     with Import, Convention => C, External_Name => "munmap";
   GGTT_Base : constant Integer_Address := 16#64000000#;
   Reset_Base : constant Integer_Address := 16#61206000#;
   GGTT_Map, Reset_Map : System.Address;
   type Entries is array (Natural range 0 .. 511) of Unsigned_64;
   type Registers is array (Natural range 0 .. 1023) of Unsigned_32;
   PTEs : Entries with Import, Volatile, Address => To_Address (GGTT_Base);
   Regs : Registers with Import, Volatile, Address => To_Address (Reset_Base);
   Baseline_Checks : Natural := 0;
   procedure Run (Lost : Natural := 0; Permit : Boolean := True;
                  Complete : Boolean := True) is
      Checks, Clocks : Natural := 0;
      function Owner return Boolean is
      begin
         Checks := Checks + 1;
         return Lost = 0 or else Checks < Lost;
      end Owner;
      function Size return Unsigned_64 is (2_097_152);
      function Allowed (Index, Value : Unsigned_64) return Boolean is
        (Permit and then Index in 1 .. 3 and then Value = 16#9001#);
      function Gate (First, Bytes : Unsigned_64) return Boolean is
        (Owner and then First = 4096 and then Bytes = 3 * 4096);
      package IO is new Intel_GPU_Native_GGTT (Owner, Size, Allowed);
      procedure Clock (Value : out Unsigned_64; OK : out Boolean) is
      begin
         Clocks := Clocks + 1;
         -- All actual mapped PTE writes/readbacks precede invalidation.
         -- Only the device's completion is simulated by this host fixture.
         for I in 1 .. 3 loop pragma Assert (PTEs (I) = 16#9001#); end loop;
         if Clocks = 3 then
            pragma Assert (Regs (16#EE8# / 4) = 1);
            if Complete then Regs (16#EE8# / 4) := 16#FFFFFFFE#; end if;
         end if;
         Value := Unsigned_64 (Clocks) * 1000;
         OK := True;
      end Clock;
      package Native_Wait is new Intel_GPU_Native_GuC_Invalidate (Owner, Clock);
      package Waiter renames Native_Wait.Completion;
      use type Waiter.Result;
      Wait_Attempt : Waiter.Attempt;
      procedure Invalidate (OK : out Boolean) is
         Status : Waiter.Result;
      begin
         Waiter.Execute (Wait_Attempt, Status);
         OK := Status = Waiter.Complete;
      end Invalidate;
      package Retirement is new Intel_GPU_GGTT_Retire
        (Gate, IO.Read_PTE, IO.Write_PTE, Invalidate);
      use type Retirement.Result;
      Attempt : Retirement.Attempt;
      Ledger : R.Ledger;
      Claim : R.Result;
      Status : Retirement.Result;
      OK : Boolean;
      Saved_Checks, Saved_Clocks : Natural;
      Saved_PTEs : Entries;
   begin
      PTEs := [others => 16#AB25AB25AB25AB25#];
      PTEs (1) := 16#10001#; PTEs (2) := 16#11001#; PTEs (3) := 16#12001#;
      Regs := [others => 16#A5A5A5A5#];
      R.Admit (Ledger, 2_097_152, 4096, 16 * 4096, OK); pragma Assert (OK);
      R.Reserve (Ledger, 4096, 3 * 4096, Claim); pragma Assert (Claim = R.Reserved);
      Retirement.Execute (Attempt, Ledger, 4096, 16#10000#, 3 * 4096, 16#9000#, Status);
      if Lost = 0 and Permit and Complete then
         pragma Assert (Status = Retirement.Detached and Clocks = 3);
         Baseline_Checks := Checks;
      else
         pragma Assert (Status /= Retirement.Detached);
         if Clocks > 0 or else PTEs (1) = 16#9001# then
            pragma Assert (Status = Retirement.Quarantined);
         end if;
      end if;
      if not Permit then
         pragma Assert (Clocks = 0 and PTEs (1) = 16#10001# and
           PTEs (2) = 16#11001# and PTEs (3) = 16#12001#);
      end if;
      for I in PTEs'Range loop
         if I not in 1 .. 3 then pragma Assert (PTEs (I) = 16#AB25AB25AB25AB25#); end if;
      end loop;
      for I in Regs'Range loop
         if I /= 16#EE8# / 4 then pragma Assert (Regs (I) = 16#A5A5A5A5#); end if;
      end loop;
      pragma Assert (R.Count (Ledger) = 1 and R.Has_Claim (Ledger, 4096, 3 * 4096));
      Saved_Checks := Checks; Saved_Clocks := Clocks; Saved_PTEs := PTEs;
      Retirement.Execute (Attempt, Ledger, 4096, 16#10000#, 3 * 4096, 16#9000#, Status);
      pragma Assert (Status = Retirement.Rejected and Checks = Saved_Checks and
        Clocks = Saved_Clocks and PTEs = Saved_PTEs);
   end Run;
begin
   -- Hints only; do not overwrite an existing host mapping.
   GGTT_Map := Mmap (To_Address (GGTT_Base), 4096, 3, 16#22#, -1, 0);
   Reset_Map := Mmap (To_Address (Reset_Base), 4096, 3, 16#22#, -1, 0);
   pragma Assert (GGTT_Map = To_Address (GGTT_Base) and Reset_Map = To_Address (Reset_Base));
   Run;
   for Lost in 1 .. Baseline_Checks loop Run (Lost); end loop;
   Run (Permit => False);
   Run (Complete => False);
   pragma Assert (Munmap (GGTT_Map, 4096) = 0 and Munmap (Reset_Map, 4096) = 0);
   Ada.Text_IO.Put_Line ("Native GGTT retirement PASS: composed MMIO, GuC completion, ownership sweep" &
     Natural'Image (Baseline_Checks) & ", denial/timeout/replay and retained claims (host RAM, NOT GPU)");
end Native_GGTT_Retire_Tests;

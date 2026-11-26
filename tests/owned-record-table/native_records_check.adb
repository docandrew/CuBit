with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Owned_Reservations;
-- Disposable startup fixture. Uses only caller-owned mapping syscalls
-- (owned regions, ABI-002).
procedure Native_Records_Check is
   Addresses : array (1 .. 8193) of Unsigned_64 := (others => 0);
   Ignore, Base, Other : Unsigned_64;
   package Owned renames CuBit.Owned_Reservations;
   function Allocate (Bytes : Unsigned_64) return Unsigned_64 renames Owned.Allocate;
   function Free (Base, Bytes : Unsigned_64) return Boolean renames Owned.Free;
   procedure Check (OK : Boolean; Detail : String) is
   begin
      if not OK then
         debugPrint ("TEST: FAIL native owned records: " & Detail & ASCII.LF);
         Ignore := syscall (SYSCALL_EXIT);
         loop null; end loop;
      end if;
   end Check;
   procedure Sentinel (Address, Expected : Unsigned_64; Write : Boolean) is
      Word : Unsigned_64 with Import, Volatile,
        Address => To_Address (Integer_Address (Address));
   begin
      if Write then Word := Expected; else Check (Word = Expected, "page content"); end if;
   end Sentinel;
begin
   debugPrint ("native owned records: caller-owned allocation and reservation oracle" & ASCII.LF);
   -- Cross the former 16 MiB boundary and exercise the whole mapping lifecycle.
   for Round in 1 .. 4 loop
      declare
         Size : constant Unsigned_64 := 20 * 1024 * 1024;
      begin
         Base := Allocate (Size);
         Check (Base /= 0, "20 MiB allocation");
         for Page in 0 .. Size / 4096 - 1 loop
            Sentinel (Base + Page * 4096, 0, False);
            Sentinel (Base + Page * 4096, Page + 1, True);
         end loop;
         Check (Owned.Change (Base, Size, Owned.Make_Read_Only), "large read-only");
         for Page in 0 .. Size / 4096 - 1 loop
            Sentinel (Base + Page * 4096, Page + 1, False);
         end loop;
         Check (Owned.Change (Base + 16 * 1024 * 1024 - 4096, 8192, Owned.Make_Read_Write), "boundary write protection");
         Sentinel (Base + 16 * 1024 * 1024, 42, True);
         Check (Free (Base, Size), "large release");
         Check (not Free (Base, Size), "large duplicate release");
      end;
   end loop;
   Base := Allocate (256 * 1024 * 1024);
   Check (Base /= 0, "maximum mapping size");
   Sentinel (Base, 0, False);
   Sentinel (Base + 256 * 1024 * 1024 - 8, 0, False);
   Sentinel (Base + 256 * 1024 * 1024 - 8, 99, True);
   Check (Free (Base, 256 * 1024 * 1024), "maximum mapping release");
   Base := Allocate (16 * 1024 * 1024 + 1);
   Check (Base /= 0, "rounding across old bound");
   Sentinel (Base + 16 * 1024 * 1024, 0, False);
   Check (Free (Base, 16 * 1024 * 1024 + 1), "rounded large release");
   --  MEM-007: past the kernel's largest region (4 TiB + 64 GiB), refused
   --  and said so (Too_Large); no limit is compiled into this program.
   Check (Owned.Reserve (2**42 + 2**36 + 4096) = 0, "above region limit");
   Check (Allocate (Unsigned_64'Last) = 0, "mapping size overflow");
   debugPrint ("TEST: PASS large owned mappings lifecycle" & ASCII.LF);
   for Round in 1 .. 2 loop
      for I in Addresses'Range loop
         Addresses(I) := Allocate (4096);
         Check (Addresses(I) /= 0, "8193 simultaneous allocations");
         Sentinel (Addresses(I), 0, False);
         Sentinel (Addresses(I), Unsigned_64(I) + Unsigned_64(Round)*65536, True);
      end loop;
      debugPrint ("native owned records: 8193 live pages" & ASCII.LF);
      for I in Addresses'Range loop
         Sentinel (Addresses(I), Unsigned_64(I) + Unsigned_64(Round)*65536, False);
         if I mod 2 = 1 then
            Check (Free (Addresses(I), 4096), "release holes");
         end if;
      end loop;
      for I in Addresses'Range loop
         if I mod 2 = 1 then
            Base := Allocate (4096);
            Check (Base = Addresses(I), "first-fit hole reuse");
            Sentinel (Base, 0, False);
            Sentinel (Base, Unsigned_64(I)+Unsigned_64(Round)*65536, True);
         end if;
      end loop;
      for I in reverse Addresses'Range loop
         Sentinel (Addresses(I), Unsigned_64(I)+Unsigned_64(Round)*65536, False);
         Check (Free (Addresses(I), 4096), "full release");
      end loop;
      Check (not Free (Addresses(1), 4096), "duplicate release");
   end loop;
   for Round in 1 .. 8 loop
      Base := Owned.Reserve (2**31);
      Check (Base/=0, "reserve virtual capacity");
      Check (not Owned.Change (Base + 2**31, 4096, Owned.Commit), "past the region");
      Check (Owned.Change (Base, 4096, Owned.Commit), "first run");
      Sentinel(Base,Unsigned_64(Round),True);
      Other := Allocate (4096);
      Check(Other/=0 and then (Other<Base or Other>=Base+2**31),"reservation excludes mapping");
      Sentinel(Other,1234,True);
      Check (Owned.Change (Base + 4096, 8192, Owned.Commit), "second run");
      Sentinel(Base,Unsigned_64(Round),False);
      Sentinel(Base+8192,0,False);
      Check (Owned.Change (Base, 4096, Owned.Make_Read_Only), "read-only run");
      Sentinel(Base,Unsigned_64(Round),False);
      Check (Owned.Change (Base, 4096, Owned.Make_Read_Write), "restore write access");
      --  munmap semantics (MEM-007): the head page goes (the region
      --  shrinks and its runs follow), then the rest.
      Check (Owned.Release (Base, 4096), "release of the region's head");
      Sentinel(Base+8192,0,False);
      Check (Owned.Release (Base + 4096, 2**31 - 4096), "region retirement");
      Sentinel(Other,1234,False);
      Check (Free (Other, 4096), "unrelated release");
   end loop;
   debugPrint("TEST: PASS native owned records 8193 pages twice, hole reuse, protection, interleaved reservation retirement" & ASCII.LF);
   --  MEM-007: Ladybird's memory model (K1-K5, K7-K9) and typed refusals.
   declare
      GiB : constant Unsigned_64 := 2**30;
      TiB : constant Unsigned_64 := 2**40;
      MiB : constant Unsigned_64 := 2**20;
      Cage, Twice, Heap, Direct, Run, Middle, Low, High : Unsigned_64;
   begin
      --  K1: the ArrayBuffer cage, 4 TiB + 32 GiB, in one reservation.
      Cage := Owned.Reserve (4 * TiB + 32 * GiB, 0);
      Check (Cage /= 0, "K1 a 4 TiB + 32 GiB reservation");
      --  K2, K5: LibGC reserves twice its 32 GiB region, then gives back the
      --  head and the tail around a 32 GiB-aligned 32 GiB.
      Twice := Owned.Reserve (64 * GiB, 0);
      Check (Twice /= 0, "K2 a 64 GiB reservation beside the cage");
      Heap := (Twice + 32 * GiB - 1) and not (32 * GiB - 1);
      if Heap > Twice then
         Check (Owned.Release (Twice, Heap - Twice), "K5 release of the head");
      end if;
      if Heap + 32 * GiB < Twice + 64 * GiB then
         Check (Owned.Release (Heap + 32 * GiB, Twice + 64 * GiB - (Heap + 32 * GiB)),
                "K5 release of the tail");
      end if;
      --  K3: or the kernel aligns it.
      Direct := Owned.Reserve (32 * GiB, 32 * GiB);
      Check (Direct /= 0 and then Direct mod (32 * GiB) = 0, "K3 a 32 GiB-aligned reservation");
      --  K7: one commit, any page multiple, inside the reservation.
      Run := Heap + 3 * 4096;
      Check (Owned.Change (Run, 64 * MiB, Owned.Commit), "K7 one 64 MiB commit");
      Sentinel (Run, 0, False);
      Sentinel (Run + 64 * MiB - 8, 0, False);
      Sentinel (Run, 16#A11CE#, True);
      Sentinel (Run + 64 * MiB - 8, 16#B0B#, True);
      Check (not Owned.Change (Run + 32 * MiB, 4096, Owned.Commit), "a commit over committed pages");
      --  K8, K9: decommit a subrange; protect another read-only and back.
      Check (Owned.Change (Run + 8 * MiB, MiB, Owned.Decommit), "K8 decommit a subrange");
      Check (Owned.Change (Run + 16 * MiB, MiB, Owned.Make_Read_Only), "K9 read-only");
      Check (Owned.Change (Run + 16 * MiB, MiB, Owned.Make_Read_Write), "K9 read/write again");
      --  K5, the middle: a hole in the middle of the run and the region;
      --  both sides keep their pages and the hole is no region.
      Middle := Run + 32 * MiB;
      Check (Owned.Release (Middle, MiB), "K5 release of a middle range");
      Sentinel (Run, 16#A11CE#, False);
      Sentinel (Run + 64 * MiB - 8, 16#B0B#, False);
      Check (not Owned.Change (Middle, 4096, Owned.Commit), "the released middle is no region");
      Check (Owned.Change (Middle - MiB, 4096, Owned.Decommit), "the lower part is still a region");
      Check (Owned.Change (Middle + MiB, 4096, Owned.Decommit), "the upper part is a region of its own");
      --  munmap across regions and the holes between them: everything from
      --  the lowest to the highest of the three, in one call.
      Low := Unsigned_64'Min (Cage, Unsigned_64'Min (Heap, Direct));
      High := Unsigned_64'Max (Cage + 4 * TiB + 32 * GiB,
                               Unsigned_64'Max (Heap + 32 * GiB, Direct + 32 * GiB));
      Check (Owned.Release (Low, High - Low), "a release across regions and holes");
      Check (not Owned.Change (Run, 4096, Owned.Decommit) and then
             not Owned.Change (Direct, 4096, Owned.Commit) and then
             not Owned.Change (Cage, 4096, Owned.Commit), "every region in it gone");
      Check (not Owned.Release (Low, High - Low), "a second release touches nothing");
      debugPrint ("TEST: PASS MEM-007 4 TiB + 32 GiB cage, 64 GiB trimmed to a 32 GiB-aligned heap, "
                  & "32 GiB alignment, one 64 MiB commit, decommit, read-only, a middle release, "
                  & "a release across regions and holes" & ASCII.LF);
   end;
   for I in 1 .. 64 loop
      Base := Allocate (4096);
      Check (Base /= 0, "exit cleanup allocation");
      Sentinel (Base, Unsigned_64(I), True);
   end loop;
   debugPrint ("native owned records: exiting with 64 live mappings" & ASCII.LF);
   Ignore := syscall (SYSCALL_EXIT);
   loop null; end loop;
end Native_Records_Check;

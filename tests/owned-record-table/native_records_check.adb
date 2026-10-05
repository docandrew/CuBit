with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
-- Disposable startup fixture. Uses only caller-owned mapping syscalls.
procedure Native_Records_Check is
   Addresses : array (1 .. 8193) of Unsigned_64 := (others => 0);
   Ignore, Base, Other : Unsigned_64;
   Rejected : constant Unsigned_64 := Unsigned_64'Last;
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
         Base := syscall (SYSCALL_ALLOCATE_OWNED_MEMORY, Size);
         Check (Base /= 0, "20 MiB allocation");
         for Page in 0 .. Size / 4096 - 1 loop
            Sentinel (Base + Page * 4096, 0, False);
            Sentinel (Base + Page * 4096, Page + 1, True);
         end loop;
         Check (syscall (SYSCALL_PROTECT_OWNED_MEMORY, Base, Size, 1) = 0, "large read-only");
         for Page in 0 .. Size / 4096 - 1 loop
            Sentinel (Base + Page * 4096, Page + 1, False);
         end loop;
         Check (syscall (SYSCALL_PROTECT_OWNED_MEMORY, Base + 16 * 1024 * 1024 - 4096, 8192, 3) = 0, "boundary write protection");
         Sentinel (Base + 16 * 1024 * 1024, 42, True);
         Check (syscall (SYSCALL_RELEASE_OWNED_MEMORY, Base, Size) = 0, "large release");
         Check (syscall (SYSCALL_RELEASE_OWNED_MEMORY, Base, Size) = Rejected, "large duplicate release");
      end;
   end loop;
   Base := syscall (SYSCALL_ALLOCATE_OWNED_MEMORY, 256 * 1024 * 1024);
   Check (Base /= 0, "maximum mapping size");
   Sentinel (Base, 0, False);
   Sentinel (Base + 256 * 1024 * 1024 - 8, 0, False);
   Sentinel (Base + 256 * 1024 * 1024 - 8, 99, True);
   Check (syscall (SYSCALL_RELEASE_OWNED_MEMORY, Base, 256 * 1024 * 1024) = 0, "maximum mapping release");
   Base := syscall (SYSCALL_ALLOCATE_OWNED_MEMORY, 16 * 1024 * 1024 + 1);
   Check (Base /= 0, "rounding across old bound");
   Sentinel (Base + 16 * 1024 * 1024, 0, False);
   Check (syscall (SYSCALL_RELEASE_OWNED_MEMORY, Base, 16 * 1024 * 1024 + 1) = 0, "rounded large release");
   Check (syscall (SYSCALL_ALLOCATE_OWNED_MEMORY, 256 * 1024 * 1024 + 1) = 0, "above mapping limit");
   Check (syscall (SYSCALL_ALLOCATE_OWNED_MEMORY, Unsigned_64'Last) = 0, "mapping size overflow");
   debugPrint ("TEST: PASS large owned mappings lifecycle" & ASCII.LF);
   for Round in 1 .. 2 loop
      for I in Addresses'Range loop
         Addresses(I) := syscall (SYSCALL_ALLOCATE_OWNED_MEMORY, 4096);
         Check (Addresses(I) /= 0, "8193 simultaneous allocations");
         Sentinel (Addresses(I), 0, False);
         Sentinel (Addresses(I), Unsigned_64(I) + Unsigned_64(Round)*65536, True);
      end loop;
      debugPrint ("native owned records: 8193 live pages" & ASCII.LF);
      for I in Addresses'Range loop
         Sentinel (Addresses(I), Unsigned_64(I) + Unsigned_64(Round)*65536, False);
         if I mod 2 = 1 then
            Check (syscall (SYSCALL_RELEASE_OWNED_MEMORY, Addresses(I), 4096) = 0, "release holes");
         end if;
      end loop;
      for I in Addresses'Range loop
         if I mod 2 = 1 then
            Base := syscall (SYSCALL_ALLOCATE_OWNED_MEMORY, 4096);
            Check (Base = Addresses(I), "first-fit hole reuse");
            Sentinel (Base, 0, False);
            Sentinel (Base, Unsigned_64(I)+Unsigned_64(Round)*65536, True);
         end if;
      end loop;
      for I in reverse Addresses'Range loop
         Sentinel (Addresses(I), Unsigned_64(I)+Unsigned_64(Round)*65536, False);
         Check (syscall (SYSCALL_RELEASE_OWNED_MEMORY, Addresses(I), 4096)=0, "full release");
      end loop;
      Check (syscall (SYSCALL_RELEASE_OWNED_MEMORY, Addresses(1), 4096)=Rejected, "duplicate release");
   end loop;
   for Round in 1 .. 8 loop
      Base := syscall (SYSCALL_RESERVE_OWNED_MEMORY, 2**31);
      Check (Base/=0, "reserve virtual capacity");
      Check (syscall (SYSCALL_COMMIT_OWNED_MEMORY_PREFIX,Base,4096,4096)=Rejected,"skip prefix");
      Check (syscall (SYSCALL_COMMIT_OWNED_MEMORY_PREFIX,Base,0,4096)=0,"first chunk");
      Sentinel(Base,Unsigned_64(Round),True);
      Other := syscall(SYSCALL_ALLOCATE_OWNED_MEMORY,4096);
      Check(Other/=0 and then (Other<Base or Other>=Base+2**31),"reservation excludes mapping");
      Sentinel(Other,1234,True);
      Check(syscall(SYSCALL_COMMIT_OWNED_MEMORY_PREFIX,Base,4096,8192)=0,"second chunk");
      Sentinel(Base,Unsigned_64(Round),False);
      Sentinel(Base+8192,0,False);
      Check(syscall(SYSCALL_PROTECT_OWNED_MEMORY,Base,4096,1)=0,"read-only chunk");
      Sentinel(Base,Unsigned_64(Round),False);
      Check(syscall(SYSCALL_PROTECT_OWNED_MEMORY,Base,4096,3)=0,"restore write access");
      Check(syscall(SYSCALL_RELEASE_OWNED_MEMORY,Base,4096)=Rejected,"cannot release chunk directly");
      Check(syscall(SYSCALL_RELEASE_OWNED_RESERVATION,Base,2**31)=0,"reservation retirement");
      Sentinel(Other,1234,False);
      Check(syscall(SYSCALL_RELEASE_OWNED_MEMORY,Other,4096)=0,"unrelated release");
   end loop;
   debugPrint("TEST: PASS native owned records 8193 pages twice, hole reuse, protection, interleaved reservation retirement" & ASCII.LF);
   for I in 1 .. 64 loop
      Base := syscall (SYSCALL_ALLOCATE_OWNED_MEMORY, 4096);
      Check (Base /= 0, "exit cleanup allocation");
      Sentinel (Base, Unsigned_64(I), True);
   end loop;
   debugPrint ("native owned records: exiting with 64 live mappings" & ASCII.LF);
   Ignore := syscall (SYSCALL_EXIT);
   loop null; end loop;
end Native_Records_Check;

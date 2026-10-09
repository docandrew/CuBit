with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;

-- Privileged disposable supervisor. No GPU device accesses these pages.
procedure DMA_Growth_Check is
   PID : constant Unsigned_64 := syscall (SYSCALL_GETPID);
   Failed : constant Unsigned_64 := Unsigned_64'Last;
   Base : constant Unsigned_64 := 16#7600_0000#;
   Block_Bytes : constant Unsigned_64 := 2 * 1024 * 1024;
   Physical : array (1 .. 96) of Unsigned_64;
   Ignored : Unsigned_64;
   procedure Check (OK : Boolean; Detail : String) is
   begin
      if not OK then
         debugPrint ("TEST: FAIL native DMA growth: " & Detail & ASCII.LF);
         Ignored := syscall (SYSCALL_EXIT);
         loop null; end loop;
      end if;
   end Check;
begin
   debugPrint ("native DMA growth: real kernel allocations (NO GPU)" & ASCII.LF);
   Check (PID /= 0 and PID /= Failed, "PID");
   for I in Physical'Range loop
      Physical (I) := syscall
        (SYSCALL_ALLOC_DMA, PID, 9, Base + Unsigned_64 (I - 1) * Block_Bytes,
         (if I mod 2 = 0 then 3 else 1), 2 ** 32);
      Check (Physical (I) /= Failed, "allocation" & I'Image);
      Check (Physical (I) mod Block_Bytes = 0 and then
             Physical (I) <= 2 ** 32 - Block_Bytes, "physical bounds");
      for J in 1 .. I - 1 loop
         Check (Physical (I) /= Physical (J), "physical alias");
      end loop;
      for Page in 0 .. 511 loop
         declare
            Word : Unsigned_64 with Import, Volatile,
              Address => To_Address (Integer_Address
                (Base + Unsigned_64 (I - 1) * Block_Bytes + Unsigned_64 (Page) * 4096));
         begin
            Word := Unsigned_64 (I) * 65536 + Unsigned_64 (Page);
         end;
      end loop;
   end loop;
   -- Growth must preserve every earlier CPU mapping, not just the newest one.
   for I in Physical'Range loop
      for Page in 0 .. 511 loop
         declare
            Word : Unsigned_64 with Import, Volatile,
              Address => To_Address (Integer_Address
                (Base + Unsigned_64 (I - 1) * Block_Bytes + Unsigned_64 (Page) * 4096));
         begin
            Check (Word = Unsigned_64 (I) * 65536 + Unsigned_64 (Page), "sentinel");
         end;
      end loop;
   end loop;
   -- First leaf maps, second collides. Neither its metadata nor its prefix
   -- mapping may survive rollback.
   for Attempt in 1 .. 16 loop
      Check (syscall (SYSCALL_ALLOC_DMA, PID, 1, Base - 4096, 1) = Failed,
             "partial collision");
   end loop;
   Check (syscall (SYSCALL_ALLOC_DMA, PID, 0, Base - 4096, 0) /= Failed,
          "rollback prefix");
   debugPrint ("TEST: PASS native DMA growth 96 records 192MiB 49152 sentinels rollback (NO GPU)" & ASCII.LF);
   loop Ignored := syscall (SYSCALL_SLEEP, 100); end loop;
end DMA_Growth_Check;

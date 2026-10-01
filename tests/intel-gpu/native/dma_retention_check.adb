with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;
procedure DMA_Retention_Check is
   LF : constant Character := ASCII.LF;
   reterr : constant Unsigned_64 := Unsigned_64'Last;
   type Unsigned_64_Array is array (Positive range <>) of Unsigned_64;
   DMA_Order : constant Unsigned_64 := 9;
   DMA_Bytes : constant Unsigned_64 := 2 * 1024 * 1024;
   Blocks_Per_Owner : constant := 16;
   Addresses : array (1 .. 32) of Unsigned_64 := [others => 0];
   Virtual : Unsigned_64;
   Pid, Address, Status, Count : Unsigned_64;
   Rows : array (0 .. 255, 0 .. 15) of Unsigned_16 := [others => [others => 0]];
   Found, Gone : Boolean;
   Loan : CuBit.Memory_Grants.Grant_Reference;
   Loan_OK : Boolean;
   Other : Unsigned_64;
   procedure Reap (Target : Unsigned_64; Done : out Boolean) is
   begin
      Done := False;
      if killProcess (ProcessID (Target)) /= 0 then return; end if;
      for Attempt in 1 .. 1000 loop
         Count := syscall (SYSCALL_PROCLIST,
           Unsigned_64 (To_Integer (Rows'Address)), Rows'Size / 8);
         if Count = reterr or else Count > 256 then return; end if;
         Found := False;
         if Count > 0 then
            for I in 0 .. Natural (Count) - 1 loop
               if Unsigned_64 (Rows (I, 0)) = Target then Found := True; end if;
            end loop;
         end if;
         if not Found then Done := True; return; end if;
         Status := syscall (SYSCALL_SLEEP, 1);
      end loop;
   end Reap;
begin
   -- Failed reservations must not consume the boot budget.
   for Attempt in 1 .. 16 loop
      if syscall (SYSCALL_ALLOC_DMA, 0, DMA_Order, 16#7600_0000#, 1) /= reterr then
         debugPrint ("dma-retention: FAIL invalid target" & LF); return;
      end if;
   end loop;
   for I in Addresses'Range loop
      if (I - 1) mod Blocks_Per_Owner = 0 then
         Pid := Spawn;
      end if;
      Virtual := 16#7600_0000# + Unsigned_64 ((I - 1) mod Blocks_Per_Owner) * DMA_Bytes;
      if Pid = 0 or else Pid = reterr then
         debugPrint ("dma-retention: FAIL spawn" & LF); return;
      end if;
      if syscall (SYSCALL_ALLOC_DMA, Pid, 0, 16#7600_0000#, 2) /= reterr or else
        syscall (SYSCALL_ALLOC_DMA, Pid, 14, Virtual, 1) /= reterr or else
        syscall (SYSCALL_ALLOC_DMA, Pid, Unsigned_64'Last, 16#7600_0000#, 1) /= reterr
      then debugPrint ("dma-retention: FAIL invalid mode/order" & LF); return; end if;
      -- Impossible ceilings must leave lists and retained budget unchanged.
      for Ceiling of Unsigned_64_Array'(1, 4095, DMA_Bytes - 1) loop
         if syscall (SYSCALL_ALLOC_DMA, Pid, DMA_Order, 16#7600_0000#, 1, Ceiling) /= reterr then
            debugPrint ("dma-retention: FAIL impossible ceiling" & LF); return;
         end if;
      end loop;
      Address := syscall (SYSCALL_ALLOC_DMA, Pid, DMA_Order, Virtual,
        (if I > Blocks_Per_Owner then 3 else 1), 2 ** 32);
      if Address = reterr then
         debugPrint ("dma-retention: FAIL allocation" & LF); return;
      end if;
      if Address mod DMA_Bytes /= 0 or else Address > 2 ** 32 - DMA_Bytes then
         debugPrint ("dma-retention: FAIL retained ceiling" & LF); return;
      end if;
      for Previous in 1 .. I - 1 loop
         if Address < Addresses (Previous) + DMA_Bytes and then
           Addresses (Previous) < Address + DMA_Bytes
         then debugPrint ("dma-retention: FAIL recycled backing" & LF); return; end if;
      end loop;
      Addresses (I) := Address;
      if I = Addresses'First then
         -- First page maps, second collides with the retained allocation.
         -- Repetition also checks that failed retained reservations refund quota.
         for Attempt in 1 .. 16 loop
            if syscall (SYSCALL_ALLOC_DMA, Pid, 1, Virtual - 4096, 1) /= reterr then
               debugPrint ("dma-retention: FAIL partial collision accepted" & LF); return;
            end if;
         end loop;
         if syscall (SYSCALL_ALLOC_DMA, Pid, 0, Virtual - 4096, 0) = reterr then
            debugPrint ("dma-retention: FAIL rollback prefix left mapped" & LF); return;
         end if;
         debugPrint ("dma-retention: partial mapping rollback PASS" & LF);
         Borrow (Pid, Loan, Loan_OK);
         if not Loan_OK then
            debugPrint ("dma-retention: FAIL borrow" & LF); return;
         end if;
      end if;
      if I = Addresses'Last then
         Borrow (Pid, Loan, Loan_OK, Large => True);
         if not Loan_OK then
            debugPrint ("dma-retention: FAIL large grant acquisition" & LF); return;
         end if;
      end if;
      if I mod Blocks_Per_Owner = 0 then
         Reap (Pid, Gone);
         if not Gone then debugPrint ("dma-retention: FAIL reap" & LF); return; end if;
      end if;
      if I mod Blocks_Per_Owner = 0 then
         Other := Spawn;
         if Other = 0 or else Other = reterr or else Other = Pid then
            debugPrint ("dma-retention: FAIL loan PID reservation" & LF); return;
         end if;
         Reap (Other, Gone);
         if not Gone then
            debugPrint ("dma-retention: FAIL extra reap" & LF); return;
         end if;
         CuBit.Memory_Grants.Return_Acquisition (Loan, Loan_OK);
         if not Loan_OK then
            debugPrint ("dma-retention: FAIL return acquisition" & LF); return;
         end if;
         debugPrint ("dma-retention: deferred CPU loan returned" & LF);
      end if;
   end loop;
   Pid := Spawn;
   if Pid = 0 or else Pid = reterr then
      debugPrint ("dma-retention: FAIL final spawn" & LF); return;
   end if;
   if syscall (SYSCALL_ALLOC_DMA, Pid, 0, 16#7600_0000#, 1) /= reterr then
      debugPrint ("dma-retention: FAIL budget refunded on exit" & LF); return;
   end if;
   if syscall (SYSCALL_ALLOC_DMA, Pid, 0, 16#7600_0000#, 0, 4095) /= reterr then
      debugPrint ("dma-retention: FAIL ordinary impossible ceiling" & LF); return;
   end if;
   Address := syscall (SYSCALL_ALLOC_DMA, Pid, 0, 16#7600_0000#, 0, 2 ** 32);
   if Address = reterr then debugPrint ("dma-retention: FAIL ordinary mode" & LF); return; end if;
   if Address mod 4096 /= 0 or else Address > 2 ** 32 - 4096 then
      debugPrint ("dma-retention: FAIL ordinary ceiling" & LF); return;
   end if;
   for Previous of Addresses loop
      if Address >= Previous and then Address < Previous + DMA_Bytes then
         debugPrint ("dma-retention: FAIL ordinary reuse" & LF); return;
      end if;
   end loop;
   Reap (Pid, Gone);
   if Gone then
      debugPrint ("dma-retention: constrained ceilings PASS" & LF);
      debugPrint ("dma-retention: sixteen order9 blocks per owner PASS" & LF);
      debugPrint ("dma-retention: PASS exit retention, quota, failed reservations, ordinary mode" & LF);
   else debugPrint ("dma-retention: FAIL final reap" & LF); end if;
end DMA_Retention_Check;

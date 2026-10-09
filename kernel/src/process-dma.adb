pragma Ada_2022;
with Spinlocks;
with BuddyAllocator;
with Virtmem;
with TextIO;
package body Process.DMA is
   use type Records.Reference;
   use type Records.Arena;
   Arenas : array (ProcessID) of Records.Arena := [others => Records.No_Arena];
   Owned : array (ProcessID) of Records.List;
   Orphans : Records.List;
   Poison : Records.List;
   Poisoned : array (ProcessID) of Boolean := [others => False];
   Current : array (ProcessID) of Records.Reference := [others => null];
   Positions : array (ProcessID) of Retirement.Cursor;
   Registry_Lock : Spinlocks.Spinlock;
   Pending : array (ProcessID) of Boolean := [others => False];
   Active : array (ProcessID) of Boolean := [others => False];
   Next_PID : ProcessID := 1;
   procedure Enqueue (PID : ProcessID) is
   begin
      Spinlocks.enterCriticalSection (Registry_Lock);
      if PID /= NO_PROCESS and then not Active (PID) then
         Active (PID) := True;
         Pending (PID) := True;
      end if;
      Spinlocks.exitCriticalSection (Registry_Lock);
   end Enqueue;
   procedure Take_Ready (PID : out ProcessID) is
      Candidate : ProcessID;
   begin
      PID := NO_PROCESS;
      Spinlocks.enterCriticalSection (Registry_Lock);
      -- Bounded by the existing process namespace, not allocation count.
      for I in 1 .. Natural (ProcessID'Last) loop
         Candidate := Next_PID;
         Next_PID := (if Next_PID = ProcessID'Last then 1 else Next_PID + 1);
         if Pending (Candidate) then
            Pending (Candidate) := False;
            PID := Candidate;
            exit;
         end if;
      end loop;
      Spinlocks.exitCriticalSection (Registry_Lock);
   end Take_Ready;
   procedure Finish_Step (PID : ProcessID; Complete : Boolean) is
   begin
      Spinlocks.enterCriticalSection (Registry_Lock);
      Pending (PID) := not Complete and then not Poisoned (PID);
      -- A poisoned owner remains active but never queued: no automatic
      -- retry, duplicate wake, or PID reuse can turn rejection into progress.
      Active (PID) := not Complete or else Poisoned (PID);
      Spinlocks.exitCriticalSection (Registry_Lock);
   end Finish_Step;
   Metadata_Share_Divisor : constant Unsigned_64 := 1024;
   procedure Release_Owner (Physical_Page, Owner : Unsigned_64) is
   begin
      BuddyAllocator.releaseUserFrame (Virtmem.PhysAddress (Physical_Page), BuddyAllocator.Frame_Owner (Owner));
   end Release_Owner;
   procedure Free_Block (Physical : Unsigned_64; Order : Natural) is
   begin
      BuddyAllocator.free (BuddyAllocator.Order (Order), Virtmem.P2Va (Virtmem.PhysAddress (Physical)));
   end Free_Block;
   procedure Reserve
     (PID : ProcessID; Order : Natural; Retained : Boolean;
      Item : out Ticket; Success : out Boolean) is
      Limit : constant Unsigned_64 :=
        Unsigned_64 (BuddyAllocator.getTotalBytes) / Metadata_Share_Divisor;
   begin
      Item := (Ref => null);
      Success := False;
      if PID = NO_PROCESS or else Order > Retirement.Allocation_Order'Last then return; end if;
      Spinlocks.enterCriticalSection (Registry_Lock);
      if Arenas (PID) = Records.No_Arena then
         Records.Open (proctab(PID).memoryAccount, Limit, Arenas (PID), Success);
         if not Success then
            Spinlocks.exitCriticalSection (Registry_Lock);
            return;
         end if;
      end if;
      Records.Reserve (Arenas (PID),
        (Physical => 0, Owner => Unsigned_64 (PID),
         Generation => Unsigned_64 (generationOf (PID)), Order => Order, Retained => Retained),
        Limit, Item.Ref, Success);
      if not Success and then Records.Empty (Arenas (PID)) then
         Records.Close (Arenas (PID));
      end if;
      Spinlocks.exitCriticalSection (Registry_Lock);
   end Reserve;
   procedure Commit (Item : in out Ticket; Physical : Unsigned_64) is
      Data : Retirement.Allocation := Records.Value (Item.Ref);
   begin
      -- Both procedures are called while the target mailbox remains held.
      Data.Physical := Physical;
      Spinlocks.enterCriticalSection (Registry_Lock);
      Records.Set_Value (Item.Ref, Data);
      Records.Push (Owned (ProcessID (Data.Owner)), Item.Ref);
      Item.Ref := null;
      Spinlocks.exitCriticalSection (Registry_Lock);
   end Commit;
   procedure Cancel (Item : in out Ticket) is
   begin
      Spinlocks.enterCriticalSection (Registry_Lock);
      if Item.Ref /= null then
         declare PID : constant ProcessID := ProcessID (Records.Value (Item.Ref).Owner); begin
            Records.Release (Item.Ref);
            if Records.Empty (Arenas (PID)) then Records.Close (Arenas (PID)); end if;
         end;
      end if;
      Spinlocks.exitCriticalSection (Registry_Lock);
   end Cancel;
   function Has_Records (PID : ProcessID) return Boolean is
      Result : Boolean;
   begin
      Spinlocks.enterCriticalSection (Registry_Lock);
      Result := Current (PID) /= null or else not Records.Empty (Owned (PID));
      Spinlocks.exitCriticalSection (Registry_Lock);
      return Result;
   end Has_Records;
   procedure Retire_Step
     (PID : ProcessID; CPU_And_Grants_Retired : Boolean; Complete : out Boolean) is
      Finished : Boolean;
   begin
      Complete := False;
      if PID = NO_PROCESS or else not CPU_And_Grants_Retired then return; end if;
      Spinlocks.enterCriticalSection (Registry_Lock);
      if Poisoned (PID) then
         Spinlocks.exitCriticalSection (Registry_Lock);
         return;
      end if;
      -- Called only after CPU/grant retirement. Detach the PID's handle now;
      -- live and orphan records keep their immutable original arena alive.
      Records.Close (Arenas (PID));
      if Current (PID) = null then
         Records.Pop (Owned (PID), Current (PID));
         declare Fresh : Retirement.Cursor; begin Positions (PID) := Fresh; end;
      end if;
      if Current (PID) /= null then
         Retirement.Step (Records.Value (Current (PID)), Positions (PID), True, Finished);
         if Retirement.Rejected (Positions (PID)) then
            Poisoned (PID) := True;
            Records.Push (Poison, Current (PID));
            Current (PID) := null;
            Records.Move (Owned (PID), Poison);
            Spinlocks.exitCriticalSection (Registry_Lock);
            TextIO.println ("DMA retirement: poisoned record; backing and PID quarantined" &
              ProcessID'Image (PID));
            return;
         end if;
         if Finished then
            if Records.Value (Current (PID)).Retained then
               Records.Push (Orphans, Current (PID));
               Current (PID) := null;
            else
               Records.Release (Current (PID));
            end if;
         end if;
      end if;
      Complete := Current (PID) = null and then Records.Empty (Owned (PID));
      Spinlocks.exitCriticalSection (Registry_Lock);
   end Retire_Step;
end Process.DMA;

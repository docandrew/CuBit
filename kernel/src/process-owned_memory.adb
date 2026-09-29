with Capabilities;
with Owned_Memory_Layout;
with BuddyAllocator;
with Heap_Admission;
with Region_Release;
with TLB_Shootdown;
with System.Storage_Elements; use System.Storage_Elements;
with Spinlocks;
with Virtmem.Regions;
package body Process.Owned_Memory is
   use type FrameLists.NodePtr;
   use type Capabilities.Generation;
   type Phase is (Empty, Building, Live, Quarantined);
   type Entry_Record is record
      Status : Phase := Empty;
      Owner : ProcessID := NO_PROCESS;
      Generation : Capabilities.Generation := 0;
      Base, Bytes : Unsigned_64 := 0;
      Count : Natural := 0;
      First_Node, Last_Node : FrameLists.NodePtr := null;
      Detached : Boolean := False;
      Remaining : FrameLists.List := (null, null, 0, 0);
   end record;
   subtype Slot is Positive range 1 .. 4096;
   Entries : array (Slot) of Entry_Record;
   Registry_Lock : Spinlocks.Spinlock;
   procedure Lock is
   begin Spinlocks.enterCriticalSection (Registry_Lock); end Lock;
   procedure Unlock is
   begin Spinlocks.exitCriticalSection (Registry_Lock); end Unlock;

   function Rounded (Bytes : Unsigned_64) return Unsigned_64 is
     (if Bytes = 0 or Bytes > Maximum_Bytes then 0
      else ((Bytes + 4095) / 4096) * 4096);

   function Physical_Conflict (Base, Bytes : Unsigned_64) return Boolean is
      Node : FrameLists.NodePtr;
      Count : Natural;
   begin
      if Bytes = 0 or else Bytes > Unsigned_64'Last - Base then return True; end if;
      for Item of Entries loop
         if Item.Status /= Empty then
            Node := (if Item.Detached then Item.Remaining.head else Item.First_Node);
            Count := (if Item.Detached then Item.Remaining.length else Item.Count);
            for I in 1 .. Count loop
               if Node = null then return True; end if;
               declare
                  Frame : constant Unsigned_64 := Unsigned_64 (Node.element);
               begin
                  if Base < Frame + 4096 and then Frame < Base + Bytes then return True; end if;
               end;
               Node := Node.next;
            end loop;
         end if;
      end loop;
      return False;
   end Physical_Conflict;

   -- All pages of this allocation were added consecutively under the address
   -- lock. Later allocations prepend nodes but cannot interleave this range.
   function Inventory_Valid (Item : Entry_Record) return Boolean is
      Node : FrameLists.NodePtr := proctab(Item.Owner).frames.head;
      Position : Natural := 0;
   begin
      if Item.Detached or Item.Count = 0 or Item.First_Node = null then return False; end if;
      for I in 1 .. proctab(Item.Owner).frames.length loop
         if Node = Item.First_Node then Position := I; exit; end if;
         Node := Node.next;
      end loop;
      if Position = 0 or else Item.Count > proctab(Item.Owner).frames.length - Position + 1
      then return False; end if;
      for I in 0 .. Item.Count - 1 loop
         if not Virtmem.Regions.Matches
           (addrtab(proctab(Item.Owner).pgTable),
            Integer_Address (Item.Base) + Integer_Address (Item.Count - I - 1) * 4096,
            Virtmem.PFN (Node.element / 4096)) then return False; end if;
         if I = Item.Count - 1 then return Node = Item.Last_Node; end if;
         Node := Node.next;
      end loop;
      return False;
   end Inventory_Valid;

   procedure Retire (Index : Slot; Success : out Boolean) is
      Item : Entry_Record renames Entries(Index);
      procedure Unmap (Page : Natural; OK : out Boolean) is
      begin
         Virtmem.unmapPage (Integer_Address (Item.Base) + Integer_Address (Page) * 4096,
                           addrtab(proctab(Item.Owner).pgTable), OK);
      end Unmap;
      procedure Synchronize (OK : out Boolean) is
      begin TLB_Shootdown.Invalidate_All; OK := True; end Synchronize;
      procedure Free (OK : out Boolean) is
      begin
         FrameLists.detachRange (proctab(Item.Owner).frames, Item.First_Node,
                                 Item.Last_Node, Item.Count, Item.Remaining, OK);
         if not OK then return; end if;
         Item.Detached := True;
         Item.First_Node := null;
         Item.Last_Node := null;
         while Item.Remaining.length > 0 loop
            BuddyAllocator.freeFrame (FrameLists.front (Item.Remaining));
            FrameLists.popFront (Item.Remaining);
         end loop;
      end Free;
      package Retirement is new Region_Release (Unmap, Synchronize, Free);
      use type Retirement.Phase;
      Attempt : Retirement.Attempt;
   begin
      Success := False;
      if not Inventory_Valid (Item) then Item.Status := Quarantined; return; end if;
      Item.Status := Quarantined;
      Retirement.Apply (Attempt, Item.Count, True);
      if Retirement.State (Attempt) = Retirement.Released then
         Item := (others => <>);
         Success := True;
      end if;
   end Retire;

   function Find_Base (PID : ProcessID; Bytes : Unsigned_64) return Unsigned_64 is
      Candidate : Unsigned_64 := Owned_Memory_Layout.First;
      Moved : Boolean;
   begin
      -- Each move passes at least one existing interval, so the search is
      -- bounded even though the small descriptor table is not address-sorted.
      for Pass in 0 .. Entries'Length loop
         if Candidate > Owned_Memory_Layout.Limit - Bytes then return 0; end if;
         Moved := False;
         for Item of Entries loop
            if Item.Status /= Empty and then Item.Owner = PID and then
              Candidate < Item.Base + Item.Bytes and then Item.Base < Candidate + Bytes
            then Candidate := Item.Base + Item.Bytes; Moved := True; exit; end if;
         end loop;
         if not Moved then return Candidate; end if;
      end loop;
      return 0;
   end Find_Base;

   procedure Allocate (PID : ProcessID; Bytes : Unsigned_64; Base : out Unsigned_64) is
      Size : constant Unsigned_64 := Rounded (Bytes);
      Index : Slot := Slot'First;
      Found : Boolean := False;
      Candidate : Unsigned_64;
      Storage : System.Address;
      Result : Page_Allocation_Result;
      Ignored : Boolean;
      Capacity : Natural;
   begin
      Base := 0;
      if PID = NO_PROCESS or Size = 0 then return; end if;
      Lock;
      lockAddressSpace (PID);
      if not proctab(PID).admitted then
         unlockAddressSpace (PID); Unlock; return;
      end if;
      for I in Slot loop
         if Entries(I).Status = Empty then Index := I; Found := True; exit; end if;
      end loop;
      Candidate := Find_Base (PID, Size);
      Capacity := Heap_Admission.Expanded_Capacity
        (Positive'Max (1, proctab(PID).frames.length),
         Natural (Size / 4096) + Natural (proctab(PID).stackSize / 4096) +
           INITIAL_HEAP_FRAME_HEADROOM);
      if not Found or Candidate = 0 or Capacity = 0 then
         unlockAddressSpace (PID); Unlock; return;
      end if;
      Entries(Index) := (Status => Building, Owner => PID, Generation => generationOf (PID),
                        Base => Candidate, Bytes => Size, others => <>);
      proctab(PID).frames.capacity := Natural'Max (Capacity, proctab(PID).frames.capacity);
      for Page in 0 .. Natural (Size / 4096) - 1 loop
         tryAddPage (proctab(PID), To_Address (Integer_Address (Candidate) +
                       Integer_Address (Page) * 4096), Storage, Result);
         if Result /= Page_Added then
            if Entries(Index).Count = 0 then Entries(Index) := (others => <>);
            else Retire (Index, Ignored); end if;
            unlockAddressSpace (PID); Unlock; return;
         end if;
         if Entries(Index).Count = 0 then Entries(Index).Last_Node := proctab(PID).frames.head; end if;
         Entries(Index).First_Node := proctab(PID).frames.head;
         Entries(Index).Count := Entries(Index).Count + 1;
      end loop;
      Entries(Index).Status := Live;
      Base := Candidate;
      unlockAddressSpace (PID);
      Unlock;
   end Allocate;

   procedure Release (PID : ProcessID; Base, Bytes : Unsigned_64; Success : out Boolean) is
      Size : constant Unsigned_64 := Rounded (Bytes);
   begin
      Success := False;
      if PID = NO_PROCESS or Size = 0 then return; end if;
      Spinlocks.enterCriticalSection (grantLock);
      Lock;
      lockAddressSpace (PID);
      for I in Slot loop
         if Entries(I).Status = Live and Entries(I).Owner = PID and
           Entries(I).Generation = generationOf (PID) and
           Entries(I).Base = Base and Entries(I).Bytes = Size
         then Retire (I, Success); exit; end if;
      end loop;
      unlockAddressSpace (PID);
      Unlock;
      Spinlocks.exitCriticalSection (grantLock);
   end Release;

   procedure Protect (PID : ProcessID; Base, Bytes, Mode : Unsigned_64;
                      Success : out Boolean) is
      Size : constant Unsigned_64 := Rounded (Bytes);
      Selected : Virtmem.Regions.Access_Mode;
      procedure Change (Item : Entry_Record; Access_Value : Virtmem.Regions.Access_Mode;
                        OK : out Boolean) is
         Node : FrameLists.NodePtr := Item.First_Node;
         Address : Unsigned_64;
         Changed : Boolean;
      begin
         OK := True;
         for I in reverse 0 .. Item.Count - 1 loop
            Address := Item.Base + Unsigned_64 (I) * 4096;
            if Address >= Base and then Address - Base < Size then
               Virtmem.Regions.Set_Access
                 (addrtab(proctab(PID).pgTable), Integer_Address (Address),
                  Virtmem.PFN (Node.element / 4096), Access_Value, Changed);
               OK := OK and Changed;
            end if;
            Node := Node.next;
         end loop;
      end Change;
      OK : Boolean;
   begin
      Success := False;
      if PID = NO_PROCESS or Size = 0 or Base mod 4096 /= 0 then return; end if;
      case Mode is
         when 0 => Selected := Virtmem.Regions.Inaccessible;
         when 1 => Selected := Virtmem.Regions.Read_Only;
         when 3 => Selected := Virtmem.Regions.Read_Write;
         when others => return;
      end case;
      Spinlocks.enterCriticalSection (grantLock);
      Lock;
      lockAddressSpace (PID);
      for Item of Entries loop
         if Item.Status = Live and then Item.Owner = PID and then
           Item.Generation = generationOf (PID) and then Base >= Item.Base and then
           Base - Item.Base < Item.Bytes and then Size <= Item.Bytes - (Base - Item.Base)
         then
            if not Inventory_Valid (Item) then
               Item.Status := Quarantined;
               exit;
            end if;
            -- Revoke before installation, and retain backing on every failure.
            Item.Status := Quarantined;
            Change (Item, Virtmem.Regions.Inaccessible, OK);
            TLB_Shootdown.Invalidate_All;
            if OK and Mode /= 0 then
               Change (Item, Selected, OK);
               TLB_Shootdown.Invalidate_All;
            end if;
            if OK then Item.Status := Live; Success := True; end if;
            exit;
         end if;
      end loop;
      unlockAddressSpace (PID);
      Unlock;
      Spinlocks.exitCriticalSection (grantLock);
   end Protect;

   procedure Forget_Exited (PID : ProcessID; Retired : out Natural) is
   begin
      Retired := 0;
      if proctab(PID).frames.length /= 0 then
         raise ProcessException with "Owned region exit before frame reclamation";
      end if;
      for Item of Entries loop
         if Item.Status /= Empty and Item.Owner = PID then
            if Item.Detached and Item.Remaining.length /= 0 then
               raise ProcessException with "Owned region detached cleanup incomplete";
            end if;
            Item := (others => <>);
            Retired := Retired + 1;
         end if;
      end loop;
   end Forget_Exited;
end Process.Owned_Memory;

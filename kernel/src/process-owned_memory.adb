with Owned_Record_Tables;
with Capabilities;
with Owned_Memory_Layout;
with BuddyAllocator;
with Heap_Admission;
with Region_Release;
with TLB_Shootdown;
with System.Storage_Elements; use System.Storage_Elements;
with Spinlocks;
with Process_Pool;
with Time;
with x86;
with System.Machine_Code;
with Virtmem.Regions;
package body Process.Owned_Memory is
   package Policy renames Owned_Reservation_Policy;
   use type FrameLists.NodePtr;
   use type Capabilities.Generation;
   use type Policy.Page_Access;

   -- Building: a run being backed by a commit, or a record reserved for
   -- the upper part of a decommit's middle cut. Quarantined: a page-table
   -- retirement failed part way; the run keeps its frames (and the frame
   -- list keeps them for process exit) and is never retried piecemeal.
   type Phase is (Empty, Building, Live, Quarantined);
   -- A Region is reserved address space (no RAM). A Run is a committed,
   -- page-backed part of one region: Count pages from Base, whose frames
   -- are consecutive in the owner's frame list, the highest page first
   -- (tryAddPage prepends, and a commit backs its pages upward), so
   -- First_Node is the highest page's frame and Last_Node the lowest's.
   -- Runs of a region never overlap (a commit is refused over any run).
   type Entry_Kind is (Region, Run);
   subtype Slot is Positive range 1 .. 65_536;
   type Entry_Record is record
      Status : Phase := Empty;
      Kind : Entry_Kind := Region;
      Parent : Natural range 0 .. Slot'Last := 0;    -- a run's region
      Owner : ProcessID := NO_PROCESS;
      Generation : Capabilities.Generation := 0;
      Base, Bytes : Unsigned_64 := 0;                -- a run's Bytes: Count pages
      Count : Natural := 0;
      First_Node, Last_Node : FrameLists.NodePtr := null;
   end record;
   -- Process-local metadata ceilings; backing is allocated on demand and
   -- returned when its block becomes empty. No shared fixed record pool.
   procedure Allocate_Metadata (Bytes : Positive; Address : out System.Address) is
   begin BuddyAllocator.alloc (BuddyAllocator.getOrder (Storage_Count (Bytes)), Address); end;
   procedure Free_Metadata (Bytes : Positive; Address : System.Address) is
   begin BuddyAllocator.free (BuddyAllocator.getOrder (Storage_Count (Bytes)), Address); end;
   package Records is new Owned_Record_Tables
     (Entry_Record, Slot'Last, 24, Allocate_Metadata, Free_Metadata);
   Tables : array (ProcessID) of aliased Records.Table;
   function Entries (PID : ProcessID; Index : Slot) return Records.Reference is
     (Records.Get (Tables(PID), Index));
   Registry_Lock : Spinlocks.Spinlock;
   procedure Lock is
   begin Spinlocks.enterCriticalSection (Registry_Lock); end Lock;
   procedure Unlock is
   begin Spinlocks.exitCriticalSection (Registry_Lock); end Unlock;

   Page : constant Unsigned_64 := Policy.Page_Bytes;
   function Bytes_Of (Pages : Natural) return Unsigned_64 is (Unsigned_64 (Pages) * Page);

   function Physical_Conflict (Base, Bytes : Unsigned_64) return Boolean is
      Node : FrameLists.NodePtr;
   begin
      if Bytes = 0 or else Bytes > Unsigned_64'Last - Base then return True; end if;
      for PID in ProcessID loop
       for Index of Records.Iterate (Tables(PID)'Access) loop
        declare
         Item : Entry_Record renames Entries(PID, Index).Value.all;
        begin
         if Item.Status /= Empty and then Item.Kind = Run then
            Node := Item.First_Node;
            for I in 1 .. Item.Count loop
               if Node = null then return True; end if;
               declare
                  Frame : constant Unsigned_64 := Unsigned_64 (Node.element);
               begin
                  if Base < Frame + Page and then Frame < Base + Bytes then return True; end if;
               end;
               Node := Node.next;
            end loop;
         end if;
        end;
       end loop;
      end loop;
      return False;
   end Physical_Conflict;

   -- The run's frames are still a contiguous segment of the owner's frame
   -- list, in descending address order, and each is what its page maps.
   function Inventory_Valid (Item : Entry_Record) return Boolean is
      Node : FrameLists.NodePtr := proctab(Item.Owner).frames.head;
      Position : Natural := 0;
   begin
      if Item.Kind /= Run or Item.Count = 0 or Item.First_Node = null then return False; end if;
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

   -- Policy.Run_Split in the frame-list's terms (a run's Count).
   type Run_Cut is record
      Lower, Dropped, Upper : Natural := 0;
   end record;
   function Cut_Of (Item : Entry_Record; Address, Bytes : Unsigned_64) return Run_Cut is
      Split : constant Policy.Run_Split :=
        Policy.Split (Item.Base, Policy.Page_Count (Item.Count), Address, Bytes);
   begin
      return (Natural (Split.Lower), Natural (Split.Dropped), Natural (Split.Upper));
   end Cut_Of;

   -- Drop the Cut.Dropped pages of run Index (Policy.Split), keeping its
   -- Cut.Lower and Cut.Upper pages as runs. Order: every PTE removed, then
   -- the TLBs invalidated, then the frames detached from the frame list
   -- and freed (Region_Release); freeing returns each frame's quota charge,
   -- deferred while a grant pins it. The run keeps the lower part (or the
   -- upper one when there is no lower); Spare, a record the caller
   -- reserved, takes the upper part of a middle cut. On failure the run is
   -- quarantined with all its frames, and Spare is left for the caller.
   procedure Drop (PID : ProcessID; Index : Slot; Cut : Run_Cut;
                   Spare : Natural; Success : out Boolean;
                   Unmapped : Boolean := False) is
      Item : Entry_Record renames Entries(PID, Index).Value.all;
      Drop_Base : constant Unsigned_64 := Item.Base + Bytes_Of (Cut.Lower);
      Upper_Base : constant Unsigned_64 := Drop_Base + Bytes_Of (Cut.Dropped);
      Highest : constant FrameLists.NodePtr := Item.First_Node;
      Upper_Last, Drop_First, Drop_Last, Lower_First : FrameLists.NodePtr := null;
      Node : FrameLists.NodePtr;
      Freed : FrameLists.List := (null, null, 0, 0);
      -- Unmapped: the caller already cleared the pages' entries and
      -- completed a TLB round since (Release); only the frames go.
      procedure Unmap (Page_Index : Natural; OK : out Boolean) is
      begin
         if Unmapped then
            OK := True;
            return;
         end if;
         Virtmem.unmapPage (Integer_Address (Drop_Base) + Integer_Address (Page_Index) * 4096,
                            addrtab(proctab(PID).pgTable), OK);
      end Unmap;
      procedure Synchronize (OK : out Boolean) is
      begin
         if not Unmapped then
            TLB_Shootdown.Invalidate_All;
         end if;
         OK := True;
      end Synchronize;
      procedure Free (OK : out Boolean) is
      begin
         FrameLists.detachRange (proctab(PID).frames, Drop_First, Drop_Last,
                                 Cut.Dropped, Freed, OK);
         if not OK then return; end if;
         publishOwnedBytes (PID);
         while Freed.length > 0 loop
            BuddyAllocator.freeFrame (FrameLists.front (Freed));
            FrameLists.popFront (Freed);
         end loop;
      end Free;
      package Retirement is new Region_Release (Unmap, Synchronize, Free);
      use type Retirement.Phase;
      Attempt : Retirement.Attempt;
   begin
      Success := False;
      if Cut.Dropped = 0 or else Cut.Lower + Cut.Dropped + Cut.Upper /= Item.Count
        or else (Cut.Lower > 0 and Cut.Upper > 0 and Spare = 0)
        or else not Inventory_Valid (Item)
      then
         Item.Status := Quarantined;
         return;
      end if;
      -- Frame-list positions, highest page first: Upper, Dropped, Lower.
      Node := Highest;
      for Position in 0 .. Item.Count - 1 loop
         if Position + 1 = Cut.Upper then Upper_Last := Node; end if;
         if Position = Cut.Upper then Drop_First := Node; end if;
         if Position + 1 = Cut.Upper + Cut.Dropped then Drop_Last := Node; end if;
         if Position = Cut.Upper + Cut.Dropped then Lower_First := Node; end if;
         -- Nothing below the first kept lower page is needed: a release
         -- steps down a long run from its top (Release), one step each.
         exit when Position = Cut.Upper + Cut.Dropped;
         Node := Node.next;
      end loop;
      Item.Status := Quarantined;
      Retirement.Apply (Attempt, Cut.Dropped, True);
      if Retirement.State (Attempt) /= Retirement.Released then return; end if;
      Success := True;
      if Cut.Lower = 0 and Cut.Upper = 0 then
         Records.Release (Tables(PID), Index);
         return;
      end if;
      if Cut.Lower > 0 then
         Item.First_Node := Lower_First;
         Item.Count := Cut.Lower;
         if Cut.Upper > 0 then
            Entries(PID, Spare).Value.all :=
              (Status => Live, Kind => Run, Parent => Item.Parent, Owner => Item.Owner,
               Generation => Item.Generation, Base => Upper_Base,
               Bytes => Bytes_Of (Cut.Upper), Count => Cut.Upper,
               First_Node => Highest, Last_Node => Upper_Last);
         end if;
      else
         -- The record's table key stays its old base; Find_Base orders by
         -- regions only, so a run's key is never used for placement.
         Item.Base := Upper_Base;
         Item.Last_Node := Upper_Last;
         Item.Count := Cut.Upper;
      end if;
      Item.Bytes := Bytes_Of (Item.Count);
      Item.Status := Live;
   end Drop;

   function Is_Run_Of (Item : Entry_Record; Region_Index : Slot) return Boolean is
     (Item.Status /= Empty and then Item.Kind = Run and then Item.Parent = Region_Index);

   function Cuts (Item : Entry_Record; Address, Bytes : Unsigned_64) return Boolean is
     (Item.Count > 0 and then Policy.Overlaps (Item.Base, Policy.Page_Count (Item.Count), Address, Bytes));

   -- The lowest free range of Bytes in the aperture whose base is a
   -- multiple of Alignment (a page multiple), or 0. Runs lie inside their
   -- regions, so only regions take part.
   function Find_Base (PID : ProcessID; Bytes, Alignment : Unsigned_64) return Unsigned_64 is
      use Owned_Reservation_Policy;
      Candidate : Unsigned_64 := Align_Up (Owned_Memory_Layout.First, Alignment);
   begin
      for I of Records.Iterate (Tables(PID)'Access) loop
         declare
            Item : Entry_Record renames Entries(PID, I).Value.all;
         begin
            if Item.Kind = Region then
               if Candidate = 0 or else Candidate > Owned_Memory_Layout.Limit - Bytes then return 0; end if;
               if Candidate <= Item.Base and then Bytes <= Item.Base - Candidate then
                  return Candidate;
               end if;
               Candidate := Align_Up (Unsigned_64'Max (Candidate, Item.Base + Item.Bytes), Alignment);
            end if;
         end;
      end loop;
      return (if Candidate /= 0 and then Candidate <= Owned_Memory_Layout.Limit - Bytes then Candidate
              else 0);
   end Find_Base;

   procedure Reserve (PID : ProcessID; Bytes, Alignment : Unsigned_64; Base : out Unsigned_64) is
      Candidate : Unsigned_64;
      Index : Records.Id;
   begin
      Base := 0;
      if PID = NO_PROCESS or else not Policy.Valid_Capacity (Bytes)
        or else not Policy.Valid_Alignment (Alignment)
      then return; end if;
      Lock;
      lockAddressSpace (PID);
      if proctab(PID).admitted then
         Candidate := Find_Base (PID, Bytes, Policy.Effective_Alignment (Alignment));
         if Candidate /= 0 then
            Records.Insert (Tables(PID), Candidate,
              (Status => Live, Kind => Region, Owner => PID,
               Generation => generationOf (PID), Base => Candidate, Bytes => Bytes,
               others => <>), Index);
            if Index /= 0 then Base := Candidate; end if;
         end if;
      end if;
      unlockAddressSpace (PID);
      Unlock;
   end Reserve;

   -- Back every page of [Address, Address + Bytes) with a fresh zero page,
   -- read/write. Refused if any run of the region overlaps the range: a
   -- page is never mapped twice. A run that ends at Address and whose
   -- frames lead the frame list grows in place (one record for a region
   -- grown upward in steps); otherwise the range is a new run. All or
   -- nothing: the pages this call backed are dropped again on failure.
   -- Hand-off (huge pages): a commit of a 2 MiB-aligned, 2 MiB-multiple
   -- range is where a 2 MiB leaf could be chosen automatically, with no
   -- new parameter; that needs a run kind with one frame per 2 MiB and a
   -- Drop that splits a leaf, so it is not done here (ABI-002 results).
   procedure Commit (PID : ProcessID; Region_Index : Slot; Address, Bytes : Unsigned_64;
                     Success : out Boolean) is
      Index : Records.Id := 0;
      Extending : Boolean := False;
      Before, Added : Natural := 0;
      Storage : Process_Frames.Process_Frame;
      Result : Page_Allocation_Result;
      Capacity : Natural;
      Undone : Boolean;
   begin
      Success := False;
      for I of Records.Iterate (Tables(PID)'Access) loop
         declare
            Item : Entry_Record renames Entries(PID, I).Value.all;
         begin
            if Is_Run_Of (Item, Region_Index) then
               if Cuts (Item, Address, Bytes) then return; end if;
               if Item.Status = Live and then Item.Count > 0
                 and then Policy.Adjoins (Item.Base, Policy.Page_Count (Item.Count), Address)
                 and then Item.First_Node = proctab(PID).frames.head
               then
                  Index := I;
                  Extending := True;
               end if;
            end if;
         end;
      end loop;
      Capacity := Heap_Admission.Expanded_Capacity
        (Positive'Max (1, proctab(PID).frames.length),
         Natural (Bytes / Page) + Natural (proctab(PID).stackSize / 4096) +
           INITIAL_HEAP_FRAME_HEADROOM);
      if Capacity = 0 then return; end if;
      if not Extending then
         Records.Insert (Tables(PID), Address,
           (Status => Building, Kind => Run, Parent => Region_Index, Owner => PID,
            Generation => generationOf (PID), Base => Address, others => <>), Index);
         if Index = 0 then return; end if;
      end if;
      Before := Entries(PID, Index).Count;
      proctab(PID).frames.capacity := Natural'Max (Capacity, proctab(PID).frames.capacity);
      for Page_Index in 0 .. Natural (Bytes / Page) - 1 loop
         tryAddPage (proctab(PID), To_Address (Integer_Address (Address) +
                       Integer_Address (Page_Index) * 4096), Storage, Result);
         if Result /= Page_Added then
            Added := Entries(PID, Index).Count - Before;
            if Added > 0 then
               -- A new run is dropped whole (and its record released); an
               -- extended one goes back to its Before pages. A failed drop
               -- quarantines the run with its frames, kept until exit.
               Drop (PID, Index, (Lower => Before, Dropped => Added, Upper => 0), 0, Undone);
            elsif not Extending then
               Records.Release (Tables(PID), Index);
            end if;
            return;
         end if;
         declare
            Item : Entry_Record renames Entries(PID, Index).Value.all;
         begin
            if Item.Count = 0 then Item.Last_Node := proctab(PID).frames.head; end if;
            Item.First_Node := proctab(PID).frames.head;
            Item.Count := Item.Count + 1;
            Item.Bytes := Bytes_Of (Item.Count);
         end;
      end loop;
      Entries(PID, Index).Status := Live;
      Success := True;
   end Commit;

   -- Drop every backed page of [Address, Address + Bytes); the range stays
   -- reserved. Every run it cuts must be live, checked before any change.
   -- Runs are disjoint, so only a range strictly inside one run cuts it in
   -- the middle; that cut's upper part needs a record, reserved first, so
   -- a refusal changes nothing.
   procedure Decommit (PID : ProcessID; Region_Index : Slot; Address, Bytes : Unsigned_64;
                       Success : out Boolean) is
      Spare : Records.Id := 0;
      Cut : Run_Cut;
      Middle_Base : Unsigned_64 := 0;
      Dropped : Boolean;
   begin
      Success := False;
      for I of Records.Iterate (Tables(PID)'Access) loop
         declare
            Item : Entry_Record renames Entries(PID, I).Value.all;
         begin
            if Is_Run_Of (Item, Region_Index) and then Cuts (Item, Address, Bytes) then
               if Item.Status /= Live then return; end if;
               Cut := Cut_Of (Item, Address, Bytes);
               if Cut.Lower > 0 and Cut.Upper > 0 then Middle_Base := Address + Bytes; end if;
            end if;
         end;
      end loop;
      if Middle_Base /= 0 then
         Records.Insert (Tables(PID), Middle_Base,
           (Status => Building, Kind => Run, Parent => Region_Index, Owner => PID,
            Generation => generationOf (PID), Base => Middle_Base, others => <>), Spare);
         if Spare = 0 then return; end if;
      end if;
      Success := True;
      for I of Records.Iterate (Tables(PID)'Access) loop
         declare
            Item : Entry_Record renames Entries(PID, I).Value.all;
         begin
            if Is_Run_Of (Item, Region_Index) and then Item.Status = Live
              and then Cuts (Item, Address, Bytes)
            then
               Cut := Cut_Of (Item, Address, Bytes);
               Drop (PID, I, Cut, Spare, Dropped);
               Success := Success and Dropped;
               -- A middle cut is the only run the range touches.
               exit when Cut.Lower > 0 and Cut.Upper > 0;
            end if;
         end;
      end loop;
      if Spare /= 0 and then Entries(PID, Spare).Status = Building then
         Records.Release (Tables(PID), Spare);
      end if;
   end Decommit;

   -- Set the access of every page of [Address, Address + Bytes), which must
   -- all be committed (the runs cutting it cover it exactly). Revoke before
   -- installing, so no CPU keeps a wider translation; retain backing on
   -- every failure. Existing grants are unchanged.
   procedure Set_Access (PID : ProcessID; Region_Index : Slot; Address, Bytes : Unsigned_64;
                         Wanted : Policy.Page_Access; Success : out Boolean) is
      -- Only these three: owned memory is never executable.
      Selected : constant Virtmem.Regions.Access_Mode :=
        (case Wanted is
           when Policy.Inaccessible => Virtmem.Regions.Inaccessible,
           when Policy.Read_Only    => Virtmem.Regions.Read_Only,
           when Policy.Read_Write   => Virtmem.Regions.Read_Write);
      Covered : Natural := 0;
      procedure Change (Item : Entry_Record; Access_Value : Virtmem.Regions.Access_Mode;
                        OK : out Boolean) is
         Node : FrameLists.NodePtr := Item.First_Node;
         Address_Of_Page : Unsigned_64;
         Changed : Boolean;
      begin
         OK := True;
         for I in reverse 0 .. Item.Count - 1 loop
            Address_Of_Page := Item.Base + Bytes_Of (I);
            if Address_Of_Page >= Address and then Address_Of_Page - Address < Bytes then
               Virtmem.Regions.Set_Access
                 (addrtab(proctab(PID).pgTable), Integer_Address (Address_Of_Page),
                  Virtmem.PFN (Node.element / 4096), Access_Value, Changed);
               OK := OK and Changed;
            end if;
            Node := Node.next;
         end loop;
      end Change;
      OK : Boolean;
   begin
      Success := False;
      for I of Records.Iterate (Tables(PID)'Access) loop
         declare
            Item : Entry_Record renames Entries(PID, I).Value.all;
         begin
            if Is_Run_Of (Item, Region_Index) and then Cuts (Item, Address, Bytes) then
               if Item.Status /= Live then return; end if;
               if not Inventory_Valid (Item) then Item.Status := Quarantined; return; end if;
               Covered := Covered + Cut_Of (Item, Address, Bytes).Dropped;
            end if;
         end;
      end loop;
      if Bytes_Of (Covered) /= Bytes then return; end if;
      Success := True;
      for I of Records.Iterate (Tables(PID)'Access) loop
         declare
            Item : Entry_Record renames Entries(PID, I).Value.all;
         begin
            if Is_Run_Of (Item, Region_Index) and then Cuts (Item, Address, Bytes) then
               Item.Status := Quarantined;
               Change (Item, Virtmem.Regions.Inaccessible, OK);
               TLB_Shootdown.Invalidate_All;
               if OK and Wanted /= Policy.Inaccessible then
                  Change (Item, Selected, OK);
                  TLB_Shootdown.Invalidate_All;
               end if;
               if OK then Item.Status := Live; else Success := False; end if;
            end if;
         end;
      end loop;
   end Set_Access;

   procedure Change_Range (PID : ProcessID; Address, Bytes : Unsigned_64;
                           Operation : Owned_Reservation_Policy.Range_Operation;
                           Success : out Boolean) is
      Region_Index : Natural := 0;
   begin
      Success := False;
      if PID = NO_PROCESS then return; end if;
      -- A commit's frames are made clean first, with no lock held: the
      -- laundering and any pool growth (zeroing, TLB rounds) stay out of
      -- the locked step below, which every CPU's wait path contends for.
      if Policy."=" (Operation, Policy.Commit) and then Bytes <= Policy.Maximum_Commit_Bytes then
         Process_Pool.Prepare (Natural (Bytes / Page));
      end if;
      -- Exclude grant acquisition throughout mapping changes and rollback.
      Spinlocks.enterCriticalSection (grantLock);
      Lock;
      lockAddressSpace (PID);
      if proctab(PID).admitted then
         for I of Records.Iterate (Tables(PID)'Access) loop
            declare
               Item : Entry_Record renames Entries(PID, I).Value.all;
            begin
               if Item.Status = Live and then Item.Kind = Region and then Item.Owner = PID
                 and then Item.Generation = generationOf (PID)
                 and then Policy.Within (Item.Base, Item.Bytes, Address, Bytes)
               then Region_Index := I; exit; end if;
            end;
         end loop;
      end if;
      if Region_Index /= 0 then
         case Operation is
            when Policy.Commit =>
               if Policy.Commit_Admitted
                 (Entries(PID, Region_Index).Base, Entries(PID, Region_Index).Bytes, Address, Bytes)
               then
                  Commit (PID, Region_Index, Address, Bytes, Success);
               end if;
            when Policy.Decommit =>
               Decommit (PID, Region_Index, Address, Bytes, Success);
            when Policy.Access_Operation =>
               Set_Access (PID, Region_Index, Address, Bytes, Policy.Access_Of (Operation), Success);
         end case;
      end if;
      unlockAddressSpace (PID);
      Unlock;
      Spinlocks.exitCriticalSection (grantLock);
   end Change_Range;

   procedure Release (PID : ProcessID; Base, Bytes : Unsigned_64; Success : out Boolean) is
      -- How long a lock break lets the spinning waiters in.
      Break_Us : constant := 2;
      Region_Index : Natural := 0;
      Reclaimed : Boolean;

      -- Region_Index still names this release's quarantined region (another
      -- thread of PID may have finished releasing it while no lock was held).
      function Still_Ours return Boolean is
        (Entries(PID, Region_Index).Status = Quarantined
         and then Entries(PID, Region_Index).Kind = Region
         and then Entries(PID, Region_Index).Owner = PID
         and then Entries(PID, Region_Index).Generation = generationOf (PID)
         and then Entries(PID, Region_Index).Base = Base
         and then Entries(PID, Region_Index).Bytes = Bytes);

      procedure Take_Locks is
      begin
         Spinlocks.enterCriticalSection (grantLock);
         Lock;
         lockAddressSpace (PID);
      end Take_Locks;
      procedure Drop_Locks is
      begin
         unlockAddressSpace (PID);
         Unlock;
         Spinlocks.exitCriticalSection (grantLock);
      end Drop_Locks;

      -- The region's run with the lowest base at or above From, or 0.
      function Run_From (From : Unsigned_64) return Natural is
         Found : Natural := 0;
      begin
         for I of Records.Iterate (Tables(PID)'Access) loop
            if Is_Run_Of (Entries(PID, I).Value.all, Region_Index)
              and then Entries(PID, I).Base >= From
              and then (Found = 0 or else Entries(PID, I).Base < Entries(PID, Found).Base)
            then
               Found := I;
            end if;
         end loop;
         return Found;
      end Run_From;

      -- Phase 1, under the locks: clear the entries of at most
      -- Pages_Per_Locked_Step pages, continuing at (Next_Base, Next_Page);
      -- the frames stay the process's. Done when every run is unmapped.
      Next_Base : Unsigned_64 := 0;
      Next_Page : Natural := 0;
      procedure Unmap_Step (Done, Failed : out Boolean) is
         Child : constant Natural := Run_From (Next_Base);
         OK : Boolean;
         Last : Natural;
      begin
         Done := Child = 0;
         Failed := False;
         if Done then return; end if;
         if Entries(PID, Child).Base /= Next_Base then
            Next_Base := Entries(PID, Child).Base;
            Next_Page := 0;
         end if;
         Last := Natural'Min (Entries(PID, Child).Count,
                              Next_Page + Policy.Pages_Per_Locked_Step);
         for Page_Index in Next_Page .. Last - 1 loop
            Virtmem.unmapPage (Integer_Address (Next_Base) + Integer_Address (Page_Index) * 4096,
                               addrtab(proctab(PID).pgTable), OK);
            if not OK then
               Entries(PID, Child).Status := Quarantined;
               Failed := True;
               return;
            end if;
         end loop;
         if Last >= Entries(PID, Child).Count then
            -- This run is done: the next one has a higher base.
            Next_Base := Next_Base + 1;
            Next_Page := 0;
         else
            Next_Page := Last;
         end if;
      end Unmap_Step;

      -- Phase 3, under the locks: free at most Pages_Per_Locked_Step frames
      -- of the (unmapped, flushed) runs, the highest pages of one run
      -- first. Done when no run is left (the region record is then
      -- released); Failed when a run's retirement failed (it stays
      -- quarantined, with the region).
      procedure Free_Step (Done, Failed : out Boolean) is
         Child : constant Natural := Run_From (0);
      begin
         Done := False;
         Failed := False;
         if Child = 0 then
            Records.Release (Tables(PID), Region_Index);
            Done := True;
            return;
         end if;
         declare
            Count : constant Natural := Entries(PID, Child).Count;
            Taken : constant Natural := Natural'Min (Count, Policy.Pages_Per_Locked_Step);
         begin
            if Count = 0 then
               Records.Release (Tables(PID), Child);
               return;
            end if;
            Drop (PID, Child, (Lower => Count - Taken, Dropped => Taken, Upper => 0), 0, Reclaimed,
                  Unmapped => True);
            Failed := not Reclaimed;
         end;
      end Free_Step;

      -- Give the locks back and take them again: every CPU's wait and
      -- grant paths take grantLock, so one locked step is the longest a
      -- release holds them up. The spinlocks are not fair: retaken at
      -- once, this CPU wins them again before a waiter that is spinning on
      -- them sees them free (hda.drv and mixer.svc waited 12-14 ms in their
      -- waits behind a release, 2026-11), so the break lasts Break_Us.
      -- False when another thread releasing the same region finished it
      -- meanwhile.
      function Break return Boolean is
         Until_TSC : Unsigned_64;
      begin
         Drop_Locks;
         Until_TSC := x86.rdtsc + Break_Us * Time.tscPerDuration;
         while x86.rdtsc < Until_TSC loop
            System.Machine_Code.Asm ("pause", Volatile => True);
         end loop;
         Take_Locks;
         return Still_Ours;
      end Break;

      Done, Failed : Boolean := False;
   begin
      Success := False;
      if PID = NO_PROCESS or else not Policy.Valid_Capacity (Bytes) then return; end if;
      -- Quarantine the region (no commit, access change or grant reaches it
      -- again); unmap its pages in bounded steps; one TLB round for the
      -- whole region (a round interrupts every CPU, so one per region, not
      -- one per step); then free its frames in bounded steps.
      Take_Locks;
      for I of Records.Iterate (Tables(PID)'Access) loop
         if Entries(PID, I).Status in Live | Quarantined and then Entries(PID, I).Kind = Region
           and then Entries(PID, I).Owner = PID and then
           Entries(PID, I).Generation = generationOf (PID) and then
           Entries(PID, I).Base = Base and then Entries(PID, I).Bytes = Bytes
         then
            Entries(PID, I).Status := Quarantined;
            Region_Index := I;
            exit;
         end if;
      end loop;
      if Region_Index = 0 then
         Drop_Locks;
         return;
      end if;
      loop
         Unmap_Step (Done, Failed);
         exit when Done or else Failed;
         if not Break then
            Drop_Locks;
            return;
         end if;
      end loop;
      Drop_Locks;
      if Failed then
         return;
      end if;
      -- No CPU may still translate an unmapped page when its frame is freed.
      TLB_Shootdown.Invalidate_All;
      Take_Locks;
      if not Still_Ours then
         Drop_Locks;
         return;
      end if;
      loop
         Free_Step (Done, Failed);
         exit when Done or else Failed;
         if not Break then
            Drop_Locks;
            return;
         end if;
      end loop;
      Drop_Locks;
      Success := Done;
   end Release;

   procedure Forget_Exited (PID : ProcessID; Retired : out Natural) is
   begin
      Retired := 0;
      if proctab(PID).frames.length /= 0 then
         raise ProcessException with "Owned region exit before frame reclamation";
      end if;
      for Index of Records.Iterate (Tables(PID)'Access) loop
       declare
         Item : Entry_Record renames Entries(PID, Index).Value.all;
       begin
         if Item.Status /= Empty and Item.Owner = PID then
            -- Counted in regions; their runs' frames went with the list.
            if Item.Kind = Region then Retired := Retired + 1; end if;
            Records.Release (Tables(PID), Index);
         end if;
       end;
      end loop;
   end Forget_Exited;
end Process.Owned_Memory;

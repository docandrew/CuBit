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
with Interrupt_Windows;
with TextIO;
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
   -- list, in descending address order, and the Pages pages below its
   -- highest Skip each map their frame; when they reach its lowest page,
   -- the segment ends at Last_Node. A step checks only the pages it
   -- changes or frees, so a step of a long release, decommit or access
   -- change costs its own pages, not the whole run's.
   function Inventory_Valid (Item : Entry_Record; Skip, Pages : Natural) return Boolean is
      Node : FrameLists.NodePtr := proctab(Item.Owner).frames.head;
      Position : Natural := 0;
   begin
      if Item.Kind /= Run or Item.Count = 0 or Item.First_Node = null
        or Pages = 0 or Skip > Item.Count or Pages > Item.Count - Skip
      then return False; end if;
      for I in 1 .. proctab(Item.Owner).frames.length loop
         if Node = Item.First_Node then Position := I; exit; end if;
         Node := Node.next;
      end loop;
      if Position = 0 or else Item.Count > proctab(Item.Owner).frames.length - Position + 1
      then return False; end if;
      for I in 0 .. Skip + Pages - 1 loop
         if I >= Skip and then not Virtmem.Regions.Matches
           (addrtab(proctab(Item.Owner).pgTable),
            Integer_Address (Item.Base) + Integer_Address (Item.Count - I - 1) * 4096,
            Virtmem.PFN (Node.element / 4096)) then return False; end if;
         if I = Item.Count - 1 then return Node = Item.Last_Node; end if;
         Node := Node.next;
      end loop;
      return True;
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
        or else not Inventory_Valid (Item, Cut.Upper, Cut.Dropped)
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

   -- Refused reservations and commits go to the kernel log (BUG-009: a
   -- refused commit left no trace, and the allocator's null reached
   -- libgcc): the first Logged_Refusals in full, then every
   -- Refusal_Sample'th, with the running count.
   Logged_Refusals : constant := 32;
   Refusal_Sample : constant := 1_024;
   Refusals_Seen : Natural := 0 with Atomic;
   function Count_Refusal return Natural is
      Seen : constant Natural := Refusals_Seen;
   begin
      if Seen < Natural'Last then
         Refusals_Seen := Seen + 1;
      end if;
      return Seen + 1;
   end Count_Refusal;
   function Logged (Count : Natural) return Boolean is
     (Count <= Logged_Refusals or else Count mod Refusal_Sample = 0);

   procedure Reserve (PID : ProcessID; Bytes, Alignment : Unsigned_64; Base : out Unsigned_64;
                      Why : out Owned_Memory_ABI.Refusal) is
      Candidate : Unsigned_64 := 0;
      Index : Records.Id := 0;
      Admitted : Boolean;
   begin
      Base := 0;
      Why := Owned_Memory_ABI.Invalid_Range;
      if PID = NO_PROCESS then return; end if;
      if not Policy.Valid_Capacity (Bytes) or else not Policy.Valid_Alignment (Alignment) then
         Why := (if not Policy.Valid_Alignment (Alignment) then Owned_Memory_ABI.Invalid_Alignment
                 elsif Bytes > Policy.Maximum_Reserved_Bytes and then Bytes mod Page = 0
                 then Owned_Memory_ABI.Too_Large
                 else Owned_Memory_ABI.Invalid_Range);
         if Logged (Count_Refusal) then TextIO.println
           ("owned-memory: reserve refused pid=" & Natural'Image (Natural (PID)) &
            " bytes=" & Unsigned_64'Image (Bytes) & " alignment=" & Unsigned_64'Image (Alignment) &
            " reason=invalid-size-or-alignment"); end if;
         return;
      end if;
      Lock;
      lockAddressSpace (PID);
      Admitted := proctab(PID).admitted;
      if Admitted then
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
      Why := (if not Admitted then Owned_Memory_ABI.Not_Admitted
              elsif Candidate = 0 then Owned_Memory_ABI.No_Address_Space
              else Owned_Memory_ABI.No_Record);
      if Base = 0 and then Logged (Count_Refusal) then
         -- As a refused commit: no address range, or no record.
         TextIO.println
           ("owned-memory: reserve refused pid=" & Natural'Image (Natural (PID)) &
            " bytes=" & Unsigned_64'Image (Bytes) &
            " reason=" & (if not Admitted then "process-not-admitted"
                          elsif Candidate = 0 then "no-address-range" else "no-region-record"));
      end if;
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
   -- Why a commit was refused (BUG-009: a commit refused left no trace, and
   -- the allocator's null reached libgcc). Page: tryAddPage's answer.
   -- (MEM-007) Also why a decommit or access change was refused; each maps
   -- to the typed reason the caller gets (Owned_Memory_ABI, To_ABI).
   type Commit_Refusal is
     (No_Refusal, No_Region, Not_Admitted, Overlaps_Backed, No_Tracking_Capacity,
      No_Run_Record, Page_Refused, Process_Gone, Not_Backed_Range, Retire_Failed);
   Last_Refusal : Commit_Refusal := No_Refusal;
   Last_Page_Result : Page_Allocation_Result := Page_Added;

   -- The kernel has no enumeration images (No_Enumeration_Maps).
   function Name (Why : Commit_Refusal) return String is
     (case Why is
        when No_Refusal => "none",
        when No_Region => "no-live-region",
        when Not_Admitted => "not-admitted",
        when Overlaps_Backed => "overlaps-backed-pages",
        when No_Tracking_Capacity => "no-frame-tracking-capacity",
        when No_Run_Record => "no-run-record",
        when Page_Refused => "page-refused",
        when Process_Gone => "process-not-admitted",
        when Not_Backed_Range => "pages-not-committed",
        when Retire_Failed => "retirement-failed");
   function Name (Why : Page_Allocation_Result) return String is
     (case Why is
        when Page_Added => "added",
        when Frame_Tracking_Full => "frame-tracking-full",
        when Frame_Quota_Full => "frame-quota-full",
        when Physical_Memory_Exhausted => "physical-memory-exhausted",
        when Tracking_Storage_Exhausted => "tracking-storage-exhausted",
        when Frame_Ownership_Failed => "frame-ownership-failed",
        when Page_Table_Exhausted => "page-table-exhausted",
        when Mapping_Already_Present => "mapping-already-present");

   -- The reason the caller is told (Owned_Memory_ABI).
   function To_ABI (Why : Commit_Refusal; Page_Answer : Page_Allocation_Result)
                    return Owned_Memory_ABI.Refusal is
     (case Why is
        when No_Refusal | No_Region => Owned_Memory_ABI.No_Region,
        when Not_Admitted => Owned_Memory_ABI.Invalid_Range,
        when Overlaps_Backed => Owned_Memory_ABI.Already_Backed,
        when No_Tracking_Capacity | No_Run_Record => Owned_Memory_ABI.No_Record,
        when Process_Gone => Owned_Memory_ABI.Not_Admitted,
        when Not_Backed_Range => Owned_Memory_ABI.Not_Backed,
        when Retire_Failed => Owned_Memory_ABI.Retirement_Failed,
        when Page_Refused =>
          (case Page_Answer is
             when Frame_Quota_Full => Owned_Memory_ABI.Over_Quota,
             when Frame_Tracking_Full => Owned_Memory_ABI.No_Record,
             when Mapping_Already_Present => Owned_Memory_ABI.Already_Backed,
             when Frame_Ownership_Failed => Owned_Memory_ABI.Retirement_Failed,
             when Page_Added | Physical_Memory_Exhausted | Tracking_Storage_Exhausted |
                  Page_Table_Exhausted => Owned_Memory_ABI.Out_Of_Memory));

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
               if Cuts (Item, Address, Bytes) then
                  Last_Refusal := Overlaps_Backed;
                  return;
               end if;
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
      if Capacity = 0 then
         Last_Refusal := No_Tracking_Capacity;
         return;
      end if;
      if not Extending then
         Records.Insert (Tables(PID), Address,
           (Status => Building, Kind => Run, Parent => Region_Index, Owner => PID,
            Generation => generationOf (PID), Base => Address, others => <>), Index);
         if Index = 0 then
            Last_Refusal := No_Run_Record;
            return;
         end if;
      end if;
      Before := Entries(PID, Index).Count;
      proctab(PID).frames.capacity := Natural'Max (Capacity, proctab(PID).frames.capacity);
      for Page_Index in 0 .. Natural (Bytes / Page) - 1 loop
         tryAddPage (proctab(PID), To_Address (Integer_Address (Address) +
                       Integer_Address (Page_Index) * 4096), Storage, Result);
         if Result /= Page_Added then
            Last_Refusal := Page_Refused;
            Last_Page_Result := Result;
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
               if Item.Status /= Live then Last_Refusal := Retire_Failed; return; end if;
               Cut := Cut_Of (Item, Address, Bytes);
               if Cut.Lower > 0 and Cut.Upper > 0 then Middle_Base := Address + Bytes; end if;
            end if;
         end;
      end loop;
      if Middle_Base /= 0 then
         Records.Insert (Tables(PID), Middle_Base,
           (Status => Building, Kind => Run, Parent => Region_Index, Owner => PID,
            Generation => generationOf (PID), Base => Middle_Base, others => <>), Spare);
         if Spare = 0 then
            Last_Refusal := No_Run_Record;
            return;
         end if;
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
               if not Dropped then Last_Refusal := Retire_Failed; end if;
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
         -- Highest page first, as the frame list runs; done below the range.
         for I in reverse 0 .. Item.Count - 1 loop
            Address_Of_Page := Item.Base + Bytes_Of (I);
            exit when Address_Of_Page < Address;
            if Address_Of_Page - Address < Bytes then
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
               if Item.Status /= Live then Last_Refusal := Retire_Failed; return; end if;
               declare
                  Cut : constant Run_Cut := Cut_Of (Item, Address, Bytes);
               begin
                  if not Inventory_Valid (Item, Cut.Upper, Cut.Dropped) then
                     Item.Status := Quarantined;
                     Last_Refusal := Retire_Failed;
                     return;
                  end if;
               end;
               Covered := Covered + Cut_Of (Item, Address, Bytes).Dropped;
            end if;
         end;
      end loop;
      if Bytes_Of (Covered) /= Bytes then
         Last_Refusal := Not_Backed_Range;
         return;
      end if;
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
               if OK then
                  Item.Status := Live;
               else
                  Success := False;
                  Last_Refusal := Retire_Failed;
               end if;
            end if;
         end;
      end loop;
   end Set_Access;

   -- Before a decommit or access change goes in steps, under the locks:
   -- what the one-step call checks before it changes anything, over the
   -- whole range. Every run that cuts the range is live, and for an access
   -- change (Covered) the runs back every page of it. Steps then fail only
   -- when another thread of the process changed the range meanwhile.
   function Range_Acceptable (PID : ProcessID; Region_Index : Slot; Address, Bytes : Unsigned_64;
                              Covered : Boolean) return Boolean is
      Backed : Natural := 0;
   begin
      for I of Records.Iterate (Tables(PID)'Access) loop
         declare
            Item : Entry_Record renames Entries(PID, I).Value.all;
         begin
            if Is_Run_Of (Item, Region_Index) and then Cuts (Item, Address, Bytes) then
               if Item.Status /= Live then return False; end if;
               Backed := Backed + Cut_Of (Item, Address, Bytes).Dropped;
            end if;
         end;
      end loop;
      return not Covered or else Bytes_Of (Backed) = Bytes;
   end Range_Acceptable;

   -- Whether any run of the region backs a page of [Address, Address +
   -- Bytes): a commit of the whole range is then refused before its first
   -- step, so a refusal changes nothing.
   function Range_Backed (PID : ProcessID; Region_Index : Slot; Address, Bytes : Unsigned_64)
                          return Boolean is
   begin
      for I of Records.Iterate (Tables(PID)'Access) loop
         if Is_Run_Of (Entries(PID, I).Value.all, Region_Index)
           and then Cuts (Entries(PID, I).Value.all, Address, Bytes)
         then
            return True;
         end if;
      end loop;
      return False;
   end Range_Backed;

   -- A refused commit, in the kernel log: which process, where, how much
   -- and why. No lock is held here. Last_Refusal and Last_Page_Result are
   -- read unlocked: diagnostics only.
   procedure Report_Refused_Commit (PID : ProcessID; Address, Bytes : Unsigned_64) is
      Seen : constant Natural := Count_Refusal;
   begin
      if Logged (Seen) then
         TextIO.println
           ("owned-memory: commit refused pid=" & Natural'Image (Natural (PID)) &
            " address=" & Unsigned_64'Image (Address) & " bytes=" & Unsigned_64'Image (Bytes) &
            " reason=" & Name (Last_Refusal) &
            (if Last_Refusal = Page_Refused then " page=" & Name (Last_Page_Result) else "") &
            " refusals=" & Natural'Image (Seen));
      end if;
   end Report_Refused_Commit;

   procedure Change_Range (PID : ProcessID; Address, Bytes : Unsigned_64;
                           Operation : Owned_Reservation_Policy.Range_Operation;
                           Success : out Boolean; Why : out Owned_Memory_ABI.Refusal) is
      -- A commit whose frames another CPU took between Prepare and the
      -- locks gives the locks back and prepares again, up to this many
      -- times; the last attempt goes ahead and may launder or grow under
      -- the locks (one batch, one region), so the commit still succeeds.
      Commit_Attempts : constant := 3;
      -- A decommit or access change of a larger range goes in steps of
      -- this many pages (16 MiB), highest first, each under the locks with
      -- one TLB round (two for an access change), the locks given back and
      -- an interrupt window opened between steps. A step that fails ends
      -- the call; the steps before it stay done. A step also walks the
      -- run's frame list down to its pages (Inventory_Valid, Set_Access),
      -- so fewer, larger steps: a 256 MiB access change held grantLock
      -- 2-4 ms in one piece; in 512-page steps it took six times longer
      -- (mostly TLB rounds); in these, each step holds it about 0.5 ms.
      Pages_Per_Range_Step : constant := 4096;
      Step_Bytes : constant Unsigned_64 := Pages_Per_Range_Step * Page;
      -- A commit of any size goes in locked steps of at most
      -- Policy.Maximum_Commit_Bytes (MEM-007): Frames is the step's.
      Frames : Natural := 0;
      Is_Commit : constant Boolean := Policy."=" (Operation, Policy.Commit);
      Short : Boolean := False;

      -- One locked step over [From, From + Length); First_Step: the first
      -- of several, which checks the whole range first (Range_Acceptable).
      procedure Locked_Attempt (From, Length : Unsigned_64; Last_Attempt : Boolean;
                                First_Step : Boolean := False) is
         Region_Index : Natural := 0;
      begin
         Short := False;
         Success := False;
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
                    and then Policy.Within (Item.Base, Item.Bytes, From, Length)
                  then Region_Index := I; exit; end if;
               end;
            end loop;
         end if;
         Last_Refusal := (if Region_Index /= 0 then No_Refusal
                          elsif not proctab(PID).admitted then Process_Gone else No_Region);
         if Region_Index /= 0 and then First_Step
           and then not (Policy.Within (Entries(PID, Region_Index).Base, Entries(PID, Region_Index).Bytes,
                                        Address, Bytes)
                         and then (if Is_Commit
                                   then not Range_Backed (PID, Region_Index, Address, Bytes)
                                   else Range_Acceptable
                                          (PID, Region_Index, Address, Bytes,
                                           Covered => not Policy."=" (Operation, Policy.Decommit))))
         then
            Last_Refusal := (if Is_Commit then Overlaps_Backed else No_Region);
            Region_Index := 0;
         end if;
         if Region_Index /= 0 then
            case Operation is
               when Policy.Commit =>
                  if not Policy.Commit_Admitted
                    (Entries(PID, Region_Index).Base, Entries(PID, Region_Index).Bytes, From, Length)
                  then
                     Last_Refusal := Not_Admitted;
                  elsif not Last_Attempt and then BuddyAllocator.poolCleanFrames < Frames then
                     -- Laundering or growing here would zero and run a
                     -- TLB round under grantLock, which every CPU's
                     -- waits take: try again after preparing unlocked.
                     Short := True;
                  else
                     Commit (PID, Region_Index, From, Length, Success);
                  end if;
               when Policy.Decommit =>
                  Decommit (PID, Region_Index, From, Length, Success);
               when Policy.Access_Operation =>
                  Set_Access (PID, Region_Index, From, Length, Policy.Access_Of (Operation), Success);
            end case;
         end if;
         -- Why, while the locks still serialize Last_Refusal.
         if not Success and then not Short then
            Why := To_ABI (Last_Refusal, Last_Page_Result);
         end if;
         unlockAddressSpace (PID);
         Unlock;
         Spinlocks.exitCriticalSection (grantLock);
      end Locked_Attempt;

      Top, Step_Base : Unsigned_64;
      Done, Length : Unsigned_64 := 0;
      Rolled_Back : Boolean;
      Undo_Why : Owned_Memory_ABI.Refusal;
   begin
      Success := False;
      Why := Owned_Memory_ABI.Invalid_Range;
      if PID = NO_PROCESS then return; end if;
      if Is_Commit then
         -- Any size, in one call (MEM-007): the kernel commits the range in
         -- locked steps of at most Policy.Maximum_Commit_Bytes, upward, the
         -- locks given back and an interrupt window opened between steps
         -- (the first step checks that no page of the whole range is
         -- backed). Each step extends the run the one before it made, so
         -- the range is one run unless another thread's commit came between
         -- them. All or nothing: when a step is refused, the pages the
         -- steps before it backed are decommitted again.
         if Bytes = 0 or else Bytes mod Page /= 0 or else Address mod Page /= 0
           or else Address > Unsigned_64'Last - Bytes
         then
            Why := Owned_Memory_ABI.Invalid_Range;
            if Logged (Count_Refusal) then
               TextIO.println ("owned-memory: commit refused pid=" & Natural'Image (Natural (PID)) &
                               " address=" & Unsigned_64'Image (Address) &
                               " bytes=" & Unsigned_64'Image (Bytes) & " reason=invalid-range");
            end if;
            return;
         end if;
         loop
            Length := Unsigned_64'Min (Bytes - Done, Policy.Maximum_Commit_Bytes);
            Frames := Natural (Length / Page);
            for Attempt in 1 .. Commit_Attempts loop
               -- A step's frames are made clean first, with no lock held:
               -- the laundering and any pool growth (zeroing, TLB rounds)
               -- stay out of the locked step, which every CPU's wait path
               -- contends for.
               Process_Pool.Prepare (Frames);
               Locked_Attempt (Address + Done, Length, Last_Attempt => Attempt = Commit_Attempts,
                               First_Step => Done = 0);
               exit when not Short;
               Interrupt_Windows.Open (Hold_Us => Interrupt_Windows.Lock_Break_Us);
            end loop;
            exit when not Success;
            Done := Done + Length;
            exit when Done = Bytes;
            Interrupt_Windows.Open (Hold_Us => Interrupt_Windows.Lock_Break_Us);
         end loop;
         if not Success then
            Report_Refused_Commit (PID, Address, Bytes);
            if Done > 0 then
               -- Undo the steps already done (a decommit, in its own steps).
               Change_Range (PID, Address, Done, Policy.Decommit, Rolled_Back, Undo_Why);
               if not Rolled_Back then
                  TextIO.println ("owned-memory: a refused commit's first steps were not undone pid=" &
                                  Natural'Image (Natural (PID)) & " address=" & Unsigned_64'Image (Address) &
                                  " bytes=" & Unsigned_64'Image (Done) & " reason=" &
                                  Unsigned_64'Image (Owned_Memory_ABI.Refusal_Word (Undo_Why)));
               end if;
            end if;
         end if;
      elsif Bytes <= Step_Bytes or else Address > Unsigned_64'Last - Bytes
        or else Address mod Page /= 0 or else Bytes mod Page /= 0
      then
         -- One step (or a range the locked step refuses as it is).
         Locked_Attempt (Address, Bytes, Last_Attempt => True);
      else
         Top := Address + Bytes;
         loop
            Step_Base := (if Top - Address > Step_Bytes then Top - Step_Bytes else Address);
            Locked_Attempt (Step_Base, Top - Step_Base, Last_Attempt => True,
                            First_Step => Top = Address + Bytes);
            exit when not Success or else Step_Base = Address;
            Top := Step_Base;
            Interrupt_Windows.Open (Hold_Us => Interrupt_Windows.Lock_Break_Us);
         end loop;
      end if;
   end Change_Range;

   -- A whole region: exact base and capacity.
   procedure Release_Region (PID : ProcessID; Base, Bytes : Unsigned_64; Success : out Boolean;
                             Why : out Owned_Memory_ABI.Refusal) is
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
      -- release holds them up. The break is an interrupt window of
      -- Interrupt_Windows.Lock_Break_Us (spinning waiters take the locks;
      -- without the window a 64 MiB release held off this CPU's
      -- interrupts, the audio controller's and the timer's, for its whole
      -- ~30 ms, MEM-006). The thread may be switched away and back there,
      -- so Still_Ours checks the region again after.
      -- False when another thread releasing the same region finished it
      -- meanwhile.
      function Break return Boolean is
      begin
         Drop_Locks;
         Interrupt_Windows.Open (Hold_Us => Interrupt_Windows.Lock_Break_Us);
         Take_Locks;
         return Still_Ours;
      end Break;

      Done, Failed : Boolean := False;
   begin
      Success := False;
      Why := Owned_Memory_ABI.Invalid_Range;
      if PID = NO_PROCESS or else not Policy.Valid_Capacity (Bytes) then return; end if;
      -- Refused below: no such region, or (after Failed) a retirement.
      Why := Owned_Memory_ABI.No_Region;
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
         Why := Owned_Memory_ABI.Retirement_Failed;
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
      if Failed then Why := Owned_Memory_ABI.Retirement_Failed; end if;
   end Release_Region;

   -- munmap semantics (MEM-007, K5): every page of [Base, Base + Bytes)
   -- that lies in a live region of PID stops being part of it, across
   -- regions and the holes between them. A region the range covers is
   -- released whole (Release_Region); one it overlaps is first decommitted
   -- there (Change_Range, in its bounded steps), then shrinks: the range
   -- takes its head (the region moves to a record keyed by its new base,
   -- its runs following it), its tail, or its middle (the region splits:
   -- a second record, reserved before anything changes, takes the upper
   -- part and the runs above the range). Policy.Split names the parts, as
   -- it does a run's. Regions go lowest first; one that fails ends the
   -- call, the ones before it stay released (as munmap). A range that
   -- touches no region is refused (No_Region), so the caller can tell.
   procedure Release_Range (PID : ProcessID; Base, Bytes : Unsigned_64; Success : out Boolean;
                            Why : out Owned_Memory_ABI.Refusal) is
      Limit : Unsigned_64;
      Cursor : Unsigned_64 := Base;
      Touched : Boolean := False;
      -- A decommit can race another thread's commit into the range; give
      -- up after this many rounds rather than loop.
      Shrink_Attempts : constant := 3;

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

      function Live_Region (Item : Entry_Record) return Boolean is
        (Item.Status = Live and then Item.Kind = Region and then Item.Owner = PID
         and then Item.Generation = generationOf (PID));

      -- The lowest live region of PID overlapping [From, Limit), or 0.
      function Next_Region (From : Unsigned_64) return Natural is
         Found : Natural := 0;
      begin
         for I of Records.Iterate (Tables(PID)'Access) loop
            declare
               Item : Entry_Record renames Entries(PID, I).Value.all;
            begin
               if Live_Region (Item) and then Item.Base < Limit
                 and then Item.Base + Item.Bytes > From
                 and then (Found = 0 or else Item.Base < Entries(PID, Found).Base)
               then
                  Found := I;
               end if;
            end;
         end loop;
         return Found;
      end Next_Region;

      Region_Index : Natural;
      Region_Base, Region_Bytes, Cut_Base, Cut_Bytes, Upper_Base : Unsigned_64;
      Cut : Policy.Run_Split;
      Spare, Moved : Records.Id := 0;
      Decommitted, Shrunk : Boolean;
   begin
      Success := False;
      Why := Owned_Memory_ABI.Invalid_Range;
      if PID = NO_PROCESS or else Bytes = 0 or else Bytes mod Page /= 0
        or else Base mod Page /= 0 or else Base > Unsigned_64'Last - Bytes
      then
         return;
      end if;
      Limit := Base + Bytes;
      loop
         Take_Locks;
         Region_Index := Next_Region (Cursor);
         if Region_Index = 0 then
            Drop_Locks;
            exit;
         end if;
         Touched := True;
         Region_Base := Entries(PID, Region_Index).Base;
         Region_Bytes := Entries(PID, Region_Index).Bytes;
         Cut := Policy.Split (Region_Base, Region_Bytes / Page,
                              Unsigned_64'Max (Cursor, Region_Base),
                              Unsigned_64'Min (Limit, Region_Base + Region_Bytes) -
                                Unsigned_64'Max (Cursor, Region_Base));
         Drop_Locks;
         Cut_Base := Region_Base + Cut.Lower * Page;
         Cut_Bytes := Cut.Dropped * Page;
         Upper_Base := Cut_Base + Cut_Bytes;
         if Cut.Lower = 0 and then Cut.Upper = 0 then
            Release_Region (PID, Region_Base, Region_Bytes, Success, Why);
            if not Success then return; end if;
         else
            Shrunk := False;
            for Attempt in 1 .. Shrink_Attempts loop
               Change_Range (PID, Cut_Base, Cut_Bytes, Policy.Decommit, Decommitted, Why);
               if not Decommitted then return; end if;
               Take_Locks;
               -- The region as it was, with nothing backed in the cut now.
               if not Records.Present (Tables(PID), Region_Index)
                 or else not Live_Region (Entries(PID, Region_Index).Value.all)
                 or else Entries(PID, Region_Index).Base /= Region_Base
                 or else Entries(PID, Region_Index).Bytes /= Region_Bytes
               then
                  Drop_Locks;
                  Why := Owned_Memory_ABI.No_Region;
                  return;
               end if;
               if not Range_Backed (PID, Region_Index, Cut_Base, Cut_Bytes) then
                  if Cut.Lower > 0 and then Cut.Upper > 0 then
                     -- A middle cut: the upper part becomes a region of its own.
                     Records.Insert (Tables(PID), Upper_Base,
                       (Status => Live, Kind => Region, Owner => PID,
                        Generation => generationOf (PID), Base => Upper_Base,
                        Bytes => Cut.Upper * Page, others => <>), Spare);
                     if Spare = 0 then
                        Drop_Locks;
                        Why := Owned_Memory_ABI.No_Record;
                        return;
                     end if;
                     for I of Records.Iterate (Tables(PID)'Access) loop
                        if Is_Run_Of (Entries(PID, I).Value.all, Region_Index)
                          and then Entries(PID, I).Base >= Upper_Base
                        then
                           Entries(PID, I).Parent := Spare;
                        end if;
                     end loop;
                     Entries(PID, Region_Index).Bytes := Cut.Lower * Page;
                  elsif Cut.Upper = 0 then
                     -- The tail: the region keeps its base (and its key).
                     Entries(PID, Region_Index).Bytes := Cut.Lower * Page;
                  else
                     -- The head: the region moves to a record keyed by its
                     -- new base (the table orders regions by base), its runs
                     -- following it.
                     declare
                        Copy : Entry_Record := Entries(PID, Region_Index).Value.all;
                     begin
                        Copy.Base := Upper_Base;
                        Copy.Bytes := Cut.Upper * Page;
                        Records.Insert (Tables(PID), Upper_Base, Copy, Moved);
                     end;
                     if Moved = 0 then
                        Drop_Locks;
                        Why := Owned_Memory_ABI.No_Record;
                        return;
                     end if;
                     for I of Records.Iterate (Tables(PID)'Access) loop
                        if Is_Run_Of (Entries(PID, I).Value.all, Region_Index) then
                           Entries(PID, I).Parent := Moved;
                        end if;
                     end loop;
                     Records.Release (Tables(PID), Region_Index);
                  end if;
                  Shrunk := True;
               end if;
               Drop_Locks;
               exit when Shrunk;
               -- Another thread committed into the cut meanwhile: again.
               Interrupt_Windows.Open (Hold_Us => Interrupt_Windows.Lock_Break_Us);
            end loop;
            if not Shrunk then
               Why := Owned_Memory_ABI.Already_Backed;
               return;
            end if;
         end if;
         Cursor := Upper_Base;
         exit when Cursor >= Limit;
         Interrupt_Windows.Open (Hold_Us => Interrupt_Windows.Lock_Break_Us);
      end loop;
      Success := Touched;
      if not Touched then
         Why := Owned_Memory_ABI.No_Region;
      end if;
   end Release_Range;

   procedure Release (PID : ProcessID; Base, Bytes : Unsigned_64; Success : out Boolean;
                      Why : out Owned_Memory_ABI.Refusal) is
      Exact : Boolean := False;
   begin
      Lock;
      for I of Records.Iterate (Tables(PID)'Access) loop
         if Entries(PID, I).Status in Live | Quarantined and then Entries(PID, I).Kind = Region
           and then Entries(PID, I).Owner = PID
           and then Entries(PID, I).Base = Base and then Entries(PID, I).Bytes = Bytes
         then
            Exact := True;
            exit;
         end if;
      end loop;
      Unlock;
      if Exact then
         -- The common case: one region, whole (its retirement in steps).
         Release_Region (PID, Base, Bytes, Success, Why);
      else
         Release_Range (PID, Base, Bytes, Success, Why);
      end if;
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

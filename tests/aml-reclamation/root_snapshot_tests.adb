with Ada.Text_IO;
with AML_Identity.Issuer;
with AML_Object_Identifiers; use AML_Object_Identifiers;
with AML_Objects;
with AML_Objects.Reclamation;
with AML_Objects.Root_Snapshots;
procedure Root_Snapshot_Tests is
   package O renames AML_Objects;
   package R renames O.Root_Snapshots;
   package C renames O.Reclamation;
   use type O.Allocation_Status;
   use type O.State;
   use type R.Snapshot;
   use type R.Snapshot_Phase;
   use type R.Result_Status;
   use type C.Keep_Set;
   use type C.Reclaim_Status;
   Store : O.State := O.Empty;
   Roots : R.Snapshot := R.Empty;
   Keep, Expected : C.Keep_Set := [others => False];
   Owner, Foreign_Owner : AML_Identity.Identity;
   OK : Boolean;
   Status : R.Result_Status;
   Alloc : O.Allocation_Status;
   Collected : C.Reclaim_Status;
   Scratch : C.Workspace;
   ID, Other : Object_ID;
   Old : Object_Address;
   Checks : Natural := 0;
   procedure Check (Good : Boolean) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Checks'Image & Status'Image; end if; end Check;
   procedure Resolve (Wanted : R.Result_Status; As_Owner : AML_Identity.Identity) is
      Prior : constant R.Snapshot := Roots;
      Heap : constant O.State := Store;
   begin
      Keep := [others => True]; -- every failure must clear a prefilled result
      R.Resolve (Roots, As_Owner, Store, Keep, Status);
      Check (Status = Wanted and then Keep = Expected and then Roots = Prior and then Store = Heap);
      if Wanted = R.Ready then pragma Assert (R.Resolved (Roots, As_Owner, Store, Keep)); end if;
   end Resolve;
   procedure Begin_Build is
   begin R.Begin_Build (Roots, Owner, Status); Check (Status = R.Ready and then R.Phase (Roots) = R.Building); end Begin_Build;
   procedure Finish is
   begin R.Finish (Roots, Owner, Status); Check (Status = R.Ready and then R.Phase (Roots) = R.Published); end Finish;
begin
   AML_Identity.Issuer.Issue (Owner, OK); Check (OK);
   AML_Identity.Issuer.Issue (Foreign_Owner, OK); Check (OK);
   Resolve (R.Invalid_Owner, AML_Identity.No_Identity);
   Resolve (R.Ready, Owner);
   R.Begin_Build (Roots, AML_Identity.No_Identity, Status); Check (Status = R.Invalid_Owner);
   Resolve (R.Owner_Mismatch, Owner);
   Begin_Build; Resolve (R.Incomplete_Snapshot, Owner);
   Finish; Resolve (R.Ready, Owner); Resolve (R.Owner_Mismatch, Foreign_Owner);
   R.Finish (Roots, Owner, Status); Check (Status = R.Wrong_Phase and then R.Phase (Roots) = R.Rejected);
   Resolve (R.Invalid_Snapshot, Owner);
   Begin_Build; R.Reject (Roots); Resolve (R.Invalid_Snapshot, Owner);
   R.Clear (Roots); Check (Roots = R.Empty); Resolve (R.Ready, Owner);
   O.New_Integer (Store, 1, ID, Alloc); Check (Alloc = O.Allocated);
   O.New_Integer (Store, 2, Other, Alloc); Check (Alloc = O.Allocated);
   Old := O.Address_Of (Store, ID);
   Begin_Build;
   R.Include (Roots, Owner, Old, Status); Check (Status = R.Ready);
   declare Before : constant R.Snapshot := Roots; begin
      R.Include (Roots, Owner, Old, Status); Check (Status = R.Ready and then Roots = Before);
   end;
   Resolve (R.Incomplete_Snapshot, Owner);
   Finish; Expected (ID) := True; Resolve (R.Ready, Owner);
   Expected := [others => False]; Resolve (R.Owner_Mismatch, Foreign_Owner);
   R.Include (Roots, Owner, O.Address_Of (Store, Other), Status);
   Check (Status = R.Wrong_Phase and then R.Phase (Roots) = R.Rejected);
   Resolve (R.Invalid_Snapshot, Owner);
   Begin_Build; R.Include (Roots, Owner, Old, Status); Check (Status = R.Ready);
   R.Include (Roots, Owner, Make_Address (ID, Incarnation_Of (Old) + 1), Status);
   Check (Status = R.Generation_Conflict and then R.Phase (Roots) = R.Rejected);
   R.Finish (Roots, Owner, Status); Check (Status = R.Wrong_Phase);
   Resolve (R.Invalid_Snapshot, Owner);
   Begin_Build; R.Include (Roots, Owner, Old, Status); Check (Status = R.Ready);
   R.Include (Roots, Owner, No_Address, Status); Check (Status = R.Invalid_Address);
   Resolve (R.Invalid_Snapshot, Owner);
   Begin_Build; R.Include (Roots, Foreign_Owner, Old, Status); Check (Status = R.Owner_Mismatch);
   Resolve (R.Invalid_Snapshot, Owner);
   Begin_Build; R.Include (Roots, AML_Identity.No_Identity, Old, Status); Check (Status = R.Invalid_Owner);
   Resolve (R.Invalid_Snapshot, Owner);
   Begin_Build; R.Include (Roots, Owner, Old, Status); Check (Status = R.Ready);
   R.Finish (Roots, Foreign_Owner, Status); Check (Status = R.Owner_Mismatch);
   Resolve (R.Invalid_Snapshot, Owner);
   -- A genuine reclamation/reuse cycle, not just an invented stale stamp.
   Begin_Build; R.Include (Roots, Owner, Old, Status); Check (Status = R.Ready); Finish;
   C.Reclaim (Store, Owner, Expected, Scratch, Collected); Check (Collected = C.Reclaimed);
   Resolve (R.Stale_Root, Owner);
   O.New_Integer (Store, 9, Other, Alloc); Check (Alloc = O.Allocated and then Other = ID);
   Check (not O.Matches_Address (Store, Old)); Resolve (R.Stale_Root, Owner);
   Begin_Build; R.Include (Roots, Owner, O.Address_Of (Store, Other), Status); Check (Status = R.Ready);
   Finish; Expected (Other) := True; Resolve (R.Ready, Owner);
   -- The new snapshot contains exactly its new set, never the prior set's tail.
   Begin_Build; Finish; Expected := [others => False]; Resolve (R.Ready, Owner);
   -- All object slots fit; duplicate inclusion consumes no additional capacity.
   Store := O.Empty; Begin_Build;
   for Slot in 1 .. Max_Objects loop
      O.New_Integer (Store, 0, ID, Alloc); Check (Alloc = O.Allocated and then ID = Slot);
      R.Include (Roots, Owner, O.Address_Of (Store, ID), Status); Check (Status = R.Ready);
   end loop;
   Finish; Expected := [others => True]; Resolve (R.Ready, Owner);
   R.Clear (Roots); Expected := [others => False]; Resolve (R.Ready, Owner);
   Ada.Text_IO.Put_Line ("ROOT SNAPSHOTS" & Checks'Image);
   Ada.Text_IO.Put_Line ("Hosted snapshot bits" & R.Snapshot'Size'Image);
end Root_Snapshot_Tests;

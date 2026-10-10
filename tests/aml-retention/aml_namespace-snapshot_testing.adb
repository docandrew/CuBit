with AML_Objects.Root_Snapshots;
with AML_Identity.Issuer;
package body AML_Namespace.Snapshot_Testing is
   procedure Run (Checks : out Natural) is
      use Owned;
      package R renames AML_Objects.Root_Snapshots;
      package O renames AML_Objects;
      use type R.Result_Status;
      use type O.Allocation_Status;
      use type Frame_Pins.Result_Status;
      use type Object_Root_Set;
      A : Arena;
      Roots : R.Snapshot := R.Empty;
      Root_Status : R.Result_Status;
      Status : Root_Trace_Status;
      Ledger_Status : Frame_Pins.Result_Status;
      Scratch : Root_Workspace;
      Keep, Expected : Object_Root_Set := [others => False];
      ID, Other : O.Object_ID;
      Alloc : O.Allocation_Status;
      Foreign : AML_Identity.Identity;
      First, Second : AML_Frame_Handles.Frame_Handle;
      OK : Boolean;
      procedure Check (Good : Boolean) is
      begin Checks := Checks + 1; if not Good then raise Program_Error with Checks'Image; end if; end Check;
      procedure Trace (Wanted : Root_Trace_Status) is
         Before : constant State := Snapshot (A);
      begin
         Keep := [others => True];
         Trace_Owner_Roots (A, [], Scratch, Keep, Status);
         Check (Status = Wanted and then Keep = Expected and then Snapshot (A) = Before);
      end Trace;
      procedure Publish (Frame : AML_Frame_Handles.Frame_Handle) is
      begin
         Frame_Pins.Update_Snapshot (A.Frame_Roots, Frame, Roots, Ledger_Status);
         Check (Ledger_Status = Frame_Pins.Ready and then Valid (A));
      end Publish;
   begin
      Checks := 0;
      Reset (A, OK); Check (OK);
      AML_Identity.Issuer.Issue (Foreign, OK); Check (OK);
      O.New_Integer (A.Tree.Values, 7, ID, Alloc); Check (Alloc = O.Allocated);
      O.New_Integer (A.Tree.Values, 9, Other, Alloc); Check (Alloc = O.Allocated);
      First := AML_Frame_Handles.Bind_Frame (AML_Frame_Handles.Bind_Domain (A.Token, 1), 1, 1);
      Second := AML_Frame_Handles.Bind_Frame (AML_Frame_Handles.Bind_Domain (A.Token, 1), 2, 1);
      Frame_Pins.Reserve (A.Frame_Roots, First, Ledger_Status); Check (Ledger_Status = Frame_Pins.Ready);
      Frame_Pins.Reserve (A.Frame_Roots, Second, Ledger_Status); Check (Ledger_Status = Frame_Pins.Ready);
      Trace (Roots_Traced); -- allocated objects alone are not roots
      R.Begin_Build (Roots, A.Token, Root_Status); Check (Root_Status = R.Ready);
      R.Include (Roots, A.Token, O.Address_Of (A.Tree.Values, ID), Root_Status); Check (Root_Status = R.Ready);
      Publish (First); Trace (Invalid_Expression_Root);
      R.Finish (Roots, A.Token, Root_Status); Check (Root_Status = R.Ready);
      Publish (First); Expected (ID) := True; Trace (Roots_Traced);
      R.Begin_Build (Roots, A.Token, Root_Status);
      R.Include (Roots, A.Token, O.Address_Of (A.Tree.Values, Other), Root_Status);
      R.Finish (Roots, A.Token, Root_Status);
      Publish (Second); Expected (Other) := True; Trace (Roots_Traced);
      R.Reject (Roots); Publish (Second);
      Expected := [others => False]; Trace (Invalid_Expression_Root); -- no partial first-frame roots
      R.Begin_Build (Roots, Foreign, Root_Status);
      R.Include (Roots, Foreign, O.Address_Of (A.Tree.Values, Other), Root_Status);
      R.Finish (Roots, Foreign, Root_Status); Publish (Second); Trace (Invalid_Expression_Root);
      R.Begin_Build (Roots, A.Token, Root_Status);
      R.Include (Roots, A.Token, AML_Object_Identifiers.Make_Address (Other,
        O.Last_Incarnation (A.Tree.Values, Other) + 1), Root_Status);
      R.Finish (Roots, A.Token, Root_Status); Publish (Second); Trace (Invalid_Expression_Root);
      Roots := R.Empty; Publish (Second); Expected (ID) := True; Trace (Roots_Traced);
      Frame_Pins.Release (A.Frame_Roots, First, Ledger_Status); Check (Ledger_Status = Frame_Pins.Ready);
      Expected := [others => False]; Trace (Roots_Traced);
      Frame_Pins.Release (A.Frame_Roots, Second, Ledger_Status); Check (Ledger_Status = Frame_Pins.Ready);
      Check (Frame_Root_Count (A) = 0); Trace (Roots_Traced);
   end Run;
end AML_Namespace.Snapshot_Testing;

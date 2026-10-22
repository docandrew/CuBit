with Ada.Text_IO;
with AML_Identity;
with AML_Identity.Issuer;
with AML_References;
with AML_Delays;
with AML_Execute;
with AML_Namespace;
procedure Root_Reference_Tests is
   package N is new AML_Namespace (Capacity => 8, Perform_Delay => AML_Delays.Unavailable_Provider);
   use type N.State;
   use type AML_Execute.Execution_Status;
   use type AML_Identity.Identity;
   use type AML_References.Reference;
   use type AML_References.Reference_Kind;
   First, Second : AML_Identity.Identity;
   Root_Ref : AML_References.Reference;
   Success : Boolean;
   A, B : N.Owned.Arena;
   Owned_Root, Fresh_Root : AML_References.Reference;
   Before : N.State;
   Write_Status : AML_Execute.Execution_Status;
   Value : AML_Execute.Datum;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Message : String) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Message; end if;
   end Check;
begin
   AML_Identity.Issuer.Issue (First, Success);
   Check (Success and then AML_Identity.Issuer.Is_Issued (First), "first actual issued identity");
   AML_Identity.Issuer.Issue (Second, Success);
   Check (Success and then AML_Identity.Issuer.Is_Issued (Second) and then First /= Second,
     "distinct actual issued identity");
   Check (AML_References.Bind_Root (AML_Identity.No_Identity) = AML_References.No_Reference,
     "no owner cannot bind root");
   Root_Ref := AML_References.Bind_Root (First);
   Check (AML_References.Kind (Root_Ref) = AML_References.Root_Namespace
     and then AML_References.Well_Formed (Root_Ref), "explicit well formed root metadata reference");
   Check (AML_References.Belongs_To (Root_Ref, First), "root metadata bound to owner");
   Check (not AML_References.Belongs_To (Root_Ref, Second)
     and then not AML_References.Belongs_To (Root_Ref, AML_Identity.No_Identity), "cross-owner root rejection");
   Check (AML_References.Target (Root_Ref) = 0 and then AML_References.Offset (Root_Ref) = 0,
     "root metadata has no value-object target or byte offset");
   Check (AML_References.Kind (Root_Ref) not in AML_References.Named_Cell | AML_References.Byte_Slot
     | AML_References.Package_Slot | AML_References.Frame_Cell | AML_References.Name_Member,
     "root metadata does not impersonate a data reference kind");
   N.Owned.Reset (A, Success); Check (Success, "initialize first actual arena");
   N.Owned.Reset (B, Success); Check (Success, "initialize second actual arena");
   N.Owned.Make_Named_Identity (A, N.Root, Owned_Root, Success);
   Check (Success and then N.Owned.Named_Identity_Matches (A, Owned_Root), "actual root arena identity");
   Check (not N.Owned.Named_Identity_Matches (B, Owned_Root), "foreign arena rejects root identity");
   Check (not N.Owned.Matches (A, Owned_Root), "same-owner root metadata is not a data cell");
   Before := N.Owned.Snapshot (A);
   N.Owned.Store_Integer (A, Owned_Root, 1, Write_Status);
   Check (Write_Status = AML_Execute.Unsupported_Value and then N.Owned.Snapshot (A) = Before,
     "root metadata cannot write data in its own arena");
   N.Owned.Resolve_Value (A, Owned_Root, Value, Write_Status);
   Check (Write_Status = AML_Execute.Unsupported_Value, "root metadata cannot read a data cell");
   N.Owned.Reset (A, Success); Check (Success, "reset actual owner");
   Check (not N.Owned.Named_Identity_Matches (A, Owned_Root), "old root identity stale after reset");
   N.Owned.Make_Named_Identity (A, N.Root, Fresh_Root, Success);
   Check (Success and then Fresh_Root /= Owned_Root
     and then N.Owned.Named_Identity_Matches (A, Fresh_Root), "fresh root identity after reset");
   Ada.Text_IO.Put_Line ("Root reference checks" & Checks'Image);
end Root_Reference_Tests;

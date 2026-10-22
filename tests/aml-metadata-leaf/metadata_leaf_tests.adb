with Ada.Text_IO;
with AML_Identity;
with AML_Identity.Issuer;
with AML_References;
with AML_Index_Handles;
with AML_Object_Identifiers;
with AML_Objects;
with AML_Objects.Copies;
with AML_Objects.Reachability;
with AML_Objects.Reclamation;
procedure Metadata_Leaf_Tests is
   package O renames AML_Objects;
   package R renames AML_References;
   package Trace renames AML_Objects.Reachability;
   package GC renames AML_Objects.Reclamation;
   use type O.State;
   use type O.Object_Kind;
   use type O.Allocation_Status;
   use type R.Reference;
   use type Trace.Trace_Status;
   use type GC.Reclaim_Status;
   use type GC.Keep_Set;
   Owner, Other : AML_Identity.Identity;
   Success : Boolean;
   Root, Named, Foreign, Index_Ref : R.Reference;
   Store, Prior : O.State := O.Empty;
   Meta, Named_Meta, Foreign_Meta, Explicit_Ref, Garbage, Package_ID, Copy_ID, Fresh : O.Object_ID;
   Status : O.Allocation_Status;
   Witness : O.Copies.Copy_Witness;
   Seeds, Keep : GC.Keep_Set := [others => False];
   Targets : Trace.Reference_Targets := [others => AML_Object_Identifiers.No_Address];
   Trace_Work : Trace.Workspace;
   GC_Work : GC.Workspace;
   Trace_Status : Trace.Trace_Status;
   GC_Status : GC.Reclaim_Status;
   Checks : Natural := 0;
   First_Node : constant R.Node_Position := 1;
   First_Stamp : constant R.Node_Incarnation := 1;
   Package_Size : constant := 3;
   procedure Check (Good : Boolean; Message : String) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Message; end if;
   end Check;
   procedure Reject (Value : R.Reference; Label : String) is
   begin
      Prior := Store;
      O.New_Metadata (Store, Value, Fresh, Status);
      Check (Status = O.Invalid_Reference and then Fresh = O.No_Object
        and then Store = Prior, Label);
   end Reject;
   procedure Check_Metadata_Copy (Source : O.Object_ID; Value : R.Reference; Label : String) is
      Copy : O.Object_ID;
      Bytes_Before : constant Natural := O.Byte_Count (Store);
      Elements_Before : constant Natural := O.Element_Count (Store);
   begin
      O.Copies.Clone (Store, Source, Copy, Status, Witness);
      Check (Status = O.Allocated and then Copy /= Source
        and then O.Kind (Store, Copy) = O.Metadata_Object
        and then O.Metadata_Data (Store, Copy) = Value
        and then O.Metadata_Data (Store, Source) = Value
        and then O.Byte_Count (Store) = Bytes_Before
        and then O.Element_Count (Store) = Elements_Before, Label);
   end Check_Metadata_Copy;
begin
   AML_Identity.Issuer.Issue (Owner, Success); Check (Success, "issued owner");
   AML_Identity.Issuer.Issue (Other, Success); Check (Success, "issued foreign owner");
   Root := R.Bind_Root (Owner);
   Named := R.Bind_Named (Owner, First_Node, First_Stamp);
   Foreign := R.Bind_Root (Other);
   Reject (R.No_Reference, "absent metadata rejected atomically");
   Reject (R.Bind_Name_Member (Owner, First_Node, First_Stamp), "deferred name is not metadata");
   O.New_Bytes (Store, O.Buffer_Object, [1 => 42], Garbage, Status);
   Check (Status = O.Allocated, "buffer for actual index reference");
   Index_Ref := R.Bind (Owner, AML_Index_Handles.Bind_Byte (O.Address_Of (Store, Garbage), 0));
   Check (R.Well_Formed (Index_Ref), "actual well-formed index");
   Reject (Index_Ref, "index reference rejected as metadata");
   O.New_Metadata (Store, Root, Meta, Status);
   Check (Status = O.Allocated and then O.Kind (Store, Meta) = O.Metadata_Object
     and then O.Metadata_Data (Store, Meta) = Root and then O.Length (Store, Meta) = 0,
     "root is distinct metadata leaf");
   O.New_Metadata (Store, Named, Named_Meta, Status);
   Check (Status = O.Allocated and then O.Metadata_Data (Store, Named_Meta) = Named,
     "named identity representation retained without owner authentication");
   O.New_Metadata (Store, Foreign, Fresh, Status);
   Check (Status = O.Allocated and then O.Metadata_Data (Store, Fresh) = Foreign,
     "pool does not pretend to authenticate foreign identity");
   Foreign_Meta := Fresh;
   Check_Metadata_Copy (Named_Meta, Named, "standalone named metadata copy preserves identity");
   Check_Metadata_Copy (Foreign_Meta, Foreign, "standalone foreign metadata copy is representation only");
   O.New_Package (Store, 2, Package_ID, Status);
   Check (Status = O.Allocated, "nested named and foreign metadata package");
   O.Set_Element (Store, Package_ID, 0, Named_Meta);
   O.Set_Element (Store, Package_ID, 1, Foreign_Meta);
   O.Copies.Clone (Store, Package_ID, Copy_ID, Status, Witness);
   Check (Status = O.Allocated and then Copy_ID /= Package_ID, "nested metadata package copied");
   declare
      Named_Copy : constant O.Object_ID := O.Element (Store, Copy_ID, 0);
      Foreign_Copy : constant O.Object_ID := O.Element (Store, Copy_ID, 1);
   begin
      Check (Named_Copy /= Named_Meta and then Foreign_Copy /= Foreign_Meta
        and then Named_Copy /= Foreign_Copy
        and then O.Kind (Store, Named_Copy) = O.Metadata_Object
        and then O.Kind (Store, Foreign_Copy) = O.Metadata_Object
        and then O.Metadata_Data (Store, Named_Copy) = Named
        and then O.Metadata_Data (Store, Foreign_Copy) = Foreign,
        "nested copy retains exact named and foreign identities in distinct wrappers");
      O.Set_Element (Store, Copy_ID, 0, O.No_Object);
      Check (O.Element (Store, Package_ID, 0) = Named_Meta,
        "nested named metadata copy container is independent");
   end;
   O.New_Reference (Store, Root, Explicit_Ref, Status);
   Check (Status = O.Allocated and then O.Kind (Store, Explicit_Ref) = O.Reference_Object,
     "explicit reference remains separate from metadata");
   O.New_Package (Store, Package_Size, Package_ID, Status);
   Check (Status = O.Allocated, "package allocation");
   O.Set_Element (Store, Package_ID, 0, Meta);
   O.Set_Element (Store, Package_ID, 1, Meta);
   O.Set_Element (Store, Package_ID, 2, Explicit_Ref);
   O.Copies.Clone (Store, Package_ID, Copy_ID, Status, Witness);
   Check (Status = O.Allocated and then Copy_ID /= Package_ID, "package copied");
   declare
      Left : constant O.Object_ID := O.Element (Store, Copy_ID, 0);
      Right : constant O.Object_ID := O.Element (Store, Copy_ID, 1);
      Ref_Copy : constant O.Object_ID := O.Element (Store, Copy_ID, 2);
   begin
      Check (Left /= Right and then Left /= Meta and then Right /= Meta,
        "repeated metadata occurrences have independent wrappers");
      Check (O.Kind (Store, Left) = O.Metadata_Object and then O.Kind (Store, Right) = O.Metadata_Object
        and then O.Metadata_Data (Store, Left) = Root and then O.Metadata_Data (Store, Right) = Root,
        "copy preserves metadata identity and kind");
      Check (O.Kind (Store, Ref_Copy) = O.Reference_Object
        and then O.Reference_Data (Store, Ref_Copy) = Root, "copy preserves explicit reference kind");
      O.Set_Element (Store, Copy_ID, 0, O.No_Object);
      Check (O.Element (Store, Package_ID, 0) = Meta, "copied package mutation independent");
   end;
   Seeds (Meta) := True; Seeds (Named_Meta) := True;
   -- Even an unrelated target-map entry must not turn metadata into a GC edge.
   Targets (Meta) := O.Address_Of (Store, Garbage);
   Targets (Named_Meta) := O.Address_Of (Store, Garbage);
   Trace.Trace (Store, Seeds, Targets, Trace_Work, Keep, Trace_Status);
   Check (Trace_Status = Trace.Traced and then Keep = Seeds,
     "root and named metadata have no object-store target edge");
   GC.Reclaim (Store, Owner, Keep, GC_Work, GC_Status);
   Check (GC_Status = GC.Reclaimed and then O.Live_Count (Store) = 2
     and then O.Metadata_Data (Store, Meta) = Root and then O.Metadata_Data (Store, Named_Meta) = Named
     and then O.Byte_Count (Store) = 0 and then O.Element_Count (Store) = 0,
     "reclamation retains metadata identity without backing storage");
   -- Leave two free slots: clone allocates package and first metadata member,
   -- then fails on the second member. Nothing from the candidate may publish.
   Store := O.Empty (Max_Incarnation => AML_Object_Identifiers.First_Incarnation);
   O.New_Metadata (Store, Named, Meta, Status); Check (Status = O.Allocated, "quota named leaf");
   O.New_Package (Store, 2, Package_ID, Status); Check (Status = O.Allocated, "quota package");
   O.Set_Element (Store, Package_ID, 0, Meta); O.Set_Element (Store, Package_ID, 1, Meta);
   while O.Live_Count (Store) < O.Max_Objects - 2 loop
      O.New_Integer (Store, 0, Fresh, Status);
      Check (Status = O.Allocated, "fill bounded pool");
   end loop;
   Prior := Store;
   O.Copies.Clone (Store, Package_ID, Copy_ID, Status, Witness);
   Check (Status = O.Object_Limit and then Copy_ID = O.No_Object and then Store = Prior,
     "multi-member copy failure rolls back successful earlier allocations");
   O.New_Metadata (Store, Root, Fresh, Status); Check (Status = O.Allocated, "first remaining slot");
   O.New_Metadata (Store, Root, Fresh, Status); Check (Status = O.Allocated, "last remaining slot");
   Reject (R.No_Reference, "malformed shape precedes full quota");
   Reject (Index_Ref, "wrong kind precedes full quota");
   Prior := Store;
   O.New_Metadata (Store, Root, Fresh, Status);
   Check (Status = O.Object_Limit and then Fresh = O.No_Object and then Store = Prior,
     "full metadata allocation atomic");
   Keep := [others => False];
   GC.Reclaim (Store, Owner, Keep, GC_Work, GC_Status);
   Check (GC_Status = GC.Reclaimed and then O.Live_Count (Store) = 0, "retire all generation-one slots");
   Prior := Store;
   O.New_Metadata (Store, Named, Fresh, Status);
   Check (Status = O.Generation_Limit and then Fresh = O.No_Object and then Store = Prior,
     "metadata cannot reuse exhausted incarnations");
   Ada.Text_IO.Put_Line ("Metadata leaf checks" & Checks'Image);
end Metadata_Leaf_Tests;

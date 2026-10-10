with Ada.Text_IO;
with AML_Decode;
with AML_Identity.Issuer;
with AML_Object_Identifiers;
with AML_Objects.Reclamation;
with AML_Objects.Copies;
with AML_References;
procedure Sparse_Copy_Tests is
   package O renames AML_Objects;
   package R renames AML_Objects.Reclamation;
   package C renames AML_Objects.Copies;
   use type O.State;
   use type O.Allocation_Status;
   use type O.String_Update_Status;
   use type R.Reclaim_Status;
   use type AML_Decode.Integer_Value;
   use type AML_Decode.Integer_Origin;
   use type AML_Decode.Bytes;
   use type AML_References.Reference;
   use type AML_Object_Identifiers.Slot_Incarnation;
   Store : O.State := O.Empty;
   Scratch : R.Workspace;
   Keep : R.Keep_Set := [others => False];
   Owner : AML_Identity.Identity;
   Issued : Boolean;
   Status : O.Allocation_Status;
   Reclaimed : R.Reclaim_Status;
   Update : O.String_Update_Status;
   Witness : C.Copy_Witness;
   Root, Inner, Scalar, String_ID, Buffer_ID, Ref_ID, Target, Other : O.Object_ID;
   Left, Right, Left_Scalar, Right_Scalar, Left_String, Right_String : O.Object_ID;
   Ref : AML_References.Reference;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Label_Text : String) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Label_Text; end if;
   end Check;
   procedure Collect is
   begin
      R.Reclaim (Store, Owner, Keep, Scratch, Reclaimed);
      Check (Reclaimed = R.Reclaimed, "collection");
   end Collect;
   procedure Reject_Copy (Source : O.Object_ID; Expected : O.Allocation_Status) is
      Before : constant O.State := Store;
   begin
      C.Clone (Store, Source, Target, Status, Witness);
      Check (Status = Expected and then Target = O.No_Object and then Store = Before,
        "exact copy failure rollback");
   end Reject_Copy;
begin
   AML_Identity.Issuer.Issue (Owner, Issued); Check (Issued, "owner issuance");
   -- Retain interspersed slots so breadth-first allocation cannot be expressed
   -- as a base plus the queue index, nor as the new live-object count.
   for I in 1 .. 32 loop
      O.New_Integer (Store, AML_Decode.Integer_Value (I), Other, Status);
      Check (Status = O.Allocated, "spacer allocation");
      if I <= 8 and then I mod 2 = 0 then Keep (Other) := True; end if;
   end loop;
   O.New_Integer (Store, 1, Scalar, Status, AML_Decode.AML_Constant);
   Check (Status = O.Allocated, "constant scalar"); Keep (Scalar) := True;
   O.New_Bytes (Store, O.String_Object, [65, 66], String_ID, Status);
   Check (Status = O.Allocated, "string"); Keep (String_ID) := True;
   O.New_Bytes (Store, O.Buffer_Object, [1, 2, 3], Buffer_ID, Status);
   Check (Status = O.Allocated, "buffer"); Keep (Buffer_ID) := True;
   Ref := AML_References.Bind_Named (Owner, 1, 1);
   O.New_Reference (Store, Ref, Ref_ID, Status);
   Check (Status = O.Allocated, "reference leaf"); Keep (Ref_ID) := True;
   O.New_Package (Store, 4, Inner, Status); Check (Status = O.Allocated, "inner package"); Keep (Inner) := True;
   O.Set_Element (Store, Inner, 0, Scalar); O.Set_Element (Store, Inner, 1, String_ID);
   O.Set_Element (Store, Inner, 2, Buffer_ID); O.Set_Element (Store, Inner, 3, Ref_ID);
   O.New_Package (Store, 4, Root, Status); Check (Status = O.Allocated, "outer package"); Keep (Root) := True;
   O.Set_Element (Store, Root, 0, Inner); O.Set_Element (Store, Root, 1, Inner);
   O.Set_Element (Store, Root, 2, Ref_ID); -- final element remains absent
   Collect;
   Check (O.Live_Count (Store) = 10 and then O.Slot_Bound (Store) = 38, "fragmented source arena");
   C.Clone (Store, Root, Target, Status, Witness);
   Check (Status = O.Allocated and then Target = 1 and then O.Live_Count (Store) = 22
     and then O.Slot_Bound (Store) = 38, "clone reuses nonconsecutive holes");
   Left := O.Element (Store, Target, 0); Right := O.Element (Store, Target, 1);
   Check (Left = 3 and then Right = 5 and then Left /= Inner and then Right /= Inner, "repeated packages independently copied");
   Check (O.Element (Store, Target, 3) = 0, "absent package member preserved");
   Left_Scalar := O.Element (Store, Left, 0); Right_Scalar := O.Element (Store, Right, 0);
   Check (Left_Scalar /= Right_Scalar and then Left_Scalar /= Scalar
     and then Right_Scalar /= Scalar, "scalar occurrences independent");
   Check (O.Origin_Of (Store, Left_Scalar) = AML_Decode.AML_Constant
     and then O.Origin_Of (Store, Right_Scalar) = AML_Decode.AML_Constant, "integer provenance copied");
   O.Set_Integer (Store, Left_Scalar, 99);
   Check (O.Integer_Data (Store, Right_Scalar) = 1 and then O.Integer_Data (Store, Scalar) = 1, "scalar mutation isolated");
   Left_String := O.Element (Store, Left, 1); Right_String := O.Element (Store, Right, 1);
   O.Replace_String (Store, Left_String, [90], Update);
   Check (Update = O.String_Updated and then O.Byte_Data (Store, Right_String) = AML_Decode.Bytes'[65, 66]
     and then O.Byte_Data (Store, String_ID) = AML_Decode.Bytes'[65, 66], "string mutation isolated");
   O.Set_Stored_Byte (Store, O.Element (Store, Left, 2), 0, 9);
   Check (O.Byte_Data (Store, O.Element (Store, Right, 2)) = AML_Decode.Bytes'[1, 2, 3]
     and then O.Byte_Data (Store, Buffer_ID) = AML_Decode.Bytes'[1, 2, 3], "buffer mutation isolated");
   Check (O.Reference_Data (Store, O.Element (Store, Left, 3)) = Ref
     and then O.Reference_Data (Store, O.Element (Store, Right, 3)) = Ref
     and then O.Reference_Data (Store, O.Element (Store, Target, 2)) = Ref,
     "references copy wrapper, retain referent identity");
   -- Cloned extents and objects remain reclaimable; repeat using new generations.
   Collect;
   C.Clone (Store, Root, Target, Status, Witness);
   Check (Status = O.Allocated and then Target = 1
     and then O.Last_Incarnation (Store, Target) = 3, "clone after reclaim uses next incarnation");
   -- Byte failure after a package root is provisionally allocated must undo it.
   Store := O.Empty;
   O.New_Bytes (Store, O.Buffer_Object, AML_Decode.Bytes'(1 .. O.Max_Bytes => 7), Buffer_ID, Status);
   Check (Status = O.Allocated, "full bytes");
   O.New_Package (Store, 1, Root, Status); Check (Status = O.Allocated, "byte failure root");
   O.Set_Element (Store, Root, 0, Buffer_ID); Reject_Copy (Root, O.Byte_Limit);
   -- Element failure after the root allocation, and a cyclic package that uses
   -- the last element slot: both must preserve all state, including stamps.
   Store := O.Empty;
   O.New_Package (Store, O.Max_Elements - 2, Inner, Status); Check (Status = O.Allocated, "large package");
   O.New_Package (Store, 1, Root, Status); Check (Status = O.Allocated, "element failure root");
   O.Set_Element (Store, Root, 0, Inner); Reject_Copy (Root, O.Element_Limit);
   O.Set_Element (Store, Root, 0, Root); Reject_Copy (Root, O.Element_Limit);
   -- One available object allows a partial clone, but not its required child.
   Store := O.Empty;
   O.New_Integer (Store, 42, Scalar, Status); Check (Status = O.Allocated, "quota scalar");
   O.New_Package (Store, 1, Root, Status); Check (Status = O.Allocated, "quota package");
   O.Set_Element (Store, Root, 0, Scalar);
   for I in 3 .. O.Max_Objects - 1 loop
      O.New_Integer (Store, 0, Other, Status); Check (Status = O.Allocated, "quota filler");
   end loop;
   Reject_Copy (Root, O.Object_Limit);
   -- Exhausted generations are a separate resource from live object quota.
   Store := O.Empty (1); Keep := [others => False];
   for I in 1 .. O.Max_Objects loop
      O.New_Integer (Store, 42, Other, Status); Check (Status = O.Allocated, "generation filler");
   end loop;
   Keep (Other) := True; Collect; Reject_Copy (Other, O.Generation_Limit);
   Ada.Text_IO.Put_Line ("SPARSE COPY" & Checks'Image);
end Sparse_Copy_Tests;

with Ada.Text_IO;
with AML_Decode;
with AML_Identity.Issuer;
with AML_Object_Identifiers; use AML_Object_Identifiers;
with AML_Objects.Reclamation;
with AML_Objects.Byte_References;
with AML_Objects.Package_References;
with AML_References;
with AML_Index_Handles;
procedure Reclamation_Tests is
   package O renames AML_Objects;
   package R renames AML_Objects.Reclamation;
   package B renames AML_Objects.Byte_References;
   package P renames AML_Objects.Package_References;
   use type O.State;
   use type O.Allocation_Status;
   use type R.Reclaim_Status;
   use type B.Result_Status;
   use type P.Result_Status;
   use type AML_Decode.Bytes;
   use type AML_Decode.Integer_Value;
   Store : O.State := O.Empty;
   Scratch : R.Workspace;
   Keep : R.Keep_Set := [others => False];
   Owner, Foreign_Owner : AML_Identity.Identity;
   Issued : Boolean;
   Status : R.Reclaim_Status;
   Alloc : O.Allocation_Status;
   Dead, Bytes, Pack, Scalar, Other, Wrapper, Output : Object_ID;
   Old_Byte, Old_Package : Object_Address;
   Byte_Ref : B.Reference;
   Package_Ref : P.Reference;
   BS : B.Result_Status;
   PS : P.Result_Status;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Label_Text : String) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Label_Text; end if;
   end Check;
   procedure Collect is
   begin
      R.Reclaim (Store, Owner, Keep, Scratch, Status);
      Check (Status = R.Reclaimed and then O.Valid (Store), "reclaimed valid state");
   end Collect;
   procedure Reject (Expected : R.Reclaim_Status) is
      Before : constant O.State := Store;
   begin
      R.Reclaim (Store, Owner, Keep, Scratch, Status);
      Check (Status = Expected and then Store = Before, "reclaim failure atomicity");
   end Reject;
begin
   AML_Identity.Issuer.Issue (Owner, Issued); Check (Issued, "owner issuance");
   AML_Identity.Issuer.Issue (Foreign_Owner, Issued); Check (Issued, "foreign issuance");
   declare Before : constant O.State := Store; begin
      R.Reclaim (Store, AML_Identity.No_Identity, Keep, Scratch, Status);
      Check (Status = R.Invalid_Owner and then Store = Before, "invalid owner");
   end;
   Keep (Max_Objects) := True; Reject (R.Invalid_Keep_Set); Keep := [others => False];
   O.New_Bytes (Store, O.String_Object, [1, 2, 3, 4], Dead, Alloc);
   Check (Alloc = O.Allocated, "dead extent allocation");
   O.New_Bytes (Store, O.Buffer_Object, [16#CA#, 16#FE#], Bytes, Alloc);
   Check (Alloc = O.Allocated, "live buffer allocation");
   O.New_Package (Store, 3, Pack, Alloc); Check (Alloc = O.Allocated, "package allocation");
   O.New_Integer (Store, 42, Scalar, Alloc); Check (Alloc = O.Allocated, "scalar allocation");
   O.Set_Element (Store, Pack, 0, Bytes);
   O.Set_Element (Store, Pack, 1, Pack); -- a cycle is allowed and must survive
   O.Set_Element (Store, Pack, 2, Scalar);
   Old_Byte := O.Address_Of (Store, Bytes); Old_Package := O.Address_Of (Store, Pack);
   Byte_Ref := AML_Index_Handles.Bind_Byte (Old_Byte, 1);
   Package_Ref := AML_Index_Handles.Bind_Package (Old_Package, 2);
   Keep (Pack) := True; Reject (R.Unclosed_Package);
   Keep (Bytes) := True; Keep (Scalar) := True; Collect;
   Check (O.Live_Count (Store) = 3 and then O.Slot_Bound (Store) = Scalar
     and then not O.Is_Live (Store, Dead), "sparse bound differs from live count");
   Check (O.Byte_Count (Store) = 2 and then O.Element_Count (Store) = 3
     and then O.Byte_Data (Store, Bytes) = AML_Decode.Bytes'[16#CA#,16#FE#]
     and then O.Integer_Data (Store, Scalar) = 42, "compacted payloads");
   Check (O.Element (Store, Pack, 0) = Bytes and then O.Element (Store, Pack, 1) = Pack
     and then O.Element (Store, Pack, 2) = Scalar, "stable cyclic package edges");
   Check (O.Matches_Address (Store, Old_Byte) and then O.Matches_Address (Store, Old_Package), "kept addresses stable");
   B.Write (Store, Byte_Ref, 16#FA#, BS); Check (BS = B.Ready, "byte reference follows compacted extent");
   P.Read (Store, Package_Ref, Output, PS); Check (PS = P.Ready and then Output = Scalar, "package reference follows compacted extent");
   declare Before : constant O.State := Store; begin
      P.Write (Store, Package_Ref, Dead, PS);
      Check (PS = P.Invalid_Value and then Store = Before, "dead sparse target rejected");
   end;
   B.Make (Store, Dead, 0, Byte_Ref, BS); Check (BS = B.Invalid_Object, "dead source rejected");
   O.New_Integer (Store, 99, Other, Alloc);
   Check (Alloc = O.Allocated and then Other = Dead and then O.Last_Incarnation (Store, Other) = 2, "hole reused with fresh generation");
   Keep := [others => False]; Collect;
   Check (O.Live_Count (Store) = 0 and then O.Byte_Count (Store) = 0
     and then O.Element_Count (Store) = 0 and then O.Slot_Bound (Store) = Scalar, "all backing reclaimed, bound retained");
   O.New_Integer (Store, 0, Other, Alloc); Check (Alloc = O.Allocated, "new first slot");
   O.New_Bytes (Store, O.Buffer_Object, [7, 8], Other, Alloc);
   Check (Alloc = O.Allocated and then Other = Bytes and then not O.Matches_Address (Store, Old_Byte), "old byte address stale after real reuse");
   Byte_Ref := AML_Index_Handles.Bind_Byte (Old_Byte, 1);
   declare Before : constant O.State := Store; begin
      B.Write (Store, Byte_Ref, 0, BS);
      Check (BS = B.Invalid_Reference and then Store = Before, "stale byte write unchanged");
   end;
   O.New_Package (Store, 3, Other, Alloc);
   Check (Alloc = O.Allocated and then Other = Pack and then not O.Matches_Address (Store, Old_Package), "old package address stale after reuse");
   declare Before : constant O.State := Store; begin
      P.Write (Store, Package_Ref, 0, PS);
      Check (PS = P.Invalid_Reference and then Store = Before, "stale package write unchanged");
   end;
   -- A live same-owner reference keeps its container; stale/foreign descriptors
   -- remain data and must not retain a replacement object by slot number alone.
   O.New_Reference (Store, AML_References.Bind (Owner,
     AML_Index_Handles.Bind_Byte (O.Address_Of (Store, Bytes), 0)), Wrapper, Alloc);
   Check (Alloc = O.Allocated, "reference wrapper");
   Keep (Wrapper) := True; Reject (R.Unclosed_Container_Reference);
   Keep (Bytes) := True; Collect;
   Check (O.Live_Count (Store) = 2, "reference and container retained");
   Keep := [others => False]; Collect;
   O.New_Reference (Store, AML_References.Bind (Owner,
     AML_Index_Handles.Bind_Byte (Old_Byte, 0)), Wrapper, Alloc);
   Check (Alloc = O.Allocated, "stale descriptor is data");
   O.New_Bytes (Store, O.Buffer_Object, [9], Other, Alloc); Check (Alloc = O.Allocated and then Other = Bytes, "stale target slot replaced");
   Keep (Wrapper) := True; Collect;
   Check (not O.Is_Live (Store, Bytes), "stale descriptor does not retain replacement");
   Keep := [others => False]; Collect;
   O.New_Bytes (Store, O.Buffer_Object, [9], Bytes, Alloc); Check (Alloc = O.Allocated, "foreign container");
   O.New_Reference (Store, AML_References.Bind (Foreign_Owner,
     AML_Index_Handles.Bind_Byte (O.Address_Of (Store, Bytes), 0)), Wrapper, Alloc);
   Check (Alloc = O.Allocated, "foreign descriptor");
   Keep (Wrapper) := True; Collect;
   Check (not O.Is_Live (Store, Bytes), "foreign descriptor does not root local slot");
   -- Exhaust a deliberately tiny generation budget without wrapping or reusing
   -- an exhausted slot. Every allocation is a real production transition.
   for Budget in Incarnation_Budget range 1 .. 2 loop
      Store := O.Empty (Budget); Keep := [others => False];
      for Round in Incarnation_Budget range 1 .. Budget loop
         for I in Object_ID range 1 .. Max_Objects loop
            O.New_Integer (Store, AML_Decode.Integer_Value (I), Output, Alloc);
            Check (Alloc = O.Allocated and then Output = I
              and then O.Last_Incarnation (Store, I) = Round, "bounded generation allocation");
         end loop;
         declare Before : constant O.State := Store; begin
            O.New_Integer (Store, 0, Output, Alloc);
            Check (Alloc = O.Object_Limit and then Output = 0 and then Store = Before, "live quota atomicity");
         end;
         Collect;
      end loop;
      declare Before : constant O.State := Store; begin
         O.New_Integer (Store, 0, Output, Alloc);
         Check (Alloc = O.Generation_Limit and then Output = 0 and then Store = Before, "generation exhaustion atomicity");
         O.New_Reference (Store, AML_References.No_Reference, Output, Alloc);
         Check (Alloc = O.Invalid_Reference and then Output = 0 and then Store = Before, "invalid reference priority");
      end;
   end loop;
   Ada.Text_IO.Put_Line ("SPARSE RECLAMATION" & Checks'Image);
end Reclamation_Tests;

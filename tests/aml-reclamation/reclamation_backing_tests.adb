with Ada.Text_IO;
with AML_Decode;
with AML_Identity.Issuer;
with AML_Object_Identifiers;
with AML_Objects.Reclamation;
with AML_References;
with AML_Index_Handles;
procedure Reclamation_Backing_Tests is
   package O renames AML_Objects;
   package R renames AML_Objects.Reclamation;
   use type O.State;
   use type O.Allocation_Status;
   use type O.String_Update_Status;
   use type O.Buffer_Update_Status;
   use type R.Reclaim_Status;
   use type AML_Decode.Bytes;
   use type AML_Object_Identifiers.Object_Address;
   use type AML_References.Reference;
   Store : O.State := O.Empty;
   Scratch : R.Workspace;
   Owner : AML_Identity.Identity;
   Issued : Boolean;
   Keep : R.Keep_Set := [others => False];
   Status : R.Reclaim_Status;
   Alloc : O.Allocation_Status;
   Update : O.String_Update_Status;
   Buffer_Status : O.Buffer_Update_Status;
   Dead, Live, Pack, Wrapper, Other : O.Object_ID;
   Address : AML_Object_Identifiers.Object_Address;
   Ref : AML_References.Reference;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Label_Text : String) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Label_Text; end if;
   end Check;
   procedure Collect is
   begin
      R.Reclaim (Store, Owner, Keep, Scratch, Status);
      Check (Status = R.Reclaimed and then O.Valid (Store), "valid compaction");
   end Collect;
   procedure Reject_Closure is
      Before : constant O.State := Store;
   begin
      R.Reclaim (Store, Owner, Keep, Scratch, Status);
      Check (Status = R.Unclosed_Container_Reference and then Store = Before,
        "package-reference rejection is atomic");
   end Reject_Closure;
begin
   AML_Identity.Issuer.Issue (Owner, Issued); Check (Issued, "owner issuance");
   O.New_Bytes (Store, O.Buffer_Object, AML_Decode.Bytes'(1 .. O.Max_Bytes - 2 => 16#AD#), Dead, Alloc);
   Check (Alloc = O.Allocated, "large dead extent");
   O.New_Bytes (Store, O.String_Object, [16#55#,16#66#], Live, Alloc);
   Check (Alloc = O.Allocated and then O.Byte_Count (Store) = O.Max_Bytes, "byte arena full");
   Address := O.Address_Of (Store, Live);
   declare Before : constant O.State := Store; begin
      O.New_Bytes (Store, O.Buffer_Object, [1], Other, Alloc);
      Check (Alloc = O.Byte_Limit and then Other = 0 and then Store = Before, "byte-limit failure unchanged");
      O.Replace_String (Store, Live, [7], Update);
      Check (Update = O.String_Byte_Limit and then Store = Before, "replacement failure unchanged");
   end;
   Keep (Live) := True; Collect;
   Check (O.Byte_Count (Store) = 2 and then O.Address_Of (Store, Live) = Address
     and then O.Byte_Data (Store, Live) = AML_Decode.Bytes'[16#55#,16#66#], "live end extent compacted");
   O.New_Bytes (Store, O.Buffer_Object, AML_Decode.Bytes'(1 .. O.Max_Bytes - 2 => 16#CD#), Other, Alloc);
   Check (Alloc = O.Allocated and then Other = Dead and then O.Byte_Count (Store) = O.Max_Bytes, "full freed byte capacity reusable");
   Collect;
   O.Replace_String (Store, Live, [7, 8, 9], Update);
   Check (Update = O.String_Updated and then O.Byte_Count (Store) = 5, "string replacement appends");
   Collect;
   Check (O.Byte_Count (Store) = 3 and then O.Byte_Data (Store, Live) = AML_Decode.Bytes'[7, 8, 9]
     and then O.Address_Of (Store, Live) = Address, "obsolete string backing reclaimed");
   -- Empty objects have valid zero-sized extents and stay empty on compaction.
   O.New_Bytes (Store, O.Buffer_Object, AML_Decode.Bytes'(1 .. 0 => 0), Other, Alloc);
   Check (Alloc = O.Allocated, "empty buffer allocated"); Keep (Other) := True;
   Collect;
   O.Store_Buffer (Store, Other, [1, 2], Buffer_Status);
   Check (Buffer_Status = O.Buffer_Updated and then O.Byte_Count (Store) = 5, "empty buffer growth after compact");
   O.Store_Buffer (Store, Other, [4], Buffer_Status);
   Check (Buffer_Status = O.Buffer_Updated and then O.Byte_Data (Store, Other) = AML_Decode.Bytes'[4, 0], "buffer zero-fill preserved");
   Collect;
   Check (O.Byte_Data (Store, Other) = AML_Decode.Bytes'[4, 0], "buffer writes preserved");
   Keep := [others => False]; Collect;
   O.New_Package (Store, O.Max_Elements - 2, Dead, Alloc); Check (Alloc = O.Allocated, "large dead package");
   O.New_Package (Store, 2, Pack, Alloc); Check (Alloc = O.Allocated and then O.Element_Count (Store) = O.Max_Elements, "element arena full");
   Address := O.Address_Of (Store, Pack);
   O.Set_Element (Store, Pack, 0, Pack);
   declare Before : constant O.State := Store; begin
      O.New_Package (Store, 1, Other, Alloc);
      Check (Alloc = O.Element_Limit and then Other = 0 and then Store = Before, "element-limit failure unchanged");
   end;
   Keep (Pack) := True; Collect;
   Check (O.Element_Count (Store) = 2 and then O.Address_Of (Store, Pack) = Address
     and then O.Element (Store, Pack, 0) = Pack and then O.Element (Store, Pack, 1) = 0, "element compaction preserves self and absent edges");
   O.New_Package (Store, O.Max_Elements - 2, Other, Alloc);
   Check (Alloc = O.Allocated and then O.Element_Count (Store) = O.Max_Elements, "full element capacity reusable");
   Collect;
   Ref := AML_References.Bind (Owner, AML_Index_Handles.Bind_Package (Address, 1));
   O.New_Reference (Store, Ref, Wrapper, Alloc); Check (Alloc = O.Allocated, "package reference wrapper");
   Keep := [others => False]; Keep (Wrapper) := True; Reject_Closure;
   Keep (Pack) := True; Collect;
   Check (O.Reference_Data (Store, Wrapper) = Ref, "reference descriptor unchanged");
   -- An out-of-bounds reference is data, not a live edge. Its descriptor must
   -- survive exactly while the unreachable package can be reclaimed.
   O.New_Reference (Store, AML_References.Bind (Owner,
     AML_Index_Handles.Bind_Package (Address, 2)), Other, Alloc);
   Check (Alloc = O.Allocated, "out-of-bounds descriptor allocated");
   Keep := [others => False]; Keep (Other) := True; Collect;
   Check (not O.Is_Live (Store, Pack) and then O.Element_Count (Store) = 0,
     "out-of-bounds descriptor does not keep package");
   -- A live reference may point at a package shared by other packages. The
   -- closure must include every nested edge, with no duplicate copies.
   Keep := [others => False]; Collect;
   O.New_Bytes (Store, O.Buffer_Object, [1], Live, Alloc); Check (Alloc = O.Allocated, "shared byte leaf");
   O.New_Package (Store, 2, Pack, Alloc); Check (Alloc = O.Allocated, "shared package");
   O.Set_Element (Store, Pack, 0, Live); O.Set_Element (Store, Pack, 1, Live);
   O.New_Reference (Store, AML_References.Bind (Owner,
     AML_Index_Handles.Bind_Package (O.Address_Of (Store, Pack), 0)), Wrapper, Alloc);
   Check (Alloc = O.Allocated, "shared root");
   Keep (Wrapper) := True; Keep (Pack) := True;
   declare Before : constant O.State := Store; begin
      R.Reclaim (Store, Owner, Keep, Scratch, Status);
      Check (Status = R.Unclosed_Package and then Store = Before, "nested package closure enforced");
   end;
   Keep (Live) := True; Collect;
   Check (O.Live_Count (Store) = 3 and then O.Byte_Count (Store) = 1
     and then O.Element (Store, Pack, 0) = Live and then O.Element (Store, Pack, 1) = Live,
     "shared leaves retain identity");
   Ada.Text_IO.Put_Line ("RECLAMATION BACKING" & Checks'Image);
end Reclamation_Backing_Tests;

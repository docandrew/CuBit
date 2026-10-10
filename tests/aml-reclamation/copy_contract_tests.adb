with Ada.Text_IO;
with System;
with AML_Objects.Copies;
with AML_Objects.Reclamation;
procedure Copy_Contract_Tests is
   package O renames AML_Objects;
   package C renames AML_Objects.Copies;
   use type O.Allocation_Status;
   Source, Left, Right, Target, Added : O.Object_ID;
   Store : O.State := O.Empty;
   Alloc : O.Allocation_Status;
   Witness : C.Copy_Witness;
   Rejected : Natural := 0;
   procedure Reject (Candidate, Prior : O.State; Root : O.Object_ID) is
   begin
      pragma Assert (not C.Valid_Witness (Candidate, Prior, Source, Root, Witness));
      pragma Assert (not C.Is_Independent_Copy (Candidate, Prior, Source, Root, Witness));
      Rejected := Rejected + 1;
   end Reject;
begin
   -- This main is deliberately checked-only: these are executable ghost
   -- contract probes, never counted as release functional checks.
   O.New_Integer (Store, 7, Left, Alloc); pragma Assert (Alloc = O.Allocated);
   O.New_Integer (Store, 8, Right, Alloc); pragma Assert (Alloc = O.Allocated);
   O.New_Package (Store, 2, Source, Alloc); pragma Assert (Alloc = O.Allocated);
   O.Set_Element (Store, Source, 0, Left); O.Set_Element (Store, Source, 1, Right);
   declare
      Prior : constant O.State := Store;
      Candidate : O.State;
   begin
      C.Clone (Store, Source, Target, Alloc, Witness);
      pragma Assert (Alloc = O.Allocated);
      pragma Assert (C.Valid_Witness (Store, Prior, Source, Target, Witness));
      pragma Assert (C.Is_Independent_Copy (Store, Prior, Source, Target, Witness));
      Candidate := Store;
      O.Set_Element (Candidate, Target, 1, O.Element (Candidate, Target, 0));
      Reject (Candidate, Prior, Target); -- repeated copy aliases a different child
      Candidate := Store;
      O.Set_Element (Candidate, Target, 1, 0);
      Reject (Candidate, Prior, Target); -- missing copied edge, orphan allocation
      Candidate := Store;
      O.Set_Integer (Candidate, O.Element (Candidate, Target, 0), 99);
      Reject (Candidate, Prior, Target); -- copied scalar payload differs
      Candidate := Store;
      O.Set_Integer (Candidate, Left, 99);
      Reject (Candidate, Prior, Target); -- original object was modified
      Reject (Store, Prior, Source); -- target is a preexisting object
      Candidate := Store;
      O.New_Integer (Candidate, 99, Added, Alloc);
      pragma Assert (Alloc = O.Allocated and then O.Is_Live (Candidate, Added));
      Reject (Candidate, Prior, Target); -- unrelated allocation outside witness
      Candidate := Store;
      O.Set_Element (Candidate, Target, 1, Right);
      Reject (Candidate, Prior, Target); -- copied edge points into prior graph
   end;
   Ada.Text_IO.Put_Line ("COPY CONTRACT REJECTIONS" & Rejected'Image & " (two ghost predicates each)");
   Ada.Text_IO.Put_Line ("HOSTED State bytes" & Integer'Image (O.State'Size / System.Storage_Unit));
   Ada.Text_IO.Put_Line ("HOSTED Reclamation workspace bytes" & Integer'Image
     (O.Reclamation.Workspace'Size / System.Storage_Unit));
   Ada.Text_IO.Put_Line ("HOSTED Copy witness bytes" & Integer'Image (C.Copy_Witness'Size / System.Storage_Unit));
end Copy_Contract_Tests;

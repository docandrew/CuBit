package body AML_Objects.Integer_Shape_Probe is
   function Malformed_Integer_Accepted return Boolean is
      S : State := Empty (Object_Capacity => 2, Byte_Capacity => 2, Element_Capacity => 2);
      ID : Object_ID;
      Status : Allocation_Status;
   begin
      New_Integer (S, 7, ID, Status);
      if Status /= Allocated then raise Program_Error; end if;
      -- Test-only child access: no public mutator is claimed to emit this state.
      S.Objects (ID).Size := 1;
      return Valid (S) and then Length (S, ID) = 1;
   end Malformed_Integer_Accepted;
end AML_Objects.Integer_Shape_Probe;

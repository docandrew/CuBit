with Ada.Text_IO;
with AML_Decode;
with AML_Objects; use AML_Objects;
procedure Buffer_Access_Tests is
 use type AML_Decode.Byte;
 use type AML_Decode.Bytes;
 Store : State := Empty;
 A, B, P : Object_ID;
 Status : Allocation_Status;
 Before : Usage;
begin
 New_Bytes (Store, Buffer_Object, [1,2,3], A, Status);
 pragma Assert (Status = Allocated);
 New_Bytes (Store, Buffer_Object, [4,5,6], B, Status);
 pragma Assert (Status = Allocated);
 New_Package (Store, 2, P, Status);
 pragma Assert (Status = Allocated);
 Set_Element (Store, P, 0, A); Set_Element (Store, P, 1, B);
 Before := Usage_Of (Store);
 for I in 0 .. 2 loop
  for V in AML_Decode.Byte loop
   Set_Stored_Byte (Store, B, I, V);
   pragma Assert (Stored_Byte (Store, B, I) = V);
   pragma Assert (Byte_Data (Store, A) = [1,2,3]);
   pragma Assert (Element (Store, P, 0) = A and Element (Store, P, 1) = B);
   pragma Assert (Usage_Of (Store) = Before);
  end loop;
 end loop;
 Ada.Text_IO.Put_Line ("BUFFER-ACCESS: PASS 768 byte writes, nonzero backing offset, neighbor/package/usage isolation");
end Buffer_Access_Tests;

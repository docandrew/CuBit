with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Objects; use AML_Objects;
with AML_Objects.Copies;
procedure Integer_Origin_Tests is
   use type Integer_Value;
   A : State := Empty;
   B : State;
   ID, Copy : Object_ID;
   Status : Allocation_Status;
   Witness : AML_Objects.Copies.Copy_Witness;
   Checks : Natural := 0;
   procedure Check (Value : Boolean) is
   begin
      Checks := Checks + 1;
      if not Value then raise Program_Error with Checks'Image; end if;
   end Check;
begin
   for Op in Byte loop
      Check (Literal_Origin (Op) = (if Op in 0 | 1 | 16#FF# then AML_Constant else Ordinary_Integer));
   end loop;
   for Origin in Integer_Origin loop
      New_Integer (A, 1, ID, Status, Origin);
      Check (Status = Allocated and then Origin_Of (A, ID) = Origin);
      B := A;
      Set_Integer (A, ID, 7);
      Check (Integer_Data (A, ID) = 7 and then Origin_Of (A, ID) = Origin);
      pragma Assert (Integer_Updated (A, B, ID, 7));
      B := A;
      AML_Objects.Copies.Clone (A, ID, Copy, Status, Witness);
      Check (Status = Allocated and then Origin_Of (A, Copy) = Origin);
      pragma Assert (AML_Objects.Copies.Is_Independent_Copy (A, B, ID, Copy, Witness));
   end loop;
   Ada.Text_IO.Put_Line ("Integer origin checks" & Checks'Image);
end Integer_Origin_Tests;

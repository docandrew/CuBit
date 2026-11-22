with Ada.Text_IO;
with AML_Object_Identifiers; use AML_Object_Identifiers;
with AML_Objects;
with AML_Objects.Byte_References;
with AML_Objects.Package_References;
with AML_Index_Handles;
with AML_Decode;
procedure Generation_Handle_Tests is
   package O renames AML_Objects;
   package B renames AML_Objects.Byte_References;
   package P renames AML_Objects.Package_References;
   use type O.State;
   use type O.Allocation_Status;
   use type B.Result_Status;
   use type P.Result_Status;
   use type AML_Decode.Byte;
   Store : O.State := O.Empty;
   Buffer_ID, Package_ID, Value : Object_ID;
   Allocated : O.Allocation_Status;
   Address, Wrong : Object_Address;
   Byte_Ref : B.Reference;
   Package_Ref : P.Reference;
   Byte_Status : B.Result_Status;
   Package_Status : P.Result_Status;
   Octet : AML_Decode.Byte;
   Checks : Natural := 0;
   procedure Check (Good : Boolean) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Checks'Image; end if; end Check;
begin
   Check (Make_Address (0, First_Incarnation) = No_Address);
   Check (Make_Address (1, No_Incarnation) = No_Address);
   Check (O.Address_Of (Store, 1) = No_Address);
   O.New_Bytes (Store, O.Buffer_Object, [16#41#,16#42#], Buffer_ID, Allocated);
   Check (Allocated = O.Allocated);
   Address := O.Address_Of (Store, Buffer_ID);
   Check (Present (Address) and then Slot_Of (Address) = Buffer_ID
     and then Incarnation_Of (Address) = First_Incarnation and then O.Matches_Address (Store, Address));
   Wrong := Make_Address (Buffer_ID, First_Incarnation + 1);
   Check (not O.Matches_Address (Store, Wrong));
   Byte_Ref := AML_Index_Handles.Bind_Byte (Address, 0);
   B.Read (Store, Byte_Ref, Octet, Byte_Status);
   Check (Byte_Status = B.Ready and then Octet = 16#41#);
   Byte_Ref := AML_Index_Handles.Bind_Byte (Wrong, 0);
   B.Read (Store, Byte_Ref, Octet, Byte_Status);
   Check (Byte_Status = B.Invalid_Reference and then Octet = 0);
   declare Before : constant O.State := Store; begin
      B.Write (Store, Byte_Ref, 0, Byte_Status);
      Check (Byte_Status = B.Invalid_Reference and then Store = Before);
   end;
   O.New_Package (Store, 1, Package_ID, Allocated); Check (Allocated = O.Allocated);
   O.Set_Element (Store, Package_ID, 0, Buffer_ID);
   Address := O.Address_Of (Store, Package_ID);
   Package_Ref := AML_Index_Handles.Bind_Package (Address, 0);
   P.Read (Store, Package_Ref, Value, Package_Status);
   Check (Package_Status = P.Ready and then Value = Buffer_ID);
   Package_Ref := AML_Index_Handles.Bind_Package
     (Make_Address (Package_ID, First_Incarnation + 1), 0);
   P.Read (Store, Package_Ref, Value, Package_Status);
   Check (Package_Status = P.Invalid_Reference and then Value = 0);
   declare Before : constant O.State := Store; begin
      P.Write (Store, Package_Ref, 0, Package_Status);
      Check (Package_Status = P.Invalid_Reference and then Store = Before);
   end;
   Ada.Text_IO.Put_Line ("GENERATION HANDLES" & Checks'Image);
end Generation_Handle_Tests;

with Ada.Text_IO;
with AML_Identity;
with AML_Identity.Issuer;
with AML_References;
with AML_Objects;
with AML_Objects.Copies;
procedure Name_Member_Identity_Tests is
   use AML_References;
   use type AML_Objects.Allocation_Status;
   use type AML_Objects.Object_Kind;
   First, Second : AML_Identity.Identity;
   OK : Boolean;
   Checks : Natural := 0;
   S : AML_Objects.State := AML_Objects.Empty;
   Leaf, Package_ID, Copied : AML_Objects.Object_ID;
   Status : AML_Objects.Allocation_Status;
   Witness : AML_Objects.Copies.Copy_Witness;
   R : Reference;
   procedure Check (B : Boolean) is
   begin Checks := Checks + 1; if not B then raise Program_Error with Checks'Image; end if; end Check;
begin
   AML_Identity.Issuer.Issue (First, OK); Check (OK);
   AML_Identity.Issuer.Issue (Second, OK); Check (OK);
   Check (Bind_Name_Member (AML_Identity.No_Identity,1,1) = No_Reference);
   Check (Bind_Name_Member (First,0,1) = No_Reference);
   Check (Bind_Name_Member (First,1,0) = No_Reference);
   for Node in Node_Position range 1 .. 4 loop
      for Stamp in Node_Incarnation range 1 .. 4 loop
         R := Bind_Name_Member (First,Node,Stamp);
         Check (Kind (R) = Name_Member and then Well_Formed (R));
         Check (Belongs_To (R, First) and then not Belongs_To (R, Second));
         Check (Named_Node (R) = Node and then Incarnation (R) = Stamp);
         Check (R /= Bind_Named (First,Node,Stamp));
         Check (Target (R) = 0 and then Offset (R) = 0);
         AML_Objects.New_Reference (S,R,Leaf,Status); Check (Status = AML_Objects.Allocated);
         AML_Objects.New_Package (S,2,Package_ID,Status); Check (Status = AML_Objects.Allocated);
         AML_Objects.Set_Element (S,Package_ID,0,Leaf);
         AML_Objects.Set_Element (S,Package_ID,1,Leaf);
         AML_Objects.Copies.Clone (S,Package_ID,Copied,Status,Witness);
         Check (Status = AML_Objects.Allocated);
         declare
            Left : constant AML_Objects.Object_ID := AML_Objects.Element (S,Copied,0);
            Right : constant AML_Objects.Object_ID := AML_Objects.Element (S,Copied,1);
         begin
            Check (Left /= Right and then Left /= Leaf and then Right /= Leaf);
            Check (AML_Objects.Kind (S,Left) = AML_Objects.Reference_Object);
            Check (AML_Objects.Kind (S,Right) = AML_Objects.Reference_Object);
            Check (AML_Objects.Reference_Data (S,Left) = R and then AML_Objects.Reference_Data (S,Right) = R);
         end;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("NAME MEMBER IDENTITY: PASS" & Checks'Image);
end Name_Member_Identity_Tests;

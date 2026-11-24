pragma Ada_2022;
package body AML_Namespace.Lifetime_Test_Fixture is
   function Run return Natural is
      use type AML_Objects.State;
      use type AML_Objects.Object_ID;
      use type AML_References.Node_Incarnation;
      use type AML_Decode.Bytes;
      Tree : State := Empty;
      Checks : Natural := 0;
      procedure Check (Good : Boolean) is
      begin
         Checks := Checks + 1;
         if not Good then raise Program_Error with Checks'Image; end if;
      end Check;
      -- Test oracle copies the named-base retirement semantics, not its indexes.
      function Reference_Retired (Item : Entry_Record) return Entry_Record is
        (if Item.Object_Type in Integer_Object | String_Object | Buffer_Object |
             Package_Object | Reference_Object | Metadata_Value_Object | Buffer_Field_Object then
           (Item with delta Alive => False, Initializing => False,
             Object_Type => Scope_Object, Object_Ref => 0)
         else (Item with delta Alive => False, Initializing => False));
      procedure Compare (Scope : Natural) is
         Expected : Entries := Tree.Items;
         Expected_Used : Node_ID := Tree.Used;
         Expected_Code : AML_Decode.Bytes (Tree.Code'Range) := Tree.Code;
         Code_Used : Aggregate_Method_Count := Tree.Code_Used;
         Kept, Prefix : Aggregate_Method_Count := 0;
         Before_Values : constant AML_Objects.State := Tree.Values;
         Before_Root : constant AML_Objects.Object_ID := Tree.Root_Value;
         Before_Stamp : constant AML_References.Node_Incarnation := Tree.Last_Stamp;
      begin
         if Scope > Root and then Scope <= Expected_Used and then Expected (Scope).Active_Calls > 0 then
            if Expected (Scope).Active_Calls > 1 then
               Expected (Scope).Active_Calls := Expected (Scope).Active_Calls - 1;
            else
               for I in 1 .. Expected_Used loop
                  if Expected (I).Owner /= Root and then
                    (Expected (I).Owner = Scope or else Expected (I).Up = Scope or else
                      (Expected (I).Up /= Root and then not Expected (Expected (I).Up).Alive))
                  then Expected (I) := Reference_Retired (Expected (I)); end if;
               end loop;
               Expected (Scope).Active_Calls := 0;
               while Expected_Used > Root and then not Expected (Expected_Used).Alive loop
                  Expected (Expected_Used) := (others => <>); Expected_Used := Expected_Used - 1;
               end loop;
               for I in 1 .. Expected_Used loop
                  Kept := Aggregate_Method_Count'Max (Kept, Expected (I).Method_Offset + Expected (I).Method_Size);
               end loop;
               if Kept < Code_Used then Expected_Code (Kept + 1 .. Code_Used) := [others => 0]; end if;
               Code_Used := Kept;
            end if;
         end if;
         End_Method (Tree, Scope);
         Check (Tree.Items = Expected and Tree.Used = Expected_Used);
         Check (Tree.Code_Used = Code_Used and Tree.Code = Expected_Code);
         Check (Tree.Values = Before_Values and Tree.Root_Value = Before_Root and Tree.Last_Stamp = Before_Stamp);
         Check (Memberships.Valid (Tree.Owner_Members) and Memberships.Valid (Tree.Temporary_Children));
         Check (Code_Bounds.Valid (Tree.Method_Bounds) and Code_Bounds.Count (Tree.Method_Bounds) = Tree.Used);
         for I in 1 .. Capacity loop
            Check (Memberships.Key_Of (Tree.Owner_Members, I) =
              (if I <= Tree.Used and then Tree.Items (I).Alive then Tree.Items (I).Owner else Root));
            Check (Memberships.Key_Of (Tree.Temporary_Children, I) =
              (if I <= Tree.Used and then Tree.Items (I).Alive and then Tree.Items (I).Owner /= Root
               then Tree.Items (I).Up else Root));
            if I <= Tree.Used then
               Prefix := Aggregate_Method_Count'Max (Prefix, Tree.Items (I).Method_Offset + Tree.Items (I).Method_Size);
               Check (Code_Bounds.Bound_At (Tree.Method_Bounds, I) = Prefix);
            else Check (Code_Bounds.Bound_At (Tree.Method_Bounds, I) = 0); end if;
         end loop;
      end Compare;
      procedure Basic (Used : Node_ID) is
      begin
         Tree := Empty_Like (Tree);
         Tree.Used := Used; Tree.Last_Stamp := AML_References.Node_Incarnation (Used);
         for I in 1 .. Used loop
            Tree.Items (I) := (Incarnation => AML_References.Node_Incarnation (I), Part => "N000", others => <>);
            Tree.Items (I).Part (4) := Character'Val (Character'Pos ('0') + I);
         end loop;
         if Used > 0 then
            Tree.Items (1).Object_Type := Method_Object;
            Tree.Items (1).Active_Calls := 1; Tree.Items (1).Method_Size := 1;
         end if;
      end Basic;
      procedure Finish_Setup is
      begin
         Tree.Child_Names := Indexed (Tree.Items, Tree.Used);
         for I in 1 .. Tree.Used loop
            if Tree.Items (I).Alive and then Tree.Items (I).Owner /= Root then
               Memberships.Attach (Tree.Owner_Members, I, Tree.Items (I).Owner);
               if Tree.Items (I).Up /= Root then
                  Memberships.Attach (Tree.Temporary_Children, I, Tree.Items (I).Up);
               end if;
            end if;
            Code_Bounds.Append (Tree.Method_Bounds, Tree.Items (I).Method_Offset + Tree.Items (I).Method_Size);
         end loop;
         for I in 1 .. Tree.Code_Used loop Tree.Code (I) := AML_Decode.Byte (I); end loop;
         Check (Valid_Context (Tree));
      end Finish_Setup;
      procedure Cross_Owners is
      begin
         Basic (8);
         Tree.Items (2).Object_Type := Method_Object; Tree.Items (2).Active_Calls := 1;
         Tree.Items (2).Method_Offset := 1; Tree.Items (2).Method_Size := 1;
         Tree.Items (3).Owner := 1; Tree.Items (3).Object_Type := Method_Object;
         Tree.Items (3).Method_Offset := 2; Tree.Items (3).Method_Size := 3;
         Tree.Items (4).Owner := 2; Tree.Items (4).Up := 3;
         Tree.Items (5).Up := 3; Tree.Items (5).Object_Type := Method_Object;
         Tree.Items (5).Method_Offset := 5;
         Tree.Items (6).Owner := 2; Tree.Items (6).Up := 5;
         Tree.Items (7).Owner := 1; Tree.Items (7).Up := 5;
         Tree.Items (8).Owner := 2;
         Tree.Code_Used := 5; Finish_Setup;
      end Cross_Owners;
   begin
      Compare (0); Compare (Capacity);
      for Repeat in 1 .. 8 loop
         Cross_Owners;
         Tree.Items (1).Active_Calls := 2;
         Compare (1);
         Check (Tree.Items (3).Alive and Tree.Items (4).Alive and Tree.Items (7).Alive);
         Compare (1);
         Check (not Tree.Items (3).Alive and not Tree.Items (4).Alive and not Tree.Items (7).Alive);
         Check (Tree.Items (5).Alive and Tree.Items (6).Alive and Tree.Items (8).Alive);
         Check (Tree.Code_Used = 5);
         Compare (2);
         Check (Tree.Used = 5 and Tree.Code_Used = 5);
         Basic (4);
         Tree.Items (3).Alive := False;
         Tree.Items (4).Alive := False; Tree.Items (4).Object_Type := Method_Object;
         Tree.Items (4).Method_Offset := 1; Tree.Items (4).Method_Size := 2;
         Tree.Code_Used := 3; Finish_Setup;
         Compare (1);
         Check (Tree.Used = 2 and Tree.Code_Used = 1);
         Basic (4);
         Tree.Items (2).Owner := 1; Tree.Items (2).Object_Type := Method_Object;
         Tree.Items (2).Method_Offset := 1; Tree.Items (2).Method_Size := 3;
         Tree.Code_Used := 4; Finish_Setup;
         Compare (1);
         Check (not Tree.Items (2).Alive and Tree.Used = 4 and Tree.Code_Used = 4);
      end loop;
      return Checks;
   end Run;
end AML_Namespace.Lifetime_Test_Fixture;

with Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed;
with CCL.Types; use CCL.Types;
with CCL.Types.Correspondence;
with CCL.Objects;

procedure Correspondence_Tests is
   Checks : Natural := 0;
   type Mutation is
     (None, Root_Name, Nested_Name, Field_Name, Field_Order, Choice_Order,
      Payload_Type, Shape_Change, Missing_Choice, Missing_Root, Unused_Padding);
   procedure Check (Condition : Boolean) is
   begin
      pragma Assert (Condition);
      Checks := Checks + 1;
   end Check;
   function Numbered (Prefix : String; Number : Natural) return Name is
     (Named (Prefix & Ada.Strings.Fixed.Trim (Number'Image, Both)));
   procedure Add (Types : in out Registry; D : Description; Ref : out Type_Reference) is
      Status : Definition_Result;
   begin
      Define (Types, D, Ref, Status);
      Check (Status = Defined);
   end Add;
   procedure Padding (Types : in out Registry; Count : Natural) is
      Ref : Type_Reference;
   begin
      for I in 1 .. Count loop
         Add (Types, (Identifier => Numbered ("Unused_", I), Form => Product, others => <>), Ref);
      end loop;
   end Padding;
   procedure Fixture (Types : in out Registry; Change : Mutation; Root : out Type_Reference) is
      D : Description;
      Leaf, Mode : Type_Reference;
      Temp : Component;
   begin
      D := (Identifier => Named (if Change = Nested_Name then "OtherDetail" else "Detail"),
            Form => Product, Count => 2, others => <>);
      D.Parts (1) := (Named (if Change = Field_Name then "other" else "label"), String_Type);
      D.Parts (2) := (Named ("enabled"), (if Change = Payload_Type then Integer_Type else Boolean_Type));
      if Change = Unused_Padding then
         D.Identifier.Data (32) := 'X';
         D.Parts (16) := (Named ("Ignored"), Handler_Type);
      end if;
      Add (Types, D, Leaf);
      D := (Identifier => Named ("Mode"), Form => (if Change = Shape_Change then Product else Sum),
            Count => (if Change = Missing_Choice then 1 else 2), others => <>);
      D.Parts (1) := (Named ("Idle"), Unit_Type);
      D.Parts (2) := (Named ("Active"), Leaf);
      if Change = Choice_Order then
         Temp := D.Parts (1); D.Parts (1) := D.Parts (2); D.Parts (2) := Temp;
      end if;
      Add (Types, D, Mode);
      Root := Invalid_Type;
      if Change = Missing_Root then return; end if;
      D := (Identifier => Named (if Change = Root_Name then "OtherPreferences" else "Preferences"),
            Form => Product, Count => 2, others => <>);
      D.Parts (1) := (Named ("title"), String_Type);
      D.Parts (2) := (Named ("mode"), Mode);
      if Change = Field_Order then
         Temp := D.Parts (1); D.Parts (1) := D.Parts (2); D.Parts (2) := Temp;
      end if;
      Add (Types, D, Root);
   end Fixture;
begin
   for Extra in 0 .. Maximum_Declarations - 3 loop
      for Change in Mutation loop
         declare
            Source, Target : Registry;
            Root, Expected : Type_Reference;
            A, B, Different_Key, Unbound : CCL.Objects.Binding;
            Good : Boolean;
         begin
            Fixture (Source, None, Root);
            Padding (Target, Extra);
            Fixture (Target, Change, Expected);
            Check (Correspondence.Resolve (Source, Root, Target) =
              (if Change in None | Unused_Padding then Expected else Invalid_Type));
            if Change in None | Unused_Padding then
               Check (Correspondence.Resolve (Target, Expected, Source) = Root);
            end if;
            CCL.Objects.Bind (Source, Root, [1, 2, 3, 4], A, Good); Check (Good);
            CCL.Objects.Bind (Target, Expected, [1, 2, 3, 4], B, Good);
            Check (Good = (Change /= Missing_Root));
            Check (CCL.Objects.Same_Schema (A, B) = (Change in None | Unused_Padding));
            Check (CCL.Objects.Same_Schema (B, A) = (Change in None | Unused_Padding));
            CCL.Objects.Bind (Target, Expected, [5, 6, 7, 8], Different_Key, Good);
            Check (not CCL.Objects.Same_Schema (A, Different_Key));
            Check (not CCL.Objects.Same_Schema (A, Unbound));
            Check (not CCL.Objects.Same_Schema (Unbound, Unbound));
         end;
      end loop;
   end loop;
   --  A reachable dependency conflict must poison its parent; unrelated
   --  conflicts must not. Registries themselves are never modified.
   declare
      Source, Target, Saved : Registry;
      Root, Expected, Ref : Type_Reference;
   begin
      Fixture (Source, None, Root);
      Fixture (Target, None, Expected);
      Add (Source, (Identifier => Named ("Unrelated"), Form => Product, others => <>), Ref);
      Add (Target, (Identifier => Named ("Unrelated"), Form => Sum, Count => 1,
        Parts => [1 => (Named ("Choice"), Integer_Type), others => <>]), Ref);
      Saved := Target;
      Check (Correspondence.Resolve (Source, Root, Target) = Expected);
      Check (Target = Saved);
      for Primitive in Invalid_Type .. Unit_Type loop
         Check (Correspondence.Resolve (Source, Primitive, Target) = Primitive);
      end loop;
      Check (Correspondence.Resolve (Source, Type_Reference'Last, Target) = Invalid_Type);
   end;
   --  Deep/shared graphs remain bounded by declarations, not traversal paths.
   for Depth in 1 .. Maximum_Declarations loop
      declare
         Source, Target : Registry;
         Root, Expected : Type_Reference := Unit_Type;
         D : Description;
      begin
         Padding (Target, (if Depth = Maximum_Declarations then 0 else 1));
         for I in 1 .. Depth loop
            D := (Identifier => Numbered ("Level_", I), Form => Product, Count => 1,
                  Parts => [1 => (Named ("child"), Root), others => <>]);
            Add (Source, D, Root);
            D.Parts (1).Payload := Expected;
            Add (Target, D, Expected);
         end loop;
         Check (Correspondence.Resolve (Source, Root, Target) = Expected);
      end;
   end loop;
   declare
      Source, Target : Registry;
      Root, Expected, Leaf : Type_Reference;
      D : Description;
   begin
      for Side in Boolean loop
         declare
            R : Registry;
         begin
            Padding (R, (if Side then 3 else 0));
            Add (R, (Identifier => Named ("Leaf"), Form => Product, Count => 1,
              Parts => [1 => (Named ("number"), Integer_Type), others => <>]), Leaf);
            D := (Identifier => Named ("Shared"), Form => Product,
                  Count => Maximum_Components, others => <>);
            for Part in D.Parts'Range loop
               D.Parts (Part) := (Numbered ("part_", Part), Leaf);
            end loop;
            if Side then Add (R, D, Expected); Target := R;
            else Add (R, D, Root); Source := R; end if;
         end;
      end loop;
      Check (Correspondence.Resolve (Source, Root, Target) = Expected);
   end;
   declare
      A_Types, B_Types : Registry;
      A_Leaf, B_Leaf, A_Root, B_Root : Type_Reference;
      A, B : CCL.Objects.Binding;
      Good : Boolean;
   begin
      -- Independent dependency declarations may be ordered differently while
      -- the product's field order and full nominal meaning stay identical.
      for Side in Boolean loop
         declare
            R : Registry;
         begin
            if Side then
               Add (R, (Identifier => Named ("B"), Form => Product, others => <>), B_Leaf);
               Add (R, (Identifier => Named ("A"), Form => Product, others => <>), A_Leaf);
            else
               Add (R, (Identifier => Named ("A"), Form => Product, others => <>), A_Leaf);
               Add (R, (Identifier => Named ("B"), Form => Product, others => <>), B_Leaf);
            end if;
            Add (R, (Identifier => Named ("Pair"), Form => Product, Count => 2,
              Parts => [1 => (Named ("a"), A_Leaf), 2 => (Named ("b"), B_Leaf), others => <>]), A_Root);
            if Side then B_Types := R; B_Root := A_Root; else A_Types := R; end if;
         end;
      end loop;
      CCL.Objects.Bind (A_Types, A_Root, [1, 2, 3, 4], A, Good); Check (Good);
      CCL.Objects.Bind (B_Types, B_Root, [1, 2, 3, 4], B, Good); Check (Good);
      Check (CCL.Objects.Same_Schema (A, B) and CCL.Objects.Same_Schema (B, A));
   end;
   Ada.Text_IO.Put_Line ("Nominal type correspondence: PASS" & Checks'Image & " checks");
end Correspondence_Tests;

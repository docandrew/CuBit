with Ada.Text_IO;
with Interfaces; use Interfaces;
with CCL.Types; use CCL.Types;
with CCL.Objects; use CCL.Objects;
with CCL.Objects.Catalog; use CCL.Objects.Catalog;

procedure Main is
   Checks : Natural := 0;
   procedure Check (Good : Boolean) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with "schema catalog check" & Checks'Image; end if;
   end Check;
   function Key_Of (I : Natural) return Schema_Key is
     ([Unsigned_64 (I + 1), 2, 3, 4]);
   function Name_Of (I : Natural) return Name is
      Number : constant String := I'Image;
   begin
      return Named ("Type" & Number (Number'First + 1 .. Number'Last));
   end Name_Of;
   function Contract_For
     (Types : Registry; Root : Type_Reference; Key : Schema_Key) return Binding
   is
      Contract : Binding;
      Accepted : Boolean;
   begin
      Bind (Types, Root, Key, Contract, Accepted);
      Check (Accepted);
      return Contract;
   end Contract_For;
   procedure Expect
     (Item : in out Schema_Catalog; Contract : Binding; Expected : Publication_Result)
   is
      Before : constant Schema_Catalog := Item;
      Result : Publication_Result;
   begin
      Publish (Item, Contract, Result);
      Check (Result = Expected);
      if Result /= Published then Check (Item = Before); end if;
   end Expect;
   procedure Add_Reading
     (Types : in out Registry; Root : out Type_Reference;
      Payload : Type_Reference := Integer_Type)
   is
      Result : Definition_Result;
   begin
      Define (Types, (Identifier => Named ("Reading"), Form => Sum, Count => 2,
        Parts => [1 => (Named ("Value"), Payload), 2 => (Named ("Unavailable"), Unit_Type),
                  others => <>]), Root, Result);
      Check (Result = Defined);
   end Add_Reading;
   Empty_Types : Registry;
   Item : Schema_Catalog;
   Unbound, Found : Binding;
   Counts : constant array (Positive range <>) of Natural := [0, 1, 2, 112, 113, Schema_Ceiling];
begin
   Resolve (Item, No_Schema, Found); Check (not Is_Bound (Found));
   Resolve (Item, Key_Of (0), Found); Check (not Is_Bound (Found));
   Expect (Item, Unbound, Unbound_Contract);
   declare
      Accepted : Boolean;
      Types : Registry;
      Root : Type_Reference;
      Defined_As : Definition_Result;
   begin
      Bind (Types, Integer_Type, No_Schema, Unbound, Accepted);
      Check (not Accepted); Expect (Item, Unbound, Unbound_Contract);
      Bind (Types, Handler_Type, Key_Of (0), Unbound, Accepted);
      Check (not Accepted); Expect (Item, Unbound, Unbound_Contract);
      Define (Types, (Identifier => Named ("HiddenAuthority"), Form => Sum, Count => 2,
        Parts => [1 => (Named ("None"), Unit_Type), 2 => (Named ("Action"), Handler_Type),
                  others => <>]), Root, Defined_As);
      Check (Defined_As = Defined);
      Bind (Types, Root, Key_Of (0), Unbound, Accepted);
      Check (not Accepted); Expect (Item, Unbound, Unbound_Contract);
   end;
   -- Schema tables around the old fixed size (112) and at the ceiling,
   -- including identity reuse when full (the table grows from what is
   -- published). Shared primitive types consume no nominal declaration slots.
   for Count of Counts loop
      declare
         Table : Schema_Catalog;
      begin
         for I in 1 .. Count loop
            Expect (Table, Contract_For (Empty_Types, Integer_Type, Key_Of (I)), Published);
            Check (Length (Table) = I);
         end loop;
         Check (Last (Visible_Types (Table)) = Unit_Type);
         for I in 1 .. Count loop
            Resolve (Table, Key_Of (I), Found);
            Check (Is_Bound (Found) and then Identity (Found) = Key_Of (I) and then
                   Root_Type (Found) = Integer_Type);
            Expect (Table, Contract_For (Empty_Types, Integer_Type, Key_Of (I)), Already_Published);
            Expect (Table, Contract_For (Empty_Types, Boolean_Type, Key_Of (I)), Identity_Conflict);
         end loop;
         Expect (Table, Contract_For (Empty_Types, Boolean_Type, Key_Of (Count + 1)),
                 (if Count = Schema_Ceiling then Capacity_Exhausted else Published));
      end;
   end loop;
   -- Local numbering and unrelated definitions are not schema identity.
   declare
      Source, Shifted, Changed : Registry;
      Root, Shifted_Root, Changed_Root, Ref : Type_Reference;
      Defined_As : Definition_Result;
      Table : Schema_Catalog;
      Original : Binding;
      Value : CCL.Objects.Image;
      Built_As : Build_Result;
   begin
      Add_Reading (Source, Root);
      Define (Shifted, (Identifier => Named ("Unused"), Form => Product, Count => 1,
        Parts => [1 => (Named ("action"), Handler_Type), others => <>]), Ref, Defined_As);
      Check (Defined_As = Defined);
      Add_Reading (Shifted, Shifted_Root); Check (Root /= Shifted_Root);
      Original := Contract_For (Source, Root, Key_Of (0));
      Expect (Table, Original, Published);
      Expect (Table, Contract_For (Shifted, Shifted_Root, Key_Of (0)), Already_Published);
      Check (Find (Visible_Types (Table), Named ("Unused")) = Invalid_Type);
      Check (Length (Table) = 1 and then Last (Visible_Types (Table)) = Root);
      -- Multiple approved keys may describe the same nominal definition; the
      -- catalog must not manufacture a new definition for each key.
      Expect (Table, Contract_For (Shifted, Shifted_Root, Key_Of (1)), Published);
      Check (Last (Visible_Types (Table)) = Root);
      Value := Empty (Original);
      Append (Value, Variant_Cell (1), Built_As); Check (Built_As = Added);
      Append (Value, Integer_Cell (-42), Built_As); Check (Built_As = Added);
      Resolve (Table, Key_Of (0), Found); Check (Validate (Value, Found));
      Resolve (Table, Key_Of (1), Found); Check (not Validate (Value, Found));
      Value.Schema := Key_Of (1); Check (Validate (Value, Found));
      Add_Reading (Changed, Changed_Root, Boolean_Type);
      Expect (Table, Contract_For (Changed, Changed_Root, Key_Of (0)), Identity_Conflict);
      Expect (Table, Contract_For (Changed, Changed_Root, Key_Of (2)), Definition_Conflict);
      Resolve (Table, Key_Of (2), Found); Check (not Is_Bound (Found));
      Resolve (Table, Key_Of (0), Found);
      Value.Schema := Key_Of (0); Check (Validate (Value, Found));
   end;
   -- A valid large closure can exhaust the shared type budget (the registry's
   -- named ceiling) before the schema table. Rejected publication leaves
   -- neither a key nor partial types. The closure is a chain of records each
   -- holding a list of the one before, so its layout stays one record deep.
   declare
      Types, Extra : Registry;
      Root : Type_Reference := Integer_Type;
      Ref : Type_Reference;
      List : Type_Reference := Invalid_Type;
      Defined_As : Definition_Result;
      Listed : List_Result;
      Table : Schema_Catalog;
   begin
      Define (Types, (Identifier => Name_Of (0), Form => Product, Count => 1,
        Parts => [1 => (Named ("item"), Root), others => <>]), Root, Defined_As);
      Check (Defined_As = Defined);
      for I in 1 .. Declaration_Ceiling / 2 loop
         Define (Types, (Identifier => Name_Of (I), Form => Product, Count => 1,
           Parts => [1 => (Named ("items"), (if I = 1 then Root else List)), others => <>]), Ref, Defined_As);
         Check (Defined_As = Defined); Root := Ref;
         if I < Declaration_Ceiling / 2 then
            Specialize_List (Types, Root, List, Listed); Check (Listed = List_Specialized);
         end if;
      end loop;
      Check (Last (Types) = Type_Reference'Last);
      --  Full: the registry refuses one more, unchanged.
      declare
         Before : constant Registry := Types;
      begin
         Define (Types, (Identifier => Named ("Overflow"), Form => Product, others => <>), Ref, Defined_As);
         Check (Defined_As = Registry_Full and Ref = Invalid_Type and Types = Before);
      end;
      Expect (Table, Contract_For (Types, Root, Key_Of (0)), Published);
      Check (Length (Table) = 1 and then Last (Visible_Types (Table)) = Type_Reference'Last);
      Define (Extra, (Identifier => Named ("Extra"), Form => Product, others => <>), Ref, Defined_As);
      Check (Defined_As = Defined);
      Expect (Table, Contract_For (Extra, Ref, Key_Of (1)), Capacity_Exhausted);
      Resolve (Table, Key_Of (1), Found); Check (not Is_Bound (Found));
      -- Existing definitions and primitive schemas still work at type capacity.
      Expect (Table, Contract_For (Types, Root, Key_Of (0)), Already_Published);
      Expect (Table, Contract_For (Empty_Types, Unit_Type, Key_Of (2)), Published);
   end;
   Ada.Text_IO.Put_Line ("Approved schema catalog: PASS" & Checks'Image & " checks");
   Ada.Text_IO.Put_Line ("Schema catalog bytes:" & Natural'Image (Schema_Catalog'Size / 8) &
                         "; one binding bytes:" & Natural'Image (Binding'Size / 8));
end Main;

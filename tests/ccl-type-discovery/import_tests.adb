with Ada.Text_IO;
with CCL.Types; use CCL.Types;
with CCL.Types.Correspondence;

procedure Import_Tests is
   Checks : Natural := 0;
   procedure Check (Good : Boolean) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with "type import check" & Checks'Image; end if;
   end Check;
   function Label (Prefix : String; Index : Positive) return Name is
     (Named (Prefix & Character'Val (Character'Pos ('A') + (Index - 1) mod 26) &
             Character'Val (Character'Pos ('A') + (Index - 1) / 26)));
   procedure Add (Item : in out Registry; D : Description; Ref : out Type_Reference) is
      Result : Definition_Result;
   begin
      Define (Item, D, Ref, Result); Check (Result = Defined);
   end Add;
   procedure Padding (Item : in out Registry; Count : Natural) is
      Ref : Type_Reference;
   begin
      for Index in 1 .. Count loop
         Add (Item, (Identifier => Label ("Pad", Index), Form => Product, others => <>), Ref);
      end loop;
   end Padding;
   procedure Check_Import
     (Source : Registry; Root : Type_Reference; Target : in out Registry; Expected : Import_Result)
   is
      Before : constant Registry := Target;
      Ref : Type_Reference;
      Result : Import_Result;
   begin
      Import_Definition (Source, Root, Target, Ref, Result);
      Check (Result = Expected);
      if Result = Imported then
         Check (Known (Target, Ref));
         Check (CCL.Types.Correspondence.Resolve (Source, Root, Target) = Ref);
         for Index in Integer_Type .. Last (Before) loop
            Check (Describe (Target, Index) = Describe (Before, Index));
            Check (Cells (Target, Index) = Cells (Before, Index));
         end loop;
         declare
            Published : constant Registry := Target;
            Again : Type_Reference;
         begin
            Import_Definition (Source, Root, Target, Again, Result);
            Check (Result = Imported and Again = Ref and Target = Published);
         end;
      else
         Check (Ref = Invalid_Type and Target = Before);
      end if;
   end Check_Import;
begin
   for Depth in 1 .. Maximum_Declarations loop
      declare
         Source : Registry;
         Root : Type_Reference := Integer_Type;
      begin
         for Index in 1 .. Depth loop
            Add (Source, (Identifier => Label ("Chain", Index), Form => Product, Count => 1,
              Parts => [1 => (Named ("value"), Root), others => <>]), Root);
         end loop;
         for Count in 0 .. Maximum_Declarations loop
            declare
               Target : Registry;
            begin
               Padding (Target, Count);
               Check_Import (Source, Root, Target,
                 (if Count + Depth <= Maximum_Declarations then Imported else Import_Full));
            end;
         end loop;
      end;
   end loop;
   for Count in 0 .. Maximum_Declarations loop
      declare
         Source, Target : Registry;
      begin
         Padding (Target, Count);
         for Root in Type_Reference loop
            Check_Import (Source, Root, Target,
              (if Root in Integer_Type .. Unit_Type then Imported else Invalid_Root));
         end loop;
      end;
   end loop;
   for Change in 0 .. 6 loop
      declare
         Source, Target : Registry;
         Payload, Root, Ref : Type_Reference;
         D : Description;
      begin
         Add (Source, (Identifier => Named ("Hidden"), Form => Product, others => <>), Ref);
         Add (Source, (Identifier => Named ("Payload"), Form => Product, Count => 1,
           Parts => [1 => (Named ("value"), Integer_Type), others => <>]), Payload);
         D := (Identifier => Named ("Outcome"), Form => Sum, Count => 2,
               Parts => [1 => (Named ("First"), Payload),
                         2 => (Named ("Second"), Payload), others => <>]);
         Add (Source, D, Root);
         Add (Source, (Identifier => Named ("LaterSecret"), Form => Product, others => <>), Ref);
         -- Unreachable conflicts must neither block the root nor expose names.
         Add (Target, (Identifier => Named ("Hidden"), Form => Sum, Count => 1,
           Parts => [1 => (Named ("Different"), Unit_Type), others => <>]), Ref);
         if Change > 0 then
            Add (Target, (Identifier => Named ("Payload"), Form => Product, Count => 1,
              Parts => [1 => (Named ("value"), Integer_Type), others => <>]), Payload);
            D.Parts (1).Payload := Payload; D.Parts (2).Payload := Payload;
            case Change is
               when 1 => null;
               when 2 => D.Form := Product;
               when 3 => D.Count := 1;
               when 4 => D.Parts (1).Identifier := Named ("Changed");
               when 5 => D.Parts (2).Payload := Boolean_Type;
               when 6 => D.Parts (1).Identifier := Named ("Second");
                  D.Parts (2).Identifier := Named ("First");
               when others => null;
            end case;
            Add (Target, D, Ref);
         end if;
         Check_Import (Source, Root, Target, (if Change <= 1 then Imported else Conflicting_Definition));
         Check (Find (Target, Named ("LaterSecret")) = Invalid_Type);
         if Change <= 1 then Check (Last (Target) = Unit_Type + 3); end if;
      end;
   end loop;
   -- A root conflict occurs after staging a previously missing dependency:
   -- even that dependency must be absent when the whole import is rejected.
   declare
      Source, Target : Registry;
      Payload, Root, Ref : Type_Reference;
   begin
      Add (Source, (Identifier => Named ("Payload"), Form => Product, others => <>), Payload);
      Add (Source, (Identifier => Named ("Root"), Form => Product, Count => 1,
        Parts => [1 => (Named ("child"), Payload), others => <>]), Root);
      Add (Target, (Identifier => Named ("Root"), Form => Product, others => <>), Ref);
      Check_Import (Source, Root, Target, Conflicting_Definition);
      Check (Find (Target, Named ("Payload")) = Invalid_Type);
   end;
   Ada.Text_IO.Put_Line ("Atomic reachable type import: PASS" & Checks'Image & " checks");
end Import_Tests;

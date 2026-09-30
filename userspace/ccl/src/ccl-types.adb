package body CCL.Types with SPARK_Mode is
   function Named (Text : String) return Name is
      Result : Name;
   begin
      if Text'Length <= Maximum_Name_Length then
         Result.Length := Text'Length;
         Result.Data (1 .. Result.Length) := Text;
      end if;
      return Result;
   end Named;

   --  A natural number as text, without Ada's leading blank.
   function Image_Of (Value : Natural) return String is
      Text : constant String := Natural'Image (Value);
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Image_Of;

   function Valid_Name (Item : Name) return Boolean is
   begin
      if Item.Length = 0 or else Item.Data (1) not in
        'A' .. 'Z' | 'a' .. 'z' | '_'
      then return False; end if;
      return (for all C of Item.Data (1 .. Item.Length) =>
        C in 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '-');
   end Valid_Name;

   function Describe (Item : Registry; Ref : Type_Reference) return Description is
   begin
      if Ref in Declared_Type and then Ref <= Item.Used then
         return Item.Definitions (Ref);
      end if;
      return
        (Identifier => Named
           (case Ref is when Integer_Type => "Integer",
            when Boolean_Type => "Boolean", when String_Type => "String",
            when Character_Type => "Character", when Handler_Type => "Handler",
            when Unit_Type => "Unit", when others => ""),
         Form => (if Ref = Unit_Type then Product else Primitive), others => <>);
   end Describe;

   function Find (Item : Registry; Identifier : Name) return Type_Reference is
   begin
      if not Valid_Name (Identifier) then return Invalid_Type; end if;
      for Ref in Integer_Type .. Item.Used loop
         if Same (Describe (Item, Ref).Identifier, Identifier) then return Ref; end if;
      end loop;
      return Invalid_Type;
   end Find;

   function Cells (Item : Registry; Ref : Type_Reference) return Cell_Count is
   begin
      if not Known (Item, Ref) then return 0;
      elsif Ref in Declared_Type then return Item.Layouts (Ref);
      else return 1; -- scalar cell or empty-product marker; text is a region reference
      end if;
   end Cells;

   function Is_Enumeration (Item : Registry; Ref : Type_Reference) return Boolean is
      D : constant Description := Describe (Item, Ref);
   begin
      return Known (Item, Ref) and then D.Form = Sum and then D.Count > 0 and then
        (for all I in 1 .. D.Count => D.Parts (I).Payload = Unit_Type);
   end Is_Enumeration;

   function Is_Scalar_Sum (Item : Registry; Ref : Type_Reference) return Boolean is
      D : constant Description := Describe (Item, Ref);
   begin
      return Ref in Declared_Type and then Known (Item, Ref) and then
        D.Form = Sum and then D.Count > 0 and then
        (for all I in 1 .. D.Count =>
          D.Parts (I).Payload in Unit_Type | Integer_Type | Boolean_Type);
   end Is_Scalar_Sum;

   function Alternative (Item : Registry; Ref : Type_Reference; Identifier : Name)
     return Component_Count is
      D : constant Description := Describe (Item, Ref);
   begin
      if D.Form /= Sum then return 0; end if;
      for I in 1 .. D.Count loop
         if Same (D.Parts (I).Identifier, Identifier) then return I; end if;
      end loop;
      return 0;
   end Alternative;

   procedure Resolve_Alternative
     (Item : Registry; Qualified : Name; Ref : out Type_Reference;
      Choice : out Component_Count) is
   begin
      Ref := Invalid_Type; Choice := 0;
      for Dot in 2 .. Qualified.Length loop
         if Qualified.Data (Dot) = '.' then
            Ref := Find (Item, Named (Qualified.Data (1 .. Dot - 1)));
            Choice := Alternative (Item, Ref,
              Named (Qualified.Data (Dot + 1 .. Qualified.Length)));
            return;
         end if;
      end loop;
   end Resolve_Alternative;

   procedure Define
     (Item : in out Registry; Definition : Description;
      Ref : out Type_Reference; Result : out Definition_Result)
   is
      Layout : Cell_Count := 1;
      Size : Cell_Count;
   begin
      Ref := Invalid_Type;
      Result := Invalid_Name;
      if not Valid_Name (Definition.Identifier) then return; end if;
      Result := Duplicate_Name;
      if Find (Item, Definition.Identifier) /= Invalid_Type then return; end if;
      Result := Invalid_Shape;
      if Definition.Form = Primitive or else
        (Definition.Form = Sum and Definition.Count = 0)
      then return; end if;
      for I in 1 .. Definition.Count loop
         Result := Invalid_Name;
         if not Valid_Name (Definition.Parts (I).Identifier) then return; end if;
         Result := Duplicate_Name;
         for J in 1 .. I - 1 loop
            if Same (Definition.Parts (I).Identifier,
                     Definition.Parts (J).Identifier) then return; end if;
         end loop;
         Result := Invalid_Reference;
         if not Known (Item, Definition.Parts (I).Payload) then return; end if;
         Size := Cells (Item, Definition.Parts (I).Payload);
         Result := Layout_Too_Large;
         if Definition.Form = Product then
            if Size > Maximum_Value_Cells - Layout then return; end if;
            Layout := Layout + Size;
         elsif Definition.Form = Sum then
            if Size = Maximum_Value_Cells then return; end if;
            Layout := Cell_Count'Max (Layout, Size + 1);
         end if;
      end loop;
      Result := Registry_Full;
      if Item.Used = Type_Reference'Last then return; end if;
      Ref := Item.Used + 1;
      Item.Definitions (Ref) := Definition;
      Item.Layouts (Ref) := Layout;
      Item.Used := Ref;
      Result := Defined;
   end Define;

   function Same_Description (Left, Right : Description) return Boolean is
   begin
      if not Same (Left.Identifier, Right.Identifier) or else
        Left.Form /= Right.Form or else Left.Count /= Right.Count
      then
         return False;
      end if;
      for I in 1 .. Left.Count loop
         if not Same (Left.Parts (I).Identifier, Right.Parts (I).Identifier)
           or else Left.Parts (I).Payload /= Right.Parts (I).Payload
         then
            return False;
         end if;
      end loop;
      return True;
   end Same_Description;

   procedure Specialize_Unary_Resource
     (Item : in out Registry; Family, Parameter_Label : Name;
      Parameter : Type_Reference; Ref : out Type_Reference;
      Result : out Unary_Resource_Result)
   is
      Candidate : Description;
      Existing : Type_Reference;
      Defined_As : Definition_Result;
   begin
      Ref := Invalid_Type;
      Result := Invalid_Resource_Family;
      if not Valid_Name (Family) then
         return;
      end if;
      Result := Invalid_Resource_Parameter;
      if not Valid_Name (Parameter_Label) or else not Known (Item, Parameter) then
         return;
      end if;
      Candidate :=
        (Identifier => Named (Image (Family) & "-" &
             Image (Describe (Item, Parameter).Identifier)),
         Form => Resource,
         Count => 1,
         Parts => [1 => (Parameter_Label, Parameter), others => <>]);
      Result := Resource_Name_Too_Long;
      if not Valid_Name (Candidate.Identifier) then
         return;
      end if;
      Existing := Find (Item, Candidate.Identifier);
      if Existing /= Invalid_Type then
         Result := Resource_Definition_Conflict;
         if Same_Description (Describe (Item, Existing), Candidate) then
            Ref := Existing;
            Result := Resource_Already_Specialized;
         end if;
         return;
      end if;
      Define (Item, Candidate, Ref, Defined_As);
      case Defined_As is
         when Defined => Result := Resource_Specialized;
         when Registry_Full => Result := Resource_Registry_Full;
         when others =>
            -- Candidate was validated above; retain a precise failure if the
            -- ordinary definition rules become more restrictive later.
            Result := Resource_Definition_Conflict;
      end case;
   end Specialize_Unary_Resource;

   procedure Specialize_List
     (Item : in out Registry; Element : Type_Reference;
      Ref : out Type_Reference; Result : out List_Result)
   is
      Candidate : Description;
      Existing : Type_Reference;
      Defined_As : Definition_Result;
   begin
      Ref := Invalid_Type;
      Result := Invalid_List_Element;
      --  Scalars, strings and enumerations first; lists of records and of
      --  lists come with object-image elements.
      if not Known (Item, Element) or else Element = Handler_Type or else
        Element = Unit_Type or else
        (Describe (Item, Element).Form /= Primitive and then
         not Is_Enumeration (Item, Element))
      then
         return;
      end if;
      Candidate :=
        (Identifier => Named ("List-" & Image (Describe (Item, Element).Identifier)),
         Form => Sequence,
         Count => 1,
         Parts => [1 => (Named ("element"), Element), others => <>]);
      Result := List_Name_Too_Long;
      if not Valid_Name (Candidate.Identifier) then
         return;
      end if;
      Existing := Find (Item, Candidate.Identifier);
      if Existing /= Invalid_Type then
         Result := Invalid_List_Element;
         if Same_Description (Describe (Item, Existing), Candidate) then
            Ref := Existing;
            Result := List_Already_Specialized;
         end if;
         return;
      end if;
      Define (Item, Candidate, Ref, Defined_As);
      Result := (if Defined_As = Defined then List_Specialized
                 elsif Defined_As = Registry_Full then List_Registry_Full
                 else Invalid_List_Element);
      if Defined_As /= Defined then Ref := Invalid_Type; end if;
   end Specialize_List;

   function Is_Function (Item : Registry; Ref : Type_Reference) return Boolean is
     (Known (Item, Ref) and then Describe (Item, Ref).Form = Callable);

   procedure Specialize_Function
     (Item : in out Registry; Parameters : Function_Parameters;
      Count : Function_Parameter_Count; Result_Type : Type_Reference;
      Ref : out Type_Reference; Result : out Function_Result)
   is
      Candidate : Description;
      Defined_As : Definition_Result;
      D : Description;
      Same_Parts : Boolean;
   begin
      Ref := Invalid_Type;
      Result := Invalid_Function_Part;
      if not Known (Item, Result_Type) or else Result_Type = Handler_Type then
         return;
      end if;
      for P in 1 .. Count loop
         if not Known (Item, Parameters (P)) or else Parameters (P) in Unit_Type | Handler_Type then
            return;
         end if;
      end loop;
      --  An existing function type with the same parts.
      for Existing in Declared_Type'First .. Last (Item) loop
         D := Describe (Item, Existing);
         if D.Form = Callable and then D.Count = Count + 1 then
            Same_Parts := D.Parts (Count + 1).Payload = Result_Type;
            for P in 1 .. Count loop
               Same_Parts := Same_Parts and then D.Parts (P).Payload = Parameters (P);
            end loop;
            if Same_Parts then
               Ref := Existing;
               Result := Function_Already_Specialized;
               return;
            end if;
         end if;
      end loop;
      Candidate :=
        (Identifier => Named ("Fn" & Image_Of (Natural (Last (Item)) + 1)),
         Form => Callable,
         Count => Count + 1,
         Parts => [others => <>]);
      for P in 1 .. Count loop
         Candidate.Parts (P) := (Named ("p" & Image_Of (P)), Parameters (P));
      end loop;
      Candidate.Parts (Count + 1) := (Named ("result"), Result_Type);
      Define (Item, Candidate, Ref, Defined_As);
      Result := (if Defined_As = Defined then Function_Specialized
                 elsif Defined_As = Registry_Full then Function_Registry_Full
                 else Invalid_Function_Part);
      if Defined_As /= Defined then Ref := Invalid_Type; end if;
   end Specialize_Function;

   function Is_List (Item : Registry; Ref : Type_Reference) return Boolean is
     (Known (Item, Ref) and then Describe (Item, Ref).Form = Sequence);

   function Element_Of (Item : Registry; Ref : Type_Reference) return Type_Reference is
     (if Is_List (Item, Ref) then Describe (Item, Ref).Parts (1).Payload
      else Invalid_Type);

   procedure Import_Definition
     (Source : Registry; Root : Type_Reference; Target : in out Registry;
      Ref : out Type_Reference; Result : out Import_Result)
   is
      Needed : array (Type_Reference) of Boolean := [others => False];
      Mapping : array (Type_Reference) of Type_Reference := [others => Invalid_Type];
      Staged : Registry := Target;
      Translated, Existing : Description;
      Candidate : Type_Reference;
      Defined_As : Definition_Result;
   begin
      Ref := Invalid_Type;
      Result := Invalid_Root;
      if not Known (Source, Root) then return; end if;
      if Root <= Unit_Type then
         Ref := Root; Result := Imported; return;
      end if;
      Needed (Root) := True;
      --  Published definitions only refer backwards. Reverse traversal marks
      --  the closure; forward traversal below translates it without recursion.
      for Index in reverse Declared_Type'First .. Root loop
         if Needed (Index) then
            declare
               D : constant Description := Describe (Source, Index);
            begin
               for Part in 1 .. D.Count loop
                  Needed (D.Parts (Part).Payload) := True;
               end loop;
            end;
         end if;
      end loop;
      for Index in Integer_Type .. Unit_Type loop Mapping (Index) := Index; end loop;
      Result := Conflicting_Definition;
      for Index in Declared_Type'First .. Root loop
         if Needed (Index) then
            Translated := Describe (Source, Index);
            for Part in 1 .. Translated.Count loop
               Translated.Parts (Part).Payload := Mapping (Translated.Parts (Part).Payload);
            end loop;
            Candidate := Find (Staged, Translated.Identifier);
            if Candidate = Invalid_Type then
               Define (Staged, Translated, Candidate, Defined_As);
               if Defined_As /= Defined then
                  if Defined_As = Registry_Full then Result := Import_Full; end if;
                  return;
               end if;
            else
               Existing := Describe (Staged, Candidate);
               if Existing.Form /= Translated.Form or Existing.Count /= Translated.Count then return; end if;
               for Part in 1 .. Translated.Count loop
                  if not Same (Existing.Parts (Part).Identifier, Translated.Parts (Part).Identifier)
                    or else Existing.Parts (Part).Payload /= Translated.Parts (Part).Payload
                  then return; end if;
               end loop;
            end if;
            Mapping (Index) := Candidate;
         end if;
      end loop;
      Target := Staged;
      Ref := Mapping (Root);
      Result := Imported;
   end Import_Definition;
end CCL.Types;

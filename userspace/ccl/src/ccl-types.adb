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
         else
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
end CCL.Types;

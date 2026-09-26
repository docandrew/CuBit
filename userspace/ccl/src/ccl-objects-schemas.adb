package body CCL.Objects.Schemas with SPARK_Mode is
   use type Types.Type_Reference;
   use type Types.Shape;
   use type Types.Definition_Result;
   use type Types.Import_Result;

   function Root_Closure (Contract : Binding) return Binding is
      Result : Binding;
      Imported : Types.Import_Result;
   begin
      if not Is_Bound (Contract) then return Result; end if;
      Types.Import_Definition (Contract.Types, Contract.Root, Result.Types, Result.Root, Imported);
      if Imported = Types.Imported then
         Result.Key := Contract.Key;
         Result.Bound := True;
      end if;
      return Result;
   end Root_Closure;

   function Wire_ID (Ref : Types.Type_Reference) return Unsigned_32 is
     (case Ref is
       when Types.Invalid_Type => 0,
       when Types.Integer_Type => Integer_ID,
       when Types.Boolean_Type => Boolean_ID,
       when Types.String_Type => String_ID,
       when Types.Character_Type => Character_ID,
       when Types.Handler_Type => Handler_ID,
       when Types.Unit_Type => Unit_ID,
       when Types.Declared_Type => Unit_ID + Unsigned_32 (Ref - Types.Unit_Type));

   function Local_ID (Ref : Unsigned_32) return Types.Type_Reference is
   begin
      case Ref is
         when Integer_ID => return Types.Integer_Type;
         when Boolean_ID => return Types.Boolean_Type;
         when String_ID => return Types.String_Type;
         when Character_ID => return Types.Character_Type;
         when Handler_ID => return Types.Handler_Type;
         when Unit_ID => return Types.Unit_Type;
         when others =>
            if Ref in Unit_ID + 1 .. Unit_ID + Types.Maximum_Declarations then
               return Types.Unit_Type + Types.Type_Reference (Ref - Unit_ID);
            end if;
            return Types.Invalid_Type;
      end case;
   end Local_ID;

   function Write_Name (Item : Types.Name) return Native_Name is
      Result : Native_Name;
   begin
      Result.Length := Unsigned_32 (Item.Length);
      Result.Text (1 .. Item.Length) := Types.Image (Item);
      return Result;
   end Write_Name;

   function Read_Name (Item : Native_Name) return Types.Name is
      Empty : Types.Name;
   begin
      if Item.Length not in 1 .. Types.Maximum_Name_Length then return Empty; end if;
      for I in Natural (Item.Length) + 1 .. Types.Maximum_Name_Length loop
         if Item.Text (I) /= Character'Val (0) then return Empty; end if;
      end loop;
      return Types.Named (Item.Text (1 .. Natural (Item.Length)));
   end Read_Name;

   procedure Write (Contract : Binding; Data : out Image; Accepted : out Boolean) is
      D : Types.Description;
      Index : Positive;
      Selected : constant Binding := Root_Closure (Contract);
   begin
      Data := (others => <>);
      Accepted := Is_Bound (Selected);
      if not Accepted then return; end if;
      Data.Key := Selected.Key;
      Data.Root := Wire_ID (Selected.Root);
      Data.Count := Unsigned_32 (Types.Last (Selected.Types) - Types.Unit_Type);
      for Ref in Types.Declared_Type'First .. Types.Last (Selected.Types) loop
         Index := Positive (Ref - Types.Unit_Type);
         D := Types.Describe (Selected.Types, Ref);
         Data.Definitions (Index).Identifier := Write_Name (D.Identifier);
         Data.Definitions (Index).Form := (if D.Form = Types.Product then Product_Form else Sum_Form);
         Data.Definitions (Index).Count := Unsigned_32 (D.Count);
         for Part in 1 .. D.Count loop
            Data.Definitions (Index).Parts (Part) :=
              (Write_Name (D.Parts (Part).Identifier), Wire_ID (D.Parts (Part).Payload));
         end loop;
      end loop;
   end Write;

   procedure Read (Data : Image; Contract : out Binding; Accepted : out Boolean) is
      Registry : Types.Registry;
      D : Types.Description;
      Ref : Types.Type_Reference;
      Result : Types.Definition_Result;
      Empty_Definition : constant Native_Definition := (others => <>);
      Empty_Part : constant Native_Part := (others => <>);
   begin
      Contract := (others => <>); Accepted := False;
      if Data.Format /= Version or else Data.Key = No_Schema or else
        Data.Count > Types.Maximum_Declarations or else Data.Reserved /= 0 or else
        (for some B of Data.Padding => B /= 0)
      then return; end if;
      for Index in Data.Definitions'Range loop
         if Unsigned_32 (Index) > Data.Count then
            if Data.Definitions (Index) /= Empty_Definition then return; end if;
         else
            declare
               Item : Native_Definition renames Data.Definitions (Index);
            begin
               if Item.Form not in Product_Form | Sum_Form or else
                 Item.Count > Types.Maximum_Components or else Item.Reserved /= 0 or else
                 (for some B of Item.Padding => B /= 0)
               then return; end if;
               D := (others => <>);
               D.Identifier := Read_Name (Item.Identifier);
               D.Form := (if Item.Form = Product_Form then Types.Product else Types.Sum);
               D.Count := Natural (Item.Count);
               for Part in Item.Parts'Range loop
                  if Unsigned_32 (Part) > Item.Count then
                     if Item.Parts (Part) /= Empty_Part then return; end if;
                  else
                     D.Parts (Part) :=
                       (Read_Name (Item.Parts (Part).Identifier), Local_ID (Item.Parts (Part).Payload));
                  end if;
               end loop;
               Types.Define (Registry, D, Ref, Result);
               if Result /= Types.Defined then return; end if;
            end;
         end if;
      end loop;
      Bind (Registry, Local_ID (Data.Root), Data.Key, Contract, Accepted);
   end Read;
end CCL.Objects.Schemas;

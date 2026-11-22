with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Interfaces;

package body CCL.Parameter_Signatures is
   package T renames CCL.Types;
   use type T.Type_Reference;
   use type T.Shape;
   use type T.Default_Kind;
   use type Interfaces.Integer_64;

   --  Ada's reserved words (RM 2.9), and the names the generated decoders
   --  and bindings declare or use unqualified.
   Reserved : constant String :=
     " abort abs abstract accept access aliased all and array at begin body case constant declare " &
     "delay delta digits do else elsif end entry exception exit for function generic goto if in " &
     "interface is limited loop mod new not null of or others out overriding package parallel " &
     "pragma private procedure protected raise range record rem renames requeue return reverse " &
     "select separate some subtype synchronized tagged task terminate then type until use when " &
     "while with xor choice text r interfaces ccl cubit decode boolean value identity parameters ";

   function Ada_Usable (Name : String) return Boolean is
     (Name'Length > 0
      and then Ada.Characters.Handling.Is_Letter (Name (Name'First))
      and then Name (Name'Last) /= '_'
      and then (for all C of Name => Ada.Characters.Handling.Is_Alphanumeric (C) or else C = '_')
      and then (for all I in Name'First .. Name'Last - 1 => not (Name (I) = '_' and then Name (I + 1) = '_'))
      and then Ada.Strings.Fixed.Index (Reserved, " " & Ada.Characters.Handling.To_Lower (Name) & " ") = 0);

   function Decimal (Value : Interfaces.Integer_64) return String is
      Image : constant String := Interfaces.Integer_64'Image (Value);
   begin
      return (if Image (Image'First) = ' ' then Image (Image'First + 1 .. Image'Last) else Image);
   end Decimal;

   procedure Render
     (Types : CCL.Types.Registry; Root_Name : String; First_Own : CCL.Types.Type_Reference;
      Item : out Result)
   is
      Root : T.Type_Reference := T.Invalid_Type;
      Order : array (1 .. T.Maximum_Declarations) of T.Type_Reference := [others => T.Invalid_Type];
      Ordered : Natural := 0;

      function Unit_Payload (Ref : T.Type_Reference) return Boolean is
        (Ref = T.Unit_Type or else Ref = T.Invalid_Type);
      function Declared (Ref : T.Type_Reference) return Boolean is
        (Ref in T.Declared_Type and then T.Known (Types, Ref));
      function Name_Of (Ref : T.Type_Reference) return String is
        (if Ref = T.Boolean_Type then "Boolean"
         elsif Ref = T.String_Type then "String"
         elsif Ref = T.Integer_Type then "Integer"
         elsif Declared (Ref) then T.Image (T.Describe (Types, Ref).Identifier)
         else "");

      procedure Refuse (Why : Refusal; Culprit : T.Type_Reference; Field : String := "") is
      begin
         if Item.Why = Accepted then
            Item.Why := Why;
            Item.Culprit_Type := T.Named (Name_Of (Culprit));
            Item.Culprit_Field := T.Named (Field);
         end if;
      end Refuse;

      --  Whether a value of Ref is, or can hold, a number.
      function Holds_Number (Ref : T.Type_Reference) return Boolean is
      begin
         if Ref = T.Integer_Type then
            return True;
         elsif not Declared (Ref) then
            return False;
         end if;
         declare
            Shape : constant T.Description := T.Describe (Types, Ref);
         begin
            return Shape.Form = T.Bounded
              or else (Shape.Form in T.Product | T.Sum
                       and then (for some P of Shape.Parts (1 .. Shape.Count) =>
                                   not Unit_Payload (P.Payload) and then Holds_Number (P.Payload)));
         end;
      end Holds_Number;

      --  Every type Ref reaches, before Ref.
      procedure Visit (Ref : T.Type_Reference) is
      begin
         if Item.Why /= Accepted or else Ref in T.Boolean_Type | T.String_Type then
            return;
         elsif Ref = T.Integer_Type then
            Refuse (Bare_Integer, Ref);
            return;
         elsif not Declared (Ref) then
            Refuse (Unsupported_Type, Ref);
            return;
         elsif (for some I in 1 .. Ordered => Order (I) = Ref) then
            return;
         end if;
         declare
            Shape : constant T.Description := T.Describe (Types, Ref);
         begin
            if Ref < First_Own then
               Refuse (Schema_Type, Ref);
               return;
            elsif Shape.Form not in T.Bounded | T.Product | T.Sum then
               Refuse (Unsupported_Type, Ref);
               return;
            elsif not Ada_Usable (T.Image (Shape.Identifier)) then
               Refuse (Unusable_Name, Ref);
               return;
            end if;
            if Shape.Form in T.Product | T.Sum then
               for P of Shape.Parts (1 .. Shape.Count) loop
                  if not Ada_Usable (T.Image (P.Identifier)) then
                     Refuse (Unusable_Name, Ref, T.Image (P.Identifier));
                     return;
                  elsif P.Payload = T.Integer_Type then
                     Refuse (Bare_Integer, Ref, T.Image (P.Identifier));
                     return;
                  elsif not Unit_Payload (P.Payload) then
                     Visit (P.Payload);
                  end if;
               end loop;
            end if;
            if Shape.Form = T.Product then
               for Part in 1 .. Shape.Count loop
                  if T.Default_Of (Types, Ref, Part).Kind /= T.No_Default
                    and then Holds_Number (Shape.Parts (Part).Payload)
                  then
                     Refuse (Defaulted_Number, Ref, T.Image (Shape.Parts (Part).Identifier));
                     return;
                  end if;
               end loop;
            end if;
         end;
         if Item.Why = Accepted then
            Ordered := Ordered + 1;
            Order (Ordered) := Ref;
         end if;
      end Visit;

      procedure Append (Text : String) is
      begin
         if Item.Why /= Accepted then
            return;
         elsif Item.Length + Text'Length > Maximum_Signature_Bytes then
            Refuse (Too_Long, Root);
            return;
         end if;
         Item.Text (Item.Length + 1 .. Item.Length + Text'Length) := Text;
         Item.Length := Item.Length + Text'Length;
      end Append;

      function Default_Image (Ref : T.Type_Reference; Part : T.Component_Index) return String is
         Shape : constant T.Description := T.Describe (Types, Ref);
         Default : constant T.Field_Default := T.Default_Of (Types, Ref, Part);
         Payload : constant T.Type_Reference := Shape.Parts (Part).Payload;
      begin
         case Default.Kind is
            when T.Boolean_Default =>
               return (if Default.Value = 1 then " true" else " false");
            when T.Alternative_Default =>
               declare
                  Alternatives : constant T.Description := T.Describe (Types, Payload);
               begin
                  return " " & T.Image (Alternatives.Identifier) & "." &
                    T.Image (Alternatives.Parts (Positive (Default.Value)).Identifier);
               end;
            when T.No_Default | T.Integer_Default | T.Empty_List_Default =>
               --  Numbers and lists were refused above.
               return "";
         end case;
      end Default_Image;

      procedure Declare_Type (Ref : T.Type_Reference) is
         Shape : constant T.Description := T.Describe (Types, Ref);
         Name : constant String := T.Image (Shape.Identifier);
      begin
         Append ((if Item.Length = 0 then "" else " ") & "(type " & Name & " (");
         if Shape.Form = T.Bounded then
            Append ("range " & Decimal (T.Low_Of (Types, Ref)) & " " & Decimal (T.High_Of (Types, Ref)));
         elsif Shape.Form = T.Product then
            Append ("record");
            for Part in 1 .. Shape.Count loop
               Append (" (" & T.Image (Shape.Parts (Part).Identifier) & " " & Name_Of (Shape.Parts (Part).Payload) &
                       Default_Image (Ref, Part) & ")");
            end loop;
         elsif (for all P of Shape.Parts (1 .. Shape.Count) => Unit_Payload (P.Payload)) then
            Append ("enum");
            for P of Shape.Parts (1 .. Shape.Count) loop
               Append (" " & T.Image (P.Identifier));
            end loop;
         else
            Append ("variant");
            for P of Shape.Parts (1 .. Shape.Count) loop
               Append (" (" & T.Image (P.Identifier) &
                       (if Unit_Payload (P.Payload) then "" else " " & Name_Of (P.Payload)) & ")");
            end loop;
         end if;
         Append ("))");
      end Declare_Type;
   begin
      Item := (others => <>);
      if Root_Name'Length in 1 .. T.Maximum_Name_Length then
         Root := T.Find (Types, T.Named (Root_Name));
      end if;
      if not Declared (Root) then
         Item.Why := Unknown_Type;
         Item.Culprit_Type := (if Root_Name'Length in 1 .. T.Maximum_Name_Length
                               then T.Named (Root_Name) else T.Named (""));
         return;
      elsif T.Describe (Types, Root).Form /= T.Product then
         Refuse (Not_A_Record, Root);
         return;
      end if;
      Visit (Root);
      for I in 1 .. Ordered loop
         Declare_Type (Order (I));
      end loop;
      if Item.Why /= Accepted then
         Item.Length := 0;
      end if;
   end Render;
end CCL.Parameter_Signatures;

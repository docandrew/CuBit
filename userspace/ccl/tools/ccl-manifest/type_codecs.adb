------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Interfaces;
with CCL.Language;
with CCL.Types;

package body Type_Codecs is
   package T renames CCL.Types;
   use type T.Type_Reference;
   use type T.Shape;
   use type CCL.Language.Analysis_Status;

   LF : constant Character := ASCII.LF;
   --  The suffixes generated names get: a CCL type T is the Ada type
   --  T_Value (CCL field names are lower case, but Ada is not case
   --  sensitive, and a field gateway of type Gateway would clash), a
   --  variant's alternatives are T_Choice, an alternative A's payload is
   --  the component A_Payload.
   Type_Suffix : constant String := "_Value";
   Choice_Suffix : constant String := "_Choice";
   Payload_Suffix : constant String := "_Payload";
   Discriminant : constant String := "Choice";
   --  The decoders' parameters, as every generated reader spells them.
   Reader_Arguments : constant String := "Source, Position, OK";
   Reader_Profile : constant String := "(Source : String; Position : in out Positive; OK : in out Boolean; Item : out ";
   Reader_Contract : constant String :=
     "   with Always_Terminates," & LF &
     "     Pre => R.In_Source (Source, Position)," & LF &
     "     Post => R.In_Source (Source, Position) and then Position >= Position'Old";

   --  Ada's reserved words (RM 2.9), and the names the generated code
   --  itself declares or uses unqualified: neither may be a CCL name.
   Reserved : constant String :=
     " abort abs abstract accept access aliased all and array at begin body case constant declare " &
     "delay delta digits do else elsif end entry exception exit for function generic goto if in " &
     "interface is limited loop mod new not null of or others out overriding package parallel " &
     "pragma private procedure protected raise range record rem renames requeue return reverse " &
     "select separate some subtype synchronized tagged task terminate then type until use when " &
     "while with xor choice text r interfaces ccl decode boolean ";

   function Lower (Text : String) return String renames Ada.Characters.Handling.To_Lower;

   function Valid_Identifier (Name : String) return Boolean is
     (Name'Length > 0
      and then Ada.Characters.Handling.Is_Letter (Name (Name'First))
      and then Name (Name'Last) /= '_'
      and then (for all C of Name => Ada.Characters.Handling.Is_Alphanumeric (C) or else C = '_')
      and then (for all I in Name'First .. Name'Last - 1 => not (Name (I) = '_' and then Name (I + 1) = '_'))
      and then Ada.Strings.Fixed.Index (Reserved, " " & Lower (Name) & " ") = 0);

   function Decimal (Value : Interfaces.Integer_64) return String is
      Image : constant String := Interfaces.Integer_64'Image (Value);
   begin
      return (if Image (Image'First) = ' ' then Image (Image'First + 1 .. Image'Last) else Image);
   end Decimal;

   procedure Generate
     (Declarations, Root_Name : String;
      Item : out Generated; Success : out Boolean; Detail : out Unbounded_String)
   is
      --  The declarations and a trivial expression: the analyser checks programs.
      Program : constant String := Declarations & LF & "0";
      Checked : CCL.Language.Analysis_Result;
      Registry : T.Registry;
      Root : T.Type_Reference;
      type Category is (Integer_Value, Boolean_Value, String_Value, Range_Value, Enumeration, Product, Variant, Unsupported);
      Order : array (1 .. T.Maximum_Declarations) of T.Type_Reference := [others => T.Invalid_Type];
      Ordered : Natural := 0;

      procedure Fail (Text : String) is
      begin
         if Success then
            Success := False;
            Detail := To_Unbounded_String (Text);
         end if;
      end Fail;

      function Unit_Payload (Ref : T.Type_Reference) return Boolean is
        (Ref = T.Unit_Type or else Ref = T.Invalid_Type);

      function Category_Of (Ref : T.Type_Reference) return Category is
      begin
         if Ref = T.Integer_Type then
            return Integer_Value;
         elsif Ref = T.Boolean_Type then
            return Boolean_Value;
         elsif Ref = T.String_Type then
            return String_Value;
         elsif Ref in T.Declared_Type and then T.Known (Registry, Ref) then
            declare
               Shape : constant T.Description := T.Describe (Registry, Ref);
            begin
               case Shape.Form is
                  when T.Bounded => return Range_Value;
                  when T.Product => return Product;
                  when T.Sum =>
                     return (if (for all P of Shape.Parts (1 .. Shape.Count) => Unit_Payload (P.Payload))
                             then Enumeration else Variant);
                  when others => null;
               end case;
            end;
         end if;
         return Unsupported;
      end Category_Of;

      function Supported (Ref : T.Type_Reference) return Boolean is (Category_Of (Ref) /= Unsupported);

      function Name_Of (Ref : T.Type_Reference) return String is (T.Image (T.Describe (Registry, Ref).Identifier));

      function Ada_Type (Ref : T.Type_Reference) return String is
        (case Category_Of (Ref) is
            when Integer_Value => "Interfaces.Integer_64",
            when Boolean_Value => "Boolean",
            when String_Value => "Text",
            when others => Name_Of (Ref) & Type_Suffix);

      function Default_Of (Ref : T.Type_Reference) return String is
        (case Category_Of (Ref) is
            when Product | Variant | Unsupported => "(others => <>)",
            when Integer_Value => "0",
            when Boolean_Value => "False",
            when String_Value => Readers & ".Empty_Text",
            when Range_Value | Enumeration => Ada_Type (Ref) & "'First");

      --  The statement reading a value of type Ref into Target.
      function Read_Call (Ref : T.Type_Reference; Target : String) return String is
        (case Category_Of (Ref) is
            when Integer_Value =>
              "R.Read_Integer (" & Reader_Arguments &
              ", Interfaces.Integer_64'First, Interfaces.Integer_64'Last, " & Target & ");",
            when Boolean_Value => "R.Read_Boolean (" & Reader_Arguments & ", " & Target & ");",
            when String_Value => "R.Read_Text (" & Reader_Arguments & ", " & Target & ");",
            when others => "Read_" & Name_Of (Ref) & " (" & Reader_Arguments & ", " & Target & ");");

      --  Every declared type Ref reaches, before Ref (declaration order).
      procedure Visit (Ref : T.Type_Reference) is
      begin
         if not Success then
            return;
         elsif not Supported (Ref) then
            Fail ("a typed value may hold records, variants, enumerations, ranges, Integer, " &
                  "Boolean and String; not " &
                  (if Ref in T.Declared_Type and then T.Known (Registry, Ref) then Name_Of (Ref) else "this type"));
            return;
         end if;
         Item.Integer_Field := Item.Integer_Field or else Category_Of (Ref) = Integer_Value;
         Item.Integer_Read := Item.Integer_Read or else Category_Of (Ref) in Integer_Value | Range_Value;
         if Category_Of (Ref) in Integer_Value | Boolean_Value | String_Value
           or else (for some I in 1 .. Ordered => Order (I) = Ref)
         then
            return;
         end if;
         declare
            Shape : constant T.Description := T.Describe (Registry, Ref);
         begin
            if not Valid_Identifier (T.Image (Shape.Identifier)) then
               Fail ("type name " & T.Image (Shape.Identifier) & " is not usable as an Ada name");
            end if;
            if Shape.Form in T.Product | T.Sum then
               for P of Shape.Parts (1 .. Shape.Count) loop
                  if not Valid_Identifier (T.Image (P.Identifier)) then
                     Fail ("name " & T.Image (P.Identifier) & " in " & T.Image (Shape.Identifier) &
                           " is not usable as an Ada name");
                  elsif not Unit_Payload (P.Payload) then
                     Visit (P.Payload);
                  end if;
               end loop;
            end if;
         end;
         Ordered := Ordered + 1;
         Order (Ordered) := Ref;
      end Visit;

      procedure Line (Into : in out Unbounded_String; Text : String) is
      begin
         Append (Into, Text & LF);
      end Line;

      procedure Declare_Type (Ref : T.Type_Reference) is
         Shape : constant T.Description := T.Describe (Registry, Ref);
         Name : constant String := Name_Of (Ref);
         Self : constant String := Ada_Type (Ref);
         function Literals return String is
            Result : Unbounded_String;
         begin
            for I in 1 .. Shape.Count loop
               Append (Result, (if I > 1 then ", " else "") & T.Image (Shape.Parts (I).Identifier));
            end loop;
            return To_String (Result);
         end Literals;
      begin
         case Category_Of (Ref) is
            when Range_Value =>
               Line (Item.Spec, "   type " & Self & " is range " & Decimal (T.Low_Of (Registry, Ref)) & " .. " &
                     Decimal (T.High_Of (Registry, Ref)) & ";");
            when Enumeration =>
               Line (Item.Spec, "   type " & Self & " is (" & Literals & ");");
            when Product =>
               if Shape.Count = 0 then
                  Line (Item.Spec, "   type " & Self & " is null record;");
               else
                  Line (Item.Spec, "   type " & Self & " is record");
                  for P of Shape.Parts (1 .. Shape.Count) loop
                     Line (Item.Spec, "      " & T.Image (P.Identifier) & " : " & Ada_Type (P.Payload) & " := " &
                           Default_Of (P.Payload) & ";");
                  end loop;
                  Line (Item.Spec, "   end record;");
               end if;
            when Variant =>
               --  The alternative, and each alternative's payload (only the
               --  chosen one's is the value's; the others keep their
               --  defaults): a plain record, so readers and proofs need no
               --  mutable discriminants.
               Line (Item.Spec, "   type " & Name & Choice_Suffix & " is (" & Literals & ");");
               Line (Item.Spec, "   type " & Self & " is record");
               Line (Item.Spec, "      " & Discriminant & " : " & Name & Choice_Suffix & " := " & Name &
                     Choice_Suffix & "'First;");
               for P of Shape.Parts (1 .. Shape.Count) loop
                  if not Unit_Payload (P.Payload) then
                     Line (Item.Spec, "      " & T.Image (P.Identifier) & Payload_Suffix & " : " &
                           Ada_Type (P.Payload) & " := " & Default_Of (P.Payload) & ";");
                  end if;
               end loop;
               Line (Item.Spec, "   end record;");
            when Integer_Value | Boolean_Value | String_Value | Unsupported =>
               null;
         end case;
      end Declare_Type;

      procedure Define_Reader (Ref : T.Type_Reference) is
         Shape : constant T.Description := T.Describe (Registry, Ref);
         Name : constant String := Name_Of (Ref);
         Self : constant String := Ada_Type (Ref);
         Reader : constant String := "Read_" & Name;
         --  Type.Member, as the canonical source spells an alternative.
         function Member (P : T.Component) return String is ("""" & Name & "." & T.Image (P.Identifier) & """");
      begin
         Line (Item.Bodies, "   procedure " & Reader & LF & "     " & Reader_Profile & Self & ")");
         Line (Item.Bodies, Reader_Contract);
         Line (Item.Bodies, "   is");
         case Category_Of (Ref) is
            when Range_Value =>
               Line (Item.Bodies, "      Value : Interfaces.Integer_64;");
               Line (Item.Bodies, "   begin");
               Line (Item.Bodies, "      R.Read_Integer (" & Reader_Arguments & ", Interfaces.Integer_64 (" & Self &
                     "'First), Interfaces.Integer_64 (" & Self & "'Last), Value);");
               Line (Item.Bodies, "      Item := " & Self & " (Value);");
            when Enumeration =>
               Line (Item.Bodies, "   begin");
               Line (Item.Bodies, "      Item := " & Default_Of (Ref) & ";");
               Line (Item.Bodies, "      if not OK then");
               Line (Item.Bodies, "         return;");
               for P of Shape.Parts (1 .. Shape.Count) loop
                  Line (Item.Bodies, "      elsif R.Next_Is_Word (Source, Position, " & Member (P) & ") then");
                  Line (Item.Bodies, "         R.Expect_Word (" & Reader_Arguments & ", " & Member (P) & ");");
                  Line (Item.Bodies, "         Item := " & T.Image (P.Identifier) & ";");
               end loop;
               Line (Item.Bodies, "      else");
               Line (Item.Bodies, "         OK := False;");
               Line (Item.Bodies, "      end if;");
            when Product =>
               Line (Item.Bodies, "   begin");
               if Shape.Count = 0 then
                  Line (Item.Bodies, "      Item := " & Default_Of (Ref) & ";");
               end if;
               Line (Item.Bodies, "      R.Expect (" & Reader_Arguments & ", ""(" & Name & """);");
               for P of Shape.Parts (1 .. Shape.Count) loop
                  Line (Item.Bodies, "      R.Expect (" & Reader_Arguments & ", "" " & T.Image (P.Identifier) &
                        " => "");");
                  Line (Item.Bodies, "      " & Read_Call (P.Payload, "Item." & T.Image (P.Identifier)));
               end loop;
               Line (Item.Bodies, "      R.Expect (" & Reader_Arguments & ", "")"");");
            when Variant =>
               Line (Item.Bodies, "   begin");
               Line (Item.Bodies, "      Item := " & Default_Of (Ref) & ";");
               Line (Item.Bodies, "      if not OK then");
               Line (Item.Bodies, "         return;");
               for P of Shape.Parts (1 .. Shape.Count) loop
                  if Unit_Payload (P.Payload) then
                     Line (Item.Bodies, "      elsif R.Next_Is_Word (Source, Position, " & Member (P) & ") then");
                     Line (Item.Bodies, "         R.Expect_Word (" & Reader_Arguments & ", " & Member (P) & ");");
                  else
                     declare
                        Opening : constant String := """(" & Name & "." & T.Image (P.Identifier) & " """;
                     begin
                        Line (Item.Bodies, "      elsif R.Next_Is (Source, Position, " & Opening & ") then");
                        Line (Item.Bodies, "         R.Expect (" & Reader_Arguments & ", " & Opening & ");");
                        Line (Item.Bodies, "         " &
                              Read_Call (P.Payload, "Item." & T.Image (P.Identifier) & Payload_Suffix));
                        Line (Item.Bodies, "         R.Expect (" & Reader_Arguments & ", "")"");");
                     end;
                  end if;
                  Line (Item.Bodies, "         Item." & Discriminant & " := " & T.Image (P.Identifier) & ";");
               end loop;
               Line (Item.Bodies, "      else");
               Line (Item.Bodies, "         OK := False;");
               Line (Item.Bodies, "      end if;");
            when Integer_Value | Boolean_Value | String_Value | Unsupported =>
               null;
         end case;
         Line (Item.Bodies, "   end " & Reader & ";");
         Line (Item.Bodies, "");
      end Define_Reader;
   begin
      Item := (others => <>);
      Success := True;
      Detail := Null_Unbounded_String;
      CCL.Language.Analyze (Program, Checked);
      if CCL.Language.Analysis_Status_Of (Checked) /= CCL.Language.Analysis_Succeeded then
         Fail ("its CCL type declarations do not analyse");
         return;
      end if;
      Registry := CCL.Language.Analysis_Types (Checked);
      Root := T.Find (Registry, T.Named (Root_Name));
      if Root not in T.Declared_Type or else not T.Known (Registry, Root) then
         Fail ("its declarations do not declare " & Root_Name);
         return;
      end if;
      Visit (Root);
      if not Success then
         return;
      end if;
      for I in 1 .. Ordered loop
         Declare_Type (Order (I));
         Define_Reader (Order (I));
      end loop;
      Item.Root_Type := To_Unbounded_String (Ada_Type (Root));
      Item.Root_Default := To_Unbounded_String (Default_Of (Root));
      Item.Root_Reader := To_Unbounded_String ("Read_" & Root_Name);
   end Generate;
end Type_Codecs;

------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Characters.Handling;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Interfaces;
with CCL.Language;
with CCL.Setting_Kinds;
with CCL.Types;

package body Setting_Codecs is
   use Ada.Strings.Unbounded;
   package T renames CCL.Types;
   package Kinds renames CCL.Setting_Kinds;
   use type T.Type_Reference;
   use type T.Shape;
   use type CCL.Language.Analysis_Status;
   use all type Setting_Catalogs.Value_Kind;
   use all type Setting_Catalogs.Shape;

   LF : constant Character := ASCII.LF;
   Codec_Parent : constant String := "CCL.Setting_Codecs";
   Readers : constant String := "CCL.Canonical_Readers";
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

   procedure Emit_One
     (Setting : Setting_Catalogs.Setting; Kind : Kinds.Setting_Kind; Directory : String;
      Success : in out Boolean; Why : in out CuBit.Failures.Failure)
   is
      Root_Name : constant String := Kinds.Root_Name (Kind);
      Package_Name : constant String := Codec_Parent & "." & Root_Name;
      File_Base : constant String := Directory & "/" & Lower ("ccl-setting_codecs-" & Root_Name);
      --  The schema and a trivial expression: the analyser checks programs.
      Program : constant String := Kinds.Schema (Kind) & LF & "0";
      Checked : CCL.Language.Analysis_Result;
      Registry : T.Registry;
      Root : T.Type_Reference;
      type Category is (Integer_Value, Boolean_Value, String_Value, Range_Value, Enumeration, Product, Variant, Unsupported);
      Order : array (1 .. T.Maximum_Declarations) of T.Type_Reference := [others => T.Invalid_Type];
      Ordered : Natural := 0;
      --  Whether a field is an Integer (the spec needs Interfaces), and
      --  whether any value is read as one (the body does).
      Integer_Field, Integer_Read : Boolean := False;
      Spec, Bodies : Unbounded_String;

      procedure Fail (Detail : String) is
      begin
         if Success then
            Success := False;
            Why := CuBit.Failures.Failed
              (CuBit.Failures.Invalid_Argument,
               Setting_Catalogs.Image (Setting.File) & ": " & Setting_Catalogs.Image (Setting.Key) & ": " & Detail);
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
            Fail ("a typed setting's value may hold records, variants, enumerations, ranges, Integer, " &
                  "Boolean and String; not " &
                  (if Ref in T.Declared_Type and then T.Known (Registry, Ref) then Name_Of (Ref) else "this type"));
            return;
         end if;
         Integer_Field := Integer_Field or else Category_Of (Ref) = Integer_Value;
         Integer_Read := Integer_Read or else Category_Of (Ref) in Integer_Value | Range_Value;
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
               Line (Spec, "   type " & Self & " is range " & Decimal (T.Low_Of (Registry, Ref)) & " .. " &
                     Decimal (T.High_Of (Registry, Ref)) & ";");
            when Enumeration =>
               Line (Spec, "   type " & Self & " is (" & Literals & ");");
            when Product =>
               if Shape.Count = 0 then
                  Line (Spec, "   type " & Self & " is null record;");
               else
                  Line (Spec, "   type " & Self & " is record");
                  for P of Shape.Parts (1 .. Shape.Count) loop
                     Line (Spec, "      " & T.Image (P.Identifier) & " : " & Ada_Type (P.Payload) & " := " &
                           Default_Of (P.Payload) & ";");
                  end loop;
                  Line (Spec, "   end record;");
               end if;
            when Variant =>
               --  The alternative, and each alternative's payload (only the
               --  chosen one's is the value's; the others keep their
               --  defaults): a plain record, so readers and proofs need no
               --  mutable discriminants.
               Line (Spec, "   type " & Name & Choice_Suffix & " is (" & Literals & ");");
               Line (Spec, "   type " & Self & " is record");
               Line (Spec, "      " & Discriminant & " : " & Name & Choice_Suffix & " := " & Name & Choice_Suffix &
                     "'First;");
               for P of Shape.Parts (1 .. Shape.Count) loop
                  if not Unit_Payload (P.Payload) then
                     Line (Spec, "      " & T.Image (P.Identifier) & Payload_Suffix & " : " & Ada_Type (P.Payload) &
                           " := " & Default_Of (P.Payload) & ";");
                  end if;
               end loop;
               Line (Spec, "   end record;");
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
         Line (Bodies, "   procedure " & Reader & LF & "     " & Reader_Profile & Self & ")");
         Line (Bodies, Reader_Contract);
         Line (Bodies, "   is");
         case Category_Of (Ref) is
            when Range_Value =>
               Line (Bodies, "      Value : Interfaces.Integer_64;");
               Line (Bodies, "   begin");
               Line (Bodies, "      R.Read_Integer (" & Reader_Arguments & ", Interfaces.Integer_64 (" & Self &
                     "'First), Interfaces.Integer_64 (" & Self & "'Last), Value);");
               Line (Bodies, "      Item := " & Self & " (Value);");
            when Enumeration =>
               Line (Bodies, "   begin");
               Line (Bodies, "      Item := " & Default_Of (Ref) & ";");
               Line (Bodies, "      if not OK then");
               Line (Bodies, "         return;");
               for P of Shape.Parts (1 .. Shape.Count) loop
                  Line (Bodies, "      elsif R.Next_Is_Word (Source, Position, " & Member (P) & ") then");
                  Line (Bodies, "         R.Expect_Word (" & Reader_Arguments & ", " & Member (P) & ");");
                  Line (Bodies, "         Item := " & T.Image (P.Identifier) & ";");
               end loop;
               Line (Bodies, "      else");
               Line (Bodies, "         OK := False;");
               Line (Bodies, "      end if;");
            when Product =>
               Line (Bodies, "   begin");
               if Shape.Count = 0 then
                  Line (Bodies, "      Item := " & Default_Of (Ref) & ";");
               end if;
               Line (Bodies, "      R.Expect (" & Reader_Arguments & ", ""(" & Name & """);");
               for P of Shape.Parts (1 .. Shape.Count) loop
                  Line (Bodies, "      R.Expect (" & Reader_Arguments & ", "" " & T.Image (P.Identifier) &
                        " => "");");
                  Line (Bodies, "      " & Read_Call (P.Payload, "Item." & T.Image (P.Identifier)));
               end loop;
               Line (Bodies, "      R.Expect (" & Reader_Arguments & ", "")"");");
            when Variant =>
               Line (Bodies, "   begin");
               Line (Bodies, "      Item := " & Default_Of (Ref) & ";");
               Line (Bodies, "      if not OK then");
               Line (Bodies, "         return;");
               for P of Shape.Parts (1 .. Shape.Count) loop
                  if Unit_Payload (P.Payload) then
                     Line (Bodies, "      elsif R.Next_Is_Word (Source, Position, " & Member (P) & ") then");
                     Line (Bodies, "         R.Expect_Word (" & Reader_Arguments & ", " & Member (P) & ");");
                  else
                     declare
                        Opening : constant String := """(" & Name & "." & T.Image (P.Identifier) & " """;
                     begin
                        Line (Bodies, "      elsif R.Next_Is (Source, Position, " & Opening & ") then");
                        Line (Bodies, "         R.Expect (" & Reader_Arguments & ", " & Opening & ");");
                        Line (Bodies, "         " & Read_Call (P.Payload, "Item." & T.Image (P.Identifier) & Payload_Suffix));
                        Line (Bodies, "         R.Expect (" & Reader_Arguments & ", "")"");");
                     end;
                  end if;
                  Line (Bodies, "         Item." & Discriminant & " := " & T.Image (P.Identifier) & ";");
               end loop;
               Line (Bodies, "      else");
               Line (Bodies, "         OK := False;");
               Line (Bodies, "      end if;");
            when Integer_Value | Boolean_Value | String_Value | Unsupported =>
               null;
         end case;
         Line (Bodies, "   end " & Reader & ";");
         Line (Bodies, "");
      end Define_Reader;

      procedure Write (Path, Text : String) is
         Output : Ada.Text_IO.File_Type;
      begin
         Ada.Text_IO.Create (Output, Ada.Text_IO.Out_File, Path);
         Ada.Text_IO.Put (Output, Text);
         Ada.Text_IO.Close (Output);
      end Write;

      Header : constant String :=
        "--  Generated from userspace/ccl/interfaces/settings/ by" & LF &
        "--  ccl-manifest --setting-codecs (CAP-003); do not edit. Regenerate with" & LF &
        "--  make -C kernel setting-kinds after changing the catalog or a type." & LF &
        "pragma Ada_2022;" & LF;
   begin
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
      Write
        (File_Base & ".ads",
         Header &
         (if Integer_Field then "with Interfaces;" & LF else "") &
         "with " & Readers & ";" & LF & LF &
         "--  The typed setting " & Setting_Catalogs.Image (Setting.Key) &
         (if Setting.Form = Family then "<member>" else "") &
         ": a CCL " & Root_Name & " (" & Setting_Catalogs.Image (Setting.Source) & ")," & LF &
         "--  as Ada types, and Decode, which reads its canonical source" & LF &
         "--  (CCL.Typed_Settings renders it; CCL.Canonical_Readers)." & LF &
         "package " & Package_Name & " with SPARK_Mode, Pure is" & LF &
         "   subtype Text is " & Readers & ".Text;" & LF &
         To_String (Spec) & LF &
         "   --  Item is the value when Success: Source is exactly the canonical" & LF &
         "   --  source of a " & Root_Name & ". Otherwise Item is " & Default_Of (Root) & "." & LF &
         "   procedure Decode (Source : String; Item : out " & Ada_Type (Root) & "; Success : out Boolean)" & LF &
         "   with Global => null, Always_Terminates;" & LF &
         "end " & Package_Name & ";" & LF);
      Write
        (File_Base & ".adb",
         Header &
         (if Integer_Read and then not Integer_Field then "with Interfaces;" & LF else "") & LF &
         "package body " & Package_Name & " with SPARK_Mode is" & LF &
         "   package R renames " & Readers & ";" & LF & LF &
         To_String (Bodies) &
         "   procedure Decode (Source : String; Item : out " & Ada_Type (Root) & "; Success : out Boolean) is" & LF &
         "   begin" & LF &
         "      Item := " & Default_Of (Root) & ";" & LF &
         "      Success := R.Readable (Source) and then Source'Length in 1 .. R.Maximum_Canonical;" & LF &
         "      if not Success then" & LF &
         "         return;" & LF &
         "      end if;" & LF &
         "      declare" & LF &
         "         Position : Positive := Source'First;" & LF &
         "      begin" & LF &
         "         Read_" & Root_Name & " (Source, Position, Success, Item);" & LF &
         "         R.Finish (Source, Position, Success);" & LF &
         "      end;" & LF &
         "      if not Success then" & LF &
         "         Item := " & Default_Of (Root) & ";" & LF &
         "      end if;" & LF &
         "   end Decode;" & LF &
         "end " & Package_Name & ";" & LF);
   end Emit_One;

   procedure Emit
     (Settings : Setting_Catalogs.Catalog; Directory : String;
      Success : out Boolean; Why : out CuBit.Failures.Failure)
   is
      Typed_Count : Natural := 0;
      Listing, Check_Withs, Check_Cases : Unbounded_String;
   begin
      Success := True;
      Why := CuBit.Failures.Failed (CuBit.Failures.Invalid_Argument, "");
      for I in 1 .. Settings.Count loop
         if Settings.Settings (I).Value = Typed then
            Typed_Count := Typed_Count + 1;
            declare
               Root : constant String := Setting_Catalogs.Image (Settings.Settings (I).Root);
               Kind : Kinds.Setting_Kind := Kinds.Untyped;
            begin
               for K in Kinds.Setting_Kind range Kinds.Setting_Kind'Succ (Kinds.Untyped) .. Kinds.Setting_Kind'Last loop
                  if Kinds.Root_Name (K) = Root then
                     Kind := K;
                  end if;
               end loop;
               if Kinds."=" (Kind, Kinds.Untyped) then
                  Success := False;
                  Why := CuBit.Failures.Failed
                    (CuBit.Failures.Invalid_Argument,
                     "CCL.Setting_Kinds, as compiled into ccl-manifest, has no " & Root & " for " &
                     Setting_Catalogs.Image (Settings.Settings (I).Key),
                     "regenerate it (ccl-manifest --setting-kinds) and rebuild ccl-manifest first");
                  return;
               end if;
               Emit_One (Settings.Settings (I), Kind, Directory, Success, Why);
               if not Success then
                  return;
               end if;
               Append (Listing, "--    " & Codec_Parent & "." & Root & ": " &
                       Setting_Catalogs.Image (Settings.Settings (I).Key) & LF);
               Append (Check_Withs, "with " & Codec_Parent & "." & Root & ";" & LF);
               Append (Check_Cases,
                       "         when Kinds." & Root & "_Setting =>" & LF &
                       "            declare" & LF &
                       "               Item : " & Root & "." & Root & Type_Suffix & ";" & LF &
                       "            begin" & LF &
                       "               " & Root & ".Decode (Source, Item, OK);" & LF &
                       "            end;" & LF);
            end;
         end if;
      end loop;
      if Typed_Count /= Kinds.Setting_Kind'Pos (Kinds.Setting_Kind'Last) then
         Success := False;
         Why := CuBit.Failures.Failed
           (CuBit.Failures.Invalid_Argument,
            "CCL.Setting_Kinds, as compiled into ccl-manifest, has settings the catalog does not",
            "regenerate it (ccl-manifest --setting-kinds) and rebuild ccl-manifest first");
         return;
      end if;
      declare
         Output : Ada.Text_IO.File_Type;
      begin
         Ada.Text_IO.Create (Output, Ada.Text_IO.Out_File, Directory & "/ccl-setting_codecs.ads");
         Ada.Text_IO.Put
           (Output,
            "--  Generated from userspace/ccl/interfaces/settings/ by" & LF &
            "--  ccl-manifest --setting-codecs (CAP-003); do not edit." & LF & LF &
            "--  One decoder per CCL-typed setting, generated from its type at build" & LF &
            "--  time (tools/ccl-manifest/setting_codecs.ads):" & LF &
            To_String (Listing) &
            "package " & Codec_Parent & " with SPARK_Mode, Pure is" & LF &
            "end " & Codec_Parent & ";" & LF);
         Ada.Text_IO.Close (Output);
         Ada.Text_IO.Create (Output, Ada.Text_IO.Out_File, Directory & "/ccl-setting_codecs-checks.ads");
         Ada.Text_IO.Put
           (Output,
            "--  Generated from userspace/ccl/interfaces/settings/ by" & LF &
            "--  ccl-manifest --setting-codecs (CAP-003); do not edit." & LF &
            "pragma Ada_2022;" & LF &
            "with CCL.Setting_Kinds;" & LF & LF &
            "--  Config's check of a typed setting's stored value: the generated" & LF &
            "--  decoder of the key's type accepts it." & LF &
            "package " & Codec_Parent & ".Checks with SPARK_Mode, Pure is" & LF &
            "   --  Whether Source is the canonical source of a value of Kind's type." & LF &
            "   function Canonical (Kind : CCL.Setting_Kinds.Setting_Kind; Source : String) return Boolean" & LF &
            "   with Pre => CCL.Setting_Kinds.""/="" (Kind, CCL.Setting_Kinds.Untyped);" & LF &
            "end " & Codec_Parent & ".Checks;" & LF);
         Ada.Text_IO.Close (Output);
         Ada.Text_IO.Create (Output, Ada.Text_IO.Out_File, Directory & "/ccl-setting_codecs-checks.adb");
         Ada.Text_IO.Put
           (Output,
            "--  Generated from userspace/ccl/interfaces/settings/ by" & LF &
            "--  ccl-manifest --setting-codecs (CAP-003); do not edit." & LF &
            "pragma Ada_2022;" & LF &
            To_String (Check_Withs) & LF &
            "package body " & Codec_Parent & ".Checks with SPARK_Mode is" & LF &
            "   package Kinds renames CCL.Setting_Kinds;" & LF &
            "   --  Only the verdict matters here, not the decoded value." & LF &
            "   pragma Warnings (GNATprove, Off, """"""Item"""" is set by """"Decode"""" but not used after the call"");" & LF & LF &
            "   function Canonical (Kind : CCL.Setting_Kinds.Setting_Kind; Source : String) return Boolean is" & LF &
            "      OK : Boolean := False;" & LF &
            "   begin" & LF &
            "      case Kind is" & LF &
            To_String (Check_Cases) &
            "         when Kinds.Untyped =>" & LF &
            "            null;" & LF &
            "      end case;" & LF &
            "      return OK;" & LF &
            "   end Canonical;" & LF &
            "end " & Codec_Parent & ".Checks;" & LF);
         Ada.Text_IO.Close (Output);
      end;
   end Emit;
end Setting_Codecs;

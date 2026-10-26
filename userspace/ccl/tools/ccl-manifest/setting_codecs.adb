------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Characters.Handling;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with CCL.Setting_Kinds;
with Type_Codecs;

package body Setting_Codecs is
   use Ada.Strings.Unbounded;
   package Kinds renames CCL.Setting_Kinds;
   use all type Setting_Catalogs.Value_Kind;
   use all type Setting_Catalogs.Shape;

   LF : constant Character := ASCII.LF;
   Codec_Parent : constant String := "CCL.Setting_Codecs";
   Readers : constant String := Type_Codecs.Readers;
   Type_Suffix : constant String := "_Value";
   function Lower (Text : String) return String renames Ada.Characters.Handling.To_Lower;

   procedure Emit_One
     (Setting : Setting_Catalogs.Setting; Kind : Kinds.Setting_Kind; Directory : String;
      Success : in out Boolean; Why : in out CuBit.Failures.Failure)
   is
      Root_Name : constant String := Kinds.Root_Name (Kind);
      Package_Name : constant String := Codec_Parent & "." & Root_Name;
      File_Base : constant String := Directory & "/" & Lower ("ccl-setting_codecs-" & Root_Name);
      Item : Type_Codecs.Generated;
      Generated : Boolean;
      Detail : Unbounded_String;

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
      Type_Codecs.Generate (Kinds.Schema (Kind), Root_Name, Item, Generated, Detail);
      if not Generated then
         Success := False;
         Why := CuBit.Failures.Failed
           (CuBit.Failures.Invalid_Argument,
            Setting_Catalogs.Image (Setting.File) & ": " & Setting_Catalogs.Image (Setting.Key) & ": " &
            To_String (Detail));
         return;
      end if;
      Write
        (File_Base & ".ads",
         Header &
         (if Item.Integer_Field then "with Interfaces;" & LF else "") &
         "with " & Readers & ";" & LF & LF &
         "--  The typed setting " & Setting_Catalogs.Image (Setting.Key) &
         (if Setting.Form = Family then "<member>" else "") &
         ": a CCL " & Root_Name & " (" & Setting_Catalogs.Image (Setting.Source) & ")," & LF &
         "--  as Ada types, and Decode, which reads its canonical source" & LF &
         "--  (CCL.Typed_Settings renders it; " & Readers & ")." & LF &
         "package " & Package_Name & " with SPARK_Mode, Pure is" & LF &
         "   subtype Text is " & Readers & ".Text;" & LF &
         To_String (Item.Spec) & LF &
         "   --  Item is the value when Success: Source is exactly the canonical" & LF &
         "   --  source of a " & Root_Name & ". Otherwise Item is " & To_String (Item.Root_Default) & "." & LF &
         "   procedure Decode (Source : String; Item : out " & To_String (Item.Root_Type) &
         "; Success : out Boolean)" & LF &
         "   with Global => null, Always_Terminates;" & LF &
         "end " & Package_Name & ";" & LF);
      Write
        (File_Base & ".adb",
         Header &
         (if Item.Integer_Read and then not Item.Integer_Field then "with Interfaces;" & LF else "") & LF &
         "package body " & Package_Name & " with SPARK_Mode is" & LF &
         "   package R renames " & Readers & ";" & LF & LF &
         To_String (Item.Bodies) &
         "   procedure Decode (Source : String; Item : out " & To_String (Item.Root_Type) &
         "; Success : out Boolean) is" & LF &
         "   begin" & LF &
         "      Item := " & To_String (Item.Root_Default) & ";" & LF &
         "      Success := R.Readable (Source) and then Source'Length in 1 .. R.Maximum_Canonical;" & LF &
         "      if not Success then" & LF &
         "         return;" & LF &
         "      end if;" & LF &
         "      declare" & LF &
         "         Position : Positive := Source'First;" & LF &
         "      begin" & LF &
         "         " & To_String (Item.Root_Reader) & " (Source, Position, Success, Item);" & LF &
         "         R.Finish (Source, Position, Success);" & LF &
         "      end;" & LF &
         "      if not Success then" & LF &
         "         Item := " & To_String (Item.Root_Default) & ";" & LF &
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

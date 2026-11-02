------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Characters.Handling;
with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Text_IO;
with Interfaces;
with CCL.Catalog;
with CCL.Diagnostics;
with CCL.Evaluation;
with CCL.Host_Values;
with CCL.Interfaces.Wallpapers;
with CCL.Language;
with CCL.Objects;
with CCL.Objects.Views;
with CCL.Typed_Values;
with CCL.Types;
with Type_Codecs;

package body Asset_Catalogs is
   package Views renames CCL.Objects.Views;
   package W renames CCL.Interfaces.Wallpapers;
   use type CCL.Language.Interpretation_Status;
   use type CCL.Language.Analysis_Status;

   LF : constant Character := ASCII.LF;
   Tab : constant Character := ASCII.HT;
   Codec_Parent : constant String := "CCL.Asset_Codecs";

   --  Reading a source list is pure: no host operation is visible.
   type No_Host is null record;
   procedure Refuse
     (Context : in out No_Host; Binding : Interfaces.Unsigned_32; Argument : CCL.Host_Values.Value;
      Reply : out CCL.Host_Values.Call_Result);
   procedure Refuse
     (Context : in out No_Host; Binding : Interfaces.Unsigned_32; Argument : CCL.Host_Values.Value;
      Reply : out CCL.Host_Values.Call_Result)
   is
      pragma Unreferenced (Context, Binding, Argument);
   begin
      Reply := (Value => CCL.Host_Values.Integer_Constant (0), Success => False, Why => <>);
   end Refuse;
   procedure Evaluate is new CCL.Evaluation.Evaluate_Object_With_Values (No_Host, Refuse);

   SOURCES_FUEL : constant := 100_000;
   --  The source list never leaves this process: an in-process key.
   LOCAL_KEY : constant CCL.Objects.Schema_Key :=
     [16#5741_4C4C_5041_5045#, 16#522D_534F_5552_4345#, 0, 1];

   --  A wallpaper name (CCL.Interfaces.Wallpapers): a-z, 0-9 and single
   --  '-', starting with a letter and not ending with '-'.
   function Valid_Name (Name : String) return Boolean is
     (Name'Length in 1 .. W.MAXIMUM_NAME
      and then Name (Name'First) in 'a' .. 'z'
      and then Name (Name'Last) /= '-'
      and then (for all C of Name => C in 'a' .. 'z' | '0' .. '9' | '-')
      and then (for all I in Name'First .. Name'Last - 1 =>
                  not (Name (I) = '-' and then Name (I + 1) = '-')));

   --  Printable ASCII on one line.
   function Printable (Text : String) return Boolean is
     (for all C of Text => C in ' ' .. '~');

   --  A source path beneath the source directory: relative, no "..".
   function Valid_Source (Path : String) return Boolean is
     (Path'Length > 0 and then Path (Path'First) /= '/'
      and then (for all C of Path => C in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '_' | '.' | '/')
      and then (for all I in Path'First .. Path'Last - 1 =>
                  not (Path (I) = '.' and then Path (I + 1) = '.')));

   --  A CCL string literal for Text (Printable, so only '"' and '\').
   function Quoted (Text : String) return String is
      Result : Unbounded_String := To_Unbounded_String ("""");
   begin
      for C of Text loop
         if C in '"' | '\' then
            Append (Result, '\');
         end if;
         Append (Result, C);
      end loop;
      Append (Result, '"');
      return To_String (Result);
   end Quoted;

   function Decimal (Value : Long_Long_Integer) return String is
      Image : constant String := Long_Long_Integer'Image (Value);
   begin
      return Image (Image'First + 1 .. Image'Last);
   end Decimal;

   procedure Build_Wallpapers
     (Source, Source_Path, Source_Directory, Catalog_Path : String;
      Plan : out Unbounded_String; Success : out Boolean; Why : out CuBit.Failures.Failure)
   is
      Schema : constant String := W.SOURCES_TYPE_SOURCE;
      Program : constant String := Schema & LF & Source;
      Checked : CCL.Language.Analysis_Result;
      Sources_Type : CCL.Types.Type_Reference;
      Catalog_View : CCL.Catalog.Interface_Catalog;
      Grants : CCL.Catalog.Granted_Bindings;
      Contract : CCL.Objects.Binding;
      Published, Captured : Boolean;
      Host : No_Host;
      Value : CCL.Language.Object_Interpretation_Result;
      Object : Views.Snapshot;
      Catalog : Unbounded_String;
      Names : Unbounded_String := To_Unbounded_String (" ");

      procedure Fail (Detail : String; Remedy : String := "") is
      begin
         if Success then
            Success := False;
            Why := CuBit.Failures.Failed
              (CuBit.Failures.Invalid_Argument, Ada.Directories.Simple_Name (Source_Path) & ": " & Detail, Remedy);
         end if;
      end Fail;

      function Text (At_Cursor : Views.Cursor) return String is (Views.Text (Object, At_Cursor));
      --  A record's field by name: the schema decides the order.
      function Named (At_Cursor : Views.Cursor; Name : String) return Views.Cursor is
         Shape_Of : constant CCL.Types.Description := Views.Describe (Object, At_Cursor);
      begin
         for Part in 1 .. Shape_Of.Count loop
            if CCL.Types.Image (Shape_Of.Parts (Part).Identifier) = Name then
               return Views.Field (Object, At_Cursor, Part);
            end if;
         end loop;
         return Views.No_Value;
      end Named;
      function Extent (At_Cursor : Views.Cursor) return Long_Long_Integer is
        (Long_Long_Integer (CCL.Objects.Integer_Of (Views.Scalar (Object, At_Cursor))));

      procedure Add_Wallpaper (At_Cursor : Views.Cursor; Number : Positive) is
         Name : constant String := Text (Named (At_Cursor, "name"));
         Label : constant String := Text (Named (At_Cursor, "label"));
         Picture : constant String := Text (Named (At_Cursor, "source"));
         Attribution : constant String := Text (Named (At_Cursor, "attribution"));
         Width : constant Long_Long_Integer := Extent (Named (At_Cursor, "width"));
         Height : constant Long_Long_Integer := Extent (Named (At_Cursor, "height"));
         Where : constant String := "images entry" & Positive'Image (Number);
         Entry_Source : constant String :=
           "(" & W.ROOT_TYPE_NAME & " name => " & Quoted (Name) & " label => " & Quoted (Label) &
           " file => " & Quoted (Name & W.FILE_EXTENSION) & " attribution => " & Quoted (Attribution) & ")";
         Rendered : CCL.Typed_Values.Check_Result;
      begin
         if not Valid_Name (Name) then
            Fail (Where & ": name """ & Name & """ is not 1 to" & Natural'Image (W.MAXIMUM_NAME) &
                  " of a-z, 0-9 and single '-', starting with a letter");
            return;
         elsif Index (Names, " " & Name & " ") > 0 then
            Fail (Where & ": the name """ & Name & """ is declared twice");
            return;
         elsif Label'Length not in 1 .. W.MAXIMUM_LABEL or else not Printable (Label) then
            Fail (Where & ": label """ & Label & """ is not 1 to" & Natural'Image (W.MAXIMUM_LABEL) &
                  " printable characters");
            return;
         elsif Attribution'Length = 0 or else not Printable (Attribution) then
            Fail (Where & ": """ & Name & """ needs a one-line attribution (where it came from, its terms)");
            return;
         elsif not Valid_Source (Picture) then
            Fail (Where & ": source """ & Picture & """ is not a path beneath the source list");
            return;
         elsif not Ada.Directories.Exists (Source_Directory & "/" & Picture) then
            Fail (Where & ": source " & Picture & " does not exist");
            return;
         end if;
         --  The published line is the type system's rendering of the entry.
         CCL.Typed_Values.Check (W.TYPE_SOURCE, W.ROOT_TYPE_NAME, Entry_Source, Rendered);
         if not Rendered.Success then
            Fail (Where & ": """ & Name & """ is not a catalog Wallpaper: " &
                  Rendered.Message (1 .. Rendered.Message_Length));
            return;
         end if;
         Append (Names, Name & " ");
         Append (Catalog, Rendered.Canonical (1 .. Rendered.Length) & LF);
         Append (Plan, Name & Tab & Picture & Tab & Decimal (Width) & Tab & Decimal (Height) & LF);
      end Add_Wallpaper;
   begin
      Success := True;
      Why := (others => <>);
      Plan := Null_Unbounded_String;
      if Program'Length > CCL.Language.MAX_SOURCE_LENGTH then
         Fail ("the source list and its schema are longer than" &
               Natural'Image (CCL.Language.MAX_SOURCE_LENGTH) & " characters");
         return;
      end if;
      CCL.Catalog.Initialize (Catalog_View);
      CCL.Catalog.Initialize (Grants);
      CCL.Language.Analyze (Program, Catalog_View, Checked);
      if CCL.Language.Analysis_Status_Of (Checked) /= CCL.Language.Analysis_Succeeded then
         Fail ("not a " & W.SOURCES_ROOT_TYPE_NAME & ": character" &
               Natural'Image (Natural'Max (0, CCL.Language.Analysis_Diagnostic_Position (Checked) - Schema'Length - 1)) &
               ": " & CCL.Diagnostics.Message (CCL.Language.Analysis_Diagnostic (Checked)));
         return;
      end if;
      Sources_Type := CCL.Types.Find (CCL.Language.Analysis_Types (Checked), CCL.Types.Named (W.SOURCES_ROOT_TYPE_NAME));
      CCL.Objects.Bind (CCL.Language.Analysis_Types (Checked), Sources_Type, LOCAL_KEY, Contract, Published);
      if not Published then
         Fail ("the schema does not declare a persistable " & W.SOURCES_ROOT_TYPE_NAME);
         return;
      end if;
      Evaluate (Program, SOURCES_FUEL, Catalog_View, Grants, Host, Contract, Value);
      if Value.Status /= CCL.Language.Succeeded or else not Value.Has_Value then
         Fail ("does not evaluate to a " & W.SOURCES_ROOT_TYPE_NAME & ": " & CCL.Diagnostics.Message (Value.Status));
         return;
      end if;
      Views.Capture (Object, Contract, Value.Value, Captured);
      if not Captured then
         Fail ("its value cannot be read");
         return;
      end if;
      declare
         Images : constant Views.Cursor := Named (Views.Root (Object), "images");
      begin
         if Views.Length (Object, Images) = 0 then
            Fail ("lists no wallpaper", "a package needs at least its default");
            return;
         end if;
         for Item in 1 .. Views.Length (Object, Images) loop
            exit when not Success;
            Add_Wallpaper (Views.Element (Object, Images, Item), Item);
         end loop;
      end;
      if not Success then
         return;
      end if;
      declare
         --  Exactly these bytes: Text_IO would add a line terminator of its own.
         Output : Ada.Streams.Stream_IO.File_Type;
         Temporary : constant String := Catalog_Path & ".tmp";
      begin
         Ada.Streams.Stream_IO.Create (Output, Ada.Streams.Stream_IO.Out_File, Temporary);
         String'Write (Ada.Streams.Stream_IO.Stream (Output), To_String (Catalog));
         Ada.Streams.Stream_IO.Close (Output);
         if Ada.Directories.Exists (Catalog_Path) then
            Ada.Directories.Delete_File (Catalog_Path);
         end if;
         Ada.Directories.Rename (Temporary, Catalog_Path);
      end;
   end Build_Wallpapers;

   procedure Emit_Codecs
     (Directory : String; Success : out Boolean; Why : out CuBit.Failures.Failure)
   is
      Item : Type_Codecs.Generated;
      Detail : Unbounded_String;
      Root_Name : constant String := W.ROOT_TYPE_NAME;
      Package_Name : constant String := Codec_Parent & "." & Root_Name;
      Header : constant String :=
        "--  Generated from CCL.Interfaces.Wallpapers.TYPE_SOURCE by" & LF &
        "--  ccl-manifest --asset-codecs; do not edit. Regenerate with" & LF &
        "--  make -C kernel asset-codecs after changing the type." & LF &
        "pragma Ada_2022;" & LF;
      Output : Ada.Text_IO.File_Type;
   begin
      Why := (others => <>);
      Type_Codecs.Generate (W.TYPE_SOURCE, Root_Name, Item, Success, Detail);
      if not Success then
         Why := CuBit.Failures.Failed
           (CuBit.Failures.Invalid_Argument, "CCL.Interfaces.Wallpapers: " & To_String (Detail));
         return;
      end if;
      Type_Codecs.Write_Codec
        (Item, Package_Name,
         Directory & "/" & Ada.Characters.Handling.To_Lower ("ccl-asset_codecs-" & Root_Name),
         Root_Name, Header,
         "--  One line of a wallpaper package's catalog: a CCL " & Root_Name & LF &
         "--  (CCL.Interfaces.Wallpapers.TYPE_SOURCE), as Ada types, and Decode," & LF &
         "--  which reads its canonical source (CCL.Typed_Values renders it;" & LF &
         "--  " & Type_Codecs.Readers & ")." & LF);
      Ada.Text_IO.Create (Output, Ada.Text_IO.Out_File, Directory & "/ccl-asset_codecs.ads");
      Ada.Text_IO.Put
        (Output,
         "--  Generated by ccl-manifest --asset-codecs; do not edit." & LF & LF &
         "--  One decoder per asset catalog's entry type, generated from it at build" & LF &
         "--  time (tools/ccl-manifest/asset_catalogs.ads):" & LF &
         "--    " & Package_Name & ": Assets/cubit-wallpapers/<version>/catalog.ccl" & LF &
         "package " & Codec_Parent & " with SPARK_Mode, Pure is" & LF &
         "end " & Codec_Parent & ";" & LF);
      Ada.Text_IO.Close (Output);
   end Emit_Codecs;
end Asset_Catalogs;

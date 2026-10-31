------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces;
with CCL.Catalog;
with CCL.Diagnostics;
with CCL.Evaluation;
with CCL.Host_Values;
with CCL.Language;
with CCL.Objects;
with CCL.Objects.Views;
with CCL.Types;
with CuBit.Config_Inspection;

package body Setting_Catalogs is
   package Views renames CCL.Objects.Views;
   use type CCL.Language.Interpretation_Status;
   use type CCL.Language.Analysis_Status;

   --  Reading a catalog is pure: no host operation is visible.
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

   ROOT_TYPE_NAME : constant String := "Setting_Catalog";
   CATALOG_FUEL : constant := 100_000;
   Maximum_Key : constant := 64;
   --  The catalog's value never leaves this process: an in-process key.
   LOCAL_KEY : constant CCL.Objects.Schema_Key :=
     [16#5345_5454_494E_4753#, 16#2D4C_4F43_414C_0001#, 0, 0];

   function To_Text (Value : String) return Bounded_Text is
      Result : Bounded_Text;
   begin
      Result.Length := Natural'Min (Value'Length, Maximum_Text);
      Result.Data (1 .. Result.Length) := Value (Value'First .. Value'First + Result.Length - 1);
      return Result;
   end To_Text;

   subtype Separator is Character with Static_Predicate => Separator in '.' | '-' | '_';
   function Valid_Key (Key : String) return Boolean is
     (Key'Length in 1 .. Maximum_Key
      and then Key (Key'First) in 'a' .. 'z'
      and then (for all C of Key => C in 'a' .. 'z' | '0' .. '9' | Separator)
      --  One separator at a time, so each key names one Ada package.
      and then (for all I in Key'First .. Key'Last - 1 =>
                  not (Key (I) in Separator and then Key (I + 1) in Separator)));

   function Covers (Item : Catalog; Scope : String; Index : Positive) return Boolean is
      Key : constant String := Image (Item.Settings (Index).Key);
   begin
      return CuBit.Config_Inspection.Contains (Scope, Key)
        or else (Item.Settings (Index).Form = Family
                 and then CuBit.Config_Inspection.Contains (Scope => Key, Key => Scope));
   end Covers;

   function Find (Item : Catalog; Key : String) return Setting_Count is
   begin
      for I in 1 .. Item.Count loop
         declare
            Name : constant String := Image (Item.Settings (I).Key);
         begin
            if Name = Key
              or else (Item.Settings (I).Form = Family
                       and then Key'Length > Name'Length
                       and then Key (Key'First .. Key'First + Name'Length - 1) = Name)
            then
               return I;
            end if;
         end;
      end loop;
      return 0;
   end Find;

   procedure Add
     (Item : in out Catalog; File, Source, Schema : String;
      Success : out Boolean; Why : out CuBit.Failures.Failure)
   is
      Program : constant String := Schema & ASCII.LF & Source;
      Checked : CCL.Language.Analysis_Result;
      Catalog_Type : CCL.Types.Type_Reference;
      Catalog_View : CCL.Catalog.Interface_Catalog;
      Grants : CCL.Catalog.Granted_Bindings;
      Contract : CCL.Objects.Binding;
      Published, Captured : Boolean;
      Host : No_Host;
      Value : CCL.Language.Object_Interpretation_Result;
      Object : Views.Snapshot;

      procedure Fail (Detail : String; Remedy : String := "") is
      begin
         if Success then
            Success := False;
            Why := CuBit.Failures.Failed (CuBit.Failures.Invalid_Argument, File & ": " & Detail, Remedy);
         end if;
      end Fail;

      function Text (At_Cursor : Views.Cursor) return String is (Views.Text (Object, At_Cursor));
      function Alt (At_Cursor : Views.Cursor) return String is
        (if Views.Alternative (Object, At_Cursor) in 1 .. Views.Describe (Object, At_Cursor).Count
         then CCL.Types.Image
                (Views.Describe (Object, At_Cursor).Parts (Views.Alternative (Object, At_Cursor)).Identifier)
         else "");
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

      procedure Add_Setting (At_Cursor : Views.Cursor; Index : Positive) is
         New_Setting : Setting;
         Key : constant String := Text (Named (At_Cursor, "key"));
         Value_Cursor : constant Views.Cursor := Named (At_Cursor, "value");
         Value_Name : constant String := Alt (Value_Cursor);
         Where : constant String := "settings entry" & Positive'Image (Index);
      begin
         if not Valid_Key (Key) then
            Fail (Where & ": key """ & Key & """ is not 1 to 64 of a-z, 0-9 and single '.', '-' or '_', starting with a letter");
            return;
         end if;
         New_Setting.Key := To_Text (Key);
         New_Setting.Form := (if Alt (Named (At_Cursor, "shape")) = "Family" then Family else Single);
         if (New_Setting.Form = Family) /= (Key (Key'Last) = '.') then
            Fail (Where & ": """ & Key & """: a family's prefix ends with '.', and only a family's does");
            return;
         end if;
         New_Setting.Kind :=
           (if Alt (Named (At_Cursor, "class")) = "Application_State" then Application_State
            elsif Alt (Named (At_Cursor, "class")) = "Secret" then Secret
            else Declaration_Managed);
         New_Setting.Codec := To_Text (Text (Named (At_Cursor, "codec")));
         New_Setting.File := To_Text (File);
         if Value_Name = "Typed" then
            declare
               Payload : constant Views.Cursor := Views.Payload (Object, Value_Cursor);
            begin
               New_Setting.Value := Typed;
               New_Setting.Source := To_Text (Text (Named (Payload, "source")));
               New_Setting.Root := To_Text (Text (Named (Payload, "root")));
            end;
            if New_Setting.Codec.Length = 0 then
               Fail (Where & ": """ & Key & """ has a CCL type but no codec",
                     "name the Ada package programs read it with");
               return;
            end if;
         else
            New_Setting.Value := (if Value_Name = "Address" then Address
                                  elsif Value_Name = "Count" then Count
                                  elsif Value_Name = "Collection" then Collection else Text);
            if New_Setting.Codec.Length > 0 then
               Fail (Where & ": """ & Key & """ is " & Value_Name & ", read with the built-in codec",
                     "leave codec empty");
               return;
            end if;
         end if;
         for I in 1 .. Item.Count loop
            declare
               Other : constant String := Image (Item.Settings (I).Key);
            begin
               if Other = Key
                 or else (Item.Settings (I).Form = Family and then CuBit.Config_Inspection.Contains (Scope => Other, Key => Key))
                 or else (New_Setting.Form = Family and then CuBit.Config_Inspection.Contains (Scope => Key, Key => Other))
               then
                  Fail (Where & ": """ & Key & """ overlaps """ & Other & """ (" &
                        Image (Item.Settings (I).File) & ")");
                  return;
               end if;
            end;
         end loop;
         if Item.Count = Maximum_Settings then
            Fail ("more than" & Natural'Image (Maximum_Settings) & " settings");
            return;
         end if;
         Item.Count := Item.Count + 1;
         Item.Settings (Item.Count) := New_Setting;
      end Add_Setting;
   begin
      Success := True;
      Why := (others => <>);
      if Program'Length > CCL.Language.MAX_SOURCE_LENGTH then
         Fail ("the catalog and its schema are longer than" & Natural'Image (CCL.Language.MAX_SOURCE_LENGTH) &
               " characters", "split the file");
         return;
      end if;
      CCL.Catalog.Initialize (Catalog_View);
      CCL.Catalog.Initialize (Grants);
      CCL.Language.Analyze (Program, Catalog_View, Checked);
      if CCL.Language.Analysis_Status_Of (Checked) /= CCL.Language.Analysis_Succeeded then
         Fail ("not a Setting_Catalog: character" &
               Natural'Image (Natural'Max (0, CCL.Language.Analysis_Diagnostic_Position (Checked) - Schema'Length - 1)) &
               ": " & CCL.Diagnostics.Message (CCL.Language.Analysis_Diagnostic (Checked)));
         return;
      end if;
      Catalog_Type := CCL.Types.Find (CCL.Language.Analysis_Types (Checked), CCL.Types.Named (ROOT_TYPE_NAME));
      CCL.Objects.Bind (CCL.Language.Analysis_Types (Checked), Catalog_Type, LOCAL_KEY, Contract, Published);
      if not Published then
         Fail ("the schema does not declare a persistable " & ROOT_TYPE_NAME);
         return;
      end if;
      Evaluate (Program, CATALOG_FUEL, Catalog_View, Grants, Host, Contract, Value);
      if Value.Status /= CCL.Language.Succeeded or else not Value.Has_Value then
         Fail ("does not evaluate: " & CCL.Diagnostics.Message (Value.Status));
         return;
      end if;
      Views.Capture (Object, Contract, Value.Value, Captured);
      if not Captured then
         Fail ("its value cannot be read");
         return;
      end if;
      declare
         Settings : constant Views.Cursor := Named (Views.Root (Object), "settings");
      begin
         for Index in 1 .. Views.Length (Object, Settings) loop
            exit when not Success;
            Add_Setting (Views.Element (Object, Settings, Index), Index);
         end loop;
      end;
   end Add;

end Setting_Catalogs;

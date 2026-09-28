with Ada.Text_IO;
with CCL.Types; use CCL.Types;
with CCL.Types.Encoding;
with CCL.Types.Correspondence;
with CCL.Objects;
with CCL.Objects.Schemas;
with CCL.Catalog;
with CCL.Language;
with CCL.VM;
with CCL.Format;

procedure Resource_Tests is
   package E renames CCL.Types.Encoding;
   package O renames CCL.Objects;
   package S renames CCL.Objects.Schemas;
   use type E.Bytes;
   use type S.Image;
   use type O.Binding;
   R, Target : Registry;
   Settings, Collection, Ref, Imported_Ref : Type_Reference;
   D, Decoded : Description;
   Status : Definition_Result;
   Imported : Import_Result;
   Contract, Restored, Empty : O.Binding;
   Data, Expected : S.Image;
   Bytes : E.Bytes;
   Good : Boolean;
   Checks : Natural := 0;
   Key : constant O.Schema_Key := [1, 2, 3, 4];
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with "resource metadata check" & Checks'Image; end if;
   end Check;
   procedure Reject (Wanted : Definition_Result) is
      Before : constant Registry := R;
   begin
      Define (R, D, Ref, Status);
      Check (Status = Wanted and Ref = Invalid_Type and R = Before);
   end Reject;
   procedure Not_Data (Root : Type_Reference) is
   begin
      Check (not O.Persistable (R, Root));
      O.Bind (R, Root, Key, Contract, Good);
      Check (not Good and Contract = Empty);
   end Not_Data;
begin
   D := (Identifier => Named ("Settings"), Form => Product, Count => 1,
         Parts => [1 => (Named ("caption"), String_Type), others => <>]);
   Define (R, D, Settings, Status); Check (Status = Defined);
   O.Bind (R, Settings, Key, Contract, Good); Check (Good);
   S.Write (Contract, Expected, Good); Check (Good);
   D := (Identifier => Named ("SettingsCollection"), Form => Resource, Count => 1,
         Parts => [1 => (Named ("Value"), Settings), others => <>]);
   Define (R, D, Collection, Status); Check (Status = Defined);
   Check (Cells (R, Collection) = 1 and Describe (R, Collection) = D);
   Check (not Is_Enumeration (R, Collection) and not Is_Scalar_Sum (R, Collection));
   Check (Alternative (R, Collection, Named ("Value")) = 0);
   Bytes := E.Encode (D);
   E.Decode (Bytes, Decoded, Good); Check (Good and Decoded = D);
   Check (E.Encode (Decoded) = Bytes);
   Bytes (E.Reserved_Offset) := 1;
   E.Decode (Bytes, Decoded, Good); Check (not Good);
   Bytes := E.Encode (D); Bytes (E.Shape_Offset) := 255;
   E.Decode (Bytes, Decoded, Good); Check (not Good);
   Bytes := E.Encode (D); Bytes (E.Parts_Offset + E.Part_Size) := 1;
   E.Decode (Bytes, Decoded, Good); Check (not Good);
   Not_Data (Collection);
   D.Identifier := Named ("Bad"); D.Parts (1).Payload := Last (R) + 1;
   Reject (Invalid_Reference);
   D.Parts (1).Payload := Invalid_Type; Reject (Invalid_Reference);
   D.Parts (1).Payload := Settings; D.Count := 2; D.Parts (2) := D.Parts (1);
   Reject (Duplicate_Name);
   D.Parts (2).Identifier := Named ("No.Dot"); Reject (Invalid_Name);
   D := (Identifier => Named ("Window"), Form => Resource, others => <>);
   Define (R, D, Ref, Status); Check (Status = Defined and Cells (R, Ref) = 1);
   Not_Data (Ref);
   D := (Identifier => Named ("Wrapped"), Form => Product, Count => 1,
         Parts => [1 => (Named ("collection"), Collection), others => <>]);
   Define (R, D, Ref, Status); Check (Status = Defined); Not_Data (Ref);
   D := (Identifier => Named ("MaybeCollection"), Form => Sum, Count => 2,
         Parts => [1 => (Named ("Empty"), Unit_Type),
                   2 => (Named ("Ready"), Ref), others => <>]);
   Define (R, D, Ref, Status); Check (Status = Defined); Not_Data (Ref);

   -- Visible resource declarations must not poison or leak through a data
   -- schema. A primitive root has no declaration dependencies at all.
   O.Bind (R, Settings, Key, Contract, Good); Check (Good);
   S.Write (Contract, Data, Good); Check (Good and Data = Expected);
   S.Read (Data, Restored, Good); Check (Good and O.Same_Schema (Restored, Contract));
   O.Bind (R, Integer_Type, Key, Contract, Good); Check (Good);
   S.Write (Contract, Data, Good); Check (Good);
   declare
      Blank : Registry;
   begin
      O.Bind (Blank, Integer_Type, Key, Restored, Good); Check (Good);
      S.Write (Restored, Expected, Good); Check (Good and Data = Expected);
   end;

   -- Local IDs are not identity. Import translates parameters and remains
   -- atomic even if it imports a dependency before discovering a conflict.
   D := (Identifier => Named ("Unrelated"), Form => Product, others => <>);
   Define (Target, D, Ref, Status); Check (Status = Defined);
   Import_Definition (R, Collection, Target, Imported_Ref, Imported);
   Check (Imported = CCL.Types.Imported and Imported_Ref /= Collection);
   Check (Correspondence.Resolve (R, Collection, Target) = Imported_Ref);
   Check (Correspondence.Resolve (Target, Imported_Ref, R) = Collection);
   declare
      Before : constant Registry := Target;
   begin
      Import_Definition (R, Collection, Target, Ref, Imported);
      Check (Imported = CCL.Types.Imported and Ref = Imported_Ref and Target = Before);
   end;
   for Form in Product .. Resource loop
      declare
         Conflict : Registry;
      begin
         D := (Identifier => Named ("SettingsCollection"), Form => Form, Count => 1,
               Parts => [1 => (Named ("Value"), Integer_Type), others => <>]);
         Define (Conflict, D, Ref, Status); Check (Status = Defined);
         Check (Correspondence.Resolve (R, Collection, Conflict) = Invalid_Type);
         declare
            Before : constant Registry := Conflict;
         begin
            Import_Definition (R, Collection, Conflict, Ref, Imported);
            Check (Imported = Conflicting_Definition and Ref = Invalid_Type and Conflict = Before);
         end;
      end;
   end loop;

   -- Discovery/CCLB may carry metadata without granting or constructing an
   -- instance. The data-value ABI must reject resource locals and imports.
   declare
      package V renames CCL.VM;
      package F renames CCL.Format;
      use type V.Validation_Error;
      use type F.Format_Error;
      use type CCL.Language.Analysis_Status;
      Catalog : CCL.Catalog.Interface_Catalog;
      Analysis : CCL.Language.Analysis_Result;
      Program, Loaded : V.Program;
      Checked : V.Validated_Program;
      Error : V.Validation_Error;
      Linkage : CCL.Catalog.Linkage_Table;
      Wire : F.Byte_Array;
      Length : F.Module_Length;
      Limits : F.Resource_Limits;
      Format_Error : F.Format_Error;
   begin
      CCL.Catalog.Publish_Type (Catalog, R, Collection, Ref, Imported);
      Check (Imported = CCL.Types.Imported);
      Check (Describe (CCL.Catalog.Visible_Types (Catalog), Ref).Form = Resource);
      CCL.Language.Analyze ("(SettingsCollection (Settings ""forged""))", Catalog, Analysis);
      Check (CCL.Language.Analysis_Status_Of (Analysis) /= CCL.Language.Analysis_Succeeded);
      CCL.Language.Analyze ("42", Catalog, Analysis);
      Check (CCL.Language.Analysis_Status_Of (Analysis) = CCL.Language.Analysis_Succeeded);
      Program.Data_Types := R; Program.Length := 2;
      Program.Code (0) := (Op => V.Push_Integer, Immediate => 42, others => <>);
      F.Encode (Program, (1024, 4096, 1), Wire, Length, Format_Error, Error);
      Check (Format_Error = F.Format_Valid and Error = V.Valid);
      F.Decode (Wire, Length, Loaded, Linkage, Limits, Format_Error, Error);
      Check (Format_Error = F.Format_Valid and Error = V.Valid and Loaded.Data_Types = R);
      Program.Locals_Length := 1;
      Program.Local_Kinds (0) := V.Object_Value;
      Program.Local_Data_Types (0) := Collection;
      V.Verify (Program, Checked, Error);
      Check (Error = V.Invalid_Data_Type and not V.Is_Valid (Checked));
      Program.Locals_Length := 0; Program.Imports_Length := 1;
      Program.Imports (0).Result := V.Object_Value;
      Program.Imports (0).Result_Data_Type := Collection;
      V.Verify (Program, Checked, Error);
      Check (Error = V.Invalid_Data_Type and not V.Is_Valid (Checked));
      Program.Imports (0).Result := V.Integer_Value;
      Program.Imports (0).Result_Data_Type := Invalid_Type;
      Program.Imports (0).Argument := V.Object_Value;
      Program.Imports (0).Argument_Data_Type := Collection;
      V.Verify (Program, Checked, Error);
      Check (Error = V.Invalid_Data_Type and not V.Is_Valid (Checked));
   end;

   -- Parameters describe types, not embedded data. A large parameter does
   -- not consume the resource's value-cell budget, even when repeated.
   D := (Identifier => Named ("Wide"), Form => Product, Count => 16, others => <>);
   for I in 1 .. D.Count loop
      D.Parts (I) := (Named ("part_" & Character'Val (Character'Pos ('A') + I - 1)), Integer_Type);
   end loop;
   Define (R, D, Ref, Status); Check (Status = Defined and Cells (R, Ref) = 17);
   D.Identifier := Named ("Large"); D.Count := 15;
   for I in 1 .. D.Count loop D.Parts (I).Payload := Ref; end loop;
   Define (R, D, Ref, Status); Check (Status = Defined and Cells (R, Ref) = Maximum_Value_Cells);
   D.Identifier := Named ("LargeResource"); D.Form := Resource;
   for I in 1 .. D.Count loop D.Parts (I).Payload := Ref; end loop;
   Define (R, D, Ref, Status); Check (Status = Defined and Cells (R, Ref) = 1);
   Not_Data (Ref);
   Ada.Text_IO.Put_Line ("Opaque resource type metadata: PASS" & Checks'Image & " checks");
end Resource_Tests;

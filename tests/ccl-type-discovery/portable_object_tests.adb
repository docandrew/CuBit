with Ada.Text_IO;
with Interfaces; use Interfaces;
with CCL.Catalog; use CCL.Catalog;
with CCL.Types; use CCL.Types;
with CCL.Objects;
with CCL.Objects.Catalog;
with CCL.Host_Values;
with CCL.Language;
with CCL.Compiler;
with CCL.Format;
with Module_Patches; use type Module_Patches.Bytes;
with CCL.VM;

procedure Portable_Object_Tests is
   package V renames CCL.VM;
   package F renames CCL.Format;
   package O renames CCL.Objects;
   use type O.Schema_Key;
   use type O.Catalog.Publication_Result;
   use type CCL.Language.Analysis_Status;
   use type CCL.Compiler.Compilation_Status;
   use type F.Format_Error;
   use type F.Byte_Array;
   use type V.Validation_Error;
   use type V.Execution_Status;
   use type V.Value;
   use type V.Program;
   Key : constant O.Schema_Key := [11, 12, 13, 14];
   Catalog, Shifted, Wrong : Interface_Catalog;
   Grants, None : Granted_Bindings;
   Types : Registry;
   Root : Type_Reference;
   Contract : O.Binding;
   Good : Boolean;
   Analysis : CCL.Language.Analysis_Result;
   Compiled : CCL.Compiler.Compilation_Result;
   Bytes, Changed, Again : F.Byte_Array;
   Size, Again_Size, Changed_Size : F.Module_Length;
   Error : F.Format_Error;
   Validation : V.Validation_Error;
   Decoded, Original : V.Program;
   Links, Again_Links : Linkage_Table;
   Limits : F.Resource_Limits;
   Linked : Link_Result;
   Patched : Boolean;
   Checks : Natural := 0;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with "portable object check" & Checks'Image; end if;
   end Check;
   procedure Schema (View : in out Interface_Catalog; Prefix, Bad : Boolean) is
      Local : Registry;
      Ref, Unused : Type_Reference;
      Defined : Definition_Result;
      Bound : O.Binding;
      Accepted : Boolean;
      Published : O.Catalog.Publication_Result;
      Imported : Import_Result;
   begin
      if Prefix then
         Define (Local, (Identifier => Named ("Other"), Form => Product, others => <>), Ref, Defined);
         Check (Defined = CCL.Types.Defined);
         Publish_Type (View, Local, Ref, Unused, Imported); Check (Imported = CCL.Types.Imported);
      end if;
      Define (Local, (Identifier => Named ("Reading"), Form => Sum, Count => 2,
        Parts => [1 => (Named ("Value"), (if Bad then Boolean_Type else Integer_Type)),
                  2 => (Named ("Unavailable"), Unit_Type), others => <>]), Ref, Defined);
      Check (Defined = CCL.Types.Defined);
      O.Bind (Local, Ref, Key, Bound, Accepted); Check (Accepted);
      Publish_Schema (View, Bound, Published); Check (Published = O.Catalog.Published);
   end Schema;
   procedure Describe (View : in out Interface_Catalog; Bindings : out Granted_Bindings) is
      Descriptor : Interface_Descriptor;
      Op : Operation_Descriptor;
      E : Catalog_Error;
      Resolved : Resolved_Operation;
      Found : Boolean;
      Installed : Grant_Result;
   begin
      Initialize (Bindings);
      Define_Interface ("objects", 1, 0, [101, 102, 103, 104], Descriptor, E); Check (E = Catalog_Valid);
      Define_Host_Operation ("get", 0,
        (Result => CCL.Host_Values.Object_Value, Result_Schema => Key, others => <>), Op, E);
      Check (E = Catalog_Valid);
      Add_Operation (Descriptor, Op, E); Check (E = Catalog_Valid);
      Define_Host_Operation ("echo", 1,
        (Argument => CCL.Host_Values.Object_Value, Argument_Schema => Key,
         Result => CCL.Host_Values.Object_Value, Result_Schema => Key, others => <>), Op, E);
      Check (E = Catalog_Valid);
      Add_Operation (Descriptor, Op, E); Check (E = Catalog_Valid);
      Publish (View, Descriptor, E); Check (E = Catalog_Valid);
      Resolve (View, "objects.get", Resolved, Found); Check (Found);
      Install (Bindings, Resolved, 77, Installed); Check (Installed = Grant_Added);
      Resolve (View, "objects.echo", Resolved, Found); Check (Found);
      Install (Bindings, Resolved, 78, Installed); Check (Installed = Grant_Added);
   end Describe;
   procedure Decode_Changed is
   begin
      F.Decode (Changed, Changed_Size, Decoded, Again_Links, Limits, Error, Validation);
   end Decode_Changed;
   --  Changed := Bytes with the Occurrence'th (0: only) From replaced by To.
   procedure Patch (From, To : Module_Patches.Bytes; Occurrence : Natural := 0) is
   begin
      Changed := Bytes;
      Changed_Size := Size;
      Module_Patches.Replace (Changed, Changed_Size, From, To, Patched, Occurrence);
      Check (Patched);
   end Patch;
   --  "CCLB" and the format version.
   Header : constant Module_Patches.Bytes := [16#44#, 16#43#, 16#43#, 16#4C#, 16#42#, F.FORMAT_VERSION];
   procedure Execute (Expected : V.Value) is
      Verified : V.Validated_Program;
      Machine : V.Machine_State;
      Outcome : V.Execution_Result;
      Calls : Natural := 0;
   begin
      V.Verify (Decoded, Verified, Validation); Check (Validation = V.Valid);
      V.Initialize (Verified, 32, Machine);
      loop
         V.Continue_Execution_For (Verified, Machine, 1, Outcome);
         exit when Outcome.Status not in V.Paused | V.Waiting_For_Host;
         if Outcome.Status = V.Waiting_For_Host then
            if Outcome.Requested_Binding = 77 then
               Check (Outcome.Request_Argument = V.Integer_Constant (0));
            else
               Check (Outcome.Requested_Binding = 78 and Outcome.Request_Argument = Expected);
            end if;
            Calls := Calls + 1;
            V.Complete_Host_Call (Verified, Machine, Expected, True);
         end if;
      end loop;
      Check (Outcome.Status = V.Completed and Outcome.Has_Value and Outcome.Result_Value = Expected and Calls = 2);
   end Execute;
begin
   Schema (Catalog, False, False);
   Schema (Shifted, True, False);
   Schema (Wrong, False, True);
   Describe (Catalog, Grants);
   Types := Visible_Types (Catalog); Root := Schema_Type (Catalog, Key);
   Check (Root /= Schema_Type (Shifted, Key));
   CCL.Language.Analyze ("(objects.echo (objects.get))", Catalog, Analysis);
   Check (CCL.Language.Analysis_Status_Of (Analysis) = CCL.Language.Analysis_Succeeded);
   CCL.Compiler.Compile (Analysis, Compiled); Check (Compiled.Status = CCL.Compiler.Compilation_Succeeded);
   F.Encode (Compiled.Program, Compiled.Linkage, (Fuel => 32, Memory => 4096, In_Flight => 1), Bytes, Size, Error, Validation);
   Check (Error = F.Format_Valid and Validation = V.Valid);
   F.Decode (Bytes, Size, Decoded, Links, Limits, Error, Validation);
   Check (Error = F.Format_Valid and Decoded = Compiled.Program);
   F.Encode (Decoded, Links, Limits, Again, Again_Size, Error, Validation);
   Check (Error = F.Format_Valid and Again_Size = Size and Again = Bytes);
   Original := Decoded;
   Link_Program (Grants, Links, Decoded, Linked);
   Check (Linked = Import_Contract_Mismatch and Decoded = Original);
   Link_Program (None, Links, Decoded, Linked, Catalog);
   Check (Linked = Authority_Not_Granted and Decoded = Original);
   Link_Program (Grants, Links, Decoded, Linked, Wrong);
   Check (Linked = Import_Contract_Mismatch and Decoded = Original);
   -- Forge a changed program definition under the original approved key.
   Decoded.Data_Types := Visible_Types (Wrong);
   declare Before : constant V.Program := Decoded; begin
      Link_Program (Grants, Links, Decoded, Linked, Catalog);
      Check (Linked = Import_Contract_Mismatch and Decoded = Before);
   end;
   Decoded := Original;
   Link_Program (Grants, Links, Decoded, Linked, Shifted); Check (Linked = Link_Valid);
   Execute ((Kind => V.Variant_Value, Data_Type => Root, Alternative => 1, Integer => 42, others => <>));
   declare
      Import : constant Resolved_Operation := Element (Links, 0);
      Result_Schema : constant Module_Patches.Bytes :=
        Module_Patches.Encoded_Digest (Import.Import.Result_Schema);
      Interface_Digest : constant Module_Patches.Bytes :=
        Module_Patches.Encoded_Digest (Import.Interface_Digest);
      Result_Type : constant Unsigned_8 := Unsigned_8 (Decoded.Imports (0).Result_Data_Type);
      Zeroed : Module_Patches.Bytes := Result_Schema;
      Moved : Module_Patches.Bytes := Result_Schema;
   begin
      Zeroed (3 .. Zeroed'Last) := [others => 0];
      Moved (3) := 99;
      for Mutation in 1 .. 5 loop
         case Mutation is
            when 1 => Patch (Header, [16#44#, 16#43#, 16#43#, 16#4C#, 16#42#, 4]);
            when 2 => Patch (Interface_Digest, [1 => 16#58#, 2 => 16#20#, 3 .. 34 => 0], Occurrence => 1);
            when 3 => Patch (Result_Type & Interface_Digest, [16#18#, 16#FF#] & Interface_Digest, Occurrence => 1);
            when 4 => Patch (Result_Schema, Zeroed, Occurrence => 1);
            when others => Patch (Result_Schema, Moved, Occurrence => 1);
         end case;
         Decode_Changed;
         Check (Error = (case Mutation is when 1 => F.Unsupported_Version,
           when 2 => F.Invalid_Linkage, when 3 => F.Invalid_Type_Metadata,
           when 4 => F.Invalid_Linkage, when others => F.Format_Valid));
         if Mutation = 5 then
            Original := Decoded;
            Link_Program (Grants, Again_Links, Decoded, Linked, Catalog);
            Check (Linked = Authority_Not_Granted and Decoded = Original);
         end if;
      end loop;
      -- A bad later import must not leave the earlier authorized import bound.
      Patch (Result_Schema, Moved, Occurrence => 2);
      Decode_Changed; Check (Error = F.Format_Valid);
      Original := Decoded;
      Link_Program (Grants, Again_Links, Decoded, Linked, Catalog);
      Check (Linked = Authority_Not_Granted and Decoded = Original);
   end;
   -- Every truncated prefix is rejected before it can become executable.
   for Prefix in F.Module_Length range 0 .. Size - 1 loop
      F.Decode (Bytes, Prefix, Decoded, Again_Links, Limits, Error, Validation);
      Check (Error /= F.Format_Valid);
   end loop;
   -- Same let/conditional structure as the native Config suspension fixture.
   -- Locals are outside conditional branches, as required by the compiler.
   CCL.Language.Analyze
     ("(let ((first (objects.echo (Reading.Value 42)))) " &
      "(let ((second (objects.get))) (if true (objects.get) Reading.Unavailable)))",
      Catalog, Analysis);
   CCL.Compiler.Compile (Analysis, Compiled);
   Check (Compiled.Status = CCL.Compiler.Compilation_Succeeded);
   F.Encode (Compiled.Program, Compiled.Linkage,
     (Fuel => 64, Memory => 4096, In_Flight => 1), Again, Again_Size, Error, Validation);
   Check (Error = F.Format_Valid and Validation = V.Valid);
   F.Decode (Again, Again_Size, Decoded, Again_Links, Limits, Error, Validation);
   Check (Error = F.Format_Valid);
   Link_Program (Grants, Again_Links, Decoded, Linked, Catalog);
   Check (Linked = Link_Valid);
   -- A schema-bearing Integer is still an object contract, not a scalar grant.
   Initialize (Catalog);
   O.Bind (Types, Integer_Type, Key, Contract, Good); Check (Good);
   declare Published : O.Catalog.Publication_Result; begin
      Publish_Schema (Catalog, Contract, Published); Check (Published = O.Catalog.Published);
   end;
   Describe (Catalog, Grants);
   CCL.Language.Analyze ("(objects.echo (objects.get))", Catalog, Analysis);
   CCL.Compiler.Compile (Analysis, Compiled); Check (Compiled.Status = CCL.Compiler.Compilation_Succeeded);
   F.Encode (Compiled.Program, Compiled.Linkage, (Fuel => 32, Memory => 4096, In_Flight => 1), Bytes, Size, Error, Validation);
   Check (Error = F.Format_Valid);
   F.Decode (Bytes, Size, Decoded, Links, Limits, Error, Validation); Check (Error = F.Format_Valid);
   Original := Decoded;
   Link_Program (Grants, Links, Decoded, Linked);
   Check (Linked = Import_Contract_Mismatch and Decoded = Original);
   Link_Program (Grants, Links, Decoded, Linked, Catalog); Check (Linked = Link_Valid);
   Execute (V.Integer_Constant (42));
   -- Removing schema keys must not turn approved object access into scalar
   -- access, even though both happen to use an Integer VM representation.
   declare
      Result_Schema : constant Module_Patches.Bytes :=
        Module_Patches.Encoded_Digest (Element (Links, 0).Import.Result_Schema);
      Zeroed : Module_Patches.Bytes := Result_Schema;
   begin
      Zeroed (3 .. Zeroed'Last) := [others => 0];
      Patch (Result_Schema, Zeroed, Occurrence => 1);
   end;
   Decode_Changed; Check (Error = F.Format_Valid);
   Original := Decoded;
   Link_Program (Grants, Again_Links, Decoded, Linked, Catalog);
   Check (Linked = Authority_Not_Granted and Decoded = Original);
   Ada.Text_IO.Put_Line ("Schema-pinned portable objects: PASS" & Checks'Image & " checks");
end Portable_Object_Tests;

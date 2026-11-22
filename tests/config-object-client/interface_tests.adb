with Ada.Text_IO;
with GNAT.Source_Info;
with Interfaces; use Interfaces;
with CCL.Types; use CCL.Types;
with CCL.Objects;
with CCL.Catalog; use CCL.Catalog;
with CCL.Catalog.Completion;
with CCL.Host_Values;
with CCL.Language;
with CCL.Compiler;
with CCL.VM;
with Config_Object_Interfaces;
with Config_Read_Outcomes;

procedure Interface_Tests is
   package O renames CCL.Objects;
   package D renames Config_Object_Interfaces;
   package V renames CCL.VM;
   use type CCL.Language.Analysis_Status;
   use type CCL.Compiler.Compilation_Status;
   use type CCL.Host_Values.Value_Kind;
   use type V.Validation_Error;
   Types : Registry;
   Contract, Other : O.Binding;
   Reads, Wrong_Reads : Config_Read_Outcomes.Description;
   Catalog : Interface_Catalog;
   Grants : Granted_Bindings;
   Kind : Type_Reference;
   Good : Boolean;
   Matches : CCL.Catalog.Completion.Match_List;
   Operation : Resolved_Operation;
   Granted : Grant_Result;
   Linked : Link_Result;
   Analysis : CCL.Language.Analysis_Result;
   Compiled : CCL.Compiler.Compilation_Result;
   Program : V.Validated_Program;
   Validity : V.Validation_Error;
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Site : String := GNAT.Source_Info.Source_Location) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Site; end if;
   end Check;
   procedure Compile (Source : String) is
   begin
      CCL.Language.Analyze (Source, Catalog, Analysis);
      Check (CCL.Language.Analysis_Status_Of (Analysis) = CCL.Language.Analysis_Succeeded);
      CCL.Compiler.Compile (Analysis, Compiled);
      Check (Compiled.Status = CCL.Compiler.Compilation_Succeeded);
   end Compile;
begin
   O.Bind (Types, Integer_Type, [1, 2, 3, 4], Contract, Good); Check (Good);
   O.Bind (Types, Boolean_Type, [5, 6, 7, 8], Other, Good); Check (Good);
   Config_Read_Outcomes.Define (Contract, Named ("Snapshot"), Named ("ConfigRead"),
     [11, 12, 13, 14], Reads, Good); Check (Good);
   Config_Read_Outcomes.Define (Other, Named ("OtherSnapshot"), Named ("OtherRead"),
     [15, 16, 17, 18], Wrong_Reads, Good); Check (Good);
   CCL.Catalog.Completion.Find (Catalog, "settings.", Matches);
   Check (Matches.Count = 0);
   D.Publish (Catalog, "settings", [21, 22, 23, 24], Contract, Wrong_Reads, Kind, Good);
   Check (not Good and Kind = Invalid_Type and Length (Catalog) = 0);
   D.Publish (Catalog, "bad alias", [21, 22, 23, 24], Contract, Reads, Kind, Good);
   Check (not Good and Kind = Invalid_Type and Length (Catalog) = 0);
   Check (Schema_Type (Catalog, O.Identity (Contract)) = Invalid_Type);
   D.Publish (Catalog, "settings", [21, 22, 23, 24], Contract, Reads, Kind, Good);
   Check (Good and Kind /= Invalid_Type and Length (Catalog) = 1);
   CCL.Catalog.Completion.Find (Catalog, "settings.", Matches);
   Check (Matches.Count = 4 and Matches.Total = 4);
   for Action in D.Operation loop
      declare
         Suggestion : CCL.Catalog.Completion.Suggestion renames Matches.Items (D.Operation'Pos (Action) + 1);
      begin
         Check (Suggestion.Name (1 .. Suggestion.Length) = "settings." & D.Name (Action));
         Resolve (Catalog, "settings." & D.Name (Action), Operation, Good); Check (Good);
         Check (Same_Operation (Suggestion.Contract, Operation));
      end;
   end loop;
   Resolve (Catalog, "settings.write", Operation, Good); Check (Good);
   Check (Operation.Import.Argument = CCL.Host_Values.Object_Value and
     Operation.Import.Receiver_Resource = Named ("ConfigCollection-Integer"));
   Compile ("(let ((c (settings.open))) (let ((r (settings.write c 42))) " &
     "(let ((closed (settings.close c))) r)))");
   Link_Program (Grants, Compiled.Linkage, Compiled.Program, Linked, Catalog);
   Check (Linked = Authority_Not_Granted); -- discovery alone is not a grant
   for Action in D.Operation loop
      Resolve (Catalog, "settings." & D.Name (Action), Operation, Good); Check (Good);
      Install (Grants, Operation, Unsigned_32 (D.Operation'Pos (Action) + 1), Granted);
      Check (Granted = Grant_Added);
   end loop;
   Link_Program (Grants, Compiled.Linkage, Compiled.Program, Linked, Catalog);
   Check (Linked = Link_Valid);
   V.Verify (Compiled.Program, Program, Validity); Check (Validity = V.Valid);
   Compile ("(let ((c (settings.open))) (let ((r (settings.read c))) " &
     "(let ((closed (settings.close c))) r)))");
   Link_Program (Grants, Compiled.Linkage, Compiled.Program, Linked, Catalog);
   Check (Linked = Link_Valid);
   V.Verify (Compiled.Program, Program, Validity); Check (Validity = V.Valid);
   CCL.Language.Analyze
     ("(let ((c (settings.open))) (let ((r (settings.write c true))) " &
      "(let ((closed (settings.close c))) r)))", Catalog, Analysis);
   Check (CCL.Language.Analysis_Status_Of (Analysis) /= CCL.Language.Analysis_Succeeded);
   D.Publish (Catalog, "settings", [31, 32, 33, 34], Contract, Reads, Kind, Good);
   Check (not Good and Kind = Invalid_Type and Length (Catalog) = 1);
   CCL.Catalog.Completion.Find (Catalog, "settings.", Matches); Check (Matches.Count = 4);
   -- The same descriptor builder accepts application-defined structured types;
   -- Integer is not baked into its compiler, resource or completion metadata.
   declare
      Preferences : Type_Reference;
      Defined_Type : Definition_Result;
   begin
      Define (Types, (Identifier => Named ("Preferences"), Form => Product, Count => 2,
        Parts => [1 => (Named ("title"), String_Type),
                  2 => (Named ("enabled"), Boolean_Type), others => <>]), Preferences, Defined_Type);
      Check (Defined_Type = Defined);
      O.Bind (Types, Preferences, [41, 42, 43, 44], Other, Good); Check (Good);
      Config_Read_Outcomes.Define (Other, Named ("PreferencesSnapshot"), Named ("PreferencesRead"),
        [45, 46, 47, 48], Wrong_Reads, Good); Check (Good);
      D.Publish (Catalog, "preferences", [51, 52, 53, 54], Other, Wrong_Reads, Kind, Good);
      Check (Good and Kind /= Invalid_Type and Length (Catalog) = 2);
      CCL.Catalog.Completion.Find (Catalog, "preferences.write", Matches);
      Check (Matches.Count = 1 and Matches.Items (1).Contract.Import.Receiver_Resource =
        Named ("ConfigCollection-Preferences"));
      Check (Schema_Type (Catalog, Matches.Items (1).Contract.Import.Argument_Schema) /= Invalid_Type);
      Compile ("(let ((c (preferences.open))) (let ((r (preferences.read c))) " &
        "(let ((closed (preferences.close c))) r)))");
      Link_Program (Grants, Compiled.Linkage, Compiled.Program, Linked, Catalog);
      Check (Linked = Authority_Not_Granted);
      CCL.Language.Analyze
        ("(let ((c (settings.open))) (let ((r (preferences.read c))) " &
         "(let ((closed (settings.close c))) r)))", Catalog, Analysis);
      Check (CCL.Language.Analysis_Status_Of (Analysis) /= CCL.Language.Analysis_Succeeded);
   end;
   D.Publish (Catalog, "second", [61, 62, 63, 64], Contract, Reads, Kind, Good);
   Check (Good and Kind /= Invalid_Type and Length (Catalog) = 3);
   Ada.Text_IO.Put_Line ("Config shared interface/completion: PASS" & Checks'Image & " checks");
end Interface_Tests;

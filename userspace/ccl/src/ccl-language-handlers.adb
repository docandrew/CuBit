with CCL.Compiler;
with CCL.Evaluation;
with CCL.VM;

package body CCL.Language.Handlers is
   use type Interfaces.Unsigned_32;
   use type CCL.Types.Type_Reference;
   --  Whether Analysis compiles and links against Grants.
   function Links
     (Analysis : Analysis_Result; Catalog : CCL.Catalog.Interface_Catalog;
      Grants : CCL.Catalog.Granted_Bindings) return Boolean
   is
      use type CCL.Compiler.Compilation_Status;
      use type CCL.Catalog.Link_Result;
      Compiled : CCL.Compiler.Compilation_Result;
      Program : CCL.VM.Program;
      Linked : CCL.Catalog.Link_Result;
   begin
      CCL.Compiler.Compile (Analysis, Compiled);
      if Compiled.Status /= CCL.Compiler.Compilation_Succeeded then return False; end if;
      Program := Compiled.Program;
      CCL.Catalog.Link_Program (Grants, Compiled.Linkage, Program, Linked, Catalog);
      return Linked = CCL.Catalog.Link_Valid;
   end Links;

   procedure Prepare
     (Source, Entry_Name : String; Expected : Profile;
      Catalog : CCL.Catalog.Interface_Catalog;
      Grants : CCL.Catalog.Granted_Bindings;
      Item : out Handler; Status : out Preparation_Status;
      Diagnostic : out Interpretation_Result) is
      Analysis : Analysis_Result;
   begin
      Item := (others => <>);
      Status := Invalid_Source;
      Diagnostic := (others => <>);
      Analyze (Source, Catalog, Analysis);
      if Analysis.Status /= Analysis_Succeeded then
         Diagnostic.Status := (if Analysis.Status = Analysis_Parse_Failed then Parse_Failed else Type_Check_Failed);
         Diagnostic.Diagnostic := Analysis.Diagnostic;
         Diagnostic.Diagnostic_Position := Analysis.Diagnostic_Position;
         return;
      end if;
      Status := Unknown_Entry;
      for F in 1 .. Analysis.Tree.Function_Count loop
         declare
            Decl : constant Function_Declaration := Analysis.Tree.Functions (F - 1);
         begin
            if Decl.Identifier.Data (1 .. Decl.Identifier.Length) = Entry_Name then
               case Expected is
                  when Boolean_Action =>
                     if Decl.Count /= 0 or else Decl.Result_Kind /= Boolean_Type then
                        Status := Wrong_Profile;
                        return;
                     end if;
               end case;
               --  Select the checked function body, never the top-level
               --  expression. There are no parameters or captured locals.
               Analysis.Tree.Root := Decl.Body_Node;
               --  It must link against the grants it is registered with.
               if not Links (Analysis, Catalog, Grants) then
                  Diagnostic.Status := Host_Import_Required;
                  Status := Admission_Denied;
                  return;
               end if;
               Item := (Valid => True, Program => Analysis, Bindings => Grants, Catalog => Catalog);
               Status := Prepared;
               return;
            end if;
         end;
      end loop;
   end Prepare;

   procedure Execute
     (Item : Handler; Fuel : Natural;
      Current_Grants : CCL.Catalog.Granted_Bindings;
      Context : in out Host_Context; Outcome : out Interpretation_Result) is
      procedure Run is new CCL.Evaluation.Evaluate_Analysis_With_Values (Host_Context, Invoke);
      Original, Current : Interfaces.Unsigned_32;
      Was_Granted, Still_Granted : Boolean;
   begin
      Outcome := (Fuel_Remaining => Fuel, others => <>);
      if not Item.Valid then return; end if;
      --  Never let a retained registration acquire new authority by rebinding
      --  a matching advertised operation to a different runtime endpoint.
      for N of Item.Program.Tree.Nodes loop
         if N.Kind = Host_Import_Form then
            CCL.Catalog.Find_Granted_Binding (Item.Bindings, N.Host_Call, Original, Was_Granted);
            CCL.Catalog.Find_Granted_Binding (Current_Grants, N.Host_Call, Current, Still_Granted);
            if not Was_Granted or else not Still_Granted or else Original /= Current then
               Outcome.Status := Host_Authority_Denied;
               Outcome.Diagnostic_Position := N.Source_Position;
               return;
            end if;
         end if;
      end loop;
      Run (Item.Program, Fuel, Item.Catalog, Item.Bindings, Context, Outcome);
   end Execute;
end CCL.Language.Handlers;

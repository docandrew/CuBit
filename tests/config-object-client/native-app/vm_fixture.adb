with CCL.Language;
with CCL.Compiler;

package body VM_Fixture is
   function Empty_Catalog return CCL.Catalog.Interface_Catalog is
      Item : CCL.Catalog.Interface_Catalog;
   begin
      return Item;
   end Empty_Catalog;
   procedure Run
     (Source : String; Types : out CCL.Types.Registry;
      Value : out CCL.VM.Value; Good : out Boolean;
      Catalog : CCL.Catalog.Interface_Catalog := Empty_Catalog)
   is
      Analysis : CCL.Language.Analysis_Result;
      Compiled : CCL.Compiler.Compilation_Result;
      Program : CCL.VM.Validated_Program;
      Valid : CCL.VM.Validation_Error;
      Executed : CCL.VM.Execution_Result;
      Empty_Types : CCL.Types.Registry;
      use type CCL.Compiler.Compilation_Status;
      use type CCL.VM.Validation_Error;
      use type CCL.VM.Execution_Status;
   begin
      Types := Empty_Types;
      Value := CCL.VM.Integer_Constant (0);
      Good := False;
      CCL.Language.Analyze (Source, Catalog, Analysis);
      CCL.Compiler.Compile (Analysis, Compiled);
      if Compiled.Status /= CCL.Compiler.Compilation_Succeeded then return; end if;
      CCL.VM.Verify (Compiled.Program, Program, Valid);
      if Valid /= CCL.VM.Valid then return; end if;
      CCL.VM.Execute (Program, 4096, Executed);
      if Executed.Status /= CCL.VM.Completed then return; end if;
      Types := Compiled.Program.Data_Types;
      Value := Executed.Result_Value;
      Good := True;
   end Run;
end VM_Fixture;

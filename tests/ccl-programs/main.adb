--  Program interfaces generated from descriptions: an ld-shaped program
--  publishes as the interface ld; calls type-check with its typed
--  parameters, file kinds are distinct types, connectors are reached by their
--  qualified names, and mistakes are type errors before anything runs.
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with CCL.Catalog;
with CCL.Language;
with CCL.Compiler;
with CCL.VM;
with CCL.Interfaces.Programs;
with CCL.Sessions;
with CCL.Types;
with CuBit.Program_Descriptions;

procedure Main is
   package PD renames CuBit.Program_Descriptions;
   package Programs renames CCL.Interfaces.Programs;
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Language.Analysis_Status;
   use type CCL.Compiler.Compilation_Status;
   use type CCL.VM.Validation_Error;

   Failures, Checks : Natural := 0;
   procedure Check (Condition : Boolean; What : String) is
   begin
      Checks := Checks + 1;
      if not Condition then
         Failures := Failures + 1;
         Put_Line ("FAIL: " & What);
      end if;
   end Check;

   function Description return PD.Signature is
      S : PD.Signature;
      procedure Parameter (Name : String; Kind : PD.Kind; Many, Optional : Boolean := False) is
      begin
         S.Parameters (S.Parameter_Total) :=
           (Of_Kind => Kind, Many => Many, Optional => Optional,
            Name => [others => ' '], Name_Length => Name'Length);
         S.Parameters (S.Parameter_Total).Name (1 .. Name'Length) := Name;
         S.Parameter_Total := S.Parameter_Total + 1;
      end Parameter;
      procedure Add_Connector (Name : String; Element : PD.Element_Kind; Direction : PD.Connector_Direction := PD.Outlet) is
      begin
         S.Connectors (S.Connector_Total) :=
           (Direction => Direction, Element => Element, Signal => PD.Stream, Pages => 1,
            Name => [others => ' '], Name_Length => Name'Length);
         S.Connectors (S.Connector_Total).Name (1 .. Name'Length) := Name;
         S.Connector_Total := S.Connector_Total + 1;
      end Add_Connector;
   begin
      Parameter ("output", PD.Output_File);
      Parameter ("inputs", PD.Input_File, Many => True);
      Parameter ("script", PD.Input_File, Optional => True);
      Parameter ("static", PD.Flag, Optional => True);
      Add_Connector ("unix.stdin", PD.Text_Lines, PD.Inlet);
      Add_Connector ("unix.stderr", PD.Text_Lines);
      Add_Connector ("org.gnu.ld.progress", PD.Integers);
      return S;
   end Description;

   Catalog : CCL.Catalog.Interface_Catalog;
   Grants : CCL.Catalog.Granted_Bindings;
   Bound : Programs.Contracts;
   Error : CCL.Catalog.Catalog_Error;

   Call : constant String :=
     "(ld.run (Ld_Parameters output => (Output_File ""@nvme:0/work/hello"") " &
     "inputs => [(Input_File ""@nvme:0/work/hello.o"")] static => true))";

   function Checks_Out (Text : String) return Boolean is
      Analysis : CCL.Language.Analysis_Result;
   begin
      CCL.Language.Analyze (Text, Catalog, Analysis);
      return CCL.Language.Analysis_Status_Of (Analysis) = CCL.Language.Analysis_Succeeded;
   end Checks_Out;

   function Compiles (Text : String) return Boolean is
      Analysis : CCL.Language.Analysis_Result;
      Compiled : CCL.Compiler.Compilation_Result;
      Program : CCL.VM.Validated_Program;
      Valid : CCL.VM.Validation_Error;
   begin
      CCL.Language.Analyze (Text, Catalog, Analysis);
      CCL.Compiler.Compile (Analysis, Compiled);
      if Compiled.Status /= CCL.Compiler.Compilation_Succeeded then
         return False;
      end if;
      CCL.VM.Verify (Compiled.Program, Program, Valid);
      return Valid = CCL.VM.Valid;
   end Compiles;
begin
   Check (Programs.Interface_Name ("ld.app") = "ld", "interface name");
   Check (Programs.Parameters_Type ("binutils-compare.app") = "Binutils_Compare_Parameters",
          "record name");
   Put_Line (Programs.Type_Source ("ld.app", Description));
   CCL.Catalog.Initialize (Catalog);
   Programs.Publish (Catalog, Grants, 0, "ld.app", Description, Bound, Error);
   Check (Error = CCL.Catalog.Catalog_Valid, "ld publishes: " & Error'Image);

   --  The typed call, its connectors, and its exit, in both engines.
   Check (Checks_Out (Call), "the typed ld call checks");
   Check (Compiles (Call), "the typed ld call compiles and verifies");
   Check (Checks_Out ("(ld.unix.stderr " & Call & ")"), "a connector by its qualified name");
   Check (Checks_Out ("(latest (ld.org.gnu.ld.progress " & Call & "))"), "an Integer connector");
   Check (Checks_Out ("(window 10 (ld.unix.stderr " & Call & "))"), "a text window");
   Check (Checks_Out ("(latest (ld.com.cubit.exit " & Call & "))"), "the exit connector");
   Check (Compiles ("(ld.unix.stderr " & Call & ")"), "a connector accessor compiles and verifies");
   Check (Checks_Out ("(ld.outlets " & Call & ")"), "a run lists its outlets");
   Check (Checks_Out ("(where (fn ((o Outlet_State)) (field o ended)) (ld.outlets " & Call & "))"),
          "outlet states are records with their counts");
   Check (Compiles ("(ld.outlets " & Call & ")"), "outlets compiles and verifies");

   --  Mistakes are type errors.
   Check (not Checks_Out ("(ld.unix.stdin " & Call & ")"), "an inlet has no output accessor");
   Check (not Checks_Out ("(ld.unix.stdout " & Call & ")"), "an undeclared connector is no accessor");
   Check (not Checks_Out
            ("(ld.run (Ld_Parameters output => (Input_File ""@nvme:0/work/hello"") inputs => []))"),
          "an Input_File is not an Output_File");
   Check (not Checks_Out
            ("(ld.run (Ld_Parameters output => ""@nvme:0/work/hello"" inputs => []))"),
          "a String is not an Output_File");
   Check (not Checks_Out
            ("(ld.run (Ld_Parameters output => (Output_File ""x"") inputs => [] verbose => true))"),
          "an undeclared parameter");
   Check (not Checks_Out ("(ld.run (Ld_Parameters inputs => []))"), "a required parameter");

   --  A type mismatch names the field or the operation and both types.
   declare
      use type CCL.Language.Diagnostic_Code;
      Analysis : CCL.Language.Analysis_Result;
      Interpreted : CCL.Language.Interpretation_Result;
      function Image (N : CCL.Types.Name) return String renames CCL.Types.Image;
   begin
      CCL.Language.Analyze
        ("(Ld_Parameters output => (Input_File ""x"") inputs => [])", Catalog, Analysis);
      Check (CCL.Language.Analysis_Diagnostic (Analysis) = CCL.Language.Field_Type_Mismatch
             and then Image (CCL.Language.Analysis_Diagnostic_Subject (Analysis)) = "output"
             and then Image (CCL.Language.Analysis_Diagnostic_Expected (Analysis)) = "Output_File"
             and then Image (CCL.Language.Analysis_Diagnostic_Found (Analysis)) = "Input_File",
             "a field mismatch names the field and both types");
      CCL.Language.Analyze ("(ld.run (Input_File ""x""))", Catalog, Analysis);
      Check (CCL.Language.Analysis_Diagnostic (Analysis) = CCL.Language.Argument_Type_Mismatch
             and then Image (CCL.Language.Analysis_Diagnostic_Subject (Analysis)) = "ld.run"
             and then Image (CCL.Language.Analysis_Diagnostic_Expected (Analysis)) = "Ld_Parameters"
             and then Image (CCL.Language.Analysis_Diagnostic_Found (Analysis)) = "Input_File",
             "an argument mismatch names the operation and both types");
      CCL.Language.Interpret
        ("(ld.run (Ld_Parameters output => (Input_File ""x"") inputs => []))", 4096, Catalog, Interpreted);
      Put_Line (CCL.Sessions.Result_Value_Image (Interpreted));
      Check (CCL.Sessions.Result_Value_Image (Interpreted) =
               "Expression does not type-check: field output takes Output_File, not Input_File" &
               " at character 34",
             "the console's message for a field mismatch");
      CCL.Language.Interpret ("(ld.run (Input_File ""x""))", 4096, Catalog, Interpreted);
      Put_Line (CCL.Sessions.Result_Value_Image (Interpreted));
   end;

   --  A second program shares the file types and Run.
   declare
      As : PD.Signature;
      As_Bound : Programs.Contracts;
   begin
      As.Parameters (0) := (Of_Kind => PD.Output_File, Many => False, Optional => False,
                            Name => [others => ' '], Name_Length => 6);
      As.Parameters (0).Name (1 .. 6) := "output";
      As.Parameter_Total := 1;
      Programs.Publish (Catalog, Grants, 1, "as.app", As, As_Bound, Error);
      Check (Error = CCL.Catalog.Catalog_Valid, "as publishes beside ld: " & Error'Image);
      Check (Checks_Out ("(as.com.cubit.exit (as.run (As_Parameters output => (Output_File ""o""))))"),
             "as runs with the shared types");
      Programs.Publish (Catalog, Grants, 2, "ld.app", Description, Bound, Error);
      Check (Error /= CCL.Catalog.Catalog_Valid, "a program publishes once");
   end;

   Put_Line ("ccl-programs:" & Checks'Image & " checks," & Failures'Image & " failures");
   if Failures /= 0 then
      raise Program_Error;
   end if;
end Main;

with Ada.Text_IO;
with Interfaces; use Interfaces;
with CCL.Types; use CCL.Types;
with CCL.Objects; use CCL.Objects;
with CCL.Objects.Values;
with CCL.Language;
with CCL.Compiler;
with CCL.VM;
with CCL.Host_Values;
with CCL.Handler_References;
with Config_Objects;
with Config_Fixture;

procedure Value_Tests is
   use type CCL.Compiler.Compilation_Status;
   use type CCL.VM.Validation_Error;
   use type CCL.VM.Execution_Status;
   use type CCL.Host_Values.Value;
   use type CCL.Host_Values.Value_Kind;
   use type Config_Objects.Outcome;
   use type Config_Objects.Read_Result;
   Schema : constant Schema_Key := [1, 2, 3, 4];
   Contract : Binding;
   Object, Loaded : CCL.Objects.Image;
   Types : Registry;
   Ok : Boolean;
   Host : CCL.Host_Values.Value;
   Text : CCL.Host_Values.Text;
   VM_Copy : CCL.VM.Value;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean) is
   begin
      pragma Assert (Condition);
      Checks := Checks + 1;
   end Check;
   procedure Run (Source, Expected : String; Root_Name : String) is
      Analysis : CCL.Language.Analysis_Result;
      Compiled : CCL.Compiler.Compilation_Result;
      Program : CCL.VM.Validated_Program;
      Valid : CCL.VM.Validation_Error;
      Executed : CCL.VM.Execution_Result;
      Store : Config_Objects.State;
      Put : Config_Objects.Outcome;
      Get : Config_Objects.Read_Result;
      Saved_Revision : Unsigned_64;
   begin
      CCL.Language.Analyze (Source, Analysis);
      CCL.Compiler.Compile (Analysis, Compiled);
      Check (Compiled.Status = CCL.Compiler.Compilation_Succeeded);
      CCL.VM.Verify (Compiled.Program, Program, Valid); Check (Valid = CCL.VM.Valid);
      CCL.VM.Execute (Program, 4096, Executed); Check (Executed.Status = CCL.VM.Completed);
      Types := Compiled.Program.Data_Types;
      Bind (Types, Find (Types, Named (Root_Name)), Schema, Contract, Ok); Check (Ok);
      CCL.Objects.Values.From_VM (Contract, Types, Executed.Result_Value, Object, Ok); Check (Ok);
      Config_Objects.Initialize (Store, Contract, Ok); Check (Ok);
      Config_Fixture.Load_Empty (Store);
      Config_Fixture.Commit (Store, Object, 0, Put); Check (Put = Config_Objects.Published);
      Config_Objects.Read (Store, Schema, Loaded, Saved_Revision, Get);
      Check (Get = Config_Objects.Found and Saved_Revision = 1 and Loaded = Object);
      CCL.Objects.Values.To_VM (Contract, Types, Loaded, VM_Copy, Ok); Check (Ok);
      Check (CCL.VM.Value_Image (Types, VM_Copy) = Expected);
      VM_Copy.Copyable := False;
      CCL.Objects.Values.From_VM (Contract, Types, VM_Copy, Object, Ok); Check (not Ok);
      VM_Copy.Copyable := True;
      VM_Copy.Type_Tag := 1;
      CCL.Objects.Values.From_VM (Contract, Types, VM_Copy, Object, Ok); Check (not Ok);
   end Run;
   Prefix : constant String :=
     "(type Reading (variant (Value Integer) (Unavailable) (Flag Boolean))) ";
   procedure Compile_And_Run
     (Source : String; Local_Types : out Registry; Value : out CCL.VM.Value)
   is
      Analysis : CCL.Language.Analysis_Result;
      Compiled : CCL.Compiler.Compilation_Result;
      Program : CCL.VM.Validated_Program;
      Valid : CCL.VM.Validation_Error;
      Executed : CCL.VM.Execution_Result;
   begin
      CCL.Language.Analyze (Source, Analysis);
      CCL.Compiler.Compile (Analysis, Compiled);
      Check (Compiled.Status = CCL.Compiler.Compilation_Succeeded);
      CCL.VM.Verify (Compiled.Program, Program, Valid); Check (Valid = CCL.VM.Valid);
      CCL.VM.Execute (Program, 4096, Executed); Check (Executed.Status = CCL.VM.Completed);
      Local_Types := Compiled.Program.Data_Types;
      Value := Executed.Result_Value;
   end Compile_And_Run;
begin
   Run ("(+ 20 22)", " 42", "Integer");
   Run ("true", "true", "Boolean");
   Run (Prefix & "(Reading.Value 42)", "Reading.Value( 42)", "Reading");
   Run (Prefix & "Reading.Unavailable", "Reading.Unavailable", "Reading");
   Run (Prefix & "(Reading.Flag true)", "Reading.Flag(true)", "Reading");
   declare
      Approved, Shifted, Conflicting : Registry;
      Value, Restored : CCL.VM.Value;
      Saved : CCL.Objects.Image;
      use type CCL.VM.Value;
   begin
      Compile_And_Run (Prefix & "(Reading.Value 42)", Approved, Value);
      Bind (Approved, Value.Data_Type, Schema, Contract, Ok); Check (Ok);
      CCL.Objects.Values.From_VM (Contract, Approved, Value, Saved, Ok); Check (Ok);
      Compile_And_Run
        ("(type Unrelated (enum Other)) " & Prefix & "(Reading.Value 42)", Shifted, Value);
      Check (Value.Data_Type /= Root_Type (Contract));
      CCL.Objects.Values.From_VM (Contract, Shifted, Value, Object, Ok);
      Check (Ok and Object = Saved);
      CCL.Objects.Values.To_VM (Contract, Shifted, Saved, Restored, Ok);
      Check (Ok and Restored = Value);
      Check (CCL.VM.Value_Image (Shifted, Restored) = "Reading.Value( 42)");
      Value.Data_Type := Root_Type (Contract); -- local ID now denotes Unrelated!
      CCL.Objects.Values.From_VM (Contract, Shifted, Value, Object, Ok); Check (not Ok);

      for Conflict in 1 .. 3 loop
         Compile_And_Run
           ((case Conflict is
             when 1 => "(type Reading (variant (Value Boolean) (Unavailable) (Flag Boolean))) (Reading.Value true)",
             when 2 => "(type Different (variant (Value Integer) (Unavailable) (Flag Boolean))) (Different.Value 42)",
             when others => "(type Reading (variant (Unavailable) (Value Integer) (Flag Boolean))) (Reading.Value 42)"),
            Conflicting, Value);
         Check (Value.Data_Type = Root_Type (Contract)); -- same number, wrong meaning
         CCL.Objects.Values.From_VM (Contract, Conflicting, Value, Object, Ok); Check (not Ok);
         CCL.Objects.Values.To_VM (Contract, Conflicting, Saved, Restored, Ok);
         Check (not Ok and Restored = CCL.VM.Integer_Constant (0));
      end loop;
   end;
   Bind (Types, String_Type, Schema, Contract, Ok); Check (Ok);
   CCL.Host_Values.Copy_Text ("Cubie" & Character'Val (0) & Character'Val (255), Text, Ok); Check (Ok);
   Host := CCL.Host_Values.Text_Constant (Text);
   CCL.Objects.Values.From_Host (Contract, Host, Object, Ok); Check (Ok);
   declare
      Restored : constant CCL.Objects.Values.Host_Result := CCL.Objects.Values.To_Host (Contract, Object);
   begin
      Check (Restored.Available and then Restored.Value = Host);
   end;
   Host := CCL.Host_Values.Integer_Constant (42);
   CCL.Objects.Values.From_Host (Contract, Host, Object, Ok); Check (not Ok);
   declare
      Ref : CCL.Handler_References.Reference;
   begin
      CCL.Handler_References.Create ("(fn cb () 42)", "cb", Ref, Ok); Check (Ok);
      Host := CCL.Host_Values.Handler_Constant (Ref);
      CCL.Objects.Values.From_Host (Contract, Host, Object, Ok); Check (not Ok);
   end;
   declare
      Built : Build_Result;
   begin
      Object := Empty (Contract);
      Append_Text (Object, String'(1 .. Maximum_Text_Bytes => 'x'), Built); Check (Built = Added);
      Check (Validate (Object, Contract));
      declare
         Restored : constant CCL.Objects.Values.Host_Result := CCL.Objects.Values.To_Host (Contract, Object);
         Copy : CCL.Objects.Image;
      begin
         Check (Restored.Available and then Restored.Value.Kind = CCL.Host_Values.Object_Value);
         Check (CCL.Host_Values.Matches (Restored.Value, Contract));
         CCL.Objects.Values.From_Host (Contract, Restored.Value, Copy, Ok);
         Check (Ok and Copy = Object);
         Host := Restored.Value;
         Host.Object.Padding (1) := 1;
         CCL.Objects.Values.From_Host (Contract, Host, Copy, Ok); Check (not Ok);
         Host := Restored.Value;
         Host.Object.Schema (0) := Host.Object.Schema (0) + 1;
         CCL.Objects.Values.From_Host (Contract, Host, Copy, Ok); Check (not Ok);
      end;
   end;
   Ada.Text_IO.Put_Line ("CCL VM/host -> native object -> Config -> value: PASS" & Checks'Image & " checks");
end Value_Tests;

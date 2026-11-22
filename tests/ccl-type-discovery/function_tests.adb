with CCL.Evaluation;
with Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces;
with CCL.Language; use CCL.Language;
   use CCL.Evaluation;
with CCL.Language.Handlers;
with CCL.Catalog;
with CCL.Host_Values;
with CCL.VM;

procedure Function_Tests is
   use type Interfaces.Integer_64;
   use type CCL.VM.Value_Kind;
   Checks : Natural := 0;
   procedure Check (Good : Boolean) is
   begin
      Checks := Checks + 1;
      if not Good then
         raise Program_Error with "function boundary check" & Checks'Image;
      end if;
   end Check;
   function Name_Of (I : Natural) return String is
      Number : constant String := I'Image;
   begin
      return "f" & Number (Number'First + 1 .. Number'Last);
   end Name_Of;
   procedure Expect_Value (Source : String; Value : Interfaces.Integer_64) is
      Result : Interpretation_Result;
   begin
      Evaluate (Source, 4096, Result);
      if Result.Status /= Succeeded then
         Ada.Text_IO.Put_Line (Source);
         Ada.Text_IO.Put_Line (Result.Status'Image & " " & Result.Diagnostic'Image);
      end if;
      Check (Result.Status = Succeeded and then Result.Has_Value);
      Check (Result.Result_Value.Kind = CCL.VM.Integer_Value and then
             Result.Result_Value.Integer = Value);
   end Expect_Value;
   procedure Expect_Error (Source : String; Code : Diagnostic_Code) is
      Result : Analysis_Result;
      Executed : Interpretation_Result;
   begin
      Analyze (Source, Result);
      Check (Analysis_Status_Of (Result) /= Analysis_Succeeded);
      Check (Analysis_Diagnostic (Result) = Code);
      Evaluate (Source, 4096, Executed);
      Check (Executed.Status in Parse_Failed | Type_Check_Failed and then
             not Executed.Has_Value);
   end Expect_Error;
   procedure Reject_Exported_Handler (Expression : String) is
      Source : constant String := "  (define (f) Boolean true) " & Expression;
      Analyzed : Analysis_Result;
      Executed : Interpretation_Result;
   begin
      Analyze (Source, Analyzed);
      Check (Analysis_Status_Of (Analyzed) /= Analysis_Succeeded);
      Check (Analysis_Diagnostic (Analyzed) = Handler_Result_Not_Exportable);
      -- The diagnostic belongs to the program's result expression (after its
      -- declarations), not a nested handler node or an absent-node sentinel.
      -- Leading spaces are retained.
      Check (Analysis_Diagnostic_Position (Analyzed) = Source'Length - Expression'Length + 1);
      Evaluate (Source, 4096, Executed);
      Check (Executed.Status = Type_Check_Failed and then not Executed.Has_Value);
      Check (Executed.Diagnostic = Handler_Result_Not_Exportable);
      Check (Executed.Diagnostic_Position = Source'Length - Expression'Length + 1);
   end Reject_Exported_Handler;
   type Host_State is record
      Called : Boolean := False;
   end record;
   procedure No_Host
     (Context : in out Host_State; Binding : Interfaces.Unsigned_32;
      Argument : CCL.Host_Values.Value; Reply : out CCL.Host_Values.Call_Result)
   is
      pragma Unreferenced (Binding, Argument);
   begin
      Context.Called := True;
      Reply.Value := CCL.Host_Values.Boolean_Constant (False);
      Reply.Success := False;
   end No_Host;
   procedure Run_Handler is new CCL.Language.Handlers.Execute (Host_State, No_Host);
   Source : Unbounded_String;
begin
   -- Exercise every table size and the highest callable slot without using
   -- up the separate execution-depth budget. No forward/self calls are legal.
   for Count in 1 .. MAX_FUNCTIONS loop
      Source := Null_Unbounded_String;
      for I in 0 .. Count - 1 loop
         Append (Source, "(define (" & Name_Of (I) & ") Integer" &
                         Natural'Image (I + 1) & ") ");
      end loop;
      Expect_Value (To_String (Source) & "(" & Name_Of (Count - 1) & ")",
                    Interfaces.Integer_64 (Count));
      Expect_Value (To_String (Source) & "(f0)", 1);
   end loop;
   Expect_Error (To_String (Source) & "(define (extra) Integer 17) (extra)",
                 Too_Many_Functions);
   Expect_Error ("(define (f) Integer (f)) (f)", Unknown_Form);
   Expect_Error ("(define (f) Integer (g)) (define (g) Integer 1) (f)", Unknown_Form);
   Expect_Error ("(define (f) Integer 1) (define (f) Integer 2) (f)", Duplicate_Declaration);
   Expect_Error ("(define (f (x Integer) (x Integer)) Integer x) (f 1 2)", Duplicate_Declaration);
   Expect_Error ("(define (f) Integer true) (f)", Function_Result_Mismatch);
   Expect_Error ("(define (f (x Integer)) Integer x) (f)", Function_Arity_Mismatch);
   Expect_Error ("(define (f (x Integer)) Integer x) (f true)", Function_Argument_Mismatch);
   Expect_Error ("(define (f (x Integer)) Integer x) x", Unknown_Name);
   Expect_Error ("(define (f) Boolean true) (handler f)", Handler_Result_Not_Exportable);
   Reject_Exported_Handler ("(handler f)");
   Reject_Exported_Handler ("(let ((action (handler f))) action)");
   Reject_Exported_Handler ("(if true (handler f) (handler f))");
   Expect_Error ("(define (f) Integer 1) (handler f)", Invalid_Handler_Profile);
   -- Parameters and let bindings must not leak between caller/callee scopes.
   Expect_Value ("(define (f (x Integer)) Integer (+ x 1)) " &
                 "(let ((x 40)) (+ (f 1) x))", 42);
   Expect_Value ("(define (f (x Integer)) Integer x) " &
                 "(define (g (x Integer)) Integer (+ (f 2) x)) (g 40)", 42);
   -- Every incomplete prefix fails cleanly, including while a reserved slot's
   -- body is being parsed. A subsequent independent analysis remains usable.
   declare
      Full : constant String := "(define (f (x Integer)) Integer (+ x 1)) (f 41)";
      Result : Analysis_Result;
   begin
      for Last in 0 .. Full'Last - 1 loop
         Analyze (Full (1 .. Last), Result);
         Check (Analysis_Status_Of (Result) /= Analysis_Succeeded);
      end loop;
      Expect_Value (Full, 42);
   end;
   -- Retained callbacks select the last valid function slot, not the default
   -- slot or the main expression. They own the checked source after editing.
   declare
      use CCL.Language.Handlers;
      Catalog : CCL.Catalog.Interface_Catalog;
      Grants : CCL.Catalog.Granted_Bindings;
      Item : Handler;
      Status : Preparation_Status;
      Result : Interpretation_Result;
      Context : Host_State;
   begin
      Source := Null_Unbounded_String;
      for I in 0 .. MAX_FUNCTIONS - 1 loop
         Append (Source, "(define (" & Name_Of (I) & ") Boolean " &
           (if I = MAX_FUNCTIONS - 1 then "false" else "true") & ") ");
      end loop;
      Append (Source, "true");
      Prepare (To_String (Source), Name_Of (MAX_FUNCTIONS - 1), Boolean_Action,
               Catalog, Grants, Item, Status, Result);
      Check (Status = Prepared and then Ready (Item));
      Source := Null_Unbounded_String;
      Run_Handler (Item, 4096, Grants, Context, Result);
      Check (Result.Status = Succeeded and then Result.Has_Value);
      Check (Result.Result_Value.Kind = CCL.VM.Boolean_Value and then
             not Result.Result_Value.Boolean);
      Check (not Context.Called);
      Prepare ("(define (f) Boolean true) (missing)", "f", Boolean_Action,
               Catalog, Grants, Item, Status, Result);
      Check (Status = Invalid_Source and then not Ready (Item));
      Run_Handler (Item, 4096, Grants, Context, Result);
      Check (not Result.Has_Value and then not Context.Called);
   end;
   Ada.Text_IO.Put_Line ("PASS: function table boundaries and visibility" & Checks'Image & " checks");
end Function_Tests;

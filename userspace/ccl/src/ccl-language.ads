with CCL.VM;

package CCL.Language with
   SPARK_Mode => On
is

   MAX_SOURCE_LENGTH : constant := 1_024;
   MAX_AST_NODES     : constant := 128;
   MAX_NAME_LENGTH   : constant := 32;
   MAX_BINDINGS      : constant := 32;
   MAX_NESTING       : constant := 32;

   type Interpretation_Status is
     (Succeeded,
      Parse_Failed,
      Type_Check_Failed,
      Evaluation_Fuel_Exhausted,
      Evaluation_Overflow);

   type Diagnostic_Code is
     (No_Diagnostic,
      Source_Too_Long,
      Unexpected_End,
      Unexpected_Token,
      Unknown_Form,
      Expected_Close,
      Expected_Name,
      Invalid_Integer,
      Nesting_Too_Deep,
      AST_Full,
      Trailing_Input,
      Unknown_Name,
      Expected_Integer,
      Expected_Boolean,
      Branch_Type_Mismatch,
      Too_Many_Bindings);

   type Interpretation_Result is record
      Status         : Interpretation_Status := Parse_Failed;
      Diagnostic     : Diagnostic_Code := No_Diagnostic;
      --  One-based source position; zero means no source diagnostic.
      Diagnostic_Position : Natural range 0 .. MAX_SOURCE_LENGTH + 1 := 0;
      Has_Value      : Boolean := False;
      Result_Value   : CCL.VM.Value := (others => <>);
      Fuel_Remaining : Natural := 0;
   end record;

   procedure Interpret
     (Source : String;
      Fuel   : Natural;
      Result : out Interpretation_Result)
   with
      Post => Result.Fuel_Remaining <= Fuel;
end CCL.Language;

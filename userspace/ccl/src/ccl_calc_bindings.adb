with Calc_Balls;
with Calc_Engine;
with Calc_Format;
with Calc_Numbers;
with Calc_Syntax;
with CCL.Interfaces.Calc;
with CCL.Objects;
with CCL.Objects.Views;
with CCL_Calc_Options;
with CuBit.Failures;

package body CCL_Calc_Bindings is
   use Interfaces;
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Catalog.Grant_Result;
   use type CCL.Host_Values.Value_Kind;
   use type CCL.Objects.Build_Result;
   use type Calc_Engine.Failure_Kind;
   use type Calc_Engine.Number_Domain;
   package Calc renames CCL.Interfaces.Calc;
   package Failures renames CuBit.Failures;

   package Views renames CCL.Objects.Views;
   Contracts : Calc.Contracts;
   --  calc.evaluate and (calc ..): the app's standard mode.
   STANDARD : constant CCL_Calc_Options.Choice := (others => <>);
   LIMITS : constant Calc_Engine.Limits := (Budget => Calc.BUDGET, Precision => Calc.PRECISION);

   function Handles (Binding : Unsigned_32) return Boolean is
     (Binding in Calc.FIRST_BINDING .. Calc.Binding_Of (Calc.Operation'Last));

   procedure Install
     (Catalog : in out CCL.Catalog.Interface_Catalog;
      Grants : in out CCL.Catalog.Granted_Bindings; Success : out Boolean)
   is
      Error : CCL.Catalog.Catalog_Error;
      Grant : CCL.Catalog.Grant_Result;
      Resolved : CCL.Catalog.Resolved_Operation;
      Found : Boolean;
   begin
      Calc.Publish (Catalog, Error);
      Success := Error = CCL.Catalog.Catalog_Valid;
      if not Success then return; end if;
      CCL.Catalog.Resolve_Schema (Catalog, Calc.NUMBER_KEY, Contracts.Number);
      CCL.Catalog.Resolve_Schema (Catalog, Calc.REQUEST_KEY, Contracts.Request);
      Success := CCL.Objects.Is_Bound (Contracts.Number) and then CCL.Objects.Is_Bound (Contracts.Request);
      for Op in Calc.Operation loop
         exit when not Success;
         CCL.Catalog.Resolve (Catalog, "calc." & Calc.Name (Op), Resolved, Found);
         Success := Found;
         if Success then
            CCL.Catalog.Install (Grants, Resolved, Calc.Binding_Of (Op), Grant);
            Success := Grant = CCL.Catalog.Grant_Added;
         end if;
      end loop;
   end Install;

   function Image (Value : Natural) return String is
      Text : constant String := Natural'Image (Value);
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Image;

   --  Text evaluated with Options, as a Calc_Number or a typed failure.
   procedure Answer (Text : String; Options : CCL_Calc_Options.Choice; Reply : in out CCL.Host_Values.Call_Result) is
      Based : constant String (1 .. Text'Length) := Text;
      Mode : constant Calc_Engine.Options := CCL_Calc_Options.Engine_Options (Options);
      Nothing, Result : Calc_Balls.Ball;
      Failure : Calc_Engine.Failure_Kind;
      Position : Calc_Syntax.Column;
      Wrapped : Boolean;
   begin
      Calc_Engine.Evaluate (Based, Nothing, False, LIMITS, Mode, Result, Wrapped, Failure, Position);
      if Failure /= Calc_Engine.No_Failure then
         Reply.Why := Failures.Failed
           ((if Failure = Calc_Engine.Too_Large then Failures.Exhausted else Failures.Invalid_Argument),
            Calc_Engine.Message (Failure) & " (column " & Image (Position) & ")",
            (case Failure is
                when Calc_Engine.Too_Large =>
                  "keep every numerator and denominator under about 39,000 digits",
                when Calc_Engine.No_Previous_Result => "bind the earlier result to a name instead of ans",
                when Calc_Engine.Needs_Word_Size =>
                  "use calc.evaluate-with and Calc_Options with mode Programmer and a word size",
                when others => ""));
         return;
      end if;
      declare
         Mid : Calc_Numbers.Number;
         Programmer : constant Boolean := Mode.Domain = Calc_Engine.Integers;
      begin
         Calc_Balls.Midpoint (Result, Mid);
         declare
            Numerator, Denominator : String (1 .. Calc_Format.Digit_Bound (Mid));
            Decimal : String (1 .. Calc_Format.Ball_Bound (Result, Calc.PLACES));
            Bound : String (1 .. Calc_Format.BOUND_TEXT);
            Hexadecimal, Octal, Binary : String (1 .. (if Programmer then Calc_Format.Base_Bound (Mid) else 0));
            Numerator_Last, Denominator_Last, Decimal_Last, Bound_Last : Natural;
            Hexadecimal_Last, Octal_Last, Binary_Last : Natural := 0;
            Form : Calc_Format.Decimal_Form;
            Value : CCL.Objects.Image := CCL.Objects.Empty (Contracts.Number);
            Step : CCL.Objects.Build_Result := CCL.Objects.Added;
            procedure Put (Cell : CCL.Objects.Cell) is
            begin
               if Step = CCL.Objects.Added then
                  CCL.Objects.Append (Value, Cell, Step);
               end if;
            end Put;
            procedure Put_Text (Text : String) is
            begin
               if Step = CCL.Objects.Added then
                  CCL.Objects.Append_Text (Value, Text, Step);
               end if;
            end Put_Text;
         begin
            Calc_Format.Numerator_Text (Mid, Numerator, Numerator_Last);
            Calc_Format.Denominator_Text (Mid, Denominator, Denominator_Last);
            Calc_Format.Ball_Text (Result, Calc.PLACES, Decimal, Decimal_Last, Bound, Bound_Last, Form);
            if Programmer and then Calc_Numbers.Is_Integer (Mid) then
               Calc_Format.Base_Text (Mid, 16, Mode.Word, Hexadecimal, Hexadecimal_Last);
               Calc_Format.Base_Text (Mid, 8, Mode.Word, Octal, Octal_Last);
               Calc_Format.Base_Text (Mid, 2, Mode.Word, Binary, Binary_Last);
            end if;
            if Numerator_Last + Denominator_Last + Decimal_Last + Bound_Last + Hexadecimal_Last + Octal_Last
              + Binary_Last > CCL.Objects.Maximum_Text_Bytes
            then
               Reply.Why := Failures.Failed
                 (Failures.Exhausted,
                  "the result's text is" & Natural'Image (Numerator_Last + Denominator_Last + Decimal_Last
                    + Bound_Last + Hexadecimal_Last + Octal_Last + Binary_Last) &
                  " bytes; one CCL value holds" & Natural'Image (CCL.Objects.Maximum_Text_Bytes),
                  "the Calculator app shows results of any size");
            else
               Put (CCL.Objects.Product_Cell (Calc.NUMBER_FIELDS));
               Put (CCL.Objects.Boolean_Cell (Calc_Balls.Is_Exact (Result)));
               Put_Text (Numerator (1 .. Numerator_Last));
               Put_Text (Denominator (1 .. Denominator_Last));
               Put_Text (Decimal (1 .. Decimal_Last));
               Put (CCL.Objects.Variant_Cell (Calc_Format.Decimal_Form'Pos (Form) + 1));
               Put (CCL.Objects.Unit_Cell);
               Put_Text (Bound (1 .. Bound_Last));
               Put_Text (Hexadecimal (1 .. Hexadecimal_Last));
               Put_Text (Octal (1 .. Octal_Last));
               Put_Text (Binary (1 .. Binary_Last));
               Put (CCL.Objects.Boolean_Cell (Wrapped));
               if Step = CCL.Objects.Added and then CCL.Objects.Validate (Value, Contracts.Number) then
                  Reply := (Value => CCL.Host_Values.Object_Constant (Value), Success => True, Why => <>);
               else
                  Reply.Why := Failures.Failed (Failures.Exhausted, "the result does not fit one CCL value");
               end if;
            end if;
         end;
         Calc_Numbers.Free (Mid);
      end;
      Calc_Balls.Free (Result);
   end Answer;

   procedure Invoke
     (Binding : Unsigned_32; Argument : CCL.Host_Values.Value;
      Reply : out CCL.Host_Values.Call_Result)
   is
      Object : Views.Snapshot;
      Captured : Boolean := False;
      --  Fields of Calc_Request.
      EXPRESSION_FIELD : constant := 1;
      OPTIONS_FIELD : constant := 2;
   begin
      Reply := (Value => CCL.Host_Values.Integer_Constant (0), Success => False, Why => <>);
      if not Handles (Binding) then
         return;
      end if;
      case Calc.Operation'Val (Binding - Calc.FIRST_BINDING) is
         when Calc.Evaluate | Calc.Calc =>
            if Argument.Kind = CCL.Host_Values.Text_Value then
               Answer (Argument.Content.Data (1 .. Argument.Content.Length), STANDARD, Reply);
            end if;
         when Calc.Evaluate_With =>
            if Argument.Kind = CCL.Host_Values.Object_Value then
               Views.Capture (Object, Contracts.Request, Argument.Object, Captured);
            end if;
            if Captured then
               declare
                  Root : constant Views.Cursor := Views.Root (Object);
                  Text : constant String := Views.Text (Object, Views.Field (Object, Root, EXPRESSION_FIELD));
               begin
                  if Text'Length > CCL.Host_Values.Maximum_Text_Length then
                     Reply.Why := Failures.Failed
                       (Failures.Invalid_Argument, "the expression is longer than" &
                        Natural'Image (CCL.Host_Values.Maximum_Text_Length) & " characters");
                  else
                     Answer (Text, CCL_Calc_Options.Read (Object, Views.Field (Object, Root, OPTIONS_FIELD)), Reply);
                  end if;
               end;
            end if;
      end case;
   end Invoke;
end CCL_Calc_Bindings;

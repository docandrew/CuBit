with Calc_Engine;
with Calc_Format;
with Calc_Numbers;
with Calc_Syntax;
with CCL.Interfaces.Calc;
with CCL.Objects;
with CuBit.Failures;

package body CCL_Calc_Bindings is
   use Interfaces;
   use type CCL.Catalog.Catalog_Error;
   use type CCL.Catalog.Grant_Result;
   use type CCL.Host_Values.Value_Kind;
   use type CCL.Objects.Build_Result;
   use type Calc_Engine.Failure_Kind;
   use type Calc_Numbers.Exactness;
   package Calc renames CCL.Interfaces.Calc;
   package Failures renames CuBit.Failures;

   Contracts : Calc.Contracts;
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
      Success := CCL.Objects.Is_Bound (Contracts.Number);
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

   procedure Invoke
     (Binding : Unsigned_32; Argument : CCL.Host_Values.Value;
      Reply : out CCL.Host_Values.Call_Result)
   is
   begin
      Reply := (Value => CCL.Host_Values.Integer_Constant (0), Success => False, Why => <>);
      if not Handles (Binding) or else Argument.Kind /= CCL.Host_Values.Text_Value then
         return;
      end if;
      declare
         Text : constant String (1 .. Argument.Content.Length) := Argument.Content.Data (1 .. Argument.Content.Length);
         Nothing, Result : Calc_Numbers.Number;
         Failure : Calc_Engine.Failure_Kind;
         Position : Calc_Syntax.Column;
      begin
         Calc_Engine.Evaluate (Text, Nothing, False, LIMITS, Result, Failure, Position);
         if Failure /= Calc_Engine.No_Failure then
            Reply.Why := Failures.Failed
              ((if Failure = Calc_Engine.Too_Large then Failures.Exhausted else Failures.Invalid_Argument),
               Calc_Engine.Message (Failure) & " (column " & Image (Position) & ")",
               (case Failure is
                   when Calc_Engine.Too_Large =>
                     "keep every numerator and denominator under about 39,000 digits",
                   when Calc_Engine.No_Previous_Result => "bind the earlier result to a name instead of ans",
                   when others => ""));
            return;
         end if;
         declare
            Numerator, Denominator : String (1 .. Calc_Format.Digit_Bound (Result));
            Decimal : String (1 .. Calc_Format.Decimal_Bound (Result, Calc.PLACES));
            Numerator_Last, Denominator_Last, Decimal_Last : Natural;
            Form : Calc_Format.Decimal_Form;
            Value : CCL.Objects.Image := CCL.Objects.Empty (Contracts.Number);
            Step : CCL.Objects.Build_Result;
         begin
            Calc_Format.Numerator_Text (Result, Numerator, Numerator_Last);
            Calc_Format.Denominator_Text (Result, Denominator, Denominator_Last);
            Calc_Format.Decimal_Text (Result, Calc.PLACES, Decimal, Decimal_Last, Form);
            if Numerator_Last + Denominator_Last + Decimal_Last > CCL.Objects.Maximum_Text_Bytes then
               Reply.Why := Failures.Failed
                 (Failures.Exhausted,
                  "the result's text is" & Natural'Image (Numerator_Last + Denominator_Last + Decimal_Last) &
                  " bytes; one CCL value holds" & Natural'Image (CCL.Objects.Maximum_Text_Bytes),
                  "the Calculator app shows results of any size");
               Calc_Numbers.Free (Result);
               return;
            end if;
            CCL.Objects.Append (Value, CCL.Objects.Product_Cell (Calc.NUMBER_FIELDS), Step);
            if Step = CCL.Objects.Added then
               CCL.Objects.Append (Value, CCL.Objects.Boolean_Cell (Calc_Numbers.Kind (Result) = Calc_Numbers.Exact),
                                   Step);
            end if;
            if Step = CCL.Objects.Added then
               CCL.Objects.Append_Text (Value, Numerator (1 .. Numerator_Last), Step);
            end if;
            if Step = CCL.Objects.Added then
               CCL.Objects.Append_Text (Value, Denominator (1 .. Denominator_Last), Step);
            end if;
            if Step = CCL.Objects.Added then
               CCL.Objects.Append_Text (Value, Decimal (1 .. Decimal_Last), Step);
            end if;
            if Step = CCL.Objects.Added then
               CCL.Objects.Append (Value, CCL.Objects.Variant_Cell (Calc_Format.Decimal_Form'Pos (Form) + 1), Step);
            end if;
            if Step = CCL.Objects.Added then
               CCL.Objects.Append (Value, CCL.Objects.Unit_Cell, Step);
            end if;
            if Step = CCL.Objects.Added and then CCL.Objects.Validate (Value, Contracts.Number) then
               Reply := (Value => CCL.Host_Values.Object_Constant (Value), Success => True, Why => <>);
            else
               Reply.Why := Failures.Failed (Failures.Exhausted, "the result does not fit one CCL value");
            end if;
         end;
         Calc_Numbers.Free (Result);
      end;
   end Invoke;
end CCL_Calc_Bindings;

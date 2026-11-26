with AML_Decode;
with AML_Runtime_Exceptions;
package AML_Runtime_Error_Policy with SPARK_Mode, Pure is
   use type AML_Decode.Integer_Value;
   subtype Status_Code is AML_Runtime_Exceptions.Status_Code;
   -- Three actual arguments and the ordinary method dispatch visit. These
   -- charges are reserved only after an eligible precise error and handler.
   Dispatch_Charges : constant Natural := 4;
   type Admission_Action is (Keep_Original, Invoke_Handler, Fuel_Exhausted, Depth_Exhausted);
   type Admission_Plan is record
      Action : Admission_Action;
      Next_Charged : Natural;
   end record;
   function Eligible
     (Enabled, Tracking, Handler_Active, Statement_Complete, Handler_Valid : Boolean;
      Thread_Tag : AML_Decode.Integer_Value) return Boolean is
     (Enabled and then Tracking and then not Handler_Active and then
      Statement_Complete and then Handler_Valid and then Thread_Tag /= 0);
   function Admit
     (Enabled, Tracking, Handler_Active, Statement_Complete, Handler_Valid : Boolean;
      Thread_Tag : AML_Decode.Integer_Value;
      Calls_Left, Charged, Budget : Natural) return Admission_Plan
     with Global => null, Pre => Charged <= Budget,
       Post => Admit'Result.Next_Charged in Charged .. Budget
         and then (if not Eligible (Enabled, Tracking, Handler_Active,
           Statement_Complete, Handler_Valid, Thread_Tag) then
             Admit'Result.Action = Keep_Original and then Admit'Result.Next_Charged = Charged
           elsif Calls_Left = 0 then
             Admit'Result.Action = Depth_Exhausted and then Admit'Result.Next_Charged = Charged
           elsif Budget - Charged < Dispatch_Charges then
             Admit'Result.Action = Fuel_Exhausted and then Admit'Result.Next_Charged = Charged
           else Admit'Result.Action = Invoke_Handler and then
             Admit'Result.Next_Charged - Charged = Dispatch_Charges);
   type Resolution_Action is (Propagate, Continue_Statement);
   type Resolution is record
      Action : Resolution_Action;
      Code : Status_Code;
   end record;
   -- The caller must propagate resource/fatal/uncertain handler failures
   -- before using this routine. Only a completed integer return can replace
   -- the precise original exception; an absent/noninteger return preserves it.
   function Resolve
     (Original : Status_Code; Completed_Integer : Boolean;
      Value : AML_Decode.Integer_Value) return Resolution
     with Global => null, Pre => Original /= 0,
       Post => Resolve'Result.Code =
         (if Completed_Integer then AML_Runtime_Exceptions.Normalize (Value) else Original)
         and then (Resolve'Result.Action = Continue_Statement) = (Resolve'Result.Code = 0);
end AML_Runtime_Error_Policy;

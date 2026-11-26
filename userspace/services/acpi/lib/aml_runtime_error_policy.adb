package body AML_Runtime_Error_Policy with SPARK_Mode is
   function Admit
     (Enabled, Tracking, Handler_Active, Statement_Complete, Handler_Valid : Boolean;
      Thread_Tag : AML_Decode.Integer_Value;
      Calls_Left, Charged, Budget : Natural) return Admission_Plan is
   begin
      if not Eligible (Enabled, Tracking, Handler_Active, Statement_Complete,
                       Handler_Valid, Thread_Tag) then
         return (Keep_Original, Charged);
      elsif Calls_Left = 0 then return (Depth_Exhausted, Charged);
      elsif Budget - Charged < Dispatch_Charges then return (Fuel_Exhausted, Charged);
      else return (Invoke_Handler, Charged + Dispatch_Charges);
      end if;
   end Admit;
   function Resolve
     (Original : Status_Code; Completed_Integer : Boolean;
      Value : AML_Decode.Integer_Value) return Resolution is
      Code : constant Status_Code :=
        (if Completed_Integer then AML_Runtime_Exceptions.Normalize (Value) else Original);
   begin
      return (if Code = 0 then (Continue_Statement, Code) else (Propagate, Code));
   end Resolve;
end AML_Runtime_Error_Policy;

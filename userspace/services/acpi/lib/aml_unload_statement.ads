with AML_Decode;
package AML_Unload_Statement with SPARK_Mode, Pure is
   type Action is (Evaluate_Operand, Complete_Invalid_Handle,
                   Invalid_Operand, Truncated, Budget_Exceeded);
   type Plan is record
      Next_Offset, Next_Charged : Natural;
      Outcome : Action;
   end record;
   -- Offset is the extended-opcode byte, after the ordinary statement charge.
   -- This routine performs no namespace operation or credential admission.
   function Prepare
     (Code : AML_Decode.Bytes; Offset, Limit, Charged, Budget : Natural) return Plan
     with Global => null,
       Pre => Offset <= Limit and then Limit <= Code'Length and then Charged <= Budget,
       Post => Prepare'Result.Next_Offset in Offset .. Limit
         and then Prepare'Result.Next_Charged in Charged .. Budget
         and then (if Prepare'Result.Outcome = Evaluate_Operand then
           Prepare'Result.Next_Offset < Limit and then
           Prepare'Result.Next_Charged = Charged)
         and then (if Prepare'Result.Outcome = Complete_Invalid_Handle then
           Prepare'Result.Next_Offset - Offset = 3 and then
           Prepare'Result.Next_Charged - Charged = 2)
         and then (if Prepare'Result.Outcome /= Complete_Invalid_Handle then
           Prepare'Result.Next_Charged = Charged);
end AML_Unload_Statement;

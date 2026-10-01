pragma Ada_2022;
with AML_Decode;
package AML_Execute with SPARK_Mode, Pure is
   type Arguments is array (Natural range 0 .. 6) of AML_Decode.Integer_Value;
   type Execution_Status is
     (Returned, No_Return, Truncated, Unsupported, Uninitialized,
      Missing_Argument, Budget_Exceeded);
   type Execution_Result (Status : Execution_Status := No_Return) is record
      Charged : Natural;
      case Status is
         when Returned => Value : AML_Decode.Integer_Value;
         when others => null;
      end case;
   end record;
   --  Raw method-body core: integer literals, Arg0..6, Local0..7, Store to
   --  locals, Noop and Return. No namespace, callbacks, synchronization or I/O.
   --  Charge one unit before each statement and each source operand.
   function Run
     (Code : AML_Decode.Bytes; Width : AML_Decode.Integer_Width;
      Args : Arguments; Argument_Count : Natural; Budget : Natural)
      return Execution_Result
     with Pre => Argument_Count <= 7,
          Post => Run'Result.Charged <= Budget;
end AML_Execute;

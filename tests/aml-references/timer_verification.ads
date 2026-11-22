with Test_Namespace;
with AML_Clock;
with AML_Execute;
with AML_Decode;
package Timer_Verification with SPARK_Mode is
   type Clock_State is limited record
      Arena : Test_Namespace.Owned.Arena;
      Microseconds : AML_Decode.Integer_Value := 0;
      Adapter : AML_Clock.State := AML_Clock.Fresh;
      Available : Boolean := True;
      Active : Natural range 0 .. 33 := 0;
      Reads : Natural := 0;
   end record;
   procedure Run
     (Code : AML_Decode.Bytes; Width : AML_Decode.Integer_Width;
      Budget : Natural; Clock : in out Clock_State;
      Result : out AML_Execute.Execution_Result;
      Args : AML_Execute.Value_Arguments; Argument_Count : Natural)
     with Pre => not Result'Constrained and Clock.Active = 0 and Argument_Count <= 7
        and Test_Namespace.Owned.Valid (Clock.Arena),
          Post => Result.Charged <= Budget;
end Timer_Verification;

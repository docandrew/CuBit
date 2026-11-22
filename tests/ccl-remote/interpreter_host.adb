package body Interpreter_Host with SPARK_Mode is
   use Interfaces;
   use type CCL.VM.Value_Kind;
   procedure Invoke
     (Context : in out State; Binding : Unsigned_32;
      Argument : CCL.VM.Value; Value : out CCL.VM.Value; Success : out Boolean) is
   begin
      Context.Calls := Context.Calls + 1;
      Success := not Context.Fail and then Binding = 77 and then
        Argument.Kind = CCL.VM.Integer_Value and then Argument.Integer = 0;
      Value := (if Context.Wrong_Type then CCL.VM.Boolean_Constant (True)
                else CCL.VM.Integer_Constant (3_661_000 + Integer_64 (Context.Calls - 1)));
   end Invoke;
end Interpreter_Host;

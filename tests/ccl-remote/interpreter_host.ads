with Interfaces;
with CCL.VM;
with CCL.Language;
with CCL.Periodic_Programs;
package Interpreter_Host with SPARK_Mode is
   type State is record
      Calls : Interfaces.Unsigned_32 := 0;
      Fail, Wrong_Type : Boolean := False;
      Tick : Interfaces.Unsigned_64 := 0;
   end record;
   procedure Invoke
     (Context : in out State; Binding : Interfaces.Unsigned_32;
      Argument : CCL.VM.Value; Value : out CCL.VM.Value; Success : out Boolean);
   procedure Evaluate is new CCL.Language.Interpret_With_Host (State, Invoke);
   function Now (Context : State) return Interfaces.Unsigned_64 is (Context.Tick);
   procedure Pump is new CCL.Periodic_Programs.Evaluate_Due (State, Now, Invoke);
end Interpreter_Host;

with Interfaces;
with CCL.Ownership;
with CCL.Imports;
with CCL.Bounded_Stacks;
with CCL.Execution_Budgets;

package CCL.VM with
   SPARK_Mode => On
is
   use Interfaces;

   MAX_INSTRUCTIONS : constant := 256;
   MAX_STACK_DEPTH  : constant := 64;
   MAX_IMPORTS      : constant := 16;

   type Instruction_Index is mod MAX_INSTRUCTIONS;
   type Stack_Index is range 0 .. MAX_STACK_DEPTH - 1;
   type Program_Length is range 0 .. MAX_INSTRUCTIONS;
   type Stack_Depth is range 0 .. MAX_STACK_DEPTH;
   subtype Import_Index is Natural range 0 .. MAX_IMPORTS - 1;
   subtype Import_Count is Natural range 0 .. MAX_IMPORTS;
   subtype Local_Count is Natural range 0 .. CCL.Ownership.MAX_BINDINGS;
   subtype Type_Count is Natural range 0 .. CCL.Ownership.MAX_TYPES;
   type Local_Type_Array is
     array (CCL.Ownership.Binding_Id) of CCL.Ownership.Type_Id;

   type Value_Kind is (Integer_Value, Boolean_Value);
   for Value_Kind use (Integer_Value => 0, Boolean_Value => 1);
   for Value_Kind'Size use 8;

   type Value is record
      Kind    : Value_Kind := Integer_Value;
      Integer : Integer_64 := 0;
      Boolean : Standard.Boolean := False;
      Type_Tag : CCL.Ownership.Type_Id := 0;
   end record;

   function Integer_Constant (Item : Integer_64) return Value is
     ((Kind => Integer_Value, Integer => Item, Boolean => False, Type_Tag => 0));

   function Boolean_Constant (Item : Standard.Boolean) return Value is
     ((Kind => Boolean_Value, Integer => 0, Boolean => Item, Type_Tag => 0));

   function With_Type
     (Item : Value; Type_Tag : CCL.Ownership.Type_Id) return Value is
     ((Kind => Item.Kind, Integer => Item.Integer, Boolean => Item.Boolean,
       Type_Tag => Type_Tag));

   type Local_Value_Array is
     array (CCL.Ownership.Binding_Id) of Value;
   type Local_Kind_Array is
     array (CCL.Ownership.Binding_Id) of Value_Kind;

   type Op_Code is
     (Halt,
      Push_Integer,
      Push_Boolean,
      Add_Integer,
      Equal_Integer,
      Not_Boolean,
      Drop,
      Jump,
      Jump_If_False,
      Invoke_Import,
      Copy_Local,
      Move_Local,
      Drop_Local,
      Borrow_Local_RO,
      Return_Local_RO,
      Borrow_Local_RW,
      Return_Local_RW,
      Apply_Local_Disposition,
      Initialize_Local,
      Multiply_Integer,
      Divide_Integer,
      Modulo_Integer);
   for Op_Code use
     (Halt                    => 0,
      Push_Integer            => 1,
      Push_Boolean            => 2,
      Add_Integer             => 3,
      Equal_Integer           => 4,
      Not_Boolean             => 5,
      Drop                    => 6,
      Jump                    => 7,
      Jump_If_False           => 8,
      Invoke_Import           => 9,
      Copy_Local              => 10,
      Move_Local              => 11,
      Drop_Local              => 12,
      Borrow_Local_RO         => 13,
      Return_Local_RO         => 14,
      Borrow_Local_RW         => 15,
      Return_Local_RW         => 16,
      Apply_Local_Disposition => 17,
      Initialize_Local        => 18,
      Multiply_Integer        => 19,
      Divide_Integer          => 20,
      Modulo_Integer          => 21);
   for Op_Code'Size use 8;

   type Authority_Class is
     (No_Authority,
      Observe_Authority,
      Control_Authority,
      Secret_Use_Authority,
      Network_Authority);
   for Authority_Class use
     (No_Authority         => 0,
      Observe_Authority    => 1,
      Control_Authority    => 2,
      Secret_Use_Authority => 3,
      Network_Authority    => 4);
   for Authority_Class'Size use 8;

   type Import_Declaration is record
      Argument  : Value_Kind := Integer_Value;
      Result    : Value_Kind := Integer_Value;
      Authority : Authority_Class := No_Authority;
      Binding   : Unsigned_32 := 0;
      Ownership_Argument : Boolean := False;
      Local       : CCL.Ownership.Binding_Id := 0;
      Transfer    : CCL.Imports.Transfer_Mode := CCL.Imports.Copy_Argument;
      Cancellation : CCL.Imports.Cancellation_Mode :=
        CCL.Imports.Not_Cancellable;
      Success_Verb : CCL.Ownership.Disposition_Id := 0;
      Failure_Verb : CCL.Ownership.Disposition_Id := 0;
      Cancel_Verb  : CCL.Ownership.Disposition_Id := 0;
   end record;

   type Import_Array is array (Import_Index) of Import_Declaration;

   type Instruction is record
      Op        : Op_Code := Halt;
      Immediate : Integer_64 := 0;
      Target    : Instruction_Index := 0;
      Import    : Import_Index := 0;
      Local     : CCL.Ownership.Binding_Id := 0;
      Verb      : CCL.Ownership.Disposition_Id := 0;
   end record;

   type Instruction_Array is array (Instruction_Index) of Instruction;

   type Program is record
      Length : Program_Length := 0;
      Code   : Instruction_Array := [others => (others => <>)];
      Imports_Length : Import_Count := 0;
      Imports : Import_Array := [others => (others => <>)];
      Locals_Length : Local_Count := 0;
      Dynamic_Locals_Length : Local_Count := 0;
      Types_Length : Type_Count := 0;
      Local_Types : Local_Type_Array := [others => 0];
      Local_Kinds : Local_Kind_Array := [others => Integer_Value];
      Types : CCL.Ownership.Type_Table := [others => (others => <>)];
   end record;

   type Validation_Error is
     (Valid,
      Empty_Program,
      Unreachable_Instruction,
      Missing_Halt,
      Invalid_Jump_Target,
      Backward_Jump,
      Stack_Underflow,
      Stack_Overflow,
      Type_Mismatch,
      Inconsistent_Stack,
      Invalid_Import,
      Invalid_Ownership);

   type Validated_Program is private;

   procedure Verify
     (Candidate : Program;
      Result    : out Validated_Program;
      Error     : out Validation_Error)
   with
      Post =>
        (if Error = Valid then Is_Valid (Result));

   function Is_Valid (Item : Validated_Program) return Boolean;

   type Execution_Status is
     (Completed,
      Paused,
      Stopped,
      Fuel_Exhausted,
      Arithmetic_Overflow,
      Division_By_Zero,
      Invalid_Bytecode,
      Waiting_For_Host,
      Host_Call_Failed,
      No_Result);

   type Execution_Result is record
      Status         : Execution_Status := No_Result;
      Has_Value      : Boolean := False;
      Result_Value   : Value := (others => <>);
      Fuel_Remaining : Unsigned_32 := 0;
      Steps          : Unsigned_32 := 0;
      Requested_Import : Import_Index := 0;
      Request_Argument : Value := (others => <>);
      Requested_Authority : Authority_Class := No_Authority;
      Requested_Binding   : Unsigned_32 := 0;
   end record;

   type Machine_State is private;

   function Is_Well_Formed
     (Item : Validated_Program; State : Machine_State) return Boolean;

   function Fuel_Limit (State : Machine_State) return Unsigned_32;

   procedure Initialize
     (Item  : Validated_Program;
      Fuel  : Natural;
      State : out Machine_State)
   with
     Pre => Is_Valid (Item),
     Post => Is_Well_Formed (Item, State) and then
       Fuel_Limit (State) = Unsigned_32 (Fuel);

   procedure Initialize_With_Locals
     (Item     : Validated_Program;
      Fuel     : Natural;
      Values   : Local_Value_Array;
      Count    : Local_Count;
      State    : out Machine_State;
      Accepted : out Boolean)
   with
     Pre => Is_Valid (Item),
     Post =>
       (if Accepted then
          Is_Well_Formed (Item, State) and then
          Fuel_Limit (State) = Unsigned_32 (Fuel));

   procedure Continue_Execution
     (Item   : Validated_Program;
      State  : in out Machine_State;
      Result : out Execution_Result)
   with
     Pre => Is_Valid (Item) and then Is_Well_Formed (Item, State),
     Post => Is_Well_Formed (Item, State) and then
       Fuel_Limit (State) = Fuel_Limit (State'Old) and then
       Result.Steps <= Fuel_Limit (State);

   procedure Continue_Execution_For
     (Item         : Validated_Program;
      State        : in out Machine_State;
      Instructions : Natural;
      Result       : out Execution_Result)
   with
     Pre => Is_Valid (Item) and then Is_Well_Formed (Item, State),
     Post => Is_Well_Formed (Item, State) and then
       Fuel_Limit (State) = Fuel_Limit (State'Old) and then
       Result.Steps <= Fuel_Limit (State);

   type Machine_Snapshot is record
      Instruction : Instruction_Index := 0;
      Fuel_Remaining : Unsigned_32 := 0;
      Steps : Unsigned_32 := 0;
      Waiting : Boolean := False;
      Terminal : Boolean := False;
      Status : Execution_Status := No_Result;
   end record;

   function Snapshot (State : Machine_State) return Machine_Snapshot;

   type Stack_Snapshot_Array is array (Stack_Index) of Value;

   type Local_Inspection is record
      Value           : CCL.VM.Value := (others => <>);
      Kind            : Value_Kind := Integer_Value;
      Type_Tag        : CCL.Ownership.Type_Id := 0;
      Mode            : CCL.Ownership.Ownership_Mode :=
        CCL.Ownership.Unrestricted;
      Ownership_State : CCL.Ownership.Binding_State :=
        CCL.Ownership.Not_Declared;
      Read_Borrows    : CCL.Ownership.Borrow_Count := 0;
      Write_Borrow    : Boolean := False;
   end record;

   type Local_Inspection_Array is
     array (CCL.Ownership.Binding_Id) of Local_Inspection;

   type Inspection_Snapshot is record
      Machine       : Machine_Snapshot;
      Stack_Length  : Stack_Depth := 0;
      --  Position zero is the current operand-stack top.
      Stack         : Stack_Snapshot_Array := [others => (others => <>)];
      Locals_Length : Local_Count := 0;
      Locals        : Local_Inspection_Array := [others => (others => <>)];
      Waiting_Import : Import_Index := 0;
      Waiting_Result_Kind : Value_Kind := Integer_Value;
      Waiting_Argument : Value := (others => <>);
      Waiting_Owned : Boolean := False;
      Import_Phase : CCL.Imports.Import_Phase := CCL.Imports.Import_Idle;
   end record;

   procedure Inspect
     (Item   : Validated_Program;
      State  : Machine_State;
      Result : out Inspection_Snapshot)
   with
      Pre => Is_Valid (Item) and then Is_Well_Formed (Item, State);

   procedure Stop (State : in out Machine_State);

   procedure Complete_Host_Call
     (Item     : Validated_Program;
      State    : in out Machine_State;
      Response : Value;
      Accepted : Boolean)
   with
     Pre => Is_Valid (Item) and then Is_Well_Formed (Item, State),
     Post => Is_Well_Formed (Item, State);

   --  Must be called after the host attempts to enqueue an owned import.
   --  Rejection preserves ownership; acceptance activates move/borrow state.
   procedure Acknowledge_Host_Submission
     (Item     : Validated_Program;
      State    : in out Machine_State;
      Accepted : Boolean)
   with
     Pre => Is_Valid (Item) and then Is_Well_Formed (Item, State),
     Post => Is_Well_Formed (Item, State);

   procedure Execute
     (Item   : Validated_Program;
      Fuel   : Natural;
      Result : out Execution_Result)
   with
      Pre => Is_Valid (Item),
      Post => Result.Steps <= Unsigned_32 (Fuel);

private
   type Runtime_Stack_Index is mod MAX_STACK_DEPTH;
   package Runtime_Stacks is new CCL.Bounded_Stacks
     (Index_Type    => Runtime_Stack_Index,
      Element_Type  => Value,
      Default_Value => (others => <>));

   type Validated_Program is record
      Checked : Boolean := False;
      Content : Program;
   end record;

   type Machine_State is record
      Stack               : Runtime_Stacks.Stack;
      PC                  : Instruction_Index := 0;
      Execution_Budget    : CCL.Execution_Budgets.Budget;
      Waiting             : Boolean := False;
      Waiting_Import      : Import_Index := 0;
      Waiting_Result_Kind : Value_Kind := Integer_Value;
      Waiting_Argument    : Value := (others => <>);
      Waiting_Owned       : Boolean := False;
      Import_Lifecycle    : CCL.Imports.Lifecycle;
      Terminal            : Boolean := False;
      Terminal_Status     : Execution_Status := No_Result;
      Has_Value           : Boolean := False;
      Result_Value        : Value := (others => <>);
      Ownership           : CCL.Ownership.Environment;
      Locals              : Local_Value_Array := [others => (others => <>)];
   end record;

   function Is_Valid (Item : Validated_Program) return Boolean is
     (Item.Checked and then Item.Content.Length > 0 and then
      Item.Content.Dynamic_Locals_Length <= Item.Content.Locals_Length);

   function Fuel_Limit (State : Machine_State) return Unsigned_32 is
     (CCL.Execution_Budgets.Limit (State.Execution_Budget));

   function Is_Well_Formed
     (Item : Validated_Program; State : Machine_State) return Boolean is
     ((not State.Waiting or else
       Natural (State.Waiting_Import) < Item.Content.Imports_Length) and then
      (not State.Waiting_Owned or else
       (State.Waiting and then
        CCL.Imports.Phase (State.Import_Lifecycle) in
          CCL.Imports.Import_Offered | CCL.Imports.Import_Accepted)) and then
      (State.Waiting_Owned or else
       CCL.Imports.Phase (State.Import_Lifecycle) not in
         CCL.Imports.Import_Offered | CCL.Imports.Import_Accepted));
end CCL.VM;

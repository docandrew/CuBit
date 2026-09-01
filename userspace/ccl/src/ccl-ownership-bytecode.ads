package CCL.Ownership.Bytecode with
   SPARK_Mode => On
is
   MAX_CODE : constant := 256;
   type Code_Index is mod MAX_CODE;
   subtype Code_Length is Natural range 0 .. MAX_CODE;
   subtype Binding_Count is Natural range 0 .. MAX_BINDINGS;

   type Op_Code is
     (No_Ownership_Op,
      Halt,
      Initialize_Local,
      Copy_Local,
      Move_Local,
      Drop_Local,
      Borrow_Local_RO,
      Return_Local_RO,
      Borrow_Local_RW,
      Return_Local_RW,
      Apply_Local_Disposition,
      Import_Local,
      Jump,
      Jump_If);

   type Import_Transfer_Mode is
     (Copy_Argument, Move_Argument, Borrowed_RO_Argument,
      Borrowed_RW_Argument);

   type Instruction is record
      Op      : Op_Code := Halt;
      Local   : Binding_Id := 0;
      Verb    : Disposition_Id := 0;
      Import_Mode  : Import_Transfer_Mode := Copy_Argument;
      Success_Verb : Disposition_Id := 0;
      Failure_Verb : Disposition_Id := 0;
      Target  : Code_Index := 0;
   end record;

   type Instruction_Array is array (Code_Index) of Instruction;
   type Local_Type_Array is array (Binding_Id) of Type_Id;

   type Program is record
      Length        : Code_Length := 0;
      Code          : Instruction_Array := [others => (others => <>)];
      Locals_Length : Binding_Count := 0;
      Dynamic_Locals_Length : Binding_Count := 0;
      Local_Types   : Local_Type_Array := [others => 0];
      Types         : Type_Table := [others => (others => <>)];
   end record;

   type Verification_Error is
     (Bytecode_Valid,
      Empty_Program,
      Invalid_Target,
      Backward_Jump,
      Missing_Halt,
      Unreachable_Instruction,
      Invalid_Local,
      Ownership_Failure,
      Ownership_Join_Failure);

   type Verification_Result is record
      Error           : Verification_Error := Bytecode_Valid;
      Ownership_Error : CCL.Ownership.Ownership_Error := Ownership_Valid;
      Position        : Code_Index := 0;
   end record;

   procedure Verify
     (Candidate : Program;
      Result    : out Verification_Result);
end CCL.Ownership.Bytecode;

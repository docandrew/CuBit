with CCL.Types;
package CCL.Ownership.Bytecode with
   SPARK_Mode => On
is
   --  A named ceiling (CAPACITY): the most instructions one program may
   --  hold. A program's storage is sized by what it holds (Program's
   --  discriminant), never by this.
   CODE_CEILING : constant := 2 ** 20;
   type Code_Index is mod CODE_CEILING;
   subtype Code_Length is Natural range 0 .. CODE_CEILING;
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
      Jump_If,
      Switch);

   type Import_Transfer_Mode is
     (Copy_Argument, Move_Argument, Borrowed_RO_Argument,
      Borrowed_RW_Argument);

   type Switch_Targets is array (CCL.Types.Component_Index) of Code_Index;
   type Instruction is record
      Op      : Op_Code := Halt;
      Local   : Binding_Id := 0;
      Verb    : Disposition_Id := 0;
      Import_Mode  : Import_Transfer_Mode := Copy_Argument;
      Success_Verb : Disposition_Id := 0;
      Failure_Verb : Disposition_Id := 0;
      Target  : Code_Index := 0;
      Target_Count : CCL.Types.Component_Count := 0;
      Targets : Switch_Targets := [others => 0];
   end record;

   type Instruction_Array is array (Code_Index range <>) of Instruction;
   type Local_Type_Array is array (Binding_Id) of Type_Id;

   --  The room the VM's default program has (CCL.VM.DEFAULT_INSTRUCTIONS).
   DEFAULT_CODE : constant := 256;
   --  Code (0 .. Last): room for Last + 1 instructions, Length of them used.
   type Program (Last : Code_Index) is record
      Length        : Code_Length := 0;
      Code          : Instruction_Array (0 .. Last) := [others => (others => <>)];
      Locals_Length : Binding_Count := 0;
      Dynamic_Locals_Length : Binding_Count := 0;
      Local_Types   : Local_Type_Array := [others => 0];
      Types         : Type_Table := [others => (others => <>)];
   end record;

   DEFAULT_LAST : constant Code_Index := DEFAULT_CODE - 1;
   subtype Default_Program is Program (Last => DEFAULT_LAST);

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

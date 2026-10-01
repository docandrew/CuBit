with CCL.Text_Operations;
with CCL.Ownership.Bytecode;
with CCL.Checked_Arithmetic;

package body CCL.VM with
   SPARK_Mode => On
is
   type Stack_Type is record
      Kind : Value_Kind := Integer_Value;
      Data_Type : CCL.Types.Type_Reference := CCL.Types.Invalid_Type;
      Copyable : Boolean := True;
      Type_Tag : CCL.Ownership.Type_Id := 0;
   end record;

   function Value_Image (Types : CCL.Types.Registry; Item : Value) return String is
      D : constant CCL.Types.Description := CCL.Types.Describe (Types, Item.Data_Type);
   begin
      if not Well_Typed (Types, Item) then return "<invalid value>"; end if;
      case Item.Kind is
         when Integer_Value => return Integer_64'Image (Item.Integer);
         when Boolean_Value => return (if Item.Boolean then "true" else "false");
         when Object_Value => return "<native object>";
         when Resource_Value => return "<resource " & CCL.Types.Image (D.Identifier) & ">";
         --  The characters live in the run's region (Execution_Result).
         when Text_Value => return "<text>";
         when Variant_Value =>
            return CCL.Types.Image (D.Identifier) & "." &
              CCL.Types.Image (D.Parts (Item.Alternative).Identifier) &
              (case D.Parts (Item.Alternative).Payload is
                when CCL.Types.Integer_Type => "(" & Integer_64'Image (Item.Integer) & ")",
                when CCL.Types.Boolean_Type => (if Item.Boolean then "(true)" else "(false)"),
                when others => "");
      end case;
   end Value_Image;
   use type CCL.Ownership.Bytecode.Verification_Error;
   use type CCL.Ownership.Ownership_Error;
   use type CCL.Ownership.Ownership_Mode;
   use type CCL.Imports.Import_Error;
   use type CCL.Imports.Import_Phase;
   use type CCL.Imports.Cancellation_Mode;
   use type CCL.Imports.Transfer_Mode;
   type Abstract_Stack_Index is mod MAX_STACK_DEPTH;
   package Abstract_Stacks is new CCL.Bounded_Stacks
     (Index_Type    => Abstract_Stack_Index,
      Element_Type  => Stack_Type,
      Default_Value => (others => <>));
   use type Abstract_Stacks.Operation_Result;
   use type Abstract_Stacks.Stack;
   use type Runtime_Stacks.Operation_Result;
   use type CCL.Execution_Budgets.Consume_Result;
   use type CCL.Checked_Arithmetic.Arithmetic_Error;

   type Abstract_State is record
      Seen  : Boolean := False;
      Values : Abstract_Stacks.Stack;
   end record;

   type State_Table is array (Instruction_Index) of Abstract_State;

   procedure Merge_State
     (States : in out State_Table;
      Target : Instruction_Index;
      Source : Abstract_State;
      Error  : in out Validation_Error)
   is
   begin
      if Error /= Valid then
         null;
      elsif not States (Target).Seen then
         States (Target) := Source;
         States (Target).Seen := True;
      elsif States (Target).Values /= Source.Values then
         Error := Inconsistent_Stack;
      end if;
   end Merge_State;

   procedure Push_Kind
     (State : in out Abstract_State;
      Kind  : Value_Kind;
      Error : in out Validation_Error;
      Data_Type : CCL.Types.Type_Reference := CCL.Types.Invalid_Type;
      Copyable : Boolean := True;
      Type_Tag : CCL.Ownership.Type_Id := 0)
   is
      Stack_Result : Abstract_Stacks.Operation_Result;
   begin
      if Error /= Valid then
         null;
      else
         Abstract_Stacks.Push (State.Values, (Kind, Data_Type, Copyable, Type_Tag), Stack_Result);
         if Stack_Result /= Abstract_Stacks.Stack_Ok then
            Error := Stack_Overflow;
         end if;
      end if;
   end Push_Kind;

   procedure Pop_Kind
     (State    : in out Abstract_State;
      Expected : Value_Kind;
      Error    : in out Validation_Error;
      Data_Type : CCL.Types.Type_Reference := CCL.Types.Invalid_Type)
   is
      Actual       : Stack_Type;
      Ignored      : Stack_Type;
      Stack_Result : Abstract_Stacks.Operation_Result;
   begin
      if Error /= Valid then
         null;
      else
         Abstract_Stacks.Peek_Top
           (State.Values, Actual, Stack_Result);
         if Stack_Result /= Abstract_Stacks.Stack_Ok then
            Error := Stack_Underflow;
         elsif Actual.Kind /= Expected or else Actual.Data_Type /= Data_Type then
            Error := Type_Mismatch;
         elsif not Actual.Copyable then
            -- Moved ownership values may be returned, but cannot be laundered
            -- through arithmetic, scalar imports, or a new unrestricted local.
            Error := Invalid_Ownership;
         else
            Abstract_Stacks.Pop (State.Values, Ignored, Stack_Result);
            if Stack_Result /= Abstract_Stacks.Stack_Ok then
               Error := Stack_Underflow;
            end if;
         end if;
      end if;
   end Pop_Kind;

   package T renames CCL.Text_Operations;
   use type T.Operation;
   use type T.Outcome;
   use type T.Operand_Kind;

   procedure Find_Operation
     (Immediate : Integer_64; Item : out T.Operation; Found : out Boolean) is
   begin
      Item := T.Operation'First;
      Found := False;
      for Candidate in T.Operation loop
         if Integer_64 (T.Operation'Enum_Rep (Candidate)) = Immediate then
            Item := Candidate;
            Found := True;
         end if;
      end loop;
   end Find_Operation;

   function Kind_Of (Kind : T.Operand_Kind) return Value_Kind is
     (case Kind is
         when T.Text_Operand => Text_Value,
         when T.Integer_Operand => Integer_Value,
         when T.Boolean_Operand => Boolean_Value);

   procedure Verify
     (Candidate : Program;
      Result    : out Validated_Program;
      Error     : out Validation_Error)
   is
      States      : State_Table := [others => (others => <>)];
      State       : Abstract_State;
      Instruction : CCL.VM.Instruction;
      Falls_Through : Boolean;
      Ownership_Candidate : CCL.Ownership.Bytecode.Program;
      Ownership_Result : CCL.Ownership.Bytecode.Verification_Result;
      Length : constant Program_Length := Candidate.Length;
      Abstract_Value, Discarded : Stack_Type;
      Stack_Result : Abstract_Stacks.Operation_Result;
      D : CCL.Types.Description;
      Branch : Abstract_State;

      --  Functions: the region of a PC is the function whose code holds it,
      --  or the main body (No_Region), which comes first.
      No_Region : constant := MAX_FUNCTIONS;
      subtype Region_Index is Natural range 0 .. No_Region;
      Region : Region_Index := No_Region;
      Limit  : Program_Length := 0;

      function Region_Of (PC : Instruction_Index) return Region_Index
        with Post => Region_Of'Result = No_Region or else
                     Region_Of'Result < Candidate.Functions_Length
      is
         R : Region_Index := No_Region;
      begin
         for F in 1 .. Candidate.Functions_Length loop
            pragma Loop_Invariant (R = No_Region or else R < F);
            if Candidate.Functions (F - 1).Entry_PC <= PC then
               R := F - 1;
            end if;
         end loop;
         return R;
      end Region_Of;

      function Region_End (R : Region_Index) return Program_Length is
        (if Candidate.Functions_Length = 0 then Length
         elsif R = No_Region then Program_Length (Candidate.Functions (0).Entry_PC)
         elsif R + 1 < Candidate.Functions_Length then
            Program_Length (Candidate.Functions (R + 1).Entry_PC)
         else Length);

      function Function_Data (Kind : Value_Kind; Ref : CCL.Types.Type_Reference) return Boolean is
        (Kind in Integer_Value | Boolean_Value | Variant_Value and then
         Known_Value_Type (Candidate.Data_Types, Kind, Ref));

      --  Whole-program stack bound. Each region's own maximum depth, and for
      --  each call site the depth under the callee's frame; callees have lower
      --  indexes, so demands combine in index order after the sweep.
      subtype Demand is Natural range 0 .. MAX_STACK_DEPTH + 1;
      type Region_Demands is array (Region_Index) of Demand;
      type Call_Demands is array (Region_Index, Function_Index) of Demand;
      type Call_Presence is array (Region_Index, Function_Index) of Boolean;
      Own_Max : Region_Demands := [others => 0];
      Base_Max : Call_Demands := [others => [others => 0]];
      Calls : Call_Presence := [others => [others => False]];
      Need : Region_Demands := [others => 0];

      function Depth_Of (Item : Abstract_State) return Demand is
        (Demand (Unsigned_32'Min (Abstract_Stacks.Depth (Item.Values), MAX_STACK_DEPTH)));
   begin
      Result := (Checked => False, Content => Candidate);
      Error := Valid;

      --  Every text constant lies inside the pool's text.
      for I in 1 .. Candidate.Constants_Length loop
         if Candidate.Constants (I - 1).Length >
           MAX_CONSTANT_BYTES - (Candidate.Constants (I - 1).First - 1)
         then
            Error := Invalid_Constant;
            return;
         end if;
      end loop;

      -- A discovery snapshot may describe more types than this VM can execute.
      -- Registry construction/CCLB decoding already validate the definitions.
      -- Enforce executable shapes at each use below (locals, match tables,
      -- constructors and comparisons), not on unrelated metadata.
      for I in 1 .. Candidate.Imports_Length loop
         declare
            Op : constant Import_Declaration := Candidate.Imports (I - 1);
         begin
            if not Known_Value_Type (Candidate.Data_Types, Op.Argument, Op.Argument_Data_Type) or else
              not Known_Value_Type (Candidate.Data_Types, Op.Result, Op.Result_Data_Type)
            then Error := Invalid_Data_Type; return; end if;
            if Op.Result = Resource_Value then
               if Op.Result_Type_Tag >= Candidate.Types_Length or else
                 Candidate.Types (Op.Result_Type_Tag).Mode = CCL.Ownership.Unrestricted
               then Error := Invalid_Ownership; return; end if;
            elsif Op.Result_Type_Tag /= 0 then Error := Invalid_Ownership; return;
            end if;
            if Has_Receiver (Op) and then
              (not Known_Value_Type (Candidate.Data_Types, Resource_Value, Op.Receiver_Data_Type) or else
               not Op.Ownership_Argument or else Op.Transfer = CCL.Imports.Copy_Argument or else
               Op.Argument = Resource_Value)
            then Error := Invalid_Import; return; end if;
            -- Opaque resources can only be passed by a checked local borrow or
            -- move, never copied as a data argument.
            if Op.Argument = Resource_Value and then
              (not Op.Ownership_Argument or else Op.Transfer = CCL.Imports.Copy_Argument)
            then Error := Invalid_Import; return; end if;
            if Op.Ownership_Argument and then not
              (Scalar_Import (Op) or else
               ((Has_Receiver (Op) or else Op.Argument = Resource_Value) and then Op.Result /= Resource_Value)) then
               Error := Invalid_Import; return;
            end if;
         end;
      end loop;
      for L in 1 .. Candidate.Locals_Length loop
         --  Text may live in a compiler-created (dynamic) local, never in an
         --  initial local the host supplies: hosts cannot forge descriptors.
         if not Known_Value_Type (Candidate.Data_Types,
           Candidate.Local_Kinds (L - 1), Candidate.Local_Data_Types (L - 1)) and then
           not (Candidate.Local_Kinds (L - 1) = Text_Value and then
                Candidate.Local_Data_Types (L - 1) = CCL.Types.Invalid_Type and then
                L - 1 >= Candidate.Locals_Length - Candidate.Dynamic_Locals_Length)
         then Error := Invalid_Data_Type; return; end if;
         if Candidate.Local_Kinds (L - 1) = Resource_Value and then
           (Candidate.Local_Types (L - 1) >= Candidate.Types_Length or else
            Candidate.Types (Candidate.Local_Types (L - 1)).Mode = CCL.Ownership.Unrestricted)
         then Error := Invalid_Ownership; return; end if;
      end loop;
      for M in 1 .. Candidate.Matches_Length loop
         if CCL.Types.Describe (Candidate.Data_Types, Candidate.Matches (M - 1).Data_Type).Form /= CCL.Types.Sum or else
           not CCL.Objects.Persistable (Candidate.Data_Types, Candidate.Matches (M - 1).Data_Type)
         then Error := Invalid_Match; return; end if;
         D := CCL.Types.Describe (Candidate.Data_Types, Candidate.Matches (M - 1).Data_Type);
         for A in D.Count + 1 .. CCL.Types.Maximum_Components loop
            if Candidate.Matches (M - 1).Targets (A) /= 0 then
               Error := Invalid_Match; return;
            end if;
         end loop;
      end loop;

      for F in 1 .. Candidate.Functions_Length loop
         declare
            Decl : constant Function_Declaration := Candidate.Functions (F - 1);
         begin
            if Decl.Entry_PC = 0 or else Program_Length (Decl.Entry_PC) >= Length or else
              (F > 1 and then Decl.Entry_PC <= Candidate.Functions (F - 2).Entry_PC) or else
              not Function_Data (Decl.Result, Decl.Result_Data_Type)
            then Error := Invalid_Function; return; end if;
            for P in 1 .. Decl.Count loop
               if not Function_Data (Decl.Kinds (P), Decl.Data_Types (P)) then
                  Error := Invalid_Function; return;
               end if;
            end loop;
         end;
      end loop;

      if Length = 0 then
         Error := Empty_Program;
      elsif Candidate.Dynamic_Locals_Length > Candidate.Locals_Length then
         Error := Invalid_Ownership;
      else
         States (0).Seen := True;
         --  A function's code starts with its parameters on the stack.
         for F in 1 .. Candidate.Functions_Length loop
            Branch := (others => <>);
            Branch.Seen := True;
            for P in 1 .. Candidate.Functions (F - 1).Count loop
               Push_Kind (Branch, Candidate.Functions (F - 1).Kinds (P), Error,
                          Candidate.Functions (F - 1).Data_Types (P));
            end loop;
            States (Candidate.Functions (F - 1).Entry_PC) := Branch;
         end loop;

      for PC in Instruction_Index loop
         exit when Program_Length (PC) >= Length;
         if Error /= Valid then
            exit;
         elsif not States (PC).Seen then
            Error := Unreachable_Instruction;
            exit;
         end if;

         State := States (PC);
         Instruction := Candidate.Code (PC);
         Falls_Through := True;
         Region := Region_Of (PC);
         Limit := Region_End (Region);
         Own_Max (Region) := Demand'Max (Own_Max (Region), Depth_Of (State));

         --  Function bodies hold data only: no program locals, no ownership
         --  imports, no halting. The main body never returns.
         if Region /= No_Region and then
           (Instruction.Op in Halt | Initialize_Local | Copy_Local | Move_Local | Drop_Local |
              Borrow_Local_RO | Return_Local_RO | Borrow_Local_RW | Return_Local_RW |
              Apply_Local_Disposition or else
            (Instruction.Op = Invoke_Import and then
             (Natural (Instruction.Import) >= Candidate.Imports_Length or else
              Candidate.Imports (Instruction.Import).Ownership_Argument or else
              Candidate.Imports (Instruction.Import).Result = Resource_Value)))
         then
            Error := Invalid_Function; exit;
         elsif Region = No_Region and then Instruction.Op = Return_Function then
            Error := Invalid_Function; exit;
         end if;

         if Instruction.Op not in Make_Variant | Equal_Variant | Project_Field and then
           (Instruction.Data_Type /= CCL.Types.Invalid_Type or else Instruction.Alternative /= 0)
         then Error := Invalid_Data_Type; exit; end if;

         case Instruction.Op is
            when Project_Field =>
               D := CCL.Types.Describe (Candidate.Data_Types, Instruction.Data_Type);
               if D.Form /= CCL.Types.Product or else
                 not CCL.Objects.Persistable (Candidate.Data_Types, Instruction.Data_Type) or else
                 Instruction.Immediate not in 1 .. Integer_64 (D.Count) or else Instruction.Alternative /= 0
               then Error := Invalid_Data_Type;
               else
                  Pop_Kind (State, Object_Value, Error, Instruction.Data_Type);
                  declare
                     Ref : constant CCL.Types.Type_Reference :=
                       D.Parts (CCL.Types.Component_Index (Instruction.Immediate)).Payload;
                  begin
                     Push_Kind (State, Kind_For_Type (Candidate.Data_Types, Ref), Error, Reference_For_Type (Ref));
                  end;
               end if;
            when Halt =>
               -- The top value is returned to the host. No other moved
               -- operand may disappear when the machine completes: checking
               -- only the locals would miss ownership moved onto the stack.
               for Depth in 1 .. MAX_STACK_DEPTH - 1 loop
                  Abstract_Stacks.Peek_At (State.Values, Unsigned_32 (Depth), Abstract_Value, Stack_Result);
                  exit when Stack_Result /= Abstract_Stacks.Stack_Ok;
                  if not Abstract_Value.Copyable then
                     Error := Invalid_Ownership;
                     exit;
                  end if;
               end loop;
               Falls_Through := False;

            when Push_Integer =>
               Push_Kind (State, Integer_Value, Error);

            when Push_Text =>
               if Instruction.Immediate not in 0 .. Integer_64 (Candidate.Constants_Length) - 1 then
                  Error := Invalid_Constant;
               else
                  Push_Kind (State, Text_Value, Error);
               end if;

            when Concat_Text | Equal_Text =>
               Pop_Kind (State, Text_Value, Error);
               Pop_Kind (State, Text_Value, Error);
               Push_Kind (State, (if Instruction.Op = Concat_Text then Text_Value else Boolean_Value), Error);

            when Length_Text =>
               Pop_Kind (State, Text_Value, Error);
               Push_Kind (State, Integer_Value, Error);

            when Text_Builtin =>
               declare
                  Operation : CCL.Text_Operations.Operation;
                  Known : Boolean;
               begin
                  Find_Operation (Instruction.Immediate, Operation, Known);
                  if not Known then
                     Error := Invalid_Builtin;
                  else
                     declare
                        Sig : constant CCL.Text_Operations.Signature :=
                          CCL.Text_Operations.Signature_Of (Operation);
                     begin
                        Pop_Kind (State, Text_Value, Error);
                        for I in reverse 1 .. Sig.Count loop
                           Pop_Kind (State, Kind_Of (Sig.Operands (I)), Error);
                        end loop;
                        Push_Kind (State, Kind_Of (Sig.Result), Error);
                     end;
                  end if;
               end;

            when Push_Boolean =>
               Push_Kind (State, Boolean_Value, Error);

            when Make_Variant =>
               D := CCL.Types.Describe (Candidate.Data_Types, Instruction.Data_Type);
               if not CCL.Types.Is_Scalar_Sum (Candidate.Data_Types, Instruction.Data_Type) or else
                 Instruction.Alternative not in 1 .. D.Count
               then Error := Invalid_Data_Type;
               else
                  if D.Parts (Instruction.Alternative).Payload /= CCL.Types.Unit_Type then
                     Abstract_Stacks.Peek_Top (State.Values, Abstract_Value, Stack_Result);
                     if Stack_Result = Abstract_Stacks.Stack_Ok and then not Abstract_Value.Copyable then
                        Error := Invalid_Ownership;
                     end if;
                     Pop_Kind (State, (if D.Parts (Instruction.Alternative).Payload = CCL.Types.Integer_Type
                                      then Integer_Value else Boolean_Value), Error);
                  end if;
                  Push_Kind (State, Variant_Value, Error, Instruction.Data_Type);
               end if;

            when Equal_Variant =>
               if not CCL.Types.Is_Enumeration (Candidate.Data_Types, Instruction.Data_Type) or else
                 Instruction.Alternative /= 0
               then Error := Invalid_Data_Type;
               else
                  for Operand in 1 .. 2 loop
                     Abstract_Stacks.Peek_Top (State.Values, Abstract_Value, Stack_Result);
                     if Stack_Result = Abstract_Stacks.Stack_Ok and then not Abstract_Value.Copyable then
                        Error := Invalid_Ownership;
                     end if;
                     Pop_Kind (State, Variant_Value, Error, Instruction.Data_Type);
                  end loop;
                  Push_Kind (State, Boolean_Value, Error);
               end if;

            when Copy_Stack =>
               if Instruction.Immediate not in 0 .. MAX_STACK_DEPTH - 1 then Error := Stack_Underflow;
               else
                  Abstract_Stacks.Peek_At (State.Values, Unsigned_32 (Instruction.Immediate),
                                          Abstract_Value, Stack_Result);
                  if Stack_Result /= Abstract_Stacks.Stack_Ok then Error := Stack_Underflow;
                  elsif not Abstract_Value.Copyable then Error := Invalid_Ownership;
                  else Push_Kind (State, Abstract_Value.Kind, Error, Abstract_Value.Data_Type,
                                  Abstract_Value.Copyable, Abstract_Value.Type_Tag);
                  end if;
               end if;

            when Drop_Under_Top =>
               Abstract_Stacks.Pop (State.Values, Abstract_Value, Stack_Result);
               if Stack_Result /= Abstract_Stacks.Stack_Ok then Error := Stack_Underflow;
               else
                  Abstract_Stacks.Pop (State.Values, Discarded, Stack_Result);
                  if Stack_Result /= Abstract_Stacks.Stack_Ok then Error := Stack_Underflow;
                  elsif not Discarded.Copyable then Error := Invalid_Ownership;
                  else Push_Kind (State, Abstract_Value.Kind, Error, Abstract_Value.Data_Type,
                                  Abstract_Value.Copyable, Abstract_Value.Type_Tag);
                  end if;
               end if;

            when Switch_Variant =>
               Falls_Through := False;
               if Instruction.Immediate < 0 or else
                 Instruction.Immediate >= Integer_64 (Candidate.Matches_Length)
               then Error := Invalid_Match;
               else
                  declare
                     M : constant Match_Table := Candidate.Matches (Match_Index (Instruction.Immediate));
                  begin
                     -- Capture ownership control flow while the table index is
                     -- structurally in range; the second pass need not recast
                     -- an unchecked signed instruction operand.
                     Ownership_Candidate.Code (CCL.Ownership.Bytecode.Code_Index (PC)) :=
                       (Op => CCL.Ownership.Bytecode.Switch,
                        Target_Count => CCL.Types.Describe (Candidate.Data_Types, M.Data_Type).Count,
                        Targets => [for A in CCL.Types.Component_Index =>
                          CCL.Ownership.Bytecode.Code_Index (M.Targets (A))], others => <>);
                     Abstract_Stacks.Peek_Top (State.Values, Abstract_Value, Stack_Result);
                     if Stack_Result = Abstract_Stacks.Stack_Ok and then not Abstract_Value.Copyable then
                        Error := Invalid_Ownership;
                     end if;
                     Pop_Kind (State, Kind_For_Type (Candidate.Data_Types, M.Data_Type), Error, M.Data_Type);
                     D := CCL.Types.Describe (Candidate.Data_Types, M.Data_Type);
                     for A in 1 .. D.Count loop
                        if M.Targets (A) <= PC then Error := Backward_Jump;
                        elsif Program_Length (M.Targets (A)) >= Limit then Error := Invalid_Jump_Target;
                        else
                           Branch := State;
                           if D.Parts (A).Payload /= CCL.Types.Unit_Type then
                              Push_Kind (Branch, Kind_For_Type (Candidate.Data_Types, D.Parts (A).Payload),
                                Error, Reference_For_Type (D.Parts (A).Payload));
                           end if;
                           Merge_State (States, M.Targets (A), Branch, Error);
                        end if;
                     end loop;
                  end;
               end if;

            when Add_Integer | Subtract_Integer =>
               Pop_Kind (State, Integer_Value, Error);
               if Error = Valid then
                  Pop_Kind (State, Integer_Value, Error);
               end if;
               if Error = Valid then
                  Push_Kind (State, Integer_Value, Error);
               end if;

            when Multiply_Integer | Divide_Integer | Modulo_Integer =>
               Pop_Kind (State, Integer_Value, Error);
               if Error = Valid then
                  Pop_Kind (State, Integer_Value, Error);
               end if;
               if Error = Valid then
                  Push_Kind (State, Integer_Value, Error);
               end if;

            when Call_Function =>
               --  Only functions declared earlier: the call graph is acyclic.
               if Instruction.Immediate < 0 or else
                 Instruction.Immediate >= Integer_64 (Candidate.Functions_Length) or else
                 Instruction.Immediate >= Integer_64 (Region)
               then
                  Error := Invalid_Function;
               else
                  declare
                     Callee : constant Function_Index := Function_Index (Instruction.Immediate);
                     Decl : constant Function_Declaration := Candidate.Functions (Callee);
                     Base : constant Demand := Depth_Of (State);
                  begin
                     if Base < Decl.Count then
                        Error := Stack_Underflow;
                     else
                        Calls (Region, Callee) := True;
                        Base_Max (Region, Callee) :=
                          Demand'Max (Base_Max (Region, Callee), Base - Decl.Count);
                     end if;
                     for P in reverse 1 .. Decl.Count loop
                        Pop_Kind (State, Decl.Kinds (P), Error, Decl.Data_Types (P));
                     end loop;
                     Push_Kind (State, Decl.Result, Error, Decl.Result_Data_Type);
                  end;
               end if;

            when Return_Function =>
               --  Exactly the parameters and one result remain: frames balance.
               if Region = No_Region then
                  Error := Invalid_Function;
               else
                  declare
                     Decl : constant Function_Declaration := Candidate.Functions (Region);
                  begin
                     Pop_Kind (State, Decl.Result, Error, Decl.Result_Data_Type);
                     for P in reverse 1 .. Decl.Count loop
                        Pop_Kind (State, Decl.Kinds (P), Error, Decl.Data_Types (P));
                     end loop;
                     if Error = Valid and then Abstract_Stacks.Depth (State.Values) /= 0 then
                        Error := Inconsistent_Stack;
                     end if;
                  end;
               end if;
               Falls_Through := False;

            when Equal_Integer | Less_Integer | Less_Equal_Integer | Equal_Boolean =>
               Pop_Kind (State, (if Instruction.Op = Equal_Boolean then Boolean_Value else Integer_Value), Error);
               if Error = Valid then
                  Pop_Kind (State, (if Instruction.Op = Equal_Boolean then Boolean_Value else Integer_Value), Error);
               end if;
               if Error = Valid then
                  Push_Kind (State, Boolean_Value, Error);
               end if;

            when Not_Boolean =>
               Pop_Kind (State, Boolean_Value, Error);
               if Error = Valid then
                  Push_Kind (State, Boolean_Value, Error);
               end if;

            when Drop =>
               declare
                  Ignored : Stack_Type;
                  Stack_Result : Abstract_Stacks.Operation_Result;
               begin
                  Abstract_Stacks.Pop
                    (State.Values, Ignored, Stack_Result);
                  if Stack_Result /= Abstract_Stacks.Stack_Ok then
                     Error := Stack_Underflow;
                  elsif not Ignored.Copyable then
                     Error := Invalid_Ownership;
                  end if;
               end;

            when Jump | Jump_If_False =>
               if Instruction.Op = Jump_If_False then
                  Pop_Kind (State, Boolean_Value, Error);
               end if;

               if Error = Valid then
                  if Program_Length (Instruction.Target) >= Limit then
                     Error := Invalid_Jump_Target;
                  elsif Instruction.Target <= PC then
                     Error := Backward_Jump;
                  else
                     Merge_State (States, Instruction.Target, State, Error);
                     if Instruction.Op = Jump then
                        Falls_Through := False;
                     end if;
                  end if;
               end if;

            when Invoke_Import =>
               if Natural (Instruction.Import) >= Candidate.Imports_Length then
                  Error := Invalid_Import;
               elsif Candidate.Imports (Instruction.Import).Ownership_Argument
                 and then
                 (Natural (Candidate.Imports (Instruction.Import).Local) >=
                    Candidate.Locals_Length or else
                  Candidate.Imports (Instruction.Import).Cancellation /=
                    CCL.Imports.Not_Cancellable or else
                  Candidate.Local_Kinds
                    (Candidate.Imports (Instruction.Import).Local) /=
                      Local_Argument_Kind (Candidate.Imports (Instruction.Import)) or else
                  Candidate.Local_Data_Types (Candidate.Imports (Instruction.Import).Local) /=
                    Local_Argument_Type (Candidate.Imports (Instruction.Import)))
               then
                  Error := Invalid_Import;
               else
                  if not Candidate.Imports (Instruction.Import).Ownership_Argument or else
                    Has_Receiver (Candidate.Imports (Instruction.Import)) then
                     Pop_Kind
                       (State,
                        Candidate.Imports (Instruction.Import).Argument,
                        Error, Candidate.Imports (Instruction.Import).Argument_Data_Type);
                  end if;
                  -- A resource receiver comes from its local; a separate data
                  -- argument, when declared, is consumed from the operand stack.
                  if Error = Valid then
                     Push_Kind
                       (State,
                        Candidate.Imports (Instruction.Import).Result,
                        Error, Candidate.Imports (Instruction.Import).Result_Data_Type,
                        Candidate.Imports (Instruction.Import).Result /= Resource_Value,
                        Candidate.Imports (Instruction.Import).Result_Type_Tag);
                  end if;
               end if;

            when Initialize_Local =>
               if Natural (Instruction.Local) >= Candidate.Locals_Length then
                  Error := Invalid_Ownership;
               else
                  Abstract_Stacks.Pop (State.Values, Abstract_Value, Stack_Result);
                  if Stack_Result /= Abstract_Stacks.Stack_Ok then
                     Error := Stack_Underflow;
                  elsif Abstract_Value.Kind /= Candidate.Local_Kinds (Instruction.Local) or else
                    Abstract_Value.Data_Type /= Candidate.Local_Data_Types (Instruction.Local)
                  then
                     Error := Type_Mismatch;
                  elsif Abstract_Value.Type_Tag /= Candidate.Local_Types (Instruction.Local) or else
                    (Abstract_Value.Copyable and then
                     Candidate.Types (Candidate.Local_Types (Instruction.Local)).Mode /= CCL.Ownership.Unrestricted)
                  then
                     Error := Invalid_Ownership;
                  end if;
               end if;

            when Copy_Local | Move_Local | Drop_Local |
                 Borrow_Local_RO | Return_Local_RO |
                 Borrow_Local_RW | Return_Local_RW |
                 Apply_Local_Disposition =>
               if Natural (Instruction.Local) >= Candidate.Locals_Length then
                  Error := Invalid_Ownership;
               elsif Instruction.Op in Copy_Local | Move_Local then
                  Push_Kind
                    (State, Candidate.Local_Kinds (Instruction.Local), Error,
                     Candidate.Local_Data_Types (Instruction.Local),
                     Instruction.Op = Copy_Local, Candidate.Local_Types (Instruction.Local));
               end if;
         end case;

         if Error = Valid then
            Own_Max (Region) := Demand'Max (Own_Max (Region), Depth_Of (State));
         end if;
         if Error = Valid and then Falls_Through then
            if Program_Length (PC) + 1 >= Limit then
               Error := (if Region = No_Region then Missing_Halt else Invalid_Function);
            else
               Merge_State
                 (States, Instruction_Index'Succ (PC), State, Error);
            end if;
         end if;
      end loop;

      --  The deepest call chain must fit the machine's stack.
      if Error = Valid then
         for R in Region_Index loop
            Need (R) := Own_Max (R);
            for C in Function_Index loop
               if C < R and then Calls (R, C) then
                  Need (R) := Demand'Min
                    (MAX_STACK_DEPTH + 1, Natural'Max (Need (R), Base_Max (R, C) + Need (C)));
               end if;
            end loop;
         end loop;
         if Need (No_Region) > MAX_STACK_DEPTH then
            Error := Stack_Overflow;
         end if;
      end if;

      --  Only the main body can hold locals or ownership operations.
      if Error = Valid then
         Ownership_Candidate.Length :=
           CCL.Ownership.Bytecode.Code_Length (Region_End (No_Region));
         Ownership_Candidate.Locals_Length := Candidate.Locals_Length;
         Ownership_Candidate.Dynamic_Locals_Length :=
           Candidate.Dynamic_Locals_Length;
         if Candidate.Locals_Length > 0 then
            for Local in 0 .. Candidate.Locals_Length - 1 loop
               Ownership_Candidate.Local_Types (Local) :=
                 Candidate.Local_Types (Local);
            end loop;
         end if;
         Ownership_Candidate.Types := Candidate.Types;
         for PC in Instruction_Index loop
            exit when Program_Length (PC) >= Region_End (No_Region);
            Ownership_Candidate.Code (CCL.Ownership.Bytecode.Code_Index (PC)) :=
              (case Candidate.Code (PC).Op is
                  when Halt => (Op => CCL.Ownership.Bytecode.Halt, others => <>),
                  when Jump =>
                    (Op => CCL.Ownership.Bytecode.Jump,
                     Target => CCL.Ownership.Bytecode.Code_Index
                       (Candidate.Code (PC).Target),
                     others => <>),
                  when Jump_If_False =>
                    (Op => CCL.Ownership.Bytecode.Jump_If,
                     Target => CCL.Ownership.Bytecode.Code_Index
                       (Candidate.Code (PC).Target),
                     others => <>),
                  when Switch_Variant =>
                    Ownership_Candidate.Code (CCL.Ownership.Bytecode.Code_Index (PC)),
                  when Initialize_Local =>
                    (Op => CCL.Ownership.Bytecode.Initialize_Local,
                     Local => Candidate.Code (PC).Local,
                     others => <>),
                  when Copy_Local =>
                    (Op => CCL.Ownership.Bytecode.Copy_Local,
                     Local => Candidate.Code (PC).Local,
                     others => <>),
                  when Move_Local =>
                    (Op => CCL.Ownership.Bytecode.Move_Local,
                     Local => Candidate.Code (PC).Local,
                     others => <>),
                  when Drop_Local =>
                    (Op => CCL.Ownership.Bytecode.Drop_Local,
                     Local => Candidate.Code (PC).Local,
                     others => <>),
                  when Borrow_Local_RO =>
                    (Op => CCL.Ownership.Bytecode.Borrow_Local_RO,
                     Local => Candidate.Code (PC).Local,
                     others => <>),
                  when Return_Local_RO =>
                    (Op => CCL.Ownership.Bytecode.Return_Local_RO,
                     Local => Candidate.Code (PC).Local,
                     others => <>),
                  when Borrow_Local_RW =>
                    (Op => CCL.Ownership.Bytecode.Borrow_Local_RW,
                     Local => Candidate.Code (PC).Local,
                     others => <>),
                  when Return_Local_RW =>
                    (Op => CCL.Ownership.Bytecode.Return_Local_RW,
                     Local => Candidate.Code (PC).Local,
                     others => <>),
                  when Apply_Local_Disposition =>
                    (Op => CCL.Ownership.Bytecode.Apply_Local_Disposition,
                     Local => Candidate.Code (PC).Local,
                     Verb => Candidate.Code (PC).Verb,
                     others => <>),
                  when Invoke_Import =>
                    (if Candidate.Imports
                       (Candidate.Code (PC).Import).
                         Ownership_Argument
                     then
                       (Op => CCL.Ownership.Bytecode.Import_Local,
                        Local => Candidate.Imports
                          (Candidate.Code (PC).Import).Local,
                        Import_Mode =>
                          (case Candidate.Imports
                             (Candidate.Code (PC).Import).
                               Transfer is
                              when CCL.Imports.Copy_Argument =>
                                CCL.Ownership.Bytecode.Copy_Argument,
                              when CCL.Imports.Move_Argument =>
                                CCL.Ownership.Bytecode.Move_Argument,
                              when CCL.Imports.Borrowed_RO_Argument =>
                                CCL.Ownership.Bytecode.Borrowed_RO_Argument,
                              when CCL.Imports.Borrowed_RW_Argument =>
                                CCL.Ownership.Bytecode.Borrowed_RW_Argument),
                        Success_Verb => Candidate.Imports
                          (Candidate.Code (PC).Import).
                            Success_Verb,
                        Failure_Verb => Candidate.Imports
                          (Candidate.Code (PC).Import).
                            Failure_Verb,
                        others => <>)
                     else
                       (Op => CCL.Ownership.Bytecode.No_Ownership_Op,
                        others => <>)),
                  when others =>
                    (Op => CCL.Ownership.Bytecode.No_Ownership_Op, others => <>));
         end loop;
         CCL.Ownership.Bytecode.Verify
           (Ownership_Candidate, Ownership_Result);
         if Ownership_Result.Error /= CCL.Ownership.Bytecode.Bytecode_Valid then
            Error := Invalid_Ownership;
         else
            Result := (Checked => True, Content => Candidate);
         end if;
      end if;
      end if;
   end Verify;

   procedure Initialize
     (Item  : Validated_Program;
      Fuel  : Natural;
      State : out Machine_State)
   is
      Initial_Locals_Length : constant Local_Count :=
        Item.Content.Locals_Length - Item.Content.Dynamic_Locals_Length;
   begin
      pragma Assert (Is_Valid (Item));
      State := (others => <>);
      CCL.Execution_Budgets.Initialize (State.Execution_Budget, Fuel);
      CCL.Ownership.Initialize (State.Ownership);
      CCL.Imports.Initialize (State.Import_Lifecycle);
      Text_Regions.Initialize (State.Text);
      if Initial_Locals_Length > 0 then
         State.Terminal := True;
         State.Terminal_Status := Invalid_Bytecode;
      end if;
   end Initialize;

   procedure Initialize_With_Locals
     (Item     : Validated_Program;
      Fuel     : Natural;
      Values   : Local_Value_Array;
      Count    : Local_Count;
      State    : out Machine_State;
      Accepted : out Boolean)
   is
      Error : CCL.Ownership.Ownership_Error;
      Initial_Locals_Length : constant Local_Count :=
        Item.Content.Locals_Length - Item.Content.Dynamic_Locals_Length;
   begin
      pragma Assert (Is_Valid (Item));
      State := (others => <>);
      CCL.Execution_Budgets.Initialize (State.Execution_Budget, Fuel);
      CCL.Ownership.Initialize (State.Ownership);
      CCL.Imports.Initialize (State.Import_Lifecycle);
      Text_Regions.Initialize (State.Text);
      Accepted := Count = Initial_Locals_Length;
      if Accepted and then Count > 0 then
         for Local in 0 .. Count - 1 loop
            if Values (Local).Kind in Object_Value | Resource_Value or else
              Values (Local).Kind /= Item.Content.Local_Kinds (Local) or else
              Values (Local).Type_Tag /= Item.Content.Local_Types (Local) or else
              Values (Local).Data_Type /= Item.Content.Local_Data_Types (Local) or else
              not Well_Typed (Item.Content.Data_Types, Values (Local))
            then
               Accepted := False;
               exit;
            end if;
         end loop;
      end if;
      if Accepted then
         State.Locals := Values;
         if Count > 0 then
            for Local in 0 .. Count - 1 loop
               CCL.Ownership.Declare_Binding
                 (State.Ownership, Local, Item.Content.Local_Types (Local), Error);
               if Error /= CCL.Ownership.Ownership_Valid then
                  Accepted := False;
                  exit;
               end if;
            end loop;
         end if;
      end if;
      if not Accepted then
         State.Terminal := True;
         State.Terminal_Status := Invalid_Bytecode;
      end if;
   end Initialize_With_Locals;

   ---------------------------------------------------------------------------
   --  Text operations of the executor. Each makes its result a new string in
   --  the run's region and reports the region's status: Storage_Full and
   --  Value_Table_Full mean the region is exhausted; anything else is an
   --  invalid descriptor.
   ---------------------------------------------------------------------------
   use type Text_Regions.Operation_Result;

   procedure Push_Constant
     (Content : Program; Index : Constant_Index;
      Region  : in out Text_Regions.Stack; Item : out Value;
      Status  : out Text_Regions.Operation_Result)
   is
      C : constant Text_Constant := Content.Constants (Index);
   begin
      Item := (Kind => Text_Value, others => <>);
      Status := Text_Regions.Invalid_Bounds;
      if C.Length > MAX_CONSTANT_BYTES - (C.First - 1) then
         return;
      end if;
      Text_Regions.Allocate_String
        (Region, Content.Constant_Text (C.First .. C.First + C.Length - 1), Item.Text, Status);
   end Push_Constant;

   procedure Concat_Texts
     (Region : in out Text_Regions.Stack; Left, Right : Value;
      Item   : out Value; Status : out Text_Regions.Operation_Result)
   is
      Buffer : String (1 .. MAX_STRING_BYTES) := [others => ' '];
      L : constant Natural := Text_Regions.Length (Left.Text);
      R : constant Natural := Text_Regions.Length (Right.Text);
   begin
      Item := (Kind => Text_Value, others => <>);
      if L > MAX_STRING_BYTES or else R > MAX_STRING_BYTES - L then
         Status := Text_Regions.Storage_Full;
         return;
      end if;
      Text_Regions.Copy_To (Region, Left.Text, Buffer (1 .. L), Status);
      if Status /= Text_Regions.Operation_Ok then return; end if;
      Text_Regions.Copy_To (Region, Right.Text, Buffer (L + 1 .. L + R), Status);
      if Status /= Text_Regions.Operation_Ok then return; end if;
      Text_Regions.Allocate_String (Region, Buffer (1 .. L + R), Item.Text, Status);
   end Concat_Texts;

   procedure Equal_Texts
     (Region : Text_Regions.Stack; Left, Right : Value;
      Same   : out Boolean; Status : out Text_Regions.Operation_Result)
   is
      A, B : Character;
   begin
      Same := False;
      Status := Text_Regions.Operation_Ok;
      if not Text_Regions.Is_Valid (Region, Left.Text) or else
        not Text_Regions.Is_Valid (Region, Right.Text)
      then
         Status := Text_Regions.Invalid_Value;
         return;
      end if;
      if Text_Regions.Length (Left.Text) /= Text_Regions.Length (Right.Text) then
         return;
      end if;
      for I in 1 .. Text_Regions.Length (Left.Text) loop
         Text_Regions.Read (Region, Left.Text, I, A, Status);
         if Status /= Text_Regions.Operation_Ok then return; end if;
         Text_Regions.Read (Region, Right.Text, I, B, Status);
         if Status /= Text_Regions.Operation_Ok then return; end if;
         if A /= B then return; end if;
      end loop;
      Same := True;
   end Equal_Texts;

   function Text_Failure (Status : Text_Regions.Operation_Result) return Execution_Status is
     (if Status in Text_Regions.Storage_Full | Text_Regions.Value_Table_Full
      then Text_Storage_Exhausted else Invalid_Bytecode);

   ---------------------------------------------------------------------------
   --  String built-ins (CCL.Text_Operations, shared with the interpreter).
   ---------------------------------------------------------------------------
   --  Patterns, separators and replacements: the interpreter's short strings.
   MAX_PATTERN_BYTES : constant := 1_024;
   type Text_Operands is array (1 .. 2) of Value;

   procedure Run_Text_Builtin
     (Region   : in out Text_Regions.Stack;
      Item     : T.Operation;
      Operands : Text_Operands;
      Subject  : Value;
      Result   : out Value;
      Status   : out Execution_Status)
   is
      S, Output : String (1 .. MAX_STRING_BYTES) := [others => ' '];
      A, B : String (1 .. MAX_PATTERN_BYTES) := [others => ' '];
      S_Length : constant Natural := Text_Regions.Length (Subject.Text);
      A_Length, B_Length : Natural range 0 .. MAX_PATTERN_BYTES := 0;
      Output_Length : T.String_Length := 0;
      Region_Status : Text_Regions.Operation_Result;
      Outcome : T.Outcome;

      procedure Copy (Source : Value; Target : out String; Good : out Boolean) is
      begin
         Text_Regions.Copy_To (Region, Source.Text, Target, Region_Status);
         Good := Region_Status = Text_Regions.Operation_Ok;
      end Copy;

      procedure Store (Text : String) is
      begin
         Result := (Kind => Text_Value, others => <>);
         Text_Regions.Allocate_String (Region, Text, Result.Text, Region_Status);
         Status := (if Region_Status = Text_Regions.Operation_Ok then Completed
                    else Text_Failure (Region_Status));
      end Store;

      Good : Boolean := True;
   begin
      Result := (others => <>);
      Status := Invalid_Bytecode;
      if S_Length > MAX_STRING_BYTES then return; end if;
      Copy (Subject, S (1 .. S_Length), Good);
      if not Good then return; end if;
      if T.Signature_Of (Item).Count >= 1 and then
        T.Signature_Of (Item).Operands (1) = T.Text_Operand
      then
         A_Length := Natural'Min (Text_Regions.Length (Operands (1).Text), MAX_PATTERN_BYTES);
         if Text_Regions.Length (Operands (1).Text) > MAX_PATTERN_BYTES then
            Status := Text_Storage_Exhausted; return;
         end if;
         Copy (Operands (1), A (1 .. A_Length), Good);
         if not Good then return; end if;
      end if;
      if Item = T.Replace then
         B_Length := Natural'Min (Text_Regions.Length (Operands (2).Text), MAX_PATTERN_BYTES);
         if Text_Regions.Length (Operands (2).Text) > MAX_PATTERN_BYTES then
            Status := Text_Storage_Exhausted; return;
         end if;
         Copy (Operands (2), B (1 .. B_Length), Good);
         if not Good then return; end if;
      end if;
      case Item is
         when T.Upper | T.Lower | T.Reverse_Text =>
            T.Transform (Item, S (1 .. S_Length), Output (1 .. S_Length));
            Store (Output (1 .. S_Length));
         when T.Trim | T.First_Chars | T.Last_Chars | T.Skip_Chars =>
            declare
               Low : Positive;
               High : Natural;
            begin
               T.Slice (Item, S (1 .. S_Length),
                        (if Item = T.Trim then 0 else Operands (1).Integer), Low, High);
               Store (S (Low .. High));
            end;
         when T.Contains | T.Starts_With | T.Ends_With =>
            Result := Boolean_Constant (T.Test (Item, S (1 .. S_Length), A (1 .. A_Length)));
            Status := Completed;
         when T.Index_Of =>
            Result := Integer_Constant
              (Integer_64 (if A_Length = 0 then 1 else T.Find (S (1 .. S_Length), A (1 .. A_Length), 1)));
            Status := Completed;
         when T.Replace =>
            T.Replace_All (S (1 .. S_Length), A (1 .. A_Length), B (1 .. B_Length),
                           Output, Output_Length, Outcome);
            if Outcome = T.Done then
               Store (Output (1 .. Output_Length));
            else
               Status := Text_Storage_Exhausted;
            end if;
         when T.Parse_Int =>
            declare
               Number : Integer_64;
            begin
               T.Parse_Integer (S (1 .. S_Length), Number, Outcome);
               case Outcome is
                  when T.Done =>
                     Result := Integer_Constant (Number);
                     Status := Completed;
                  when T.Overflow => Status := Arithmetic_Overflow;
                  when others => Status := Invalid_Number;
               end case;
            end;
      end case;
   end Run_Text_Builtin;

   --  A text result's characters, for Execution_Result.
   function Result_Text_Of (Region : Text_Regions.Stack; Item : Value) return Result_Text is
      Text : Result_Text;
      Status : Text_Regions.Operation_Result;
   begin
      if Item.Kind = Text_Value and then Text_Regions.Is_Valid (Region, Item.Text) and then
        Text_Regions.Length (Item.Text) <= MAX_RESULT_TEXT
      then
         Text.Length := Text_Regions.Length (Item.Text);
         Text_Regions.Copy_To (Region, Item.Text, Text.Data (1 .. Text.Length), Status);
         if Status /= Text_Regions.Operation_Ok then
            Text := (others => <>);
         end if;
      end if;
      return Text;
   end Result_Text_Of;

   procedure Continue_With_Native
     (Item   : Validated_Program;
      State  : in out Machine_State;
      Store : in out Native_Store;
      Instructions : Natural;
      Result : out Execution_Result)
   is
      Item_Length : constant Program_Length := Item.Content.Length;
      Stack : Runtime_Stacks.Stack;
      PC    : Instruction_Index;
      Left  : Integer_64;
      Right : Integer_64;
      Left_Value  : Value;
      Right_Value : Value;
      Stack_Result : Runtime_Stacks.Operation_Result;
      Waiting : Boolean := State.Waiting;
      Waiting_Owned : Boolean := State.Waiting_Owned;
      Done  : Boolean := State.Terminal or else Waiting;
      Status : Execution_Status := State.Terminal_Status;
      Own_Error : CCL.Ownership.Ownership_Error;
      Import_Error : CCL.Imports.Import_Error;
      Budget_Result : CCL.Execution_Budgets.Consume_Result;
      Addition_Result : Integer_64;
      Addition_Overflowed : Boolean;
      Arithmetic_Result : Integer_64;
      Arithmetic_Error : CCL.Checked_Arithmetic.Arithmetic_Error;
      Slice_Remaining : Natural := Instructions;
      Text_Status : Text_Regions.Operation_Result;
      Same_Text : Boolean;
      Joined : Value;

      --  Stop the run with Code. Only the terminal status changes.
      procedure Trap (Code : Execution_Status)
        with Post => Fuel_Limit (State) = Fuel_Limit (State'Old) and then
                     CCL.Imports.Phase (State.Import_Lifecycle) =
                       CCL.Imports.Phase (State.Import_Lifecycle'Old);
      procedure Trap (Code : Execution_Status) is
      begin
         Status := Code;
         State.Terminal := True;
         State.Terminal_Status := Code;
         Done := True;
      end Trap;

      --  Push Item and step, or trap.
      procedure Push_Next (Item : Value) is
      begin
         if Program_Length (PC) + 1 >= Item_Length then
            Trap (Invalid_Bytecode);
            return;
         end if;
         Runtime_Stacks.Push (Stack, Item, Stack_Result);
         if Stack_Result = Runtime_Stacks.Stack_Ok then
            PC := PC + 1;
         else
            Trap (Invalid_Bytecode);
         end if;
      end Push_Next;

      --  Pop a text operand, or trap.
      procedure Pop_Text (Item : out Value) is
      begin
         Runtime_Stacks.Pop (Stack, Item, Stack_Result);
         if Stack_Result /= Runtime_Stacks.Stack_Ok or else Item.Kind /= Text_Value then
            Trap (Invalid_Bytecode);
         end if;
      end Pop_Text;
   begin
      Stack := State.Stack;
      PC := State.PC;
      if Waiting then
         Status := Waiting_For_Host;
      end if;

      pragma Assert
        (Waiting_Owned or else
         CCL.Imports.Phase (State.Import_Lifecycle) not in
           CCL.Imports.Import_Offered | CCL.Imports.Import_Accepted);

      loop
         pragma Loop_Invariant
           (not Waiting or else
            Natural (State.Waiting_Import) < Item.Content.Imports_Length);
         pragma Loop_Invariant (not Waiting or else Done);
         pragma Loop_Invariant
           (not Waiting_Owned or else
            (Waiting and then
             CCL.Imports.Phase (State.Import_Lifecycle) in
               CCL.Imports.Import_Offered | CCL.Imports.Import_Accepted));
         pragma Loop_Invariant
           (Waiting_Owned or else
            CCL.Imports.Phase (State.Import_Lifecycle) not in
              CCL.Imports.Import_Offered | CCL.Imports.Import_Accepted);
         pragma Loop_Invariant
           (Fuel_Limit (State) = Fuel_Limit (State'Loop_Entry));
         exit when Done or else
           Slice_Remaining = 0 or else
           not CCL.Execution_Budgets.Has_Fuel (State.Execution_Budget);
         CCL.Execution_Budgets.Consume
           (State.Execution_Budget, Budget_Result);
         Slice_Remaining := Slice_Remaining - 1;

         if Budget_Result /= CCL.Execution_Budgets.Consumed or else
           Program_Length (PC) >= Item.Content.Length
         then
            Status := Invalid_Bytecode;
            State.Terminal := True;
            State.Terminal_Status := Invalid_Bytecode;
            Done := True;
         else
         case Item.Content.Code (PC).Op is
            when Make_Variant | Equal_Variant | Switch_Variant | Copy_Stack | Drop_Under_Top | Project_Field =>
               declare
                  Ins : constant Instruction := Item.Content.Code (PC);
                  D : constant CCL.Types.Description := CCL.Types.Describe (Item.Content.Data_Types, Ins.Data_Type);
                  Good : Boolean := True;
                  Next_PC : Instruction_Index := PC + 1;
                  Alternative : CCL.Types.Component_Count;
                  Native_Value : Value;
                  function Matches (V : Value; Ref : CCL.Types.Type_Reference) return Boolean is
                    (V.Kind = Kind_For_Type (Item.Content.Data_Types, Ref) and then
                     V.Data_Type = Reference_For_Type (Ref) and then
                     V.Copyable and then V.Type_Tag = 0 and then Well_Typed (Item.Content.Data_Types, V));
               begin
                  case Ins.Op is
                     when Project_Field =>
                        Runtime_Stacks.Pop (Stack, Right_Value, Stack_Result);
                        Good := Stack_Result = Runtime_Stacks.Stack_Ok and then
                          Matches (Right_Value, Ins.Data_Type) and then D.Form = CCL.Types.Product and then
                          Ins.Immediate in 1 .. Integer_64 (D.Count);
                        if Good then
                           Evaluate_Native (Store, Item.Content.Data_Types, Ins, Right_Value, Native_Value, Alternative, Good);
                           if Good then
                              Good := Matches (Native_Value, D.Parts (CCL.Types.Component_Index (Ins.Immediate)).Payload);
                           end if;
                           if Good then
                              Runtime_Stacks.Push (Stack, Native_Value, Stack_Result);
                              Good := Stack_Result = Runtime_Stacks.Stack_Ok;
                           end if;
                        end if;
                     when Make_Variant =>
                        Right_Value := Integer_Constant (0);
                        if Ins.Alternative not in 1 .. D.Count then Good := False;
                        elsif D.Parts (Ins.Alternative).Payload /= CCL.Types.Unit_Type then
                           Runtime_Stacks.Pop (Stack, Right_Value, Stack_Result);
                           Good := Stack_Result = Runtime_Stacks.Stack_Ok and then Right_Value.Copyable and then
                             Right_Value.Kind = (if D.Parts (Ins.Alternative).Payload = CCL.Types.Integer_Type
                                                 then Integer_Value else Boolean_Value);
                        end if;
                        if Good then
                           Right_Value := (Kind => Variant_Value, Data_Type => Ins.Data_Type,
                             Alternative => Ins.Alternative, Integer => Right_Value.Integer,
                             Boolean => Right_Value.Boolean, others => <>);
                           Runtime_Stacks.Push (Stack, Right_Value, Stack_Result);
                           Good := Stack_Result = Runtime_Stacks.Stack_Ok;
                        end if;
                     when Equal_Variant =>
                        Runtime_Stacks.Pop (Stack, Right_Value, Stack_Result);
                        Good := Stack_Result = Runtime_Stacks.Stack_Ok and then Right_Value.Kind = Variant_Value and then
                          Right_Value.Data_Type = Ins.Data_Type and then Right_Value.Copyable;
                        if Good then
                           Runtime_Stacks.Pop (Stack, Left_Value, Stack_Result);
                           Good := Stack_Result = Runtime_Stacks.Stack_Ok and then Left_Value.Kind = Variant_Value and then
                             Left_Value.Data_Type = Ins.Data_Type and then Left_Value.Copyable;
                        end if;
                        if Good then
                           Runtime_Stacks.Push (Stack, Boolean_Constant
                             (Left_Value.Alternative = Right_Value.Alternative), Stack_Result);
                           Good := Stack_Result = Runtime_Stacks.Stack_Ok;
                        end if;
                     when Copy_Stack =>
                        if Ins.Immediate not in 0 .. MAX_STACK_DEPTH - 1 then Good := False;
                        else
                           Runtime_Stacks.Peek_At (Stack, Unsigned_32 (Ins.Immediate), Right_Value, Stack_Result);
                           Good := Stack_Result = Runtime_Stacks.Stack_Ok and then Right_Value.Copyable;
                           if Good then
                              Runtime_Stacks.Push (Stack, Right_Value, Stack_Result);
                              Good := Stack_Result = Runtime_Stacks.Stack_Ok;
                           end if;
                        end if;
                     when Drop_Under_Top =>
                        Runtime_Stacks.Pop (Stack, Right_Value, Stack_Result);
                        Good := Stack_Result = Runtime_Stacks.Stack_Ok;
                        if Good then
                           Runtime_Stacks.Pop (Stack, Left_Value, Stack_Result);
                           Good := Stack_Result = Runtime_Stacks.Stack_Ok and then Left_Value.Copyable;
                        end if;
                        if Good then
                           Runtime_Stacks.Push (Stack, Right_Value, Stack_Result);
                           Good := Stack_Result = Runtime_Stacks.Stack_Ok;
                        end if;
                     when Switch_Variant =>
                        if Ins.Immediate < 0 or else Ins.Immediate >= Integer_64 (Item.Content.Matches_Length) then
                           Good := False;
                        else
                           declare
                              M : constant Match_Table := Item.Content.Matches (Match_Index (Ins.Immediate));
                              Schema : constant CCL.Types.Description := CCL.Types.Describe (Item.Content.Data_Types, M.Data_Type);
                           begin
                              Runtime_Stacks.Pop (Stack, Right_Value, Stack_Result);
                              Good := Stack_Result = Runtime_Stacks.Stack_Ok and then Right_Value.Copyable and then
                                Right_Value.Kind = Kind_For_Type (Item.Content.Data_Types, M.Data_Type) and then
                                Right_Value.Data_Type = M.Data_Type and then
                                Well_Typed (Item.Content.Data_Types, Right_Value);
                              if Good and then Right_Value.Kind = Object_Value then
                                 Evaluate_Native (Store, Item.Content.Data_Types, Ins, Right_Value, Native_Value, Alternative, Good);
                                 Good := Good and then Alternative in 1 .. Schema.Count;
                                 if Good then
                                    Next_PC := M.Targets (Alternative);
                                    if Schema.Parts (Alternative).Payload /= CCL.Types.Unit_Type then
                                       Good := Matches (Native_Value, Schema.Parts (Alternative).Payload);
                                       if Good then
                                          Runtime_Stacks.Push (Stack, Native_Value, Stack_Result);
                                          Good := Stack_Result = Runtime_Stacks.Stack_Ok;
                                       end if;
                                    end if;
                                 end if;
                              elsif Good then
                                 Next_PC := M.Targets (Right_Value.Alternative);
                                 case Schema.Parts (Right_Value.Alternative).Payload is
                                    when CCL.Types.Integer_Type =>
                                       Runtime_Stacks.Push (Stack, Integer_Constant (Right_Value.Integer), Stack_Result);
                                    when CCL.Types.Boolean_Type =>
                                       Runtime_Stacks.Push (Stack, Boolean_Constant (Right_Value.Boolean), Stack_Result);
                                    when others => null;
                                 end case;
                                 Good := Stack_Result = Runtime_Stacks.Stack_Ok;
                              end if;
                           end;
                        end if;
                     when others => null;
                  end case;
                  if not Good or else Next_PC <= PC or else Program_Length (Next_PC) >= Item.Content.Length then
                     Status := Invalid_Bytecode; State.Terminal := True;
                     State.Terminal_Status := Invalid_Bytecode; Done := True;
                  else PC := Next_PC;
                  end if;
               end;
            when Halt =>
               CCL.Ownership.Check_Scope
                 (State.Ownership, Item.Content.Types, Own_Error);
               if Own_Error /= CCL.Ownership.Ownership_Valid then
                  Status := Invalid_Bytecode;
                  State.Terminal_Status := Invalid_Bytecode;
               else
                  Status := Completed;
                  Runtime_Stacks.Peek_Top
                    (Stack, State.Result_Value, Stack_Result);
                  if Stack_Result = Runtime_Stacks.Stack_Ok then
                     State.Has_Value := True;
                  end if;
                  --  A text result longer than a result carries fails, as
                  --  in the interpreter, rather than arriving cut short.
                  if State.Has_Value and then State.Result_Value.Kind = Text_Value and then
                    Text_Regions.Length (State.Result_Value.Text) > MAX_RESULT_TEXT
                  then
                     Status := Text_Storage_Exhausted;
                     State.Has_Value := False;
                  end if;
                  State.Terminal_Status := Status;
               end if;
               State.Terminal := True;
               Done := True;

            when Push_Integer =>
               if Program_Length (PC) + 1 >= Item.Content.Length then
                  Status := Invalid_Bytecode;
                  State.Terminal := True;
                  State.Terminal_Status := Invalid_Bytecode;
                  Done := True;
               else
                  Runtime_Stacks.Push
                    (Stack,
                     Integer_Constant
                       (Item.Content.Code (PC).Immediate),
                     Stack_Result);
                  if Stack_Result = Runtime_Stacks.Stack_Ok then
                     PC := PC + 1;
                  else
                     Status := Invalid_Bytecode;
                     State.Terminal := True;
                     State.Terminal_Status := Invalid_Bytecode;
                     Done := True;
                  end if;
               end if;

            when Push_Boolean =>
               if Program_Length (PC) + 1 >= Item.Content.Length then
                  Status := Invalid_Bytecode;
                  State.Terminal := True;
                  State.Terminal_Status := Invalid_Bytecode;
                  Done := True;
               else
                  Runtime_Stacks.Push
                    (Stack,
                     Boolean_Constant
                       (Item.Content.Code (PC).Immediate /= 0),
                     Stack_Result);
                  if Stack_Result = Runtime_Stacks.Stack_Ok then
                     PC := PC + 1;
                  else
                     Status := Invalid_Bytecode;
                     State.Terminal := True;
                     State.Terminal_Status := Invalid_Bytecode;
                     Done := True;
                  end if;
               end if;

            when Add_Integer =>
               Runtime_Stacks.Pop (Stack, Right_Value, Stack_Result);
               if Stack_Result /= Runtime_Stacks.Stack_Ok or else
                 Right_Value.Kind /= Integer_Value or else
                 Program_Length (PC) + 1 >= Item.Content.Length
               then
                  Status := Invalid_Bytecode;
                  State.Terminal := True;
                  State.Terminal_Status := Invalid_Bytecode;
                  Done := True;
               else
                  Runtime_Stacks.Pop (Stack, Left_Value, Stack_Result);
                  if Stack_Result /= Runtime_Stacks.Stack_Ok or else
                    Left_Value.Kind /= Integer_Value
                  then
                     Status := Invalid_Bytecode;
                     State.Terminal := True;
                     State.Terminal_Status := Invalid_Bytecode;
                     Done := True;
                  else
                     Right := Right_Value.Integer;
                     Left := Left_Value.Integer;
                  CCL.Checked_Arithmetic.Add
                    (Left, Right, Addition_Result, Addition_Overflowed);
                  if Addition_Overflowed then
                     Status := Arithmetic_Overflow;
                     State.Terminal := True;
                     State.Terminal_Status := Arithmetic_Overflow;
                     Done := True;
                  else
                     Runtime_Stacks.Push
                       (Stack, Integer_Constant (Addition_Result), Stack_Result);
                     if Stack_Result = Runtime_Stacks.Stack_Ok then
                        PC := PC + 1;
                     else
                        Status := Invalid_Bytecode;
                        State.Terminal := True;
                        State.Terminal_Status := Invalid_Bytecode;
                        Done := True;
                     end if;
                  end if;
                  end if;
               end if;

            when Subtract_Integer | Multiply_Integer | Divide_Integer | Modulo_Integer =>
               Runtime_Stacks.Pop (Stack, Right_Value, Stack_Result);
               if Stack_Result /= Runtime_Stacks.Stack_Ok or else
                 Right_Value.Kind /= Integer_Value or else
                 Program_Length (PC) + 1 >= Item.Content.Length
               then
                  Status := Invalid_Bytecode;
                  State.Terminal := True;
                  State.Terminal_Status := Invalid_Bytecode;
                  Done := True;
               else
                  Runtime_Stacks.Pop (Stack, Left_Value, Stack_Result);
                  if Stack_Result /= Runtime_Stacks.Stack_Ok or else
                    Left_Value.Kind /= Integer_Value
                  then
                     Status := Invalid_Bytecode;
                     State.Terminal := True;
                     State.Terminal_Status := Invalid_Bytecode;
                     Done := True;
                  else
                     case Item.Content.Code (PC).Op is
                        when Subtract_Integer =>
                           CCL.Checked_Arithmetic.Subtract
                             (Left_Value.Integer, Right_Value.Integer,
                              Arithmetic_Result, Addition_Overflowed);
                           Arithmetic_Error :=
                             (if Addition_Overflowed then
                                 CCL.Checked_Arithmetic.Arithmetic_Overflow
                              else CCL.Checked_Arithmetic.Arithmetic_Ok);
                        when Multiply_Integer =>
                           CCL.Checked_Arithmetic.Multiply
                             (Left_Value.Integer, Right_Value.Integer,
                              Arithmetic_Result, Addition_Overflowed);
                           Arithmetic_Error :=
                             (if Addition_Overflowed then
                                 CCL.Checked_Arithmetic.Arithmetic_Overflow
                              else CCL.Checked_Arithmetic.Arithmetic_Ok);
                        when Divide_Integer =>
                           CCL.Checked_Arithmetic.Divide
                             (Left_Value.Integer, Right_Value.Integer,
                              Arithmetic_Result, Arithmetic_Error);
                        when Modulo_Integer =>
                           CCL.Checked_Arithmetic.Modulo
                             (Left_Value.Integer, Right_Value.Integer,
                              Arithmetic_Result, Arithmetic_Error);
                        when others =>
                           Arithmetic_Result := 0;
                           Arithmetic_Error :=
                             CCL.Checked_Arithmetic.Arithmetic_Overflow;
                     end case;
                     if Arithmetic_Error =
                       CCL.Checked_Arithmetic.Arithmetic_Overflow
                     then
                        Status := Arithmetic_Overflow;
                        State.Terminal := True;
                        State.Terminal_Status := Arithmetic_Overflow;
                        Done := True;
                     elsif Arithmetic_Error =
                       CCL.Checked_Arithmetic.Division_By_Zero
                     then
                        Status := Division_By_Zero;
                        State.Terminal := True;
                        State.Terminal_Status := Division_By_Zero;
                        Done := True;
                     else
                        Runtime_Stacks.Push
                          (Stack, Integer_Constant (Arithmetic_Result),
                           Stack_Result);
                        if Stack_Result = Runtime_Stacks.Stack_Ok then
                           PC := PC + 1;
                        else
                           Status := Invalid_Bytecode;
                           State.Terminal := True;
                           State.Terminal_Status := Invalid_Bytecode;
                           Done := True;
                        end if;
                     end if;
                  end if;
               end if;

            when Call_Function =>
               --  The arguments stay on the stack as the callee's parameters.
               if State.Frame_Count = MAX_FUNCTIONS or else
                 Item.Content.Code (PC).Immediate not in 0 .. Integer_64 (Item.Content.Functions_Length) - 1 or else
                 Program_Length (PC) + 1 >= Item.Content.Length
               then
                  Status := Invalid_Bytecode;
                  State.Terminal := True;
                  State.Terminal_Status := Invalid_Bytecode;
                  Done := True;
               else
                  State.Frames (State.Frame_Count) :=
                    (Return_PC => PC + 1,
                     Callee => Function_Index (Item.Content.Code (PC).Immediate));
                  State.Frame_Count := State.Frame_Count + 1;
                  PC := Item.Content.Functions
                    (Function_Index (Item.Content.Code (PC).Immediate)).Entry_PC;
               end if;

            when Return_Function =>
               --  Keep the result; drop the parameters beneath it.
               if State.Frame_Count = 0 then
                  Status := Invalid_Bytecode;
                  State.Terminal := True;
                  State.Terminal_Status := Invalid_Bytecode;
                  Done := True;
               else
                  declare
                     Frame : constant Call_Frame := State.Frames (State.Frame_Count - 1);
                     Count : constant Parameter_Count := Item.Content.Functions (Frame.Callee).Count;
                     Good  : Boolean;
                  begin
                     Runtime_Stacks.Pop (Stack, Right_Value, Stack_Result);
                     Good := Stack_Result = Runtime_Stacks.Stack_Ok;
                     for P in 1 .. Count loop
                        exit when not Good;
                        Runtime_Stacks.Pop (Stack, Left_Value, Stack_Result);
                        Good := Stack_Result = Runtime_Stacks.Stack_Ok;
                     end loop;
                     if Good then
                        Runtime_Stacks.Push (Stack, Right_Value, Stack_Result);
                        Good := Stack_Result = Runtime_Stacks.Stack_Ok;
                     end if;
                     if Good then
                        State.Frames (State.Frame_Count - 1) := (others => <>);
                        State.Frame_Count := State.Frame_Count - 1;
                        PC := Frame.Return_PC;
                     else
                        Status := Invalid_Bytecode;
                        State.Terminal := True;
                        State.Terminal_Status := Invalid_Bytecode;
                        Done := True;
                     end if;
                  end;
               end if;

            when Push_Text =>
               if Item.Content.Code (PC).Immediate not in
                 0 .. Integer_64 (Item.Content.Constants_Length) - 1
               then
                  Trap (Invalid_Bytecode);
               else
                  Push_Constant
                    (Item.Content, Constant_Index (Item.Content.Code (PC).Immediate),
                     State.Text, Left_Value, Text_Status);
                  if Text_Status /= Text_Regions.Operation_Ok then
                     Trap (Text_Failure (Text_Status));
                  else
                     Push_Next (Left_Value);
                  end if;
               end if;

            when Concat_Text =>
               Pop_Text (Right_Value);
               if not Done then Pop_Text (Left_Value); end if;
               if not Done then
                  Concat_Texts (State.Text, Left_Value, Right_Value, Joined, Text_Status);
                  if Text_Status /= Text_Regions.Operation_Ok then
                     Trap (Text_Failure (Text_Status));
                  else
                     Push_Next (Joined);
                  end if;
               end if;

            when Text_Builtin =>
               declare
                  Operation : CCL.Text_Operations.Operation;
                  Known : Boolean;
                  Operands : Text_Operands := [others => (others => <>)];
                  Subject, Answer : Value;
                  Outcome : Execution_Status;
               begin
                  Find_Operation (Item.Content.Code (PC).Immediate, Operation, Known);
                  if not Known then
                     Trap (Invalid_Bytecode);
                  else
                     Pop_Text (Subject);
                     for I in reverse 1 .. CCL.Text_Operations.Signature_Of (Operation).Count loop
                        exit when Done;
                        Runtime_Stacks.Pop (Stack, Operands (I), Stack_Result);
                        if Stack_Result /= Runtime_Stacks.Stack_Ok or else
                          Operands (I).Kind /= Kind_Of (CCL.Text_Operations.Signature_Of (Operation).Operands (I))
                        then
                           Trap (Invalid_Bytecode);
                        end if;
                     end loop;
                     if not Done then
                        Run_Text_Builtin (State.Text, Operation, Operands, Subject, Answer, Outcome);
                        if Outcome /= Completed then
                           Trap (Outcome);
                        else
                           Push_Next (Answer);
                        end if;
                     end if;
                  end if;
               end;

            when Length_Text =>
               Pop_Text (Left_Value);
               if not Done then
                  Push_Next (Integer_Constant
                    (Integer_64 (Text_Regions.Length (Left_Value.Text))));
               end if;

            when Equal_Text =>
               Pop_Text (Right_Value);
               if not Done then Pop_Text (Left_Value); end if;
               if not Done then
                  Equal_Texts (State.Text, Left_Value, Right_Value, Same_Text, Text_Status);
                  if Text_Status /= Text_Regions.Operation_Ok then
                     Trap (Text_Failure (Text_Status));
                  else
                     Push_Next (Boolean_Constant (Same_Text));
                  end if;
               end if;

            when Equal_Integer | Less_Integer | Less_Equal_Integer | Equal_Boolean =>
               Runtime_Stacks.Pop (Stack, Right_Value, Stack_Result);
               if Stack_Result /= Runtime_Stacks.Stack_Ok or else
                 Right_Value.Kind /=
                   (if Item.Content.Code (PC).Op = Equal_Boolean then Boolean_Value else Integer_Value) or else
                 Program_Length (PC) + 1 >= Item.Content.Length
               then
                  Status := Invalid_Bytecode;
                  State.Terminal := True;
                  State.Terminal_Status := Invalid_Bytecode;
                  Done := True;
               else
                  Runtime_Stacks.Pop (Stack, Left_Value, Stack_Result);
                  if Stack_Result /= Runtime_Stacks.Stack_Ok or else
                    Left_Value.Kind /= Right_Value.Kind
                  then
                     Status := Invalid_Bytecode;
                     State.Terminal := True;
                     State.Terminal_Status := Invalid_Bytecode;
                     Done := True;
                  else
                     Runtime_Stacks.Push
                       (Stack,
                        Boolean_Constant
                          (case Item.Content.Code (PC).Op is
                              when Less_Integer => Left_Value.Integer < Right_Value.Integer,
                              when Less_Equal_Integer => Left_Value.Integer <= Right_Value.Integer,
                              when Equal_Boolean => Left_Value.Boolean = Right_Value.Boolean,
                              when others => Left_Value.Integer = Right_Value.Integer),
                        Stack_Result);
                     if Stack_Result = Runtime_Stacks.Stack_Ok then
                        PC := PC + 1;
                     else
                        Status := Invalid_Bytecode;
                        State.Terminal := True;
                        State.Terminal_Status := Invalid_Bytecode;
                        Done := True;
                     end if;
                  end if;
               end if;

            when Not_Boolean =>
               Runtime_Stacks.Pop (Stack, Right_Value, Stack_Result);
               if Stack_Result /= Runtime_Stacks.Stack_Ok or else
                 Right_Value.Kind /= Boolean_Value or else
                 Program_Length (PC) + 1 >= Item.Content.Length
               then
                  Status := Invalid_Bytecode;
                  State.Terminal := True;
                  State.Terminal_Status := Invalid_Bytecode;
                  Done := True;
               else
                  Runtime_Stacks.Push
                    (Stack,
                     Boolean_Constant (not Right_Value.Boolean),
                     Stack_Result);
                  if Stack_Result = Runtime_Stacks.Stack_Ok then
                     PC := PC + 1;
                  else
                     Status := Invalid_Bytecode;
                     State.Terminal := True;
                     State.Terminal_Status := Invalid_Bytecode;
                     Done := True;
                  end if;
               end if;

            when Drop =>
               Runtime_Stacks.Pop (Stack, Right_Value, Stack_Result);
               if Stack_Result /= Runtime_Stacks.Stack_Ok or else
                 Program_Length (PC) + 1 >= Item.Content.Length
               then
                  Status := Invalid_Bytecode;
                  State.Terminal := True;
                  State.Terminal_Status := Invalid_Bytecode;
                  Done := True;
               else
                  PC := PC + 1;
               end if;

            when Jump =>
               if Item.Content.Code (PC).Target <= PC
                 or else Program_Length
                    (Item.Content.Code (PC).Target) >=
                      Item.Content.Length
               then
                  Status := Invalid_Bytecode;
                  State.Terminal := True;
                  State.Terminal_Status := Invalid_Bytecode;
                  Done := True;
               else
                  PC := Item.Content.Code (PC).Target;
               end if;

            when Jump_If_False =>
               Runtime_Stacks.Pop (Stack, Right_Value, Stack_Result);
               if Stack_Result /= Runtime_Stacks.Stack_Ok or else
                 Right_Value.Kind /= Boolean_Value or else
                   Item.Content.Code (PC).Target <= PC
                 or else Program_Length
                    (Item.Content.Code (PC).Target) >=
                      Item.Content.Length or else
                  Program_Length (PC) + 1 >= Item.Content.Length
               then
                  Status := Invalid_Bytecode;
                  State.Terminal := True;
                  State.Terminal_Status := Invalid_Bytecode;
                  Done := True;
               else
                  if not Right_Value.Boolean then
                     PC := Item.Content.Code (PC).Target;
                  else
                     PC := PC + 1;
                  end if;
               end if;

            when Invoke_Import =>
               declare
                  Import_Number : constant Import_Index :=
                    Item.Content.Code (PC).Import;
                  Operation : constant Import_Declaration := Item.Content.Imports (Import_Number);
                  Argument_Valid : Boolean := True;
               begin
                  State.Waiting_Receiver := CCL.Resources.No_Reference;
                  if Natural (Import_Number) < Item.Content.Imports_Length and then
                    (not Operation.Ownership_Argument or else Has_Receiver (Operation)) then
                     Runtime_Stacks.Pop (Stack, Right_Value, Stack_Result);
                     Argument_Valid := Stack_Result = Runtime_Stacks.Stack_Ok and then
                       Right_Value.Kind = Operation.Argument and then
                       Right_Value.Data_Type = Operation.Argument_Data_Type and then
                       Well_Typed (Item.Content.Data_Types, Right_Value) and then
                       Right_Value.Copyable and then Right_Value.Type_Tag = 0;
                  end if;
                  if Natural (Import_Number) >= Item.Content.Imports_Length or else not Argument_Valid
                  then
                     Status := Invalid_Bytecode;
                     State.Terminal := True;
                     State.Terminal_Status := Invalid_Bytecode;
                  elsif Item.Content.Imports (Import_Number).Ownership_Argument
                  then
                     -- A terminal completion has already returned its borrow
                     -- or applied its disposition. Start a fresh lifecycle for
                     -- the next call, never reset an offered/accepted call.
                     if CCL.Imports.Phase (State.Import_Lifecycle) = CCL.Imports.Import_Completed then
                        CCL.Imports.Initialize (State.Import_Lifecycle);
                     end if;
                     CCL.Imports.Offer
                       (State.Import_Lifecycle,
                        Item.Content.Imports (Import_Number).Local,
                        Item.Content.Imports (Import_Number).Transfer,
                        Item.Content.Imports (Import_Number).Cancellation,
                        Item.Content.Imports (Import_Number).Success_Verb,
                        Item.Content.Imports (Import_Number).Failure_Verb,
                        Item.Content.Imports (Import_Number).Cancel_Verb,
                        Import_Error);
                     if Import_Error /= CCL.Imports.Import_Valid then
                        Status := Invalid_Bytecode;
                        State.Terminal := True;
                        State.Terminal_Status := Invalid_Bytecode;
                     else
                        Waiting := True;
                        Waiting_Owned := True;
                        State.Waiting_Import := Import_Number;
                        State.Waiting_Result_Kind :=
                          Item.Content.Imports (Import_Number).Result;
                        if Has_Receiver (Operation) then
                           State.Waiting_Receiver := State.Locals (Operation.Local).Resource;
                           State.Waiting_Argument := Right_Value;
                        else
                           State.Waiting_Argument := State.Locals (Operation.Local);
                        end if;
                        Status := Waiting_For_Host;
                     end if;
                  else
                        Waiting := True;
                        State.Waiting_Import := Import_Number;
                        State.Waiting_Result_Kind :=
                          Item.Content.Imports (Import_Number).Result;
                        State.Waiting_Argument := Right_Value;
                        Waiting_Owned := False;
                        Status := Waiting_For_Host;
                  end if;
                  Done := True;
               end;

            when Initialize_Local =>
               Runtime_Stacks.Pop (Stack, Right_Value, Stack_Result);
               if Stack_Result /= Runtime_Stacks.Stack_Ok or else
                 Natural (Item.Content.Code (PC).Local) >=
                   Item.Content.Locals_Length or else
                 Right_Value.Kind /= Item.Content.Local_Kinds
                   (Item.Content.Code (PC).Local) or else
                 Right_Value.Type_Tag /= Item.Content.Local_Types
                   (Item.Content.Code (PC).Local) or else
                 Right_Value.Data_Type /= Item.Content.Local_Data_Types
                   (Item.Content.Code (PC).Local) or else
                 (Right_Value.Copyable and then Item.Content.Types
                   (Item.Content.Local_Types (Item.Content.Code (PC).Local)).Mode /= CCL.Ownership.Unrestricted) or else
                 Program_Length (PC) + 1 >= Item.Content.Length
               then
                  Status := Invalid_Bytecode;
                  State.Terminal := True;
                  State.Terminal_Status := Invalid_Bytecode;
                  Done := True;
               else
                  CCL.Ownership.Declare_Binding
                    (State.Ownership, Item.Content.Code (PC).Local,
                     Item.Content.Local_Types (Item.Content.Code (PC).Local),
                     Own_Error);
                  if Own_Error /= CCL.Ownership.Ownership_Valid then
                     Status := Invalid_Bytecode;
                     State.Terminal := True;
                     State.Terminal_Status := Invalid_Bytecode;
                     Done := True;
                  else
                     State.Locals (Item.Content.Code (PC).Local) := Right_Value;
                     PC := PC + 1;
                  end if;
               end if;

            when Copy_Local | Move_Local | Drop_Local |
                 Borrow_Local_RO | Return_Local_RO |
                 Borrow_Local_RW | Return_Local_RW |
                 Apply_Local_Disposition =>
               if Program_Length (PC) + 1 >= Item.Content.Length then
                  Status := Invalid_Bytecode;
                  State.Terminal := True;
                  State.Terminal_Status := Invalid_Bytecode;
                  Done := True;
               else
                  case Item.Content.Code (PC).Op is
                     when Copy_Local =>
                        CCL.Ownership.Copy_Value
                          (State.Ownership, Item.Content.Types,
                           Item.Content.Code (PC).Local,
                           Own_Error);
                     when Move_Local =>
                        CCL.Ownership.Move_Value
                          (State.Ownership,
                           Item.Content.Code (PC).Local,
                           Own_Error);
                     when Drop_Local =>
                        CCL.Ownership.Drop_Value
                          (State.Ownership, Item.Content.Types,
                           Item.Content.Code (PC).Local,
                           Own_Error);
                     when Borrow_Local_RO =>
                        CCL.Ownership.Borrow_RO
                          (State.Ownership,
                           Item.Content.Code (PC).Local,
                           Own_Error);
                     when Return_Local_RO =>
                        CCL.Ownership.Return_RO
                          (State.Ownership,
                           Item.Content.Code (PC).Local,
                           Own_Error);
                     when Borrow_Local_RW =>
                        CCL.Ownership.Borrow_RW
                          (State.Ownership,
                           Item.Content.Code (PC).Local,
                           Own_Error);
                     when Return_Local_RW =>
                        CCL.Ownership.Return_RW
                          (State.Ownership,
                           Item.Content.Code (PC).Local,
                           Own_Error);
                     when Apply_Local_Disposition =>
                        CCL.Ownership.Apply_Disposition
                          (State.Ownership, Item.Content.Types,
                           Item.Content.Code (PC).Local,
                           Item.Content.Code (PC).Verb,
                           Own_Error);
                     when others =>
                        Own_Error := CCL.Ownership.Ownership_Valid;
                  end case;
                  if Own_Error /= CCL.Ownership.Ownership_Valid then
                     Status := Invalid_Bytecode;
                     State.Terminal := True;
                     State.Terminal_Status := Invalid_Bytecode;
                     Done := True;
                  else
                     if Item.Content.Code (PC).Op in Copy_Local | Move_Local then
                        Right_Value := State.Locals (Item.Content.Code (PC).Local);
                        Right_Value.Copyable := Item.Content.Code (PC).Op = Copy_Local;
                        Runtime_Stacks.Push
                          (Stack,
                           Right_Value,
                           Stack_Result);
                        if Stack_Result /= Runtime_Stacks.Stack_Ok then
                           Status := Invalid_Bytecode;
                           State.Terminal := True;
                           State.Terminal_Status := Invalid_Bytecode;
                           Done := True;
                        else
                           PC := PC + 1;
                        end if;
                     else
                        PC := PC + 1;
                     end if;
                  end if;
               end if;
         end case;
         end if;
      end loop;

      if not Done and then
        not CCL.Execution_Budgets.Has_Fuel (State.Execution_Budget)
      then
         Status := Fuel_Exhausted;
         State.Terminal := True;
         State.Terminal_Status := Fuel_Exhausted;
      elsif not Done then
         Status := Paused;
      end if;

      State.Stack := Stack;
      State.PC := PC;
      pragma Assert
        (Waiting_Owned or else
         CCL.Imports.Phase (State.Import_Lifecycle) not in
           CCL.Imports.Import_Offered | CCL.Imports.Import_Accepted);
      State.Waiting := Waiting;
      State.Waiting_Owned := Waiting_Owned;

      Result :=
        (Status         => Status,
         Has_Value      => State.Has_Value,
         Result_Value   => State.Result_Value,
         Fuel_Remaining =>
           CCL.Execution_Budgets.Remaining (State.Execution_Budget),
         Steps          => CCL.Execution_Budgets.Steps (State.Execution_Budget),
         Requested_Import => State.Waiting_Import,
         Request_Argument => State.Waiting_Argument,
         Result_Text_Value => Result_Text_Of (State.Text, State.Result_Value),
         Request_Receiver => State.Waiting_Receiver,
         Request_Owned => Waiting_Owned,
         Requested_Authority =>
           (if Waiting then
               Item.Content.Imports (State.Waiting_Import).Authority
            else No_Authority),
         Requested_Binding =>
           (if Waiting then
               Item.Content.Imports (State.Waiting_Import).Binding
            else 0));
   end Continue_With_Native;

   type No_Native_Store is null record;
   procedure Reject_Native
     (Store : in out No_Native_Store; Types : CCL.Types.Registry;
      Op : Instruction; Source : Value; Result : out Value;
      Alternative : out CCL.Types.Component_Count; Accepted : out Boolean) is
      pragma Unreferenced (Store, Types, Op, Source);
   begin
      Result := (others => <>); Alternative := 0; Accepted := False;
   end Reject_Native;
   procedure Scalar_Continue is new Continue_With_Native (No_Native_Store, Reject_Native);
   procedure Continue_Execution_For
     (Item : Validated_Program; State : in out Machine_State;
      Instructions : Natural; Result : out Execution_Result) is
      Store : No_Native_Store;
   begin
      Scalar_Continue (Item, State, Store, Instructions, Result);
   end Continue_Execution_For;

   procedure Continue_Execution
     (Item   : Validated_Program;
      State  : in out Machine_State;
      Result : out Execution_Result)
   is
   begin
      Continue_Execution_For
        (Item, State,
         Natural (CCL.Execution_Budgets.Limit (State.Execution_Budget)),
         Result);
   end Continue_Execution;

   function Snapshot (State : Machine_State) return Machine_Snapshot is
     ((Instruction => State.PC,
       Fuel_Remaining =>
         CCL.Execution_Budgets.Remaining (State.Execution_Budget),
       Steps => CCL.Execution_Budgets.Steps (State.Execution_Budget),
       Waiting => State.Waiting,
       Terminal => State.Terminal,
       Status => State.Terminal_Status));

   procedure Inspect
     (Item   : Validated_Program;
      State  : Machine_State;
      Result : out Inspection_Snapshot)
   is
      Stack_Copy : Runtime_Stacks.Stack := State.Stack;
      Stack_Value : Value;
      Stack_Result : Runtime_Stacks.Operation_Result;
      Local : CCL.Ownership.Binding_Id;
      Type_Tag : CCL.Ownership.Type_Id;
   begin
      Result := (others => <>);
      Result.Machine := Snapshot (State);
      Result.Locals_Length := Item.Content.Locals_Length;
      Result.Waiting_Import := State.Waiting_Import;
      Result.Waiting_Result_Kind := State.Waiting_Result_Kind;
      Result.Waiting_Argument := State.Waiting_Argument;
      Result.Waiting_Receiver := State.Waiting_Receiver;
      Result.Waiting_Owned := State.Waiting_Owned;
      Result.Import_Phase := CCL.Imports.Phase (State.Import_Lifecycle);

      for Position in Stack_Index loop
         Runtime_Stacks.Pop (Stack_Copy, Stack_Value, Stack_Result);
         exit when Stack_Result /= Runtime_Stacks.Stack_Ok;
         Result.Stack (Position) := Stack_Value;
         Result.Stack_Length := Stack_Depth (Natural (Position) + 1);
      end loop;

      if Item.Content.Locals_Length > 0 then
         for Position in 0 .. Item.Content.Locals_Length - 1 loop
            Local := CCL.Ownership.Binding_Id (Position);
            Type_Tag := Item.Content.Local_Types (Local);
            Result.Locals (Local) :=
              (Value => State.Locals (Local),
               Kind => Item.Content.Local_Kinds (Local),
               Type_Tag => Type_Tag,
               Mode => Item.Content.Types (Type_Tag).Mode,
               Ownership_State =>
                 CCL.Ownership.State (State.Ownership, Local),
               Read_Borrows =>
                 CCL.Ownership.Read_Borrows (State.Ownership, Local),
               Write_Borrow =>
                 CCL.Ownership.Has_Write_Borrow (State.Ownership, Local));
         end loop;
      end if;
   end Inspect;

   procedure Stop (State : in out Machine_State) is
   begin
      if not State.Terminal then
         State.Terminal := True;
         State.Terminal_Status := Stopped;
      end if;
   end Stop;

   procedure Complete_Checked_Host_Call
     (Item     : Validated_Program;
      State    : in out Machine_State;
      Response : Value;
      Accepted : Boolean;
      Native_Response : Boolean;
      Resource_Response : Boolean := False)
   is
      Import_Error : CCL.Imports.Import_Error;
      Stack_Result : Runtime_Stacks.Operation_Result;
   begin
      if not State.Waiting or else State.Terminal then
         null;
      elsif not Accepted then
         if State.Waiting_Owned then
            CCL.Imports.Complete
              (State.Import_Lifecycle, State.Ownership, Item.Content.Types,
               CCL.Imports.Import_Failed, Import_Error);
            if Import_Error /= CCL.Imports.Import_Valid then
               State.Terminal := True;
               State.Terminal_Status := Invalid_Bytecode;
               return;
            end if;
         end if;
         State.Waiting := False;
         State.Waiting_Owned := False;
         State.Terminal := True;
         State.Terminal_Status := Host_Call_Failed;
      else
         if State.Waiting_Owned then
            CCL.Imports.Complete
              (State.Import_Lifecycle, State.Ownership, Item.Content.Types,
               CCL.Imports.Import_Succeeded, Import_Error);
            if Import_Error /= CCL.Imports.Import_Valid then
               State.Terminal := True;
               State.Terminal_Status := Invalid_Bytecode;
               return;
            end if;
         end if;
         if (Response.Kind = Object_Value and then not Native_Response) or else
           (Response.Kind = Resource_Value and then not Resource_Response) or else
           Response.Kind /= State.Waiting_Result_Kind or else
           Response.Data_Type /= Item.Content.Imports (State.Waiting_Import).Result_Data_Type or else
           not Well_Typed (Item.Content.Data_Types, Response) or else
           Response.Copyable /= (Response.Kind /= Resource_Value) or else
           Response.Type_Tag /= Item.Content.Imports (State.Waiting_Import).Result_Type_Tag or else
           Program_Length (State.PC) + 1 >= Item.Content.Length
         then
            State.Terminal := True;
            State.Terminal_Status := Invalid_Bytecode;
         else
            Runtime_Stacks.Push (State.Stack, Response, Stack_Result);
            if Stack_Result = Runtime_Stacks.Stack_Ok then
               State.PC := State.PC + 1;
            else
               State.Terminal := True;
               State.Terminal_Status := Invalid_Bytecode;
            end if;
         end if;
         State.Waiting := False;
         State.Waiting_Owned := False;
      end if;
   end Complete_Checked_Host_Call;

   procedure Complete_Host_Call
     (Item : Validated_Program; State : in out Machine_State;
      Response : Value; Accepted : Boolean) is
   begin
      Complete_Checked_Host_Call (Item, State, Response, Accepted, False);
   end Complete_Host_Call;

   procedure Acknowledge_Host_Submission
     (Item     : Validated_Program;
      State    : in out Machine_State;
      Accepted : Boolean)
   is
      Import_Error : CCL.Imports.Import_Error;
   begin
      if not State.Waiting or else not State.Waiting_Owned or else
        State.Terminal
      then
         null;
      elsif Accepted then
         CCL.Imports.Accept_Submission
           (State.Import_Lifecycle, State.Ownership, Item.Content.Types,
            Import_Error);
         if Import_Error /= CCL.Imports.Import_Valid then
            State.Terminal := True;
            State.Terminal_Status := Invalid_Bytecode;
         end if;
      else
         CCL.Imports.Reject_Submission
           (State.Import_Lifecycle, Import_Error);
         State.Terminal := True;
         if Import_Error = CCL.Imports.Import_Valid then
            State.Waiting := False;
            State.Waiting_Owned := False;
            State.Terminal_Status := Host_Call_Failed;
         else
            State.Terminal_Status := Invalid_Bytecode;
         end if;
      end if;
   end Acknowledge_Host_Submission;

   procedure Execute
     (Item   : Validated_Program;
      Fuel   : Natural;
      Result : out Execution_Result)
   is
      State : Machine_State;
   begin
      Initialize (Item, Fuel, State);
      Continue_Execution (Item, State, Result);
   end Execute;
end CCL.VM;

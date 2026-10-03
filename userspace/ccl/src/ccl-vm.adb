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
         when Object_Value => return "<object " & CCL.Types.Image (D.Identifier) & ">";
         when Function_Value => return "<function" & Integer_64'Image (Item.Integer) & ">";
         when Resource_Value => return "<resource " & CCL.Types.Image (D.Identifier) & ">";
         --  The characters live in the run's region (Execution_Result).
         when Text_Value => return "<text>";
         when Character_Value => return "'" & Character'Val (Item.Integer) & "'";
         when List_Value => return "<list " & CCL.Types.Image (D.Identifier) & ">";
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

   package L renames CCL.List_Operations;
   use type L.Operation;

   procedure Find_List_Operation
     (Immediate : Integer_64; Item : out L.Operation; Found : out Boolean) is
   begin
      Item := L.Operation'First;
      Found := False;
      for Candidate in L.Operation loop
         if Integer_64 (L.Operation'Enum_Rep (Candidate)) = Immediate then
            Item := Candidate;
            Found := True;
         end if;
      end loop;
   end Find_List_Operation;

   use type L.Apply_Operation;
   use type CCL.Streams.View_Kind;

   procedure Find_View
     (Immediate : Integer_64; Item : out CCL.Streams.View_Kind; Found : out Boolean) is
   begin
      Item := CCL.Streams.View_Kind'First;
      Found := False;
      for Candidate in CCL.Streams.View_Kind loop
         if Integer_64 (CCL.Streams.View_Kind'Enum_Rep (Candidate)) = Immediate then
            Item := Candidate;
            Found := True;
         end if;
      end loop;
   end Find_View;

   procedure Find_Apply_Operation
     (Immediate : Integer_64; Item : out L.Apply_Operation; Found : out Boolean) is
   begin
      Item := L.Apply_Operation'First;
      Found := False;
      for Candidate in L.Apply_Operation loop
         if Integer_64 (L.Apply_Operation'Enum_Rep (Candidate)) = Immediate then
            Item := Candidate;
            Found := True;
         end if;
      end loop;
   end Find_Apply_Operation;

   --  The registry's List<Element>, as the type checker specialized it (each
   --  builds a list of its function's result type, a range type's values as
   --  Integers, as the interpreter does), or Invalid_Type.
   function List_Of
     (Types : CCL.Types.Registry; Result : CCL.Types.Type_Reference) return CCL.Types.Type_Reference is
      Element : constant CCL.Types.Type_Reference :=
        (if CCL.Types.Is_Range (Types, Result) then CCL.Types.Integer_Type else Result);
   begin
      for Ref in CCL.Types.Declared_Type'First .. CCL.Types.Last (Types) loop
         if CCL.Types.Is_List (Types, Ref) and then CCL.Types.Element_Of (Types, Ref) = Element then
            return Ref;
         end if;
      end loop;
      return CCL.Types.Invalid_Type;
   end List_Of;

   --  Whether List_Apply's Operation applies a function of type Fn to the
   --  list type List_Type: its parameters (fold: accumulator then element)
   --  and result, as the type checker decides.
   function Apply_Fits
     (Types : CCL.Types.Registry; Operation : L.Apply_Operation;
      List_Type, Fn : CCL.Types.Type_Reference) return Boolean is
     (Supported_List (Types, List_Type) and then CCL.Types.Is_Function (Types, Fn) and then
      CCL.Types.Describe (Types, Fn).Count = (if Operation = L.Fold_Items then 3 else 2) and then
      (CCL.Types.Describe (Types, Fn).Parts
         ((if Operation = L.Fold_Items then 2 else 1)).Payload = CCL.Types.Element_Of (Types, List_Type) or else
       (CCL.Types.Element_Of (Types, List_Type) = CCL.Types.Integer_Type and then
        CCL.Types.Is_Range (Types, CCL.Types.Describe (Types, Fn).Parts
          ((if Operation = L.Fold_Items then 2 else 1)).Payload))) and then
      (case Operation is
          when L.Each_Items =>
             Supported_List (Types, List_Of (Types, CCL.Types.Describe (Types, Fn).Parts (2).Payload)),
          when L.Where_Items | L.Any_Items | L.All_Items | L.Count_Items =>
             CCL.Types.Describe (Types, Fn).Parts (2).Payload = CCL.Types.Boolean_Type,
          when L.Fold_Items =>
             CCL.Types.Describe (Types, Fn).Parts (3).Payload = CCL.Types.Describe (Types, Fn).Parts (1).Payload,
          when L.Sort_By_Items =>
             CCL.Types.Describe (Types, Fn).Parts (2).Payload in CCL.Types.Integer_Type | CCL.Types.String_Type));

   --  Whether a list built-in applies to List_Type (the subject's list type,
   --  or the result's for range and split), as the type checker decides.
   function List_Builtin_Applies
     (Types : CCL.Types.Registry; Item : L.Operation; List_Type : CCL.Types.Type_Reference)
      return Boolean is
     (Supported_List (Types, List_Type) and then
      (case Item is
          when L.Sort_Items | L.Contains_Item =>
             CCL.Types.Element_Of (Types, List_Type) in CCL.Types.Integer_Type | CCL.Types.String_Type |
               CCL.Types.Character_Type or else
             (Item = L.Contains_Item and then CCL.Types.Element_Of (Types, List_Type) = CCL.Types.Boolean_Type) or else
             CCL.Types.Is_Enumeration (Types, CCL.Types.Element_Of (Types, List_Type)),
          when L.Sum_Items | L.Min_Items | L.Max_Items | L.Range_Items =>
             Element_Kind (Types, List_Type) = Integer_Value,
          when L.Join_Items | L.Split_Text => Element_Kind (Types, List_Type) = Text_Value,
          when others => True));

   --  The operand kind of a comparison opcode.
   function Comparison_Kind (Op : Op_Code) return Value_Kind is
     (case Op is
         when Equal_Boolean => Boolean_Value,
         when Equal_Character => Character_Value,
         when others => Integer_Value);

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

      --  A parameter or result: any value a run holds (not a resource).
      function Function_Data (Kind : Value_Kind; Ref : CCL.Types.Type_Reference) return Boolean is
        (case Kind is
            when Integer_Value =>
               Ref = CCL.Types.Invalid_Type or else CCL.Types.Is_Stream (Candidate.Data_Types, Ref),
            when Boolean_Value | Text_Value | Character_Value => Ref = CCL.Types.Invalid_Type,
            when Variant_Value => CCL.Types.Is_Scalar_Sum (Candidate.Data_Types, Ref),
            when List_Value => Supported_List (Candidate.Data_Types, Ref),
            when Object_Value => Node_Type (Candidate.Data_Types, Ref),
            when Function_Value => CCL.Types.Is_Function (Candidate.Data_Types, Ref),
            when Resource_Value => False);

      --  Whether function F is a value of the function type T: its
      --  parameters after the captures, and its result, are T's.
      function Matches_Signature (F : Function_Index; T : CCL.Types.Type_Reference) return Boolean is
        (CCL.Types.Is_Function (Candidate.Data_Types, T) and then
         CCL.Types.Describe (Candidate.Data_Types, T).Count >= 1 and then
         Candidate.Functions (F).Captures <= Candidate.Functions (F).Count and then
         Candidate.Functions (F).Count - Candidate.Functions (F).Captures =
           CCL.Types.Describe (Candidate.Data_Types, T).Count - 1 and then
         (for all P in 1 .. CCL.Types.Describe (Candidate.Data_Types, T).Count - 1 =>
            Candidate.Functions (F).Captures + P in Parameter_Index and then
            Candidate.Functions (F).Kinds (Candidate.Functions (F).Captures + P) =
              Kind_For_Type (Candidate.Data_Types, CCL.Types.Describe (Candidate.Data_Types, T).Parts (P).Payload) and then
            Candidate.Functions (F).Data_Types (Candidate.Functions (F).Captures + P) =
              Reference_For_Type (Candidate.Data_Types, CCL.Types.Describe (Candidate.Data_Types, T).Parts (P).Payload)) and then
         Candidate.Functions (F).Result =
           Kind_For_Type (Candidate.Data_Types,
             CCL.Types.Describe (Candidate.Data_Types, T).Parts (CCL.Types.Describe (Candidate.Data_Types, T).Count).Payload) and then
         Candidate.Functions (F).Result_Data_Type =
           Reference_For_Type (Candidate.Data_Types,
             CCL.Types.Describe (Candidate.Data_Types, T).Parts (CCL.Types.Describe (Candidate.Data_Types, T).Count).Payload));

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
         --  Text and characters may live in a compiler-created (dynamic)
         --  local, never in an initial local the host supplies: hosts cannot
         --  forge descriptors or out-of-range codes.
         if not Known_Value_Type (Candidate.Data_Types,
           Candidate.Local_Kinds (L - 1), Candidate.Local_Data_Types (L - 1)) and then
           not (L - 1 >= Candidate.Locals_Length - Candidate.Dynamic_Locals_Length and then
                ((Candidate.Local_Kinds (L - 1) in Text_Value | Character_Value and then
                  Candidate.Local_Data_Types (L - 1) = CCL.Types.Invalid_Type) or else
                 (Candidate.Local_Kinds (L - 1) = List_Value and then
                  Supported_List (Candidate.Data_Types, Candidate.Local_Data_Types (L - 1))) or else
                 (Candidate.Local_Kinds (L - 1) = Object_Value and then
                  Node_Type (Candidate.Data_Types, Candidate.Local_Data_Types (L - 1))) or else
                 (Candidate.Local_Kinds (L - 1) = Function_Value and then
                  CCL.Types.Is_Function (Candidate.Data_Types, Candidate.Local_Data_Types (L - 1))) or else
                 (Candidate.Local_Kinds (L - 1) = Integer_Value and then
                  CCL.Types.Is_Stream (Candidate.Data_Types, Candidate.Local_Data_Types (L - 1)))))
         then Error := Invalid_Data_Type; return; end if;
         if Candidate.Local_Kinds (L - 1) = Resource_Value and then
           (Candidate.Local_Types (L - 1) >= Candidate.Types_Length or else
            Candidate.Types (Candidate.Local_Types (L - 1)).Mode = CCL.Ownership.Unrestricted)
         then Error := Invalid_Ownership; return; end if;
      end loop;
      for M in 1 .. Candidate.Matches_Length loop
         if CCL.Types.Describe (Candidate.Data_Types, Candidate.Matches (M - 1).Data_Type).Form /= CCL.Types.Sum or else
           not CCL.Objects.Storable (Candidate.Data_Types, Candidate.Matches (M - 1).Data_Type)
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
              Decl.Captures > Decl.Count or else
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

         if Instruction.Op not in Make_Variant | Equal_Variant | Project_Field | Variant_To_Text |
           New_List | Fill_List | Length_List | List_At | List_Builtin | Make_Node | Check_Range |
           Make_Closure | Call_Value | List_Apply | Push_Stream | Stream_View and then
           (Instruction.Data_Type /= CCL.Types.Invalid_Type or else Instruction.Alternative /= 0)
         then Error := Invalid_Data_Type; exit; end if;

         case Instruction.Op is
            when Project_Field =>
               D := CCL.Types.Describe (Candidate.Data_Types, Instruction.Data_Type);
               if D.Form /= CCL.Types.Product or else
                 not Node_Type (Candidate.Data_Types, Instruction.Data_Type) or else
                 Instruction.Immediate not in 1 .. Integer_64 (D.Count) or else Instruction.Alternative /= 0
               then Error := Invalid_Data_Type;
               else
                  Pop_Kind (State, Object_Value, Error, Instruction.Data_Type);
                  declare
                     Ref : constant CCL.Types.Type_Reference :=
                       D.Parts (CCL.Types.Component_Index (Instruction.Immediate)).Payload;
                  begin
                     Push_Kind (State, Kind_For_Type (Candidate.Data_Types, Ref), Error,
                                Reference_For_Type (Candidate.Data_Types, Ref));
                  end;
               end if;
            when Make_Closure =>
               if not CCL.Types.Is_Function (Candidate.Data_Types, Instruction.Data_Type) or else
                 Instruction.Alternative /= 0 or else
                 Instruction.Immediate not in 0 .. Integer_64 (Candidate.Functions_Length) - 1
               then Error := Invalid_Function;
               elsif not Matches_Signature (Function_Index (Instruction.Immediate), Instruction.Data_Type) then
                  Error := Invalid_Function;
               else
                  declare
                     Decl : constant Function_Declaration :=
                       Candidate.Functions (Function_Index (Instruction.Immediate));
                  begin
                     for P in reverse 1 .. Decl.Captures loop
                        Pop_Kind (State, Decl.Kinds (P), Error, Decl.Data_Types (P));
                     end loop;
                     Push_Kind (State, Function_Value, Error, Instruction.Data_Type);
                  end;
               end if;
            when Call_Value =>
               if not CCL.Types.Is_Function (Candidate.Data_Types, Instruction.Data_Type) or else
                 CCL.Types.Describe (Candidate.Data_Types, Instruction.Data_Type).Count = 0 or else
                 Instruction.Immediate /= 0 or else Instruction.Alternative /= 0
               then Error := Invalid_Function;
               else
                  declare
                     D : constant CCL.Types.Description :=
                       CCL.Types.Describe (Candidate.Data_Types, Instruction.Data_Type);
                  begin
                     --  The arguments, then the function value beneath them.
                     for P in reverse 1 .. D.Count - 1 loop
                        Pop_Kind (State, Kind_For_Type (Candidate.Data_Types, D.Parts (P).Payload), Error,
                                  Reference_For_Type (Candidate.Data_Types, D.Parts (P).Payload));
                     end loop;
                     Pop_Kind (State, Function_Value, Error, Instruction.Data_Type);
                     Push_Kind (State, Kind_For_Type (Candidate.Data_Types, D.Parts (D.Count).Payload), Error,
                                Reference_For_Type (Candidate.Data_Types, D.Parts (D.Count).Payload));
                  end;
               end if;
            when List_Apply =>
               --  Operands: the function value, fold's initial value, the
               --  list on top. The function's type is read from the stack.
               declare
                  Operation : L.Apply_Operation;
                  Known : Boolean;
                  Fn_Slot : Stack_Type;
               begin
                  Find_Apply_Operation (Instruction.Immediate, Operation, Known);
                  Stack_Result := Abstract_Stacks.Stack_Ok;
                  if Known then
                     Abstract_Stacks.Peek_At
                       (State.Values, (if Operation = L.Fold_Items then 2 else 1), Fn_Slot, Stack_Result);
                  end if;
                  if not Known or else Instruction.Alternative /= 0 or else
                    Stack_Result /= Abstract_Stacks.Stack_Ok or else Fn_Slot.Kind /= Function_Value or else
                    not Apply_Fits (Candidate.Data_Types, Operation, Instruction.Data_Type, Fn_Slot.Data_Type)
                  then
                     Error := Invalid_Builtin;
                  else
                     declare
                        D : constant CCL.Types.Description := CCL.Types.Describe (Candidate.Data_Types, Fn_Slot.Data_Type);
                        Result : constant CCL.Types.Type_Reference := D.Parts (D.Count).Payload;
                     begin
                        Pop_Kind (State, List_Value, Error, Instruction.Data_Type);
                        if Operation = L.Fold_Items then
                           Pop_Kind (State, Kind_For_Type (Candidate.Data_Types, Result), Error,
                                     Reference_For_Type (Candidate.Data_Types, Result));
                        end if;
                        Pop_Kind (State, Function_Value, Error, Fn_Slot.Data_Type);
                        case Operation is
                           when L.Each_Items =>
                              Push_Kind (State, List_Value, Error, List_Of (Candidate.Data_Types, Result));
                           when L.Where_Items | L.Sort_By_Items =>
                              Push_Kind (State, List_Value, Error, Instruction.Data_Type);
                           when L.Any_Items | L.All_Items =>
                              Push_Kind (State, Boolean_Value, Error);
                           when L.Count_Items =>
                              Push_Kind (State, Integer_Value, Error);
                           when L.Fold_Items =>
                              Push_Kind (State, Kind_For_Type (Candidate.Data_Types, Result), Error,
                                         Reference_For_Type (Candidate.Data_Types, Result));
                        end case;
                     end;
                  end if;
               end;
            when Check_Range =>
               if not CCL.Types.Is_Range (Candidate.Data_Types, Instruction.Data_Type) or else
                 Instruction.Immediate /= 0 or else Instruction.Alternative /= 0
               then Error := Invalid_Data_Type;
               else
                  Pop_Kind (State, Integer_Value, Error);
                  Push_Kind (State, Integer_Value, Error);
               end if;
            when Make_Node =>
               D := CCL.Types.Describe (Candidate.Data_Types, Instruction.Data_Type);
               if not Node_Type (Candidate.Data_Types, Instruction.Data_Type) or else
                 Instruction.Immediate /= 0 or else
                 (if D.Form = CCL.Types.Product then Instruction.Alternative /= 0
                  else Instruction.Alternative not in 1 .. D.Count)
               then Error := Invalid_Data_Type;
               else
                  --  Components in source order; the last is on top.
                  if D.Form = CCL.Types.Product then
                     for P in reverse 1 .. D.Count loop
                        Pop_Kind (State, Kind_For_Type (Candidate.Data_Types, D.Parts (P).Payload), Error,
                                  Reference_For_Type (Candidate.Data_Types, D.Parts (P).Payload));
                     end loop;
                  elsif D.Parts (Instruction.Alternative).Payload /= CCL.Types.Unit_Type then
                     Pop_Kind (State, Kind_For_Type (Candidate.Data_Types, D.Parts (Instruction.Alternative).Payload),
                               Error, Reference_For_Type (Candidate.Data_Types, D.Parts (Instruction.Alternative).Payload));
                  end if;
                  Push_Kind (State, Object_Value, Error, Instruction.Data_Type);
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

            when Push_Stream =>
               if not CCL.Types.Is_Stream (Candidate.Data_Types, Instruction.Data_Type) or else
                 Instruction.Immediate not in 1 .. CCL.Streams.Maximum_Handle or else
                 Instruction.Alternative /= 0
               then Error := Invalid_Data_Type;
               else
                  Push_Kind (State, Integer_Value, Error, Instruction.Data_Type);
               end if;

            when Stream_View =>
               declare
                  View : CCL.Streams.View_Kind;
                  Known : Boolean;
                  Result : CCL.Types.Type_Reference;
               begin
                  Find_View (Instruction.Immediate, View, Known);
                  if not Known then
                     Error := Invalid_Builtin;
                  else
                     Result := Stream_View_Type (Candidate.Data_Types, Instruction.Data_Type, View);
                     if Instruction.Alternative /= 0 or else Result = CCL.Types.Invalid_Type or else
                       not Function_Data (Kind_For_Type (Candidate.Data_Types, Result),
                                          Reference_For_Type (Candidate.Data_Types, Result))
                     then Error := Invalid_Data_Type;
                     else
                        Pop_Kind (State, Integer_Value, Error, Instruction.Data_Type);
                        if View = CCL.Streams.Window_View then
                           Pop_Kind (State, Integer_Value, Error);
                        end if;
                        Push_Kind (State, Kind_For_Type (Candidate.Data_Types, Result), Error,
                                   Reference_For_Type (Candidate.Data_Types, Result));
                     end if;
                  end if;
               end;

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

            when Text_At =>
               Pop_Kind (State, Integer_Value, Error);
               Pop_Kind (State, Text_Value, Error);
               Push_Kind (State, Character_Value, Error);

            when Integer_To_Text =>
               Pop_Kind (State, Integer_Value, Error);
               Push_Kind (State, Text_Value, Error);

            when List_Builtin =>
               declare
                  Operation : L.Operation;
                  Known : Boolean;
                  List_Type : constant CCL.Types.Type_Reference := Instruction.Data_Type;
               begin
                  Find_List_Operation (Instruction.Immediate, Operation, Known);
                  if not Known or else Instruction.Alternative /= 0 or else
                    not List_Builtin_Applies (Candidate.Data_Types, Operation, List_Type)
                  then
                     Error := Invalid_Builtin;
                  else
                     case Operation is
                        when L.Range_Items =>
                           Pop_Kind (State, Integer_Value, Error);
                           Pop_Kind (State, Integer_Value, Error);
                        when L.Split_Text =>
                           Pop_Kind (State, Text_Value, Error);
                           Pop_Kind (State, Text_Value, Error);
                        when others =>
                           Pop_Kind (State, List_Value, Error, List_Type);
                           case Operation is
                              when L.First_Items | L.Last_Items | L.Skip_Items =>
                                 Pop_Kind (State, Integer_Value, Error);
                              when L.Contains_Item =>
                                 Pop_Kind (State, Element_Kind (Candidate.Data_Types, List_Type), Error,
                                           Element_Data_Type (Candidate.Data_Types, List_Type));
                              when L.Join_Items =>
                                 Pop_Kind (State, Text_Value, Error);
                              when others => null;
                           end case;
                     end case;
                     case Operation is
                        when L.Sum_Items | L.Min_Items | L.Max_Items =>
                           Push_Kind (State, Integer_Value, Error);
                        when L.Contains_Item =>
                           Push_Kind (State, Boolean_Value, Error);
                        when L.Join_Items =>
                           Push_Kind (State, Text_Value, Error);
                        when others =>
                           Push_Kind (State, List_Value, Error, List_Type);
                     end case;
                  end if;
               end;

            when New_List | Fill_List | Length_List | List_At =>
               if not Supported_List (Candidate.Data_Types, Instruction.Data_Type) or else
                 Instruction.Alternative /= 0 or else
                 (case Instruction.Op is
                     when New_List => Instruction.Immediate not in 0 .. MAX_LIST_ELEMENTS,
                     when Fill_List => Instruction.Immediate not in 1 .. MAX_LIST_ELEMENTS,
                     when others => Instruction.Immediate /= 0)
               then Error := Invalid_Data_Type;
               else
                  case Instruction.Op is
                     when New_List => null;
                     when Fill_List =>
                        Pop_Kind (State, Element_Kind (Candidate.Data_Types, Instruction.Data_Type), Error,
                                  Element_Data_Type (Candidate.Data_Types, Instruction.Data_Type));
                        Pop_Kind (State, List_Value, Error, Instruction.Data_Type);
                     when Length_List =>
                        Pop_Kind (State, List_Value, Error, Instruction.Data_Type);
                     when others =>
                        Pop_Kind (State, Integer_Value, Error);
                        Pop_Kind (State, List_Value, Error, Instruction.Data_Type);
                  end case;
                  case Instruction.Op is
                     when New_List | Fill_List =>
                        Push_Kind (State, List_Value, Error, Instruction.Data_Type);
                     when Length_List =>
                        Push_Kind (State, Integer_Value, Error);
                     when others =>
                        Push_Kind (State, Element_Kind (Candidate.Data_Types, Instruction.Data_Type), Error,
                                   Element_Data_Type (Candidate.Data_Types, Instruction.Data_Type));
                  end case;
               end if;

            when Variant_To_Text =>
               if not CCL.Types.Is_Enumeration (Candidate.Data_Types, Instruction.Data_Type) or else
                 Instruction.Alternative /= 0
               then Error := Invalid_Data_Type;
               else
                  Pop_Kind (State, Variant_Value, Error, Instruction.Data_Type);
                  Push_Kind (State, Text_Value, Error);
               end if;

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
                                Error, Reference_For_Type (Candidate.Data_Types, D.Parts (A).Payload));
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

            when Equal_Integer | Less_Integer | Less_Equal_Integer | Equal_Boolean | Equal_Character =>
               Pop_Kind (State, Comparison_Kind (Instruction.Op), Error);
               if Error = Valid then
                  Pop_Kind (State, Comparison_Kind (Instruction.Op), Error);
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

      --  The deepest chain of named calls must fit the machine's stack. Calls
      --  through function values are bounded when they run (the frame table
      --  and the stack: Call_Depth_Exhausted), since which function a value
      --  holds is not a static fact.
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
      List_Regions.Initialize (State.Lists);
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
      List_Regions.Initialize (State.Lists);
      Accepted := Count = Initial_Locals_Length;
      if Accepted and then Count > 0 then
         for Local in 0 .. Count - 1 loop
            if Values (Local).Kind in Object_Value | Resource_Value | List_Value or else
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
   use type List_Regions.Operation_Result;

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
   --  A value as a cell: its scalar fields, whatever its kind (List_Element).
   function To_Slot (Item : Value) return Slot is
     ((Element => (Integer => Item.Integer, Boolean => Item.Boolean,
                   Alternative => (if Item.Kind in Variant_Value | Object_Value then Item.Alternative else 0),
                   Text => Item.Text, Node => Item.Node),
       Items => Item.Items));

   procedure From_Slot
     (Arena : Value_Arena; Types : CCL.Types.Registry; Ref : CCL.Types.Type_Reference;
      Item : Slot; Result : out Value; Good : out Boolean)
   is
      Kind : constant Value_Kind := Kind_For_Type (Types, Ref);
      D : constant CCL.Types.Description := CCL.Types.Describe (Types, Ref);
   begin
      Result := (Kind => Kind, Data_Type => Reference_For_Type (Types, Ref), others => <>);
      Good := True;
      case Kind is
         when Integer_Value => Result.Integer := Item.Element.Integer;
         when Boolean_Value => Result.Boolean := Item.Element.Boolean;
         when Character_Value =>
            Good := Item.Element.Integer in 0 .. MAX_CHARACTER_CODE;
            Result.Integer := Item.Element.Integer;
         when Text_Value => Result.Text := Item.Element.Text;
         when List_Value => Result.Items := Item.Items;
         when Variant_Value =>
            Good := Item.Element.Alternative in 1 .. D.Count;
            if Good then
               Result.Alternative := Item.Element.Alternative;
               Result.Integer := Item.Element.Integer;
               Result.Boolean := Item.Element.Boolean;
            end if;
         when Object_Value =>
            if Item.Element.Node = 0 then
               --  A payload variant's unit member, carried inline.
               Good := D.Form = CCL.Types.Sum and then Item.Element.Alternative in 1 .. D.Count and then
                 D.Parts (Item.Element.Alternative).Payload = CCL.Types.Unit_Type;
               if Good then Result.Alternative := Item.Element.Alternative; end if;
            else
               Good := Item.Element.Node <= Arena.Nodes_Used and then
                 Arena.Nodes (Item.Element.Node).Data_Type = Ref;
               if Good then
                  Result.Node := Item.Element.Node;
                  if Arena.Nodes (Item.Element.Node).Alternative in CCL.Types.Component_Index then
                     Result.Alternative := Arena.Nodes (Item.Element.Node).Alternative;
                  end if;
               end if;
            end if;
         --  Function values never sit in a slot or list (not Storable).
         when Resource_Value | Function_Value => Good := False;
      end case;
      if not Good then
         Result := (others => <>);
      end if;
   end From_Slot;

   --  Whether Item is a well-formed value of type Ref that may go in a slot
   --  of a node allocated after every node now in the arena.
   function Fits
     (Arena : Value_Arena; Types : CCL.Types.Registry; Ref : CCL.Types.Type_Reference; Item : Value)
      return Boolean is
     (Item.Kind = Kind_For_Type (Types, Ref) and then
      Item.Data_Type = Reference_For_Type (Types, Ref) and then
      Item.Copyable and then Item.Type_Tag = 0 and then
      Item.Kind not in Resource_Value and then
      (Item.Kind /= Object_Value or else Item.Node <= Arena.Nodes_Used) and then
      (Item.Kind /= Character_Value or else Item.Integer in 0 .. MAX_CHARACTER_CODE));

   procedure Allocate_Node
     (Arena : in out Value_Arena; Types : CCL.Types.Registry;
      Data_Type : CCL.Types.Type_Reference; Alternative : CCL.Types.Component_Count;
      Components : Component_Values; Count : CCL.Types.Component_Count;
      Result : out Value; Good : out Boolean)
   is
      D : constant CCL.Types.Description := CCL.Types.Describe (Types, Data_Type);
      Expected : constant CCL.Types.Component_Count :=
        (if D.Form = CCL.Types.Product then D.Count
         elsif Alternative in 1 .. D.Count and then D.Parts (Alternative).Payload /= CCL.Types.Unit_Type then 1
         else 0);
   begin
      Result := (others => <>);
      Good := Node_Type (Types, Data_Type) and then Count = Expected and then
        (if D.Form = CCL.Types.Product then Alternative = 0 else Alternative in 1 .. D.Count);
      if Good then
         for P in 1 .. Count loop
            if not Fits (Arena, Types,
                         D.Parts (if D.Form = CCL.Types.Product then P else Alternative).Payload,
                         Components (P))
            then
               Good := False;
            end if;
         end loop;
      end if;
      if not Good then
         return;
      elsif D.Form = CCL.Types.Sum and then Count = 0 then
         --  A unit member: no node. (An empty record still gets one, as in
         --  the interpreter.)
         Result := (Kind => Object_Value, Data_Type => Data_Type,
                    Alternative => (if Alternative in CCL.Types.Component_Index then Alternative else 1),
                    others => <>);
      elsif Arena.Nodes_Used = MAX_VALUE_NODES or else MAX_VALUE_SLOTS - Arena.Slots_Used < Count then
         Good := False;
      else
         for P in 1 .. Count loop
            Arena.Slots (Arena.Slots_Used + P) := To_Slot (Components (P));
         end loop;
         Arena.Nodes_Used := Arena.Nodes_Used + 1;
         Arena.Nodes (Arena.Nodes_Used) :=
           (Data_Type => Data_Type, Alternative => Alternative,
            First => Arena.Slots_Used + 1, Count => Count);
         Arena.Slots_Used := Arena.Slots_Used + Count;
         Result := (Kind => Object_Value, Data_Type => Data_Type,
                    Alternative => (if Alternative in CCL.Types.Component_Index then Alternative else 1),
                    Node => Arena.Nodes_Used, others => <>);
      end if;
   end Allocate_Node;

   procedure Component
     (Arena : Value_Arena; Types : CCL.Types.Registry; Owner : Value;
      P : CCL.Types.Component_Index; Result : out Value; Good : out Boolean)
   is
      D : constant CCL.Types.Description := CCL.Types.Describe (Types, Owner.Data_Type);
   begin
      Result := (others => <>);
      Good := Owner.Kind = Object_Value and then Owner.Node in 1 .. Arena.Nodes_Used and then
        Arena.Nodes (Owner.Node).Data_Type = Owner.Data_Type and then
        P <= Arena.Nodes (Owner.Node).Count and then
        Arena.Nodes (Owner.Node).First <= MAX_VALUE_SLOTS - (P - 1) and then
        (D.Form = CCL.Types.Product or else Arena.Nodes (Owner.Node).Alternative in CCL.Types.Component_Index);
      if Good then
         declare
            Part : constant CCL.Types.Component_Index :=
              (if D.Form = CCL.Types.Product then P else Arena.Nodes (Owner.Node).Alternative);
         begin
            From_Slot (Arena, Types, D.Parts (Part).Payload,
                       Arena.Slots (Arena.Nodes (Owner.Node).First + (P - 1)), Result, Good);
         end;
      end if;
   end Component;

   --  A list element and the value it stands for.
   function To_Element (Item : Value) return List_Element is (To_Slot (Item).Element);
   procedure From_Element
     (Arena : Value_Arena; Types : CCL.Types.Registry; List_Type : CCL.Types.Type_Reference;
      Element : List_Element; Item : out Value; Good : out Boolean) is
   begin
      From_Slot (Arena, Types, CCL.Types.Element_Of (Types, List_Type),
                 (Element => Element, Items => <>), Item, Good);
   end From_Element;

   --  The interpreter's string order (sort): byte by byte, a prefix first.
   function Text_Less
     (Region : Text_Regions.Stack; Left, Right : Text_Regions.String_Value) return Boolean
   is
      A, B : Character;
      Status_A, Status_B : Text_Regions.Operation_Result;
      Shorter : constant Natural := Natural'Min (Text_Regions.Length (Left), Text_Regions.Length (Right));
   begin
      for I in 1 .. Shorter loop
         if I - 1 > Text_Regions.String_Index'Last - Text_Regions.First_Index (Left) or else
           I - 1 > Text_Regions.String_Index'Last - Text_Regions.First_Index (Right)
         then
            return False;
         end if;
         Text_Regions.Read (Region, Left, Text_Regions.First_Index (Left) + (I - 1), A, Status_A);
         Text_Regions.Read (Region, Right, Text_Regions.First_Index (Right) + (I - 1), B, Status_B);
         if Status_A /= Text_Regions.Operation_Ok or else Status_B /= Text_Regions.Operation_Ok then
            return False;
         elsif A /= B then
            return A < B;
         end if;
      end loop;
      return Text_Regions.Length (Left) < Text_Regions.Length (Right);
   end Text_Less;

   --  The interpreter keeps a split piece as a region string of at most
   --  1 KiB (CCL.Host_Values.Maximum_Text_Length); a longer piece fills it.
   MAX_SPLIT_PIECE : constant := MAX_RESULT_TEXT;

   --  A list built-in, with the interpreter's fuel: one per element read and
   --  per sort comparison or joined piece. Subject is the list (the text,
   --  for split); A and B are the operands before it in source.
   procedure Run_List_Builtin
     (Lists  : in out List_Regions.Stack;
      Texts  : in out Text_Regions.Stack;
      Budget : in out CCL.Execution_Budgets.Budget;
      Arena  : Value_Arena;
      Types  : CCL.Types.Registry;
      Item   : L.Operation;
      List_Type : CCL.Types.Type_Reference;
      Subject, A, B : Value;
      Result : out Value;
      Status : out Execution_Status)
   with Post => CCL.Execution_Budgets.Limit (Budget) = CCL.Execution_Budgets.Limit (Budget'Old)
   is
      Length : constant Natural :=
        (if Subject.Kind = List_Value then List_Regions.Length (Subject.Items) else 0);
      Kind : constant Value_Kind := Element_Kind (Types, List_Type);
      Region_Status : List_Regions.Operation_Result := List_Regions.Operation_Ok;
      Good : Boolean := True;
      E : List_Element;

      procedure Fail (Code : Execution_Status) is
      begin
         if Good then Status := Code; end if;
         Good := False;
      end Fail;

      procedure Spend
        with Post => CCL.Execution_Budgets.Limit (Budget) = CCL.Execution_Budgets.Limit (Budget'Old);
      procedure Spend is
         Spent : CCL.Execution_Budgets.Consume_Result;
      begin
         if not Good then return; end if;
         if not CCL.Execution_Budgets.Has_Fuel (Budget) then
            Fail (Fuel_Exhausted);
            return;
         end if;
         CCL.Execution_Budgets.Consume (Budget, Spent);
         if Spent /= CCL.Execution_Budgets.Consumed then
            Fail (Fuel_Exhausted);
         end if;
      end Spend;

      --  Count fuel spent after the fact (a sort's comparisons).
      procedure Charge (Count : Natural)
        with Post => CCL.Execution_Budgets.Limit (Budget) = CCL.Execution_Budgets.Limit (Budget'Old);
      procedure Charge (Count : Natural) is
         Spent : CCL.Execution_Budgets.Consume_Result;
      begin
         for C in 1 .. Count loop
            pragma Loop_Invariant
              (CCL.Execution_Budgets.Limit (Budget) = CCL.Execution_Budgets.Limit (Budget'Loop_Entry));
            CCL.Execution_Budgets.Consume (Budget, Spent);
            exit when Spent /= CCL.Execution_Budgets.Consumed;
         end loop;
      end Charge;

      procedure Get (List : List_Regions.Array_Value; I : Positive; Element : out List_Element) is
      begin
         Element := Null_List_Element;
         if not Good then return; end if;
         if I > List_Regions.Array_Index'Last then
            Fail (Invalid_Bytecode);
            return;
         end if;
         List_Regions.Read (Lists, List, List_Regions.Array_Index (I), Element, Region_Status);
         if Region_Status /= List_Regions.Operation_Ok then
            Element := Null_List_Element;
            Fail (Invalid_Bytecode);
         end if;
      end Get;

      procedure Put (List : List_Regions.Array_Value; I : Positive; Element : List_Element) is
      begin
         if not Good then return; end if;
         if I > List_Regions.Array_Index'Last then
            Fail (Invalid_Bytecode);
            return;
         end if;
         List_Regions.Write (Lists, List, List_Regions.Array_Index (I), Element, Region_Status);
         if Region_Status /= List_Regions.Operation_Ok then
            Fail (Invalid_Bytecode);
         end if;
      end Put;

      --  Element I of the subject, for one fuel.
      procedure Read (I : Positive; Element : out List_Element)
        with Post => CCL.Execution_Budgets.Limit (Budget) = CCL.Execution_Budgets.Limit (Budget'Old);
      procedure Read (I : Positive; Element : out List_Element) is
      begin
         Element := Null_List_Element;
         Spend;
         Get (Subject.Items, I, Element);
      end Read;

      procedure Reserve (Size : Natural) is
      begin
         Result := (Kind => List_Value, Data_Type => List_Type, others => <>);
         List_Regions.Reserve (Lists, Size, Result.Items, Region_Status);
         if Region_Status /= List_Regions.Operation_Ok then
            Fail (List_Storage_Exhausted);
         end if;
      end Reserve;
   begin
      Result := (others => <>);
      Status := Completed;
      case Item is
         when L.First_Items | L.Last_Items | L.Skip_Items =>
            declare
               From : Positive;
               To : Natural;
            begin
               L.Take_Bounds (Item, A.Integer, Length, From, To);
               Reserve (if To >= From then To - From + 1 else 0);
               for I in From .. To loop
                  pragma Loop_Invariant (CCL.Execution_Budgets.Limit (Budget) = CCL.Execution_Budgets.Limit (Budget'Loop_Entry));
                  exit when not Good;
                  Read (I, E);
                  Put (Result.Items, I - From + 1, E);
               end loop;
            end;

         when L.Reverse_Items =>
            Reserve (Length);
            for I in 1 .. Length loop
               pragma Loop_Invariant (CCL.Execution_Budgets.Limit (Budget) = CCL.Execution_Budgets.Limit (Budget'Loop_Entry));
               exit when not Good;
               Read (Length - I + 1, E);
               Put (Result.Items, I, E);
            end loop;

         when L.Sort_Items =>
            Reserve (Length);
            for I in 1 .. Length loop
               pragma Loop_Invariant (CCL.Execution_Budgets.Limit (Budget) = CCL.Execution_Budgets.Limit (Budget'Loop_Entry));
               exit when not Good;
               Read (I, E);
               Put (Result.Items, I, E);
            end loop;
            --  One fuel per comparison, as in the interpreter: the comparisons
            --  are counted against the fuel left (so the sort stops at the same
            --  point) and charged after it, outside the generic sort.
            declare
               Allowance : constant Natural :=
                 Natural (Unsigned_32'Min (CCL.Execution_Budgets.Remaining (Budget),
                                           Unsigned_32 (Natural'Last)));
               Compared : Natural := 0;
               procedure Sort_Less (I, J : Positive; Before : out Boolean; Ok : out Boolean) is
                  X, Y : List_Element;
               begin
                  Before := False;
                  if Compared >= Allowance then
                     Fail (Fuel_Exhausted);
                  else
                     Compared := Compared + 1;
                  end if;
                  Get (Result.Items, I, X);
                  Get (Result.Items, J, Y);
                  Before := Good and then
                    (if Kind = Text_Value then Text_Less (Texts, X.Text, Y.Text)
                     elsif Kind = Variant_Value then X.Alternative < Y.Alternative
                     else X.Integer < Y.Integer);
                  Ok := Good;
               end Sort_Less;
               procedure Sort_Swap (I, J : Positive; Ok : out Boolean) is
                  X, Y : List_Element;
               begin
                  Get (Result.Items, I, X);
                  Get (Result.Items, J, Y);
                  Put (Result.Items, I, Y);
                  Put (Result.Items, J, X);
                  Ok := Good;
               end Sort_Swap;
               procedure Sort is new L.Heap_Sort (Sort_Less, Sort_Swap);
               Sorted : Boolean;
            begin
               if Good then
                  Sort (Length, Sorted);
                  Charge (Compared);
                  if not Sorted then
                     Fail (Invalid_Bytecode);
                  end if;
               end if;
            end;

         when L.Sum_Items =>
            Result := Integer_Constant (0);
            for I in 1 .. Length loop
               pragma Loop_Invariant (CCL.Execution_Budgets.Limit (Budget) = CCL.Execution_Budgets.Limit (Budget'Loop_Entry));
               exit when not Good;
               Read (I, E);
               exit when not Good;
               declare
                  Total : Integer_64;
                  Overflowed : Boolean;
               begin
                  CCL.Checked_Arithmetic.Add (Result.Integer, E.Integer, Total, Overflowed);
                  if Overflowed then
                     Fail (Arithmetic_Overflow);
                  else
                     Result.Integer := Total;
                  end if;
               end;
            end loop;

         when L.Min_Items | L.Max_Items =>
            if Length = 0 then
               Fail (Index_Out_Of_Range);
            else
               Read (1, E);
               Result := Integer_Constant (E.Integer);
               for I in 2 .. Length loop
                  pragma Loop_Invariant (CCL.Execution_Budgets.Limit (Budget) = CCL.Execution_Budgets.Limit (Budget'Loop_Entry));
                  exit when not Good;
                  Read (I, E);
                  if Good and then
                    (if Item = L.Min_Items then E.Integer < Result.Integer else E.Integer > Result.Integer)
                  then
                     Result.Integer := E.Integer;
                  end if;
               end loop;
            end if;

         when L.Contains_Item =>
            Result := Boolean_Constant (False);
            for I in 1 .. Length loop
               pragma Loop_Invariant (CCL.Execution_Budgets.Limit (Budget) = CCL.Execution_Budgets.Limit (Budget'Loop_Entry));
               exit when not Good;
               Read (I, E);
               exit when not Good;
               declare
                  Member : Value;
                  Valid, Same : Boolean;
                  Compared : Text_Regions.Operation_Result;
               begin
                  From_Element (Arena, Types, List_Type, E, Member, Valid);
                  if not Valid then
                     Fail (Invalid_Bytecode);
                  else
                     case Kind is
                        when Text_Value =>
                           Equal_Texts (Texts, Member, A, Same, Compared);
                           if Compared /= Text_Regions.Operation_Ok then
                              Fail (Text_Failure (Compared));
                           end if;
                        when Boolean_Value => Same := Member.Boolean = A.Boolean;
                        when Variant_Value => Same := Member.Alternative = A.Alternative;
                        when others => Same := Member.Integer = A.Integer;
                     end case;
                     if Good and then Same then
                        Result := Boolean_Constant (True);
                        exit;
                     end if;
                  end if;
               end;
            end loop;

         when L.Join_Items =>
            declare
               Separator : String (1 .. MAX_STRING_BYTES) := [others => ' '];
               Joined : String (1 .. MAX_STRING_BYTES) := [others => ' '];
               Separator_Length : constant Natural := Text_Regions.Length (A.Text);
               Used : Natural range 0 .. MAX_STRING_BYTES := 0;
               Piece_Length : Natural;
               Copied : Text_Regions.Operation_Result;
            begin
               if Separator_Length > MAX_STRING_BYTES then
                  Fail (Text_Storage_Exhausted);
               else
                  Text_Regions.Copy_To (Texts, A.Text, Separator (1 .. Separator_Length), Copied);
                  if Copied /= Text_Regions.Operation_Ok then
                     Fail (Text_Failure (Copied));
                  end if;
               end if;
               for I in 1 .. Length loop
                  pragma Loop_Invariant (CCL.Execution_Budgets.Limit (Budget) = CCL.Execution_Budgets.Limit (Budget'Loop_Entry));
                  exit when not Good;
                  Read (I, E);
                  exit when not Good;
                  Piece_Length := Text_Regions.Length (E.Text);
                  if (I > 1 and then Separator_Length > MAX_STRING_BYTES - Used) or else
                    Piece_Length > MAX_STRING_BYTES - Used - (if I > 1 then Separator_Length else 0)
                  then
                     Fail (Text_Storage_Exhausted);
                  else
                     if I > 1 then
                        Joined (Used + 1 .. Used + Separator_Length) := Separator (1 .. Separator_Length);
                        Used := Used + Separator_Length;
                     end if;
                     Text_Regions.Copy_To (Texts, E.Text, Joined (Used + 1 .. Used + Piece_Length), Copied);
                     if Copied /= Text_Regions.Operation_Ok then
                        Fail (Text_Failure (Copied));
                     else
                        Used := Used + Piece_Length;
                     end if;
                  end if;
               end loop;
               if Good then
                  Result := (Kind => Text_Value, others => <>);
                  Text_Regions.Allocate_String (Texts, Joined (1 .. Used), Result.Text, Copied);
                  if Copied /= Text_Regions.Operation_Ok then
                     Fail (Text_Failure (Copied));
                  end if;
               end if;
            end;

         when L.Range_Items =>
            declare
               Size : Natural;
               Fits : Boolean;
               Next : Integer_64;
               Overflowed : Boolean;
            begin
               L.Range_Length (A.Integer, B.Integer, MAX_LIST_ELEMENTS, Size, Fits);
               if not Fits then
                  Fail (List_Storage_Exhausted);
               else
                  Reserve (Size);
               end if;
               for I in 1 .. Size loop
                  pragma Loop_Invariant (CCL.Execution_Budgets.Limit (Budget) = CCL.Execution_Budgets.Limit (Budget'Loop_Entry));
                  exit when not Good;
                  CCL.Checked_Arithmetic.Add (A.Integer, Integer_64 (I - 1), Next, Overflowed);
                  if Overflowed then
                     Fail (Arithmetic_Overflow);
                  else
                     Put (Result.Items, I, (Integer => Next, others => <>));
                  end if;
               end loop;
            end;

         when L.Split_Text =>
            declare
               Text : String (1 .. MAX_STRING_BYTES) := [others => ' '];
               Separator : String (1 .. MAX_PATTERN_BYTES) := [others => ' '];
               Text_Length : constant Natural range 0 .. MAX_STRING_BYTES :=
                 Natural'Min (Text_Regions.Length (Subject.Text), MAX_STRING_BYTES);
               Separator_Length : constant Natural range 0 .. MAX_PATTERN_BYTES :=
                 Natural'Min (Text_Regions.Length (A.Text), MAX_PATTERN_BYTES);
               Copied : Text_Regions.Operation_Result;
               Capacity : Natural range 0 .. MAX_LIST_ELEMENTS := 0;
               Count : Natural range 0 .. MAX_LIST_ELEMENTS := 0;
               Position : Positive := 1;
               Finished : Boolean := False;
               Low : Positive;
               High : Natural;
               Found : Boolean;
               Piece : Text_Regions.String_Value;
            begin
               if Text_Regions.Length (Subject.Text) > MAX_STRING_BYTES or else
                 Text_Regions.Length (A.Text) > MAX_PATTERN_BYTES
               then
                  Fail (Text_Storage_Exhausted);
               else
                  Text_Regions.Copy_To (Texts, Subject.Text, Text (1 .. Text_Length), Copied);
                  if Copied = Text_Regions.Operation_Ok then
                     Text_Regions.Copy_To (Texts, A.Text, Separator (1 .. Separator_Length), Copied);
                  end if;
                  if Copied /= Text_Regions.Operation_Ok then
                     Fail (Text_Failure (Copied));
                  end if;
                  Capacity := Natural'Min (Text_Length + 1, MAX_LIST_ELEMENTS);
               end if;
               if Good then
                  Reserve (Capacity);
               end if;
               --  At most Text_Length + 1 pieces; one more step reports a full
               --  list when Capacity is smaller.
               for Step in 0 .. Capacity loop
                  pragma Loop_Invariant (CCL.Execution_Budgets.Limit (Budget) = CCL.Execution_Budgets.Limit (Budget'Loop_Entry));
                  pragma Loop_Invariant (Position <= Text_Length + 1);
                  exit when not Good;
                  CCL.Text_Operations.Next_Piece
                    (Text (1 .. Text_Length), Separator (1 .. Separator_Length),
                     Position, Finished, Low, High, Found);
                  exit when not Found;
                  if Count >= Capacity or else (High >= Low and then High - Low + 1 > MAX_SPLIT_PIECE) then
                     Fail (List_Storage_Exhausted);
                  else
                     Text_Regions.Allocate_String
                       (Texts, (if High >= Low then Text (Low .. High) else ""), Piece, Copied);
                     if Copied /= Text_Regions.Operation_Ok then
                        Fail (Text_Failure (Copied));
                     else
                        Count := Count + 1;
                        Put (Result.Items, Count, (Text => Piece, others => <>));
                     end if;
                  end if;
               end loop;
               if Good then
                  List_Regions.Shrink (Lists, Result.Items, Count, Region_Status);
                  if Region_Status /= List_Regions.Operation_Ok then
                     Fail (List_Storage_Exhausted);
                  end if;
               end if;
            end;
      end case;
      if not Good then
         Result := (others => <>);
      end if;
   end Run_List_Builtin;

   --  sort-by's last step: the elements in It.Built ordered by their keys
   --  (Integer or String), one fuel per comparison as in sort: counted
   --  against the fuel left, charged after the sort. Exhausted when it ran
   --  out. Only the regions and budget it names change.
   procedure Sort_By_Keys
     (Lists : in out List_Regions.Stack; Texts : Text_Regions.Stack;
      Budget : in out CCL.Execution_Budgets.Budget; Types : CCL.Types.Registry;
      It : in out Iteration; Good : out Boolean; Exhausted : out Boolean)
     with Post => CCL.Execution_Budgets.Limit (Budget) = CCL.Execution_Budgets.Limit (Budget'Old)
   is
      D : constant CCL.Types.Description := CCL.Types.Describe (Types, It.Callee.Data_Type);
      Text_Keys : constant Boolean := D.Count >= 1 and then D.Parts (D.Count).Payload = CCL.Types.String_Type;
      Allowance : constant Natural :=
        Natural (Unsigned_32'Min (CCL.Execution_Budgets.Remaining (Budget), Unsigned_32 (Natural'Last)));
      Compared : Natural := 0;
      procedure Get (List : List_Regions.Array_Value; I : Positive; E : out List_Element; Ok : in out Boolean) is
         Read : List_Regions.Operation_Result;
      begin
         E := Null_List_Element;
         if Ok and then I <= List_Regions.Array_Index'Last then
            List_Regions.Read (Lists, List, List_Regions.Array_Index (I), E, Read);
            Ok := Read = List_Regions.Operation_Ok;
         else
            Ok := False;
         end if;
      end Get;
      procedure Put (List : List_Regions.Array_Value; I : Positive; E : List_Element; Ok : in out Boolean) is
         Written : List_Regions.Operation_Result;
      begin
         if Ok and then I <= List_Regions.Array_Index'Last then
            List_Regions.Write (Lists, List, List_Regions.Array_Index (I), E, Written);
            Ok := Written = List_Regions.Operation_Ok;
         else
            Ok := False;
         end if;
      end Put;
      procedure Less (I, J : Positive; Before : out Boolean; Ok : out Boolean) is
         A, B : List_Element;
      begin
         Before := False;
         Ok := Compared < Allowance;
         if not Ok then
            Exhausted := True;
            return;
         end if;
         Compared := Compared + 1;
         Get (It.Keys, I, A, Ok);
         Get (It.Keys, J, B, Ok);
         Before := Ok and then (if Text_Keys then Text_Less (Texts, A.Text, B.Text) else A.Integer < B.Integer);
      end Less;
      procedure Swap (I, J : Positive; Ok : out Boolean) is
         A, B, X, Y : List_Element;
      begin
         Ok := True;
         Get (It.Keys, I, A, Ok); Get (It.Keys, J, B, Ok);
         Put (It.Keys, I, B, Ok); Put (It.Keys, J, A, Ok);
         Get (It.Built.Items, I, X, Ok); Get (It.Built.Items, J, Y, Ok);
         Put (It.Built.Items, I, Y, Ok); Put (It.Built.Items, J, X, Ok);
      end Swap;
      procedure Sort is new L.Heap_Sort (Less, Swap);
   begin
      Exhausted := False;
      Sort (List_Regions.Length (It.Built.Items), Good);
      for C in 1 .. Compared loop
         pragma Loop_Invariant
           (CCL.Execution_Budgets.Limit (Budget) = CCL.Execution_Budgets.Limit (Budget'Loop_Entry));
         declare
            Spent : CCL.Execution_Budgets.Consume_Result;
         begin
            CCL.Execution_Budgets.Consume (Budget, Spent);
            exit when Spent /= CCL.Execution_Budgets.Consumed;
         end;
      end loop;
      Good := Good and then not Exhausted;
   end Sort_By_Keys;

   --  Copy a list result out, as the interpreter does: the first elements
   --  as values, strings as consecutive slices of List_Text.
   procedure Export_List
     (Lists : List_Regions.Stack; Texts : Text_Regions.Stack; Arena : Value_Arena;
      Types : CCL.Types.Registry; Item : Value; Result : in out Execution_Result)
   is
      Total : constant Natural := List_Regions.Length (Item.Items);
      Element : List_Element;
      Element_Value : Value;
      Read_Status : List_Regions.Operation_Result;
      Copied : Text_Regions.Operation_Result;
      Used : Result_Text_Length := 0;
      Size : Natural;
      Good : Boolean;
   begin
      Result.List_Total := Total;
      Result.List_Length := Natural'Min (Total, MAX_LIST_RESULT);
      for I in 1 .. Result.List_Length loop
         List_Regions.Read (Lists, Item.Items, List_Regions.Array_Index (I), Element, Read_Status);
         From_Element (Arena, Types, Item.Data_Type, Element, Element_Value, Good);
         if Read_Status /= List_Regions.Operation_Ok or else not Good then
            Result.List_Length := I - 1; exit;
         end if;
         if Element_Value.Kind = Text_Value then
            Size := Text_Regions.Length (Element.Text);
            if Size > MAX_RESULT_TEXT - Used then
               --  Carry out the elements that fit.
               Result.List_Length := I - 1; exit;
            end if;
            Text_Regions.Copy_To (Texts, Element.Text, Result.List_Text.Data (Used + 1 .. Used + Size), Copied);
            if Copied /= Text_Regions.Operation_Ok then
               Result.List_Length := I - 1; exit;
            end if;
            Used := Used + Size;
            Result.List_Text_Ends (I) := Used;
         elsif Element_Value.Kind = Character_Value then
            Result.List_Values (I) := Integer_Constant (Element.Integer);
         elsif Element_Value.Kind = Variant_Value then
            Result.List_Values (I) := Integer_Constant (Integer_64 (Element.Alternative));
         else
            Result.List_Values (I) := Element_Value;
         end if;
      end loop;
      Result.List_Text.Length := Used;
   end Export_List;

   --  A result's canonical CCL literal, as the interpreter prints it (it
   --  reads back as the same value): (Pair 42 "hi"), Shape.Empty,
   --  (Shape.Circle 3), [1 2], (list-of T). Characters have no literal.
   --  A node's components are older nodes, so Bound (the node being
   --  printed) decreases; a list's elements print at Level 0, which has no
   --  lists of its own.
   subtype Print_Bound is Natural range 0 .. MAX_VALUE_NODES + 1;
   subtype Print_Level is Natural range 0 .. 1;
   procedure Print_Value
     (Arena : Value_Arena; Texts : Text_Regions.Stack; Lists : List_Regions.Stack;
      Types : CCL.Types.Registry; Item : Value; Bound : Print_Bound; Level : Print_Level;
      Output : in out Result_Text; Good : in out Boolean)
     with Subprogram_Variant => (Decreases => Bound, Decreases => Level)
   is
      procedure Add (Text : String) is
      begin
         if Good and then Text'Length <= MAX_RESULT_TEXT - Output.Length then
            Output.Data (Output.Length + 1 .. Output.Length + Text'Length) := Text;
            Output.Length := Output.Length + Text'Length;
         else
            Good := False;
         end if;
      end Add;
      function Name_Of (Ref : CCL.Types.Type_Reference) return String is
        (CCL.Types.Image (CCL.Types.Describe (Types, Ref).Identifier));
      D : constant CCL.Types.Description := CCL.Types.Describe (Types, Item.Data_Type);
      Part : Value;
      C : Character;
      Read : Text_Regions.Operation_Result;
      Element : List_Element;
      Listed : List_Regions.Operation_Result;
   begin
      if not Good then return; end if;
      if Item.Node >= Bound then
         Good := False;
         return;
      end if;
      case Item.Kind is
         when Integer_Value => Add (CCL.Text_Operations.Decimal_Image (Item.Integer));
         when Boolean_Value => Add ((if Item.Boolean then "true" else "false"));
         when Text_Value =>
            Add ("""");
            for I in 1 .. Text_Regions.Length (Item.Text) loop
               exit when not Good;
               Text_Regions.Read (Texts, Item.Text, Text_Regions.String_Index (I), C, Read);
               Good := Read = Text_Regions.Operation_Ok;
               exit when not Good;
               case C is
                  when '"' => Add ("\""");
                  when '\' => Add ("\\");
                  when ASCII.LF => Add ("\n");
                  when ASCII.CR => Add ("\r");
                  when ASCII.HT => Add ("\t");
                  when ' ' .. '!' | '#' .. '[' | ']' .. '~' => Add ([1 => C]);
                  when others => Good := False;
               end case;
            end loop;
            Add ("""");
         when List_Value =>
            if Level = 0 then
               Good := False;
            elsif List_Regions.Length (Item.Items) = 0 then
               Add ("(list-of " & Name_Of (CCL.Types.Element_Of (Types, Item.Data_Type)) & ")");
            else
               Add ("[");
               for I in 1 .. List_Regions.Length (Item.Items) loop
                  exit when not Good;
                  List_Regions.Read (Lists, Item.Items, List_Regions.Array_Index (I), Element, Listed);
                  Good := Good and then Listed = List_Regions.Operation_Ok;
                  if Good then
                     From_Element (Arena, Types, Item.Data_Type, Element, Part, Good);
                  end if;
                  if I > 1 then Add (" "); end if;
                  if Good then
                     Print_Value (Arena, Texts, Lists, Types, Part, Bound, 0, Output, Good);
                  end if;
               end loop;
               Add ("]");
            end if;
         when Variant_Value | Object_Value =>
            if D.Form = CCL.Types.Product and then Item.Node /= 0 then
               Add ("(" & Name_Of (Item.Data_Type));
               for P in 1 .. D.Count loop
                  exit when not Good;
                  Component (Arena, Types, Item, P, Part, Good);
                  exit when not Good;
                  Add (" ");
                  Print_Value (Arena, Texts, Lists, Types, Part, Item.Node, 1, Output, Good);
               end loop;
               Add (")");
            elsif D.Form = CCL.Types.Sum and then Item.Alternative <= D.Count then
               if D.Parts (Item.Alternative).Payload = CCL.Types.Unit_Type then
                  Add (Name_Of (Item.Data_Type) & "." & CCL.Types.Image (D.Parts (Item.Alternative).Identifier));
               else
                  Add ("(" & Name_Of (Item.Data_Type) & "." &
                       CCL.Types.Image (D.Parts (Item.Alternative).Identifier) & " ");
                  if Item.Node /= 0 then
                     Component (Arena, Types, Item, 1, Part, Good);
                     if Good then
                        Print_Value (Arena, Texts, Lists, Types, Part, Item.Node, 1, Output, Good);
                     end if;
                  else
                     --  A scalar payload carried inline: no node to decrease.
                     case D.Parts (Item.Alternative).Payload is
                        when CCL.Types.Integer_Type => Add (CCL.Text_Operations.Decimal_Image (Item.Integer));
                        when CCL.Types.Boolean_Type => Add ((if Item.Boolean then "true" else "false"));
                        when others => Good := False;
                     end case;
                  end if;
                  Add (")");
               end if;
            else
               Good := False;
            end if;
         when Character_Value | Resource_Value | Function_Value => Good := False;
      end case;
   end Print_Value;

   --  Whether a list's elements are records or payload variants: such a
   --  list leaves as a literal, as the interpreter's does.
   function Compound_Elements (Types : CCL.Types.Registry; List_Type : CCL.Types.Type_Reference) return Boolean is
     (CCL.Types.Describe (Types, CCL.Types.Element_Of (Types, List_Type)).Form = CCL.Types.Product or else
      (CCL.Types.Describe (Types, CCL.Types.Element_Of (Types, List_Type)).Form = CCL.Types.Sum and then
       not CCL.Types.Is_Enumeration (Types, CCL.Types.Element_Of (Types, List_Type))));

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

   procedure Run
     (Item   : Validated_Program;
      State  : in out Machine_State;
      Instructions : Natural;
      Result : out Execution_Result)
     with Pre => Is_Valid (Item) and then Is_Well_Formed (Item, State),
       Post => Is_Well_Formed (Item, State) and then
         Fuel_Limit (State) = Fuel_Limit (State'Old) and then Result.Steps <= Fuel_Limit (State);
   procedure Run
     (Item   : Validated_Program;
      State  : in out Machine_State;
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
      Stack_Result : Runtime_Stacks.Operation_Result := Runtime_Stacks.Stack_Ok;
      Waiting : Boolean := State.Waiting;
      Waiting_Owned : Boolean := State.Waiting_Owned;
      Done  : Boolean := State.Terminal or else Waiting or else State.Waiting_Stream;
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
      Element : Character;
      List_Status : List_Regions.Operation_Result := List_Regions.Operation_Ok;

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

      --  Enter the function value Callee: its captures, then Arity
      --  arguments, as the callee's frame; Return_PC is where its result
      --  comes back to.
      procedure Enter_Call
        (Callee : Value; Arguments : Component_Values; Arity : Natural;
         Return_PC : Instruction_Index; Good : out Boolean)
        with Post => Fuel_Limit (State) = Fuel_Limit (State'Old) and then
                     CCL.Imports.Phase (State.Import_Lifecycle) =
                       CCL.Imports.Phase (State.Import_Lifecycle'Old);
      procedure Enter_Call
        (Callee : Value; Arguments : Component_Values; Arity : Natural;
         Return_PC : Instruction_Index; Good : out Boolean)
      is
         Captured : List_Element;
         Read : List_Regions.Operation_Result;
         Pushed : Runtime_Stacks.Operation_Result;
      begin
         Good := Callee.Kind = Function_Value and then
           Callee.Integer in 0 .. Integer_64 (Item.Content.Functions_Length) - 1 and then
           State.Frame_Count < MAX_FUNCTIONS and then Arity <= CCL.Types.Maximum_Components;
         if not Good then return; end if;
         declare
            F : constant Function_Index := Function_Index (Callee.Integer);
            Decl : constant Function_Declaration := Item.Content.Functions (F);
         begin
            Good := Decl.Captures <= Decl.Count and then Decl.Count - Decl.Captures = Arity and then
              List_Regions.Length (Callee.Items) = Decl.Captures;
            for C in 1 .. Decl.Captures loop
               exit when not Good;
               List_Regions.Read (State.Lists, Callee.Items, List_Regions.Array_Index (C), Captured, Read);
               Good := Read = List_Regions.Operation_Ok;
               if Good then
                  Runtime_Stacks.Push
                    (Stack, (Kind => Decl.Kinds (C), Data_Type => Decl.Data_Types (C),
                             Integer => Captured.Integer, Boolean => Captured.Boolean,
                             Alternative => (if Captured.Alternative in CCL.Types.Component_Index
                                             then Captured.Alternative else 1),
                             Text => Captured.Text, Node => Captured.Node, others => <>),
                     Pushed);
                  Good := Pushed = Runtime_Stacks.Stack_Ok;
               end if;
            end loop;
            for P in 1 .. Arity loop
               exit when not Good;
               Runtime_Stacks.Push (Stack, Arguments (P), Pushed);
               Good := Pushed = Runtime_Stacks.Stack_Ok;
            end loop;
            if Good then
               State.Frames (State.Frame_Count) := (Return_PC => Return_PC, Callee => F);
               State.Frame_Count := State.Frame_Count + 1;
               PC := Decl.Entry_PC;
            end if;
         end;
      end Enter_Call;

      --  List_Apply at PC: start an iteration, or resume it with the result
      --  of its last call; then call for the next element or finish.
      procedure Run_Apply
        with Post => Fuel_Limit (State) = Fuel_Limit (State'Old) and then
                     CCL.Imports.Phase (State.Import_Lifecycle) =
                       CCL.Imports.Phase (State.Import_Lifecycle'Old);
      procedure Run_Apply is
         Ins : constant Instruction := Item.Content.Code (PC);
         Types : constant CCL.Types.Registry := Item.Content.Data_Types;
         Good : Boolean := True;
         Answer : Value;
         Element : List_Element;
         Read : List_Regions.Operation_Result;
         Written : List_Regions.Operation_Result;
         Operation : L.Apply_Operation;
         Known : Boolean;
         Decided : Boolean := False;
      begin
         if State.Iteration_Depth > 0 and then
           State.Iterations (State.Iteration_Depth).Active and then
           State.Iterations (State.Iteration_Depth).Awaiting and then
           State.Iterations (State.Iteration_Depth).At_PC = PC and then
           State.Iterations (State.Iteration_Depth).Frame_Level = State.Frame_Count
         then
            --  Resume: the function's result for element Position.
            declare
               --  A copy, written back below (an element whose index can
               --  change cannot be renamed).
               It : Iteration := State.Iterations (State.Iteration_Depth);
            begin
               Runtime_Stacks.Pop (Stack, Answer, Stack_Result);
               Good := Stack_Result = Runtime_Stacks.Stack_Ok and then It.Position >= 1 and then
                 It.Position <= List_Regions.Length (It.Subject.Items);
               if Good then
                  It.Awaiting := False;
                  case It.Operation is
                     when L.Each_Items =>
                        List_Regions.Write (State.Lists, It.Built.Items, List_Regions.Array_Index (It.Position),
                                            To_Element (Answer), Written);
                        Good := Written = List_Regions.Operation_Ok;
                        It.Kept := It.Position;
                     when L.Where_Items =>
                        if Answer.Boolean then
                           List_Regions.Read (State.Lists, It.Subject.Items, List_Regions.Array_Index (It.Position),
                                              Element, Read);
                           Good := Read = List_Regions.Operation_Ok and then It.Kept < MAX_LIST_ELEMENTS;
                           if Good then
                              It.Kept := It.Kept + 1;
                              List_Regions.Write (State.Lists, It.Built.Items, List_Regions.Array_Index (It.Kept),
                                                  Element, Written);
                              Good := Written = List_Regions.Operation_Ok;
                           end if;
                        end if;
                     when L.Fold_Items => It.Accumulator := Answer;
                     when L.Any_Items | L.All_Items =>
                        if Answer.Boolean = (It.Operation = L.Any_Items) then
                           It.Accumulator := Boolean_Constant (Answer.Boolean);
                           Decided := True;
                        end if;
                     when L.Count_Items =>
                        if Answer.Boolean and then It.Accumulator.Integer < Integer_64'Last then
                           It.Accumulator.Integer := It.Accumulator.Integer + 1;
                        end if;
                     when L.Sort_By_Items =>
                        List_Regions.Write (State.Lists, It.Keys, List_Regions.Array_Index (It.Position),
                                            To_Element (Answer), Written);
                        Good := Written = List_Regions.Operation_Ok;
                  end case;
               end if;
               State.Iterations (State.Iteration_Depth) := It;
            end;
         else
            --  Start: the list, fold's initial value, the function value.
            Find_Apply_Operation (Ins.Immediate, Operation, Known);
            Good := Known and then State.Iteration_Depth < MAX_ITERATIONS;
            declare
               Subject, Initial, Callee : Value;
            begin
               if Good then
                  Runtime_Stacks.Pop (Stack, Subject, Stack_Result);
                  Good := Stack_Result = Runtime_Stacks.Stack_Ok and then Subject.Kind = List_Value and then
                    Subject.Data_Type = Ins.Data_Type;
               end if;
               if Good and then Operation = L.Fold_Items then
                  Runtime_Stacks.Pop (Stack, Initial, Stack_Result);
                  Good := Stack_Result = Runtime_Stacks.Stack_Ok;
               end if;
               if Good then
                  Runtime_Stacks.Pop (Stack, Callee, Stack_Result);
                  Good := Stack_Result = Runtime_Stacks.Stack_Ok and then Callee.Kind = Function_Value and then
                    Apply_Fits (Types, Operation, Ins.Data_Type, Callee.Data_Type);
               end if;
               if Good then
                  State.Iteration_Depth := State.Iteration_Depth + 1;
                  State.Iterations (State.Iteration_Depth) :=
                    (Active => True, Awaiting => False, At_PC => PC, Frame_Level => State.Frame_Count,
                     Operation => Operation, List_Type => Ins.Data_Type, Subject => Subject, Callee => Callee,
                     Accumulator =>
                       (case Operation is
                           when L.Fold_Items => Initial,
                           when L.Count_Items => Integer_Constant (0),
                           when L.All_Items => Boolean_Constant (True),
                           when others => Boolean_Constant (False)),
                     Position => 0, Built => (others => <>), Kept => 0, Keys => <>);
                  declare
                     It : Iteration := State.Iterations (State.Iteration_Depth);
                     Length : constant Natural := List_Regions.Length (Subject.Items);
                     D : constant CCL.Types.Description := CCL.Types.Describe (Types, Callee.Data_Type);
                  begin
                     if Operation in L.Each_Items | L.Where_Items | L.Sort_By_Items then
                        It.Built := (Kind => List_Value, Data_Type =>
                                       (if Operation = L.Each_Items then List_Of (Types, D.Parts (D.Count).Payload)
                                        else Ins.Data_Type), others => <>);
                        List_Regions.Reserve (State.Lists, Length, It.Built.Items, Written);
                        Good := Written = List_Regions.Operation_Ok;
                     end if;
                     if Good and then Operation = L.Sort_By_Items then
                        List_Regions.Reserve (State.Lists, Length, It.Keys, Written);
                        Good := Written = List_Regions.Operation_Ok;
                        --  The elements, in order, to be sorted with their keys.
                        for I in 1 .. Length loop
                           pragma Loop_Invariant (Fuel_Limit (State) = Fuel_Limit (State'Loop_Entry));
                           pragma Loop_Invariant
                             (CCL.Imports.Phase (State.Import_Lifecycle) =
                                CCL.Imports.Phase (State.Import_Lifecycle'Loop_Entry));
                           exit when not Good;
                           List_Regions.Read (State.Lists, Subject.Items, List_Regions.Array_Index (I), Element, Read);
                           Good := Read = List_Regions.Operation_Ok;
                           if Good then
                              List_Regions.Write (State.Lists, It.Built.Items, List_Regions.Array_Index (I),
                                                  Element, Written);
                              Good := Written = List_Regions.Operation_Ok;
                           end if;
                        end loop;
                     end if;
                     State.Iterations (State.Iteration_Depth) := It;
                     if not Good then
                        Trap (List_Storage_Exhausted);
                        return;
                     end if;
                  end;
               end if;
            end;
         end if;
         if not Good or else State.Iteration_Depth = 0 then
            Trap (Invalid_Bytecode);
            return;
         end if;

         declare
            Depth : constant Iteration_Count := State.Iteration_Depth;
            It : Iteration := State.Iterations (Depth);
            Length : constant Natural := List_Regions.Length (It.Subject.Items);
            D : constant CCL.Types.Description := CCL.Types.Describe (Types, It.Callee.Data_Type);
            Parameter : constant CCL.Types.Type_Reference :=
              D.Parts (if It.Operation = L.Fold_Items then 2 else 1).Payload;
            Arguments : Component_Values := [others => (others => <>)];
            Argument : Value;
            Result : Value;
         begin
            if not Decided and then It.Position < Length then
               --  The next element, as the function's argument.
               It.Position := It.Position + 1;
               List_Regions.Read (State.Lists, It.Subject.Items, List_Regions.Array_Index (It.Position), Element, Read);
               From_Element (State.Arena, Types, It.List_Type, Element, Argument, Good);
               Good := Good and then Read = List_Regions.Operation_Ok;
               if not Good then
                  Trap (Invalid_Bytecode);
               elsif CCL.Types.Is_Range (Types, Parameter) and then
                 (Argument.Integer < CCL.Types.Low_Of (Types, Parameter) or else
                  Argument.Integer > CCL.Types.High_Of (Types, Parameter))
               then
                  Trap (Range_Error);
               else
                  if It.Operation = L.Fold_Items then
                     Arguments (1) := It.Accumulator;
                     Arguments (2) := Argument;
                  else
                     Arguments (1) := Argument;
                  end if;
                  It.Awaiting := True;
                  State.Iterations (Depth) := It;
                  if State.Frame_Count = MAX_FUNCTIONS then
                     Trap (Call_Depth_Exhausted);
                  else
                     Enter_Call (It.Callee, Arguments, (if It.Operation = L.Fold_Items then 2 else 1), PC, Good);
                     if not Good then
                        Trap (Invalid_Bytecode);
                     end if;
                  end if;
               end if;
            else
               --  Finished: the result, then past the instruction.
               case It.Operation is
                  when L.Each_Items | L.Where_Items =>
                     List_Regions.Shrink (State.Lists, It.Built.Items, It.Kept, Written);
                     Good := Written = List_Regions.Operation_Ok;
                     Result := It.Built;
                  when L.Sort_By_Items =>
                     declare
                        Exhausted : Boolean;
                     begin
                        Sort_By_Keys (State.Lists, State.Text, State.Execution_Budget, Types, It, Good, Exhausted);
                        if Exhausted then
                           Trap (Fuel_Exhausted);
                        end if;
                     end;
                     Result := It.Built;
                  when others =>
                     Result := It.Accumulator;
               end case;
               State.Iterations (Depth) := (others => <>);
               State.Iteration_Depth := Depth - 1;
               if Good then
                  Push_Next (Result);
               elsif not Done then
                  Trap (Invalid_Bytecode);
               end if;
            end if;
         end;
      end Run_Apply;

      --  The list opcodes, with their frame: they change the stack, the
      --  list region and the terminal status, never the fuel limit or the
      --  import lifecycle.
      procedure Run_Region_Op (Op : Op_Code)
        with Pre => Op in New_List | Fill_List | Length_List | List_At | List_Builtin | Make_Node | Check_Range |
                          Make_Closure | Call_Value | List_Apply,
             Post => Fuel_Limit (State) = Fuel_Limit (State'Old) and then
                     CCL.Imports.Phase (State.Import_Lifecycle) =
                       CCL.Imports.Phase (State.Import_Lifecycle'Old);
      procedure Run_Region_Op (Op : Op_Code) is
      begin
         case Op is
            when New_List =>
               Joined := (Kind => List_Value, Data_Type => Item.Content.Code (PC).Data_Type, others => <>);
               if Item.Content.Code (PC).Immediate not in 0 .. MAX_LIST_ELEMENTS then
                  Trap (Invalid_Bytecode);
               else
                  List_Regions.Reserve
                    (State.Lists, Natural (Item.Content.Code (PC).Immediate), Joined.Items, List_Status);
                  if List_Status /= List_Regions.Operation_Ok then
                     Trap (List_Storage_Exhausted);
                  else
                     Push_Next (Joined);
                  end if;
               end if;

            when Fill_List =>
               Runtime_Stacks.Pop (Stack, Right_Value, Stack_Result);
               if Stack_Result = Runtime_Stacks.Stack_Ok then
                  Runtime_Stacks.Peek_Top (Stack, Left_Value, Stack_Result);
               end if;
               if Stack_Result /= Runtime_Stacks.Stack_Ok or else
                 Left_Value.Kind /= List_Value or else
                 Left_Value.Data_Type /= Item.Content.Code (PC).Data_Type or else
                 Right_Value.Kind /= Element_Kind (Item.Content.Data_Types, Left_Value.Data_Type) or else
                 Right_Value.Data_Type /= Element_Data_Type (Item.Content.Data_Types, Left_Value.Data_Type) or else
                 Item.Content.Code (PC).Immediate not in 1 .. Integer_64 (List_Regions.Length (Left_Value.Items)) or else
                 Program_Length (PC) + 1 >= Item.Content.Length
               then
                  Trap (Invalid_Bytecode);
               else
                  List_Regions.Write
                    (State.Lists, Left_Value.Items,
                     List_Regions.Array_Index (Item.Content.Code (PC).Immediate),
                     To_Element (Right_Value), List_Status);
                  if List_Status /= List_Regions.Operation_Ok then
                     Trap (Invalid_Bytecode);
                  else
                     PC := PC + 1;
                  end if;
               end if;

            when Length_List =>
               Runtime_Stacks.Pop (Stack, Left_Value, Stack_Result);
               if Stack_Result /= Runtime_Stacks.Stack_Ok or else Left_Value.Kind /= List_Value then
                  Trap (Invalid_Bytecode);
               else
                  Push_Next (Integer_Constant (Integer_64 (List_Regions.Length (Left_Value.Items))));
               end if;

            when List_At =>
               Runtime_Stacks.Pop (Stack, Right_Value, Stack_Result);
               if Stack_Result = Runtime_Stacks.Stack_Ok then
                  Runtime_Stacks.Pop (Stack, Left_Value, Stack_Result);
               end if;
               if Stack_Result /= Runtime_Stacks.Stack_Ok or else Right_Value.Kind /= Integer_Value or else
                 Left_Value.Kind /= List_Value or else
                 Left_Value.Data_Type /= Item.Content.Code (PC).Data_Type
               then
                  Trap (Invalid_Bytecode);
               elsif Right_Value.Integer < 1 or else
                 Right_Value.Integer > Integer_64 (List_Regions.Length (Left_Value.Items))
               then
                  Trap (Index_Out_Of_Range);
               else
                  declare
                     Element : List_Element;
                     Good : Boolean;
                  begin
                     List_Regions.Read
                       (State.Lists, Left_Value.Items, List_Regions.Array_Index (Right_Value.Integer),
                        Element, List_Status);
                     From_Element (State.Arena, Item.Content.Data_Types, Left_Value.Data_Type, Element, Joined, Good);
                     if List_Status /= List_Regions.Operation_Ok or else not Good then
                        Trap (Invalid_Bytecode);
                     else
                        Push_Next (Joined);
                     end if;
                  end;
               end if;
            when List_Builtin =>
               declare
                  Ins : constant Instruction := Item.Content.Code (PC);
                  Operation : L.Operation;
                  Known : Boolean;
                  Subject, A, B, Answer : Value;
                  Outcome : Execution_Status;

                  --  Pop an operand of Kind (and list type, for lists), or trap.
                  procedure Take (Kind : Value_Kind; Operand : out Value) is
                  begin
                     Operand := (others => <>);
                     if Done then return; end if;
                     Runtime_Stacks.Pop (Stack, Operand, Stack_Result);
                     if Stack_Result /= Runtime_Stacks.Stack_Ok or else Operand.Kind /= Kind or else
                       (Kind = List_Value and then Operand.Data_Type /= Ins.Data_Type)
                     then
                        Trap (Invalid_Bytecode);
                     end if;
                  end Take;
               begin
                  Find_List_Operation (Ins.Immediate, Operation, Known);
                  if not Known or else
                    not List_Builtin_Applies (Item.Content.Data_Types, Operation, Ins.Data_Type)
                  then
                     Trap (Invalid_Bytecode);
                  else
                     case Operation is
                        when L.Range_Items =>
                           Take (Integer_Value, B);
                           Take (Integer_Value, A);
                        when L.Split_Text =>
                           Take (Text_Value, Subject);
                           Take (Text_Value, A);
                        when others =>
                           Take (List_Value, Subject);
                           case Operation is
                              when L.First_Items | L.Last_Items | L.Skip_Items =>
                                 Take (Integer_Value, A);
                              when L.Contains_Item =>
                                 Take (Element_Kind (Item.Content.Data_Types, Ins.Data_Type), A);
                              when L.Join_Items =>
                                 Take (Text_Value, A);
                              when others => null;
                           end case;
                     end case;
                     if not Done then
                        Run_List_Builtin
                          (State.Lists, State.Text, State.Execution_Budget, State.Arena, Item.Content.Data_Types,
                           Operation, Ins.Data_Type, Subject, A, B, Answer, Outcome);
                        if Outcome /= Completed then
                           Trap (Outcome);
                        else
                           Push_Next (Answer);
                        end if;
                     end if;
                  end if;
               end;
            when Make_Node =>
               declare
                  Ins : constant Instruction := Item.Content.Code (PC);
                  D : constant CCL.Types.Description := CCL.Types.Describe (Item.Content.Data_Types, Ins.Data_Type);
                  Count : constant CCL.Types.Component_Count :=
                    (if D.Form = CCL.Types.Product then D.Count
                     elsif Ins.Alternative in 1 .. D.Count and then
                       D.Parts (Ins.Alternative).Payload /= CCL.Types.Unit_Type then 1
                     else 0);
                  Parts : Component_Values := [others => (others => <>)];
                  Built : Value;
                  Good : Boolean := True;
               begin
                  for P in reverse 1 .. Count loop
                     Runtime_Stacks.Pop (Stack, Parts (P), Stack_Result);
                     if Stack_Result /= Runtime_Stacks.Stack_Ok then
                        Good := False;
                        exit;
                     end if;
                  end loop;
                  if not Good then
                     Trap (Invalid_Bytecode);
                  elsif (D.Form = CCL.Types.Product or else Count > 0) and then
                    (State.Arena.Nodes_Used = MAX_VALUE_NODES or else
                     MAX_VALUE_SLOTS - State.Arena.Slots_Used < Count)
                  then
                     Trap (Object_Storage_Exhausted);
                  else
                     Allocate_Node (State.Arena, Item.Content.Data_Types, Ins.Data_Type, Ins.Alternative,
                                    Parts, Count, Built, Good);
                     if Good then
                        Push_Next (Built);
                     else
                        Trap (Invalid_Bytecode);
                     end if;
                  end if;
               end;
            when Make_Closure =>
               --  The captures, in order, into the list region.
               declare
                  Ins : constant Instruction := Item.Content.Code (PC);
                  Captured : List_Element_Array (Parameter_Index) := [others => Null_List_Element];
                  Closure : Value;
                  Good : Boolean := True;
               begin
                  if Ins.Immediate not in 0 .. Integer_64 (Item.Content.Functions_Length) - 1 or else
                    not CCL.Types.Is_Function (Item.Content.Data_Types, Ins.Data_Type)
                  then
                     Trap (Invalid_Bytecode);
                  else
                     declare
                        Decl : constant Function_Declaration := Item.Content.Functions (Function_Index (Ins.Immediate));
                     begin
                        for P in reverse 1 .. Decl.Captures loop
                           Runtime_Stacks.Pop (Stack, Right_Value, Stack_Result);
                           if Stack_Result /= Runtime_Stacks.Stack_Ok or else Right_Value.Kind /= Decl.Kinds (P) or else
                             Right_Value.Data_Type /= Decl.Data_Types (P)
                           then
                              Good := False;
                              exit;
                           end if;
                           Captured (P) := To_Element (Right_Value);
                        end loop;
                        Closure := (Kind => Function_Value, Data_Type => Ins.Data_Type,
                                    Integer => Ins.Immediate, others => <>);
                        if not Good then
                           Trap (Invalid_Bytecode);
                        elsif Decl.Captures > 0 then
                           List_Regions.Allocate (State.Lists, Captured (1 .. Decl.Captures), Closure.Items, List_Status);
                           if List_Status /= List_Regions.Operation_Ok then
                              Trap (List_Storage_Exhausted);
                           else
                              Push_Next (Closure);
                           end if;
                        else
                           Push_Next (Closure);
                        end if;
                     end;
                  end if;
               end;
            when Call_Value =>
               declare
                  Ins : constant Instruction := Item.Content.Code (PC);
                  D : constant CCL.Types.Description := CCL.Types.Describe (Item.Content.Data_Types, Ins.Data_Type);
                  Arity : constant Natural := (if D.Count >= 1 then D.Count - 1 else 0);
                  Arguments : Component_Values := [others => (others => <>)];
                  Callee : Value;
                  Good : Boolean := D.Count >= 1 and then Arity <= CCL.Types.Maximum_Components;
               begin
                  for P in reverse 1 .. Arity loop
                     exit when not Good;
                     Runtime_Stacks.Pop (Stack, Arguments (P), Stack_Result);
                     Good := Stack_Result = Runtime_Stacks.Stack_Ok;
                  end loop;
                  if Good then
                     Runtime_Stacks.Pop (Stack, Callee, Stack_Result);
                     Good := Stack_Result = Runtime_Stacks.Stack_Ok and then Callee.Data_Type = Ins.Data_Type and then
                       Program_Length (PC) + 1 < Item.Content.Length;
                  end if;
                  if Good and then State.Frame_Count = MAX_FUNCTIONS then
                     Trap (Call_Depth_Exhausted);
                  elsif Good then
                     Enter_Call (Callee, Arguments, Arity, PC + 1, Good);
                     if not Good then
                        Trap (Invalid_Bytecode);
                     end if;
                  else
                     Trap (Invalid_Bytecode);
                  end if;
               end;
            when List_Apply =>
               Run_Apply;
            when Check_Range =>
               Runtime_Stacks.Pop (Stack, Right_Value, Stack_Result);
               if Stack_Result /= Runtime_Stacks.Stack_Ok or else Right_Value.Kind /= Integer_Value or else
                 not CCL.Types.Is_Range (Item.Content.Data_Types, Item.Content.Code (PC).Data_Type)
               then
                  Trap (Invalid_Bytecode);
               elsif Right_Value.Integer < CCL.Types.Low_Of (Item.Content.Data_Types, Item.Content.Code (PC).Data_Type) or else
                 Right_Value.Integer > CCL.Types.High_Of (Item.Content.Data_Types, Item.Content.Code (PC).Data_Type)
               then
                  Trap (Range_Error);
               else
                  Push_Next (Right_Value);
               end if;
            when others => null;
         end case;
      end Run_Region_Op;

      --  The opcodes of the variant, record and stack-shuffling opcodes, framed like Run_Region_Op: they never
      --  change the fuel limit or the import lifecycle.
      procedure Run_Data_Op
        with Post => Fuel_Limit (State) = Fuel_Limit (State'Old) and then
                     CCL.Imports.Phase (State.Import_Lifecycle) =
                       CCL.Imports.Phase (State.Import_Lifecycle'Old);
      procedure Run_Data_Op is
      begin
         declare
            Ins : constant Instruction := Item.Content.Code (PC);
            D : constant CCL.Types.Description := CCL.Types.Describe (Item.Content.Data_Types, Ins.Data_Type);
            Good : Boolean := True;
            Next_PC : Instruction_Index := PC + 1;
            Alternative : CCL.Types.Component_Count;
            Native_Value : Value;
            function Matches (V : Value; Ref : CCL.Types.Type_Reference) return Boolean is
              (V.Kind = Kind_For_Type (Item.Content.Data_Types, Ref) and then
               V.Data_Type = Reference_For_Type (Item.Content.Data_Types, Ref) and then
               V.Copyable and then V.Type_Tag = 0 and then Well_Typed (Item.Content.Data_Types, V));
         begin
            case Ins.Op is
               when Project_Field =>
                  Runtime_Stacks.Pop (Stack, Right_Value, Stack_Result);
                  Good := Stack_Result = Runtime_Stacks.Stack_Ok and then
                    Matches (Right_Value, Ins.Data_Type) and then D.Form = CCL.Types.Product and then
                    Ins.Immediate in 1 .. Integer_64 (D.Count);
                  if Good then
                     Component (State.Arena, Item.Content.Data_Types, Right_Value,
                                CCL.Types.Component_Index (Ins.Immediate), Native_Value, Good);
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
                           Alternative := Right_Value.Alternative;
                           Good := Alternative in 1 .. Schema.Count;
                           if Good then
                              Next_PC := M.Targets (Alternative);
                              if Schema.Parts (Alternative).Payload /= CCL.Types.Unit_Type then
                                 Component (State.Arena, Item.Content.Data_Types, Right_Value, 1, Native_Value, Good);
                                 Good := Good and then Matches (Native_Value, Schema.Parts (Alternative).Payload);
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
      end Run_Data_Op;

      --  The opcodes of integer arithmetic, framed like Run_Region_Op: they never
      --  change the fuel limit or the import lifecycle.
      procedure Run_Arithmetic_Op
        with Post => Fuel_Limit (State) = Fuel_Limit (State'Old) and then
                     CCL.Imports.Phase (State.Import_Lifecycle) =
                       CCL.Imports.Phase (State.Import_Lifecycle'Old);
      procedure Run_Arithmetic_Op is
      begin
         case Item.Content.Code (PC).Op is
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

            when others => null;
         end case;
      end Run_Arithmetic_Op;

      --  The opcodes of the text opcodes, framed like Run_Region_Op: they never
      --  change the fuel limit or the import lifecycle.
      procedure Run_Text_Op
        with Post => Fuel_Limit (State) = Fuel_Limit (State'Old) and then
                     CCL.Imports.Phase (State.Import_Lifecycle) =
                       CCL.Imports.Phase (State.Import_Lifecycle'Old);
      procedure Run_Text_Op is
      begin
         case Item.Content.Code (PC).Op is
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

         when Text_At =>
            Runtime_Stacks.Pop (Stack, Right_Value, Stack_Result);
            if Stack_Result /= Runtime_Stacks.Stack_Ok or else Right_Value.Kind /= Integer_Value then
               Trap (Invalid_Bytecode);
            end if;
            if not Done then Pop_Text (Left_Value); end if;
            if not Done then
               if Right_Value.Integer < 1 or else
                 Right_Value.Integer > Integer_64 (Text_Regions.Length (Left_Value.Text))
               then
                  Trap (Index_Out_Of_Range);
               else
                  Text_Regions.Read
                    (State.Text, Left_Value.Text, Text_Regions.String_Index (Right_Value.Integer),
                     Element, Text_Status);
                  if Text_Status /= Text_Regions.Operation_Ok then
                     Trap (Text_Failure (Text_Status));
                  else
                     Push_Next (Character_Constant (Element));
                  end if;
               end if;
            end if;

         when Integer_To_Text | Variant_To_Text =>
            Runtime_Stacks.Pop (Stack, Left_Value, Stack_Result);
            if Stack_Result /= Runtime_Stacks.Stack_Ok or else
              Left_Value.Kind /=
                (if Item.Content.Code (PC).Op = Integer_To_Text then Integer_Value else Variant_Value) or else
              (Left_Value.Kind = Variant_Value and then
               (Left_Value.Data_Type /= Item.Content.Code (PC).Data_Type or else
                not CCL.Types.Is_Enumeration (Item.Content.Data_Types, Left_Value.Data_Type) or else
                Left_Value.Alternative > CCL.Types.Describe (Item.Content.Data_Types, Left_Value.Data_Type).Count))
            then
               Trap (Invalid_Bytecode);
            else
               Joined := (Kind => Text_Value, others => <>);
               Text_Regions.Allocate_String
                 (State.Text,
                  (if Left_Value.Kind = Integer_Value then T.Decimal_Image (Left_Value.Integer)
                   else CCL.Types.Image (CCL.Types.Describe (Item.Content.Data_Types, Left_Value.Data_Type)
                     .Parts (Left_Value.Alternative).Identifier)),
                  Joined.Text, Text_Status);
               if Text_Status /= Text_Regions.Operation_Ok then
                  Trap (Text_Failure (Text_Status));
               else
                  Push_Next (Joined);
               end if;
            end if;

            when others => null;
         end case;
      end Run_Text_Op;

      --  The opcodes of comparisons and Boolean negation, framed like Run_Region_Op: they never
      --  change the fuel limit or the import lifecycle.
      procedure Run_Logic_Op
        with Post => Fuel_Limit (State) = Fuel_Limit (State'Old) and then
                     CCL.Imports.Phase (State.Import_Lifecycle) =
                       CCL.Imports.Phase (State.Import_Lifecycle'Old);
      procedure Run_Logic_Op is
      begin
         case Item.Content.Code (PC).Op is
         when Equal_Integer | Less_Integer | Less_Equal_Integer | Equal_Boolean | Equal_Character =>
            Runtime_Stacks.Pop (Stack, Right_Value, Stack_Result);
            if Stack_Result /= Runtime_Stacks.Stack_Ok or else
              Right_Value.Kind /= Comparison_Kind (Item.Content.Code (PC).Op) or else
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

            when others => null;
         end case;
      end Run_Logic_Op;

      --  The opcodes of the local-variable opcodes, framed like Run_Region_Op: they never
      --  change the fuel limit or the import lifecycle.
      procedure Run_Local_Op
        with Post => Fuel_Limit (State) = Fuel_Limit (State'Old) and then
                     CCL.Imports.Phase (State.Import_Lifecycle) =
                       CCL.Imports.Phase (State.Import_Lifecycle'Old);
      procedure Run_Local_Op is
      begin
         case Item.Content.Code (PC).Op is
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
            when others => null;
         end case;
      end Run_Local_Op;
   begin
      Stack := State.Stack;
      PC := State.PC;
      if Waiting or else (State.Waiting_Stream and then not State.Terminal) then
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
               Run_Data_Op;
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
                  State.Terminal_Status := Status;
               end if;
               State.Terminal := True;
               Done := True;

            when Push_Stream =>
               if Program_Length (PC) + 1 >= Item.Content.Length then
                  Trap (Invalid_Bytecode);
                  Done := True;
               else
                  Runtime_Stacks.Push
                    (Stack, (Kind => Integer_Value, Integer => Item.Content.Code (PC).Immediate,
                             Data_Type => Item.Content.Code (PC).Data_Type, others => <>),
                     Stack_Result);
                  if Stack_Result = Runtime_Stacks.Stack_Ok then
                     PC := PC + 1;
                  else
                     Trap (Invalid_Bytecode);
                     Done := True;
                  end if;
               end if;

            when Stream_View =>
               --  Pop the stream (and a window's count), then suspend until
               --  the host's reader answers (Complete_Stream_Call).
               declare
                  View : CCL.Streams.View_Kind;
                  Known : Boolean;
                  Count : Value := Integer_Constant (1);
               begin
                  Find_View (Item.Content.Code (PC).Immediate, View, Known);
                  Runtime_Stacks.Pop (Stack, Right_Value, Stack_Result);
                  if Known and then Stack_Result = Runtime_Stacks.Stack_Ok and then
                    View = CCL.Streams.Window_View
                  then
                     Runtime_Stacks.Pop (Stack, Count, Stack_Result);
                  end if;
                  if not Known or else Stack_Result /= Runtime_Stacks.Stack_Ok or else
                    Right_Value.Kind /= Integer_Value or else Count.Kind /= Integer_Value or else
                    Right_Value.Data_Type /= Item.Content.Code (PC).Data_Type or else
                    Right_Value.Integer not in 1 .. CCL.Streams.Maximum_Handle
                  then
                     Trap (Invalid_Bytecode);
                  elsif Count.Integer not in 1 .. CCL.Streams.Maximum_Window then
                     Trap (Stream_Window_Out_Of_Range);
                  else
                     State.Waiting_Stream := True;
                     State.Stream_Request :=
                       (Stream => CCL.Streams.Handle (Right_Value.Integer), View => View,
                        Count => CCL.Streams.Window_Length (Count.Integer));
                     State.Stream_Result_Type := Stream_View_Type
                       (Item.Content.Data_Types, Item.Content.Code (PC).Data_Type, View);
                     Status := Waiting_For_Host;
                  end if;
                  Done := True;
               end;

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

            when Add_Integer | Subtract_Integer | Multiply_Integer | Divide_Integer | Modulo_Integer =>
               Run_Arithmetic_Op;
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

            when Push_Text | Concat_Text | Text_Builtin | Length_Text | Equal_Text | Text_At | Integer_To_Text | Variant_To_Text =>
               Run_Text_Op;
            when New_List | Fill_List | Length_List | List_At | List_Builtin | Make_Node | Check_Range |
                 Make_Closure | Call_Value | List_Apply =>
               Run_Region_Op (Item.Content.Code (PC).Op);

            when Equal_Integer | Less_Integer | Less_Equal_Integer | Equal_Boolean | Equal_Character | Not_Boolean =>
               Run_Logic_Op;
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

            when Initialize_Local | Copy_Local | Move_Local | Drop_Local | Borrow_Local_RO | Return_Local_RO | Borrow_Local_RW | Return_Local_RW | Apply_Local_Disposition =>
               Run_Local_Op;
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
         Has_Result_Text => State.Has_Value and then State.Result_Value.Kind = Text_Value and then
           Text_Regions.Is_Valid (State.Text, State.Result_Value.Text) and then
           Text_Regions.Length (State.Result_Value.Text) <= MAX_RESULT_TEXT,
         Request_Receiver => State.Waiting_Receiver,
         Request_Owned => Waiting_Owned,
         Requested_Authority =>
           (if Waiting then
               Item.Content.Imports (State.Waiting_Import).Authority
            else No_Authority),
         Requested_Binding =>
           (if Waiting then
               Item.Content.Imports (State.Waiting_Import).Binding
            else 0),
         Stream_Requested => State.Waiting_Stream and then not State.Terminal,
         Stream_Request => State.Stream_Request,
         others => <>);
      if State.Has_Value and then
        (State.Result_Value.Kind = Object_Value or else
         (State.Result_Value.Kind = List_Value and then
          Compound_Elements (Item.Content.Data_Types, State.Result_Value.Data_Type)))
      then
         declare
            Printed : Boolean := True;
         begin
            Print_Value (State.Arena, State.Text, State.Lists, Item.Content.Data_Types, State.Result_Value,
                         MAX_VALUE_NODES + 1, 1, Result.Literal, Printed);
            --  The literal is for display; a value without one (a Character
            --  field, or longer than a result carries) still completes, and a
            --  host takes it through Native_Objects.Export_Result.
            if Printed then
               Result.Has_Literal := True;
               Result.Literal_Shape := CCL.Types.Shapes.Shape_Of
                 (Item.Content.Data_Types, State.Result_Value.Data_Type);
            else
               Result.Literal := (others => <>);
            end if;
         end;
      elsif State.Has_Value and then State.Result_Value.Kind = List_Value then
         Export_List (State.Lists, State.Text, State.Arena, Item.Content.Data_Types, State.Result_Value, Result);
      end if;
   end Run;

   procedure Continue_Execution_For
     (Item : Validated_Program; State : in out Machine_State;
      Instructions : Natural; Result : out Execution_Result) is
   begin
      Run (Item, State, Instructions, Result);
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

   procedure Complete_Stream_Call
     (Item : Validated_Program; State : in out Machine_State;
      Response : Value; Failure : Execution_Status)
   is
      Stack_Result : Runtime_Stacks.Operation_Result;
      Expected : constant CCL.Types.Type_Reference := State.Stream_Result_Type;
   begin
      if not State.Waiting_Stream or else State.Terminal then
         return;
      end if;
      State.Waiting_Stream := False;
      if Failure /= Completed then
         State.Terminal := True;
         State.Terminal_Status := Failure;
      elsif not CCL.Types.Known (Item.Content.Data_Types, Expected) or else
        Response.Kind /= Kind_For_Type (Item.Content.Data_Types, Expected) or else
        Response.Data_Type /= Reference_For_Type (Item.Content.Data_Types, Expected) or else
        not Well_Typed (Item.Content.Data_Types, Response) or else
        not Response.Copyable or else Response.Type_Tag /= 0 or else
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
   end Complete_Stream_Call;

   procedure Complete_Checked_Host_Call
     (Item     : Validated_Program;
      State    : in out Machine_State;
      Host_Response : Value;
      Accepted : Boolean;
      Native_Response : Boolean;
      Resource_Response : Boolean := False)
   is
      Import_Error : CCL.Imports.Import_Error;
      --  A stream import's reply is the bare handle: it takes the stream
      --  type the program declared (and Well_Typed checks the handle).
      Response : Value := Host_Response;
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
         if Response.Kind = Integer_Value and then Response.Data_Type = CCL.Types.Invalid_Type and then
           CCL.Types.Is_Stream (Item.Content.Data_Types, Item.Content.Imports (State.Waiting_Import).Result_Data_Type)
         then
            Response.Data_Type := Item.Content.Imports (State.Waiting_Import).Result_Data_Type;
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

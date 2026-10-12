with CCL.VM.Machine;
with CCL.VM.Operations;
with CCL.VM.Scalar_Ops;
with CCL.VM.Text_Ops;
with CCL.VM.Data_Ops;
with CCL.VM.List_Ops;
with CCL.VM.Call_Ops;
with CCL.VM.Local_Ops;
with CCL.VM.Results;
with CCL.VM.Verifier_State;
with CCL.VM.Verifier_Tables;
with CCL.VM.Verifier_Sweep;

--  The verifier's sweep and the executor's dispatch loop. Each opcode
--  family's transfer function (CCL.VM.Verifier_*) and step (CCL.VM.*_Ops)
--  is a separate unit with its own contract, so this body's proof context
--  holds only the loops and the opcodes they keep inline (CODE-002).
package body CCL.VM with
   SPARK_Mode => On
is
   use type CCL.Ownership.Ownership_Error;
   use type CCL.Imports.Import_Error;
   use type CCL.Imports.Import_Phase;
   use type Runtime_Stacks.Operation_Result;
   use type CCL.Execution_Budgets.Consume_Result;
   use type CCL.Streams.View_Kind;

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


   procedure Verify
     (Candidate : Program;
      Result    : out Validated_Program;
      Error     : out Validation_Error)
   is
      use CCL.VM.Verifier_State;
      States : State_Table := [others => (others => <>)];
      Switches : Switch_Table := [others => (others => <>)];
      Length : constant Program_Length := Candidate.Length;
      --  Whole-program stack bound. Each region's own maximum depth, and for
      --  each call site the depth under the callee's frame.
      Own_Max : Region_Demands := [others => 0];
      Base_Max : Call_Demands := [others => [others => 0]];
      Calls : Call_Presence := [others => [others => False]];
   begin
      Result := (Checked => False, Content => Candidate);
      Verifier_Tables.Check_Tables (Candidate, Error);
      if Error /= Valid then
         return;
      elsif Length = 0 then
         Error := Empty_Program;
         return;
      elsif Candidate.Dynamic_Locals_Length > Candidate.Locals_Length then
         Error := Invalid_Ownership;
         return;
      end if;

      Verifier_Sweep.Enter (Candidate, States, Error);

      for PC in Instruction_Index loop
         exit when Program_Length (PC) >= Length;
         if Error /= Valid then
            exit;
         elsif not States (PC).Seen then
            Error := Unreachable_Instruction;
            exit;
         end if;

         Verifier_Sweep.Step (Candidate, PC, States, Switches, Own_Max, Base_Max, Calls, Error);
      end loop;

      if Error = Valid and then not Verifier_Tables.Stack_Fits (Own_Max, Base_Max, Calls) then
         Error := Stack_Overflow;
      end if;
      if Error = Valid then
         Verifier_Tables.Check_Ownership (Candidate, Switches, Error);
         if Error = Valid then
            Result := (Checked => True, Content => Candidate);
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
      if Accepted then
         for Position in 1 .. Count loop
            declare
               Local : constant CCL.Ownership.Binding_Id := Local_At (Position - 1);
            begin
               if Values (Local).Kind in Object_Value | Resource_Value | List_Value or else
                 Values (Local).Kind /= Item.Content.Local_Kinds (Local) or else
                 Values (Local).Type_Tag /= Item.Content.Local_Types (Local) or else
                 Values (Local).Data_Type /= Item.Content.Local_Data_Types (Local) or else
                 not Well_Typed (Item.Content.Data_Types, Values (Local))
               then
                  Accepted := False;
                  exit;
               end if;
            end;
         end loop;
      end if;
      if Accepted then
         State.Locals := Values;
         for Position in 1 .. Count loop
            CCL.Ownership.Declare_Binding
              (State.Ownership, Local_At (Position - 1), Item.Content.Local_Types (Local_At (Position - 1)), Error);
            if Error /= CCL.Ownership.Ownership_Valid then
               Accepted := False;
               exit;
            end if;
         end loop;
      end if;
      if not Accepted then
         State.Terminal := True;
         State.Terminal_Status := Invalid_Bytecode;
      end if;
   end Initialize_With_Locals;

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
      elsif Arena.Nodes_Used = Node_Count'Last or else Slot_Count'Last - Arena.Slots_Used < Slots_For (Count) then
         Good := False;
      else
         for P in 1 .. Count loop
            Arena.Slots (Arena.Slots_Used + Slots_For (P)) := To_Slot (Components (P));
         end loop;
         Arena.Nodes_Used := Arena.Nodes_Used + 1;
         Arena.Nodes (Arena.Nodes_Used) :=
           (Data_Type => Data_Type, Alternative => Alternative,
            First => Arena.Slots_Used + 1, Count => Count);
         Arena.Slots_Used := Arena.Slots_Used + Slots_For (Count);
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
        Arena.Nodes (Owner.Node).First <= Slot_Count'Last - Slots_For (P - 1) and then
        (D.Form = CCL.Types.Product or else Arena.Nodes (Owner.Node).Alternative in CCL.Types.Component_Index);
      if Good then
         declare
            Part : constant CCL.Types.Component_Index :=
              (if D.Form = CCL.Types.Product then P else Arena.Nodes (Owner.Node).Alternative);
         begin
            From_Slot (Arena, Types, D.Parts (Part).Payload,
                       Arena.Slots (Arena.Nodes (Owner.Node).First + Slots_For (P - 1)), Result, Good);
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
      use CCL.VM.Machine;
      Waiting : Boolean := State.Waiting;
      Waiting_Owned : Boolean := State.Waiting_Owned;
      Regs : Registers :=
        (Stack => State.Stack, PC => State.PC,
         Done => State.Terminal or else State.Waiting or else State.Waiting_Stream,
         Status => State.Terminal_Status);
      Right_Value, Left_Value : Value := (others => <>);
      Stack_Result : Runtime_Stacks.Operation_Result;
      Own_Error : CCL.Ownership.Ownership_Error;
      Import_Error : CCL.Imports.Import_Error;
      Budget_Result : CCL.Execution_Budgets.Consume_Result;
      Slice_Remaining : Natural := Instructions;
   begin
      if Waiting or else (State.Waiting_Stream and then not State.Terminal) then
         Regs.Status := Waiting_For_Host;
      end if;

      pragma Assert
        (Waiting_Owned or else
         CCL.Imports.Phase (State.Import_Lifecycle) not in
           CCL.Imports.Import_Offered | CCL.Imports.Import_Accepted);

      loop
         pragma Loop_Invariant
           (not Waiting or else State.Waiting_Import < Item.Content.Imports_Length);
         pragma Loop_Invariant (not Waiting or else Regs.Done);
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
         exit when Regs.Done or else
           Slice_Remaining = 0 or else
           not CCL.Execution_Budgets.Has_Fuel (State.Execution_Budget);
         CCL.Execution_Budgets.Consume (State.Execution_Budget, Budget_Result);
         Slice_Remaining := Slice_Remaining - 1;

         if Budget_Result /= CCL.Execution_Budgets.Consumed or else
           Program_Length (Regs.PC) >= Item.Content.Length
         then
            Trap (Regs, State, Invalid_Bytecode);
         else
            declare
               Ins : constant Instruction := Item.Content.Code (Regs.PC);
            begin
               case Ins.Op is
                  when Push_Integer | Push_Boolean =>
                     if Program_Length (Regs.PC) + 1 >= Item.Content.Length then
                        Trap (Regs, State, Invalid_Bytecode);
                     else
                        Runtime_Stacks.Push
                          (Regs.Stack,
                           (if Ins.Op = Push_Integer then Integer_Constant (Ins.Immediate)
                            else Boolean_Constant (Ins.Immediate /= 0)),
                           Stack_Result);
                        if Stack_Result = Runtime_Stacks.Stack_Ok then
                           Regs.PC := Regs.PC + 1;
                        else
                           Trap (Regs, State, Invalid_Bytecode);
                        end if;
                     end if;

                  when Add_Integer | Subtract_Integer | Multiply_Integer | Divide_Integer | Modulo_Integer =>
                     Scalar_Ops.Run_Arithmetic (Item, State, Regs);

                  when Equal_Integer | Less_Integer | Less_Equal_Integer | Equal_Boolean | Equal_Character |
                       Not_Boolean =>
                     Scalar_Ops.Run_Logic (Item, State, Regs);

                  when Drop =>
                     Runtime_Stacks.Pop (Regs.Stack, Right_Value, Stack_Result);
                     if Stack_Result /= Runtime_Stacks.Stack_Ok or else
                       Program_Length (Regs.PC) + 1 >= Item.Content.Length
                     then
                        Trap (Regs, State, Invalid_Bytecode);
                     else
                        Regs.PC := Regs.PC + 1;
                     end if;

                  when Jump =>
                     if Ins.Target <= Regs.PC or else Program_Length (Ins.Target) >= Item.Content.Length then
                        Trap (Regs, State, Invalid_Bytecode);
                     else
                        Regs.PC := Ins.Target;
                     end if;

                  when Jump_If_False =>
                     Runtime_Stacks.Pop (Regs.Stack, Right_Value, Stack_Result);
                     if Stack_Result /= Runtime_Stacks.Stack_Ok or else
                       Right_Value.Kind /= Boolean_Value or else
                       Ins.Target <= Regs.PC or else
                       Program_Length (Ins.Target) >= Item.Content.Length or else
                       Program_Length (Regs.PC) + 1 >= Item.Content.Length
                     then
                        Trap (Regs, State, Invalid_Bytecode);
                     elsif not Right_Value.Boolean then
                        Regs.PC := Ins.Target;
                     else
                        Regs.PC := Regs.PC + 1;
                     end if;

                  when Call_Function =>
                     --  The arguments stay on the stack as the callee's parameters.
                     if State.Frame_Count = Call_Depth'Last or else
                       Ins.Immediate not in 0 .. Integer_64 (Item.Content.Functions_Length) - 1 or else
                       Program_Length (Regs.PC) + 1 >= Item.Content.Length
                     then
                        Trap (Regs, State, Invalid_Bytecode);
                     else
                        State.Frames (State.Frame_Count) :=
                          (Return_PC => Regs.PC + 1, Callee => Operations.Function_Operand (Ins.Immediate));
                        State.Frame_Count := State.Frame_Count + 1;
                        Regs.PC := Item.Content.Functions (Operations.Function_Operand (Ins.Immediate)).Entry_PC;
                     end if;

                  when Return_Function =>
                     --  Keep the result; drop the parameters beneath it.
                     if State.Frame_Count = 0 then
                        Trap (Regs, State, Invalid_Bytecode);
                     else
                        declare
                           Frame : constant Call_Frame := State.Frames (State.Frame_Count - 1);
                           Count : constant Parameter_Count := Item.Content.Functions (Frame.Callee).Count;
                           Good  : Boolean;
                        begin
                           Runtime_Stacks.Pop (Regs.Stack, Right_Value, Stack_Result);
                           Good := Stack_Result = Runtime_Stacks.Stack_Ok;
                           for P in 1 .. Count loop
                              exit when not Good;
                              Runtime_Stacks.Pop (Regs.Stack, Left_Value, Stack_Result);
                              Good := Stack_Result = Runtime_Stacks.Stack_Ok;
                           end loop;
                           if Good then
                              Runtime_Stacks.Push (Regs.Stack, Right_Value, Stack_Result);
                              Good := Stack_Result = Runtime_Stacks.Stack_Ok;
                           end if;
                           if Good then
                              State.Frames (State.Frame_Count - 1) := (others => <>);
                              State.Frame_Count := State.Frame_Count - 1;
                              Regs.PC := Frame.Return_PC;
                           else
                              Trap (Regs, State, Invalid_Bytecode);
                           end if;
                        end;
                     end if;

                  when Make_Variant | Equal_Variant | Switch_Variant | Copy_Stack | Drop_Under_Top |
                       Project_Field =>
                     Data_Ops.Run_Data (Item, State, Regs);

                  when Push_Text | Concat_Text | Text_Builtin | Length_Text | Equal_Text | Text_At |
                       Integer_To_Text | Variant_To_Text =>
                     Text_Ops.Run_Text (Item, State, Regs);

                  when New_List | Fill_List | Length_List | List_At | List_Builtin | Make_Node | Check_Range =>
                     List_Ops.Run_List (Item, State, Regs);

                  when Make_Closure | Call_Value | List_Apply =>
                     Call_Ops.Run_Call (Item, State, Regs);

                  when Initialize_Local | Copy_Local | Move_Local | Drop_Local | Borrow_Local_RO |
                       Return_Local_RO | Borrow_Local_RW | Return_Local_RW | Apply_Local_Disposition =>
                     Local_Ops.Run_Local (Item, State, Regs);

                  when Halt =>
                     CCL.Ownership.Check_Scope (State.Ownership, Item.Content.Types, Own_Error);
                     if Own_Error /= CCL.Ownership.Ownership_Valid then
                        Regs.Status := Invalid_Bytecode;
                        State.Terminal_Status := Invalid_Bytecode;
                     else
                        Regs.Status := Completed;
                        Runtime_Stacks.Peek_Top (Regs.Stack, State.Result_Value, Stack_Result);
                        if Stack_Result = Runtime_Stacks.Stack_Ok then
                           State.Has_Value := True;
                        end if;
                        State.Terminal_Status := Completed;
                     end if;
                     State.Terminal := True;
                     Regs.Done := True;

                  when Push_Stream =>
                     if Program_Length (Regs.PC) + 1 >= Item.Content.Length then
                        Trap (Regs, State, Invalid_Bytecode);
                     else
                        Runtime_Stacks.Push
                          (Regs.Stack, (Kind => Integer_Value, Integer => Ins.Immediate,
                                        Data_Type => Ins.Data_Type, others => <>),
                           Stack_Result);
                        if Stack_Result = Runtime_Stacks.Stack_Ok then
                           Regs.PC := Regs.PC + 1;
                        else
                           Trap (Regs, State, Invalid_Bytecode);
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
                        Operations.Find_View (Ins.Immediate, View, Known);
                        Runtime_Stacks.Pop (Regs.Stack, Right_Value, Stack_Result);
                        if Known and then Stack_Result = Runtime_Stacks.Stack_Ok and then
                          View = CCL.Streams.Window_View
                        then
                           Runtime_Stacks.Pop (Regs.Stack, Count, Stack_Result);
                        end if;
                        if not Known or else Stack_Result /= Runtime_Stacks.Stack_Ok or else
                          Right_Value.Kind /= Integer_Value or else Count.Kind /= Integer_Value or else
                          Right_Value.Data_Type /= Ins.Data_Type or else
                          Right_Value.Integer not in 1 .. CCL.Streams.Maximum_Handle
                        then
                           Trap (Regs, State, Invalid_Bytecode);
                        elsif Count.Integer not in 1 .. CCL.Streams.Maximum_Window then
                           Trap (Regs, State, Stream_Window_Out_Of_Range);
                        else
                           State.Waiting_Stream := True;
                           State.Stream_Request :=
                             (Stream => CCL.Streams.Handle (Right_Value.Integer), View => View,
                              Count => CCL.Streams.Window_Length (Count.Integer));
                           State.Stream_Result_Type := Stream_View_Type (Item.Content.Data_Types, Ins.Data_Type, View);
                           Regs.Status := Waiting_For_Host;
                        end if;
                        Regs.Done := True;
                     end;

                  when Invoke_Import =>
                     declare
                        Import_Number : constant Import_Index := Ins.Import;
                        Operation : constant Import_Declaration := Item.Content.Imports (Import_Number);
                        Argument_Valid : Boolean := True;
                     begin
                        State.Waiting_Receiver := CCL.Resources.No_Reference;
                        if Import_Number < Item.Content.Imports_Length and then
                          (not Operation.Ownership_Argument or else Has_Receiver (Operation))
                        then
                           Runtime_Stacks.Pop (Regs.Stack, Right_Value, Stack_Result);
                           Argument_Valid := Stack_Result = Runtime_Stacks.Stack_Ok and then
                             Right_Value.Kind = Operation.Argument and then
                             Right_Value.Data_Type = Operation.Argument_Data_Type and then
                             Well_Typed (Item.Content.Data_Types, Right_Value) and then
                             Right_Value.Copyable and then Right_Value.Type_Tag = 0;
                        end if;
                        if Import_Number >= Item.Content.Imports_Length or else not Argument_Valid then
                           Regs.Status := Invalid_Bytecode;
                           State.Terminal := True;
                           State.Terminal_Status := Invalid_Bytecode;
                        elsif Operation.Argument = Text_Value and then
                          (not Text_Regions.Is_Valid (State.Text, Right_Value.Text) or else
                           not Within (Text_Regions.Length (Right_Value.Text), Operation.Argument_Text_Limit))
                        then
                           Regs.Status := Host_Argument_Out_Of_Bounds;
                           State.Terminal := True;
                           State.Terminal_Status := Host_Argument_Out_Of_Bounds;
                        elsif Operation.Ownership_Argument then
                           -- A terminal completion has already returned its borrow
                           -- or applied its disposition. Start a fresh lifecycle for
                           -- the next call, never reset an offered/accepted call.
                           if CCL.Imports.Phase (State.Import_Lifecycle) = CCL.Imports.Import_Completed then
                              CCL.Imports.Initialize (State.Import_Lifecycle);
                           end if;
                           CCL.Imports.Offer
                             (State.Import_Lifecycle, Operation.Local, Operation.Transfer,
                              Operation.Cancellation, Operation.Success_Verb, Operation.Failure_Verb,
                              Operation.Cancel_Verb, Import_Error);
                           if Import_Error /= CCL.Imports.Import_Valid then
                              Regs.Status := Invalid_Bytecode;
                              State.Terminal := True;
                              State.Terminal_Status := Invalid_Bytecode;
                           else
                              Waiting := True;
                              Waiting_Owned := True;
                              State.Waiting_Import := Import_Number;
                              State.Waiting_Result_Kind := Operation.Result;
                              if Has_Receiver (Operation) then
                                 State.Waiting_Receiver := State.Locals (Operation.Local).Resource;
                                 State.Waiting_Argument := Right_Value;
                              else
                                 State.Waiting_Argument := State.Locals (Operation.Local);
                              end if;
                              Regs.Status := Waiting_For_Host;
                           end if;
                        else
                           Waiting := True;
                           State.Waiting_Import := Import_Number;
                           State.Waiting_Result_Kind := Operation.Result;
                           State.Waiting_Argument := Right_Value;
                           Waiting_Owned := False;
                           Regs.Status := Waiting_For_Host;
                        end if;
                        Regs.Done := True;
                     end;
               end case;
            end;
         end if;
      end loop;

      if not Regs.Done and then
        not CCL.Execution_Budgets.Has_Fuel (State.Execution_Budget)
      then
         Regs.Status := Fuel_Exhausted;
         State.Terminal := True;
         State.Terminal_Status := Fuel_Exhausted;
      elsif not Regs.Done then
         Regs.Status := Paused;
      end if;

      State.Stack := Regs.Stack;
      State.PC := Regs.PC;
      pragma Assert
        (Waiting_Owned or else
         CCL.Imports.Phase (State.Import_Lifecycle) not in
           CCL.Imports.Import_Offered | CCL.Imports.Import_Accepted);
      State.Waiting := Waiting;
      State.Waiting_Owned := Waiting_Owned;
      Results.Report (Item, State, Regs.Status, Waiting, Waiting_Owned, Result);
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
         Result.Stack_Length := Depth_After (Position);
      end loop;

      if Item.Content.Locals_Length > 0 then
         for Position in 0 .. Item.Content.Locals_Length - 1 loop
            Local := Local_At (Position);
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
           CCL.Types.Is_Handle (Item.Content.Data_Types, Item.Content.Imports (State.Waiting_Import).Result_Data_Type)
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

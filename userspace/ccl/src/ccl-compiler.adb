with Interfaces;
with CCL.Ownership;
with CCL.Host_Values;
with CCL.Types;
with CCL.Resource_Policies;

package body CCL.Compiler with
   SPARK_Mode => On
is
   use type CCL.Language.Analysis_Status;
   use type CCL.Language.Node_Kind;
   use type CCL.Language.Static_Type;
   use type CCL.VM.Program_Length;
   use type CCL.Host_Values.Value_Kind;
   use type CCL.Debug_Maps.Add_Result;
   use type CCL.Catalog.Intern_Result;
   use type CCL.Types.Shape;
   use type CCL.Resource_Policies.Layout_Result;
   use type CCL.VM.Validation_Error;

   type Local_Binding is record
      Identifier : CCL.Language.Name;
      Local      : CCL.Ownership.Binding_Id := 0;
      On_Stack : Boolean := False;
      Stack_Level : Natural := 0;
   end record;

   type Local_Environment is
     array (Natural range 0 .. CCL.Language.MAX_BINDINGS - 1) of Local_Binding;

   procedure Compile
     (Analysis : CCL.Language.Analysis_Result;
      Result   : out Compilation_Result)
   is
      Status  : Compilation_Status := Compilation_Succeeded;
      Program : CCL.VM.Program;
      Failed_Node : CCL.Language.Node_Reference := CCL.Language.NO_NODE;
      Failed_Position : CCL.Language.Source_Position := 0;
      Environment : Local_Environment := [others => (others => <>)];
      Environment_Length : CCL.VM.Local_Count := 0;
      Next_Local : CCL.VM.Local_Count := 0;
      Debug : CCL.Debug_Maps.Debug_Map;
      Linkage : CCL.Catalog.Linkage_Table;
      Resource_Tags : CCL.Resource_Policies.Binding_Map := [others => 0];
      Resource_Roots : CCL.Resource_Policies.Selection := [others => False];
      Layout_Result : CCL.Resource_Policies.Layout_Result;
      Checked : CCL.VM.Validated_Program;
      Validation : CCL.VM.Validation_Error;

      procedure Find_Local
        (Name : CCL.Language.Name; Binding : out Local_Binding; Found : out Boolean) is
      begin
         Binding := (others => <>); Found := False;
         for Position in reverse 0 .. Environment_Length - 1 loop
            if CCL.Language.Names_Equal (Environment (Position).Identifier, Name) then
               Binding := Environment (Position); Found := True; return;
            end if;
         end loop;
      end Find_Local;

      procedure Fail
        (Reason : Compilation_Status;
         Index  : CCL.Language.Node_Reference;
         Position : CCL.Language.Source_Position := 0)
      is
      begin
         if Status = Compilation_Succeeded then
            Status := Reason;
            Failed_Node := Index;
            Failed_Position := Position;
         end if;
      end Fail;

      procedure Read_Node
        (Index : CCL.Language.Node_Reference;
         Item : out CCL.Language.Node)
      is
      begin
         Item := (others => <>);
         if Index >= CCL.Language.Analysis_Node_Count (Analysis) then
            Fail (Malformed_Typed_Tree, Index);
         else
            Item := CCL.Language.Analysis_Node
              (Analysis, CCL.Language.Node_Index (Index));
         end if;
      end Read_Node;

      procedure Emit
        (Op        : CCL.VM.Op_Code;
         Immediate : Interfaces.Integer_64 := 0;
         Target    : CCL.VM.Instruction_Index := 0;
         Local     : CCL.Ownership.Binding_Id := 0;
         Import    : CCL.VM.Import_Index := 0;
         Data_Type : CCL.Types.Type_Reference := CCL.Types.Invalid_Type;
         Alternative : CCL.Types.Component_Count := 0)
      is
      begin
         if Status /= Compilation_Succeeded then
            return;
         elsif Program.Length = CCL.VM.MAX_INSTRUCTIONS then
            Fail (Program_Full, CCL.Language.NO_NODE);
         else
            Program.Code (CCL.VM.Instruction_Index (Program.Length)) :=
              (Op => Op, Immediate => Immediate, Target => Target,
               Local => Local, Import => Import, Data_Type => Data_Type,
               Alternative => Alternative, others => <>);
            Program.Length := CCL.VM.Program_Length'Succ (Program.Length);
         end if;
      end Emit;

      procedure Mark_Target
        (Position : out CCL.VM.Instruction_Index;
         Ok       : out Boolean)
      is
      begin
         Ok := Status = Compilation_Succeeded and then
           Program.Length < CCL.VM.MAX_INSTRUCTIONS;
         if Ok then
            Position := CCL.VM.Instruction_Index (Program.Length);
         else
            Position := 0;
            if Status = Compilation_Succeeded then
               Fail (Program_Full, CCL.Language.NO_NODE);
            end if;
         end if;
      end Mark_Target;

      procedure Emit_Node
        (Index : CCL.Language.Node_Reference;
         Depth : Natural;
         In_Conditional_Branch : Boolean;
         Stack_Base : Natural := 0)
      is
         Item : CCL.Language.Node;
         False_Jump : CCL.VM.Instruction_Index := 0;
         End_Jump   : CCL.VM.Instruction_Index := 0;
         Target     : CCL.VM.Instruction_Index := 0;
         Ok         : Boolean;
         Found      : Boolean := False;
         Binding : Local_Binding;
         Local      : CCL.Ownership.Binding_Id := 0;
         Entry_Environment_Length : constant CCL.VM.Local_Count :=
           Environment_Length;
         Initializer : CCL.Language.Node;
         First_PC    : constant CCL.VM.Program_Length := Program.Length;
         Map_Result  : CCL.Debug_Maps.Add_Result;
         Import_Position : CCL.VM.Import_Index := 0;
         Bytecode_Import : CCL.VM.Import_Declaration;
         Import_Lowered : Boolean;
         Interned        : CCL.Catalog.Intern_Result;
         Argument_Node   : CCL.Language.Node;
         Receiver_Node   : CCL.Language.Node;
         Argument_Index  : CCL.Language.Node_Reference := CCL.Language.NO_NODE;
      begin
         if Status /= Compilation_Succeeded then
            return;
         elsif Stack_Base >= CCL.VM.MAX_STACK_DEPTH then
            Fail (Stack_Limit, Index); return;
         elsif Depth >= CCL.Language.MAX_NESTING or else
           Index >= CCL.Language.Analysis_Node_Count (Analysis)
         then
            Fail (Malformed_Typed_Tree, Index);
            return;
         end if;

         Read_Node (Index, Item);
         case Item.Kind is
            when CCL.Language.Field_Form =>
               Read_Node (Item.First, Initializer);
               Emit_Node (Item.First, Depth + 1, In_Conditional_Branch, Stack_Base);
               Emit (CCL.VM.Project_Field, Immediate => Interfaces.Integer_64 (Item.Alternative),
                 Data_Type => Initializer.Static_Kind);
            when CCL.Language.Type_Definition =>
               Emit_Node (Item.Second, Depth + 1, In_Conditional_Branch, Stack_Base);
               return; -- a declaration has no executable source-map range
            when CCL.Language.Variant_Literal | CCL.Language.Variant_Construct =>
               if not CCL.Types.Is_Scalar_Sum (Program.Data_Types, Item.Declared_Kind) then
                  Fail (Unsupported_Form, Index); return;
               end if;
               if Item.Kind = CCL.Language.Variant_Construct then
                  Emit_Node (Item.First, Depth + 1, In_Conditional_Branch, Stack_Base);
               end if;
               Emit (CCL.VM.Make_Variant, Data_Type => Item.Declared_Kind, Alternative => Item.Alternative);
            when CCL.Language.Match_Form =>
               if Program.Matches_Length = CCL.VM.Maximum_Matches then
                  Fail (Too_Many_Matches, Index); return;
               end if;
               Read_Node (Item.First, Initializer);
               declare
                  M : constant CCL.VM.Match_Index := Program.Matches_Length;
                  Scrutinee : constant CCL.Language.Node := Initializer;
                  D : constant CCL.Types.Description := CCL.Types.Describe (Program.Data_Types, Scrutinee.Static_Kind);
                  Arm : CCL.Language.Node_Reference := Item.Second;
                  N : CCL.Language.Node;
                  Exits : CCL.VM.Alternative_Targets := [others => 0];
                  Has_Payload : Boolean;
               begin
                  Program.Matches_Length := Program.Matches_Length + 1;
                  Program.Matches (M).Data_Type := Scrutinee.Static_Kind;
                  Emit_Node (Item.First, Depth + 1, In_Conditional_Branch, Stack_Base);
                  Emit (CCL.VM.Switch_Variant, Immediate => Interfaces.Integer_64 (M));
                  while Arm < CCL.Language.Analysis_Node_Count (Analysis) and then Status = Compilation_Succeeded loop
                     Read_Node (Arm, N);
                     Mark_Target (Target, Ok); exit when not Ok;
                     Program.Matches (M).Targets (N.Alternative) := Target;
                     Has_Payload := D.Parts (N.Alternative).Payload /= CCL.Types.Unit_Type;
                     if Has_Payload then
                        if Environment_Length = CCL.Language.MAX_BINDINGS then
                           Fail (Too_Many_Locals, Arm); exit;
                        end if;
                        Environment (Environment_Length) :=
                          (Identifier => N.Identifier, On_Stack => True, Stack_Level => Stack_Base, others => <>);
                        Environment_Length := Environment_Length + 1;
                     end if;
                     Emit_Node (N.First, Depth + 1, True, Stack_Base + (if Has_Payload then 1 else 0));
                     Environment_Length := Entry_Environment_Length;
                     if Has_Payload then Emit (CCL.VM.Drop_Under_Top); end if;
                     Mark_Target (Target, Ok); exit when not Ok;
                     Exits (N.Alternative) := Target;
                     Emit (CCL.VM.Jump);
                     Arm := N.Second;
                  end loop;
                  Mark_Target (Target, Ok);
                  if Ok then
                     for A in 1 .. D.Count loop Program.Code (Exits (A)).Target := Target; end loop;
                  end if;
               end;
            when CCL.Language.Match_Arm => Fail (Malformed_Typed_Tree, Index);
            when CCL.Language.Integer_Literal =>
               if Item.Static_Kind /= CCL.Language.Integer_Type then
                  Fail (Malformed_Typed_Tree, Index, Item.Source_Position);
               else
                  Emit (CCL.VM.Push_Integer, Item.Integer_Value);
               end if;

            when CCL.Language.Boolean_Literal =>
               if Item.Static_Kind /= CCL.Language.Boolean_Type then
                  Fail (Malformed_Typed_Tree, Index, Item.Source_Position);
               else
                  Emit
                    (CCL.VM.Push_Boolean,
                     (if Item.Boolean_Value then 1 else 0));
               end if;

            when CCL.Language.Add_Form | CCL.Language.Equal_Form =>
               Emit_Node (Item.First, Depth + 1, In_Conditional_Branch, Stack_Base);
               Emit_Node (Item.Second, Depth + 1, In_Conditional_Branch, Stack_Base + 1);
               if Item.Kind = CCL.Language.Add_Form and then
                 Item.Static_Kind = CCL.Language.Integer_Type
               then
                  Emit (CCL.VM.Add_Integer);
               elsif Item.Kind = CCL.Language.Equal_Form and then
                 Item.Static_Kind = CCL.Language.Boolean_Type
               then
                  Read_Node (Item.First, Initializer);
                  if Initializer.Static_Kind = CCL.Language.Integer_Type then Emit (CCL.VM.Equal_Integer);
                  else Emit (CCL.VM.Equal_Variant, Data_Type => Initializer.Static_Kind); end if;
               else
                  Fail (Malformed_Typed_Tree, Index, Item.Source_Position);
               end if;

            when CCL.Language.Multiply_Form | CCL.Language.Divide_Form |
                 CCL.Language.Modulo_Form =>
               Emit_Node (Item.First, Depth + 1, In_Conditional_Branch, Stack_Base);
               Emit_Node (Item.Second, Depth + 1, In_Conditional_Branch, Stack_Base + 1);
               if Item.Static_Kind /= CCL.Language.Integer_Type then
                  Fail (Malformed_Typed_Tree, Index, Item.Source_Position);
               elsif Item.Kind = CCL.Language.Multiply_Form then
                  Emit (CCL.VM.Multiply_Integer);
               elsif Item.Kind = CCL.Language.Divide_Form then
                  Emit (CCL.VM.Divide_Integer);
               else
                  Emit (CCL.VM.Modulo_Integer);
               end if;

            when CCL.Language.Not_Form =>
               Emit_Node (Item.First, Depth + 1, In_Conditional_Branch, Stack_Base);
               if Item.Static_Kind = CCL.Language.Boolean_Type then
                  Emit (CCL.VM.Not_Boolean);
               else
                  Fail (Malformed_Typed_Tree, Index, Item.Source_Position);
               end if;

            when CCL.Language.If_Form =>
               Emit_Node (Item.First, Depth + 1, In_Conditional_Branch, Stack_Base);
               Mark_Target (False_Jump, Ok);
               if Ok then
                  Emit (CCL.VM.Jump_If_False);
                  Emit_Node (Item.Second, Depth + 1, True, Stack_Base);
                  Mark_Target (End_Jump, Ok);
               end if;
               if Ok then
                  Emit (CCL.VM.Jump);
                  Mark_Target (Target, Ok);
               end if;
               if Ok then
                  Program.Code (False_Jump).Target := Target;
                  Emit_Node (Item.Third, Depth + 1, True, Stack_Base);
                  Mark_Target (Target, Ok);
               end if;
               if Ok then
                  Program.Code (End_Jump).Target := Target;
               end if;

            when CCL.Language.Name_Reference =>
               Find_Local (Item.Identifier, Binding, Found);
               Local := Binding.Local;
               if Found then
                  if Binding.On_Stack then
                     if Stack_Base <= Binding.Stack_Level then Fail (Malformed_Typed_Tree, Index);
                     else Emit (CCL.VM.Copy_Stack, Immediate => Interfaces.Integer_64 (Stack_Base - Binding.Stack_Level - 1));
                     end if;
                  elsif CCL.Types.Describe (Program.Data_Types, Item.Static_Kind).Form = CCL.Types.Resource then
                     Emit (CCL.VM.Move_Local, Local => Local);
                  else Emit (CCL.VM.Copy_Local, Local => Local); end if;
               else
                  Fail (Malformed_Typed_Tree, Index, Item.Source_Position);
               end if;

            when CCL.Language.Let_Form =>
               if In_Conditional_Branch then
                  Fail (Unsupported_Form, Index, Item.Source_Position);
               elsif Next_Local = CCL.Language.MAX_BINDINGS or else
                 Environment_Length = CCL.Language.MAX_BINDINGS
               then
                  Fail (Too_Many_Locals, Index, Item.Source_Position);
               elsif Item.First >= CCL.Language.Analysis_Node_Count (Analysis)
               then
                  Fail (Malformed_Typed_Tree, Index, Item.Source_Position);
               else
                  Read_Node (Item.First, Initializer);
                  Local := CCL.Ownership.Binding_Id (Next_Local);
                  Program.Local_Kinds (Local) :=
                    (case Initializer.Static_Kind is
                        when CCL.Language.Integer_Type => CCL.VM.Integer_Value,
                        when CCL.Language.Boolean_Type => CCL.VM.Boolean_Value,
                        when others => (if CCL.Types.Describe (Program.Data_Types, Initializer.Static_Kind).Form = CCL.Types.Resource
                          then CCL.VM.Resource_Value
                          elsif CCL.Types.Is_Scalar_Sum (Program.Data_Types, Initializer.Static_Kind)
                          then CCL.VM.Variant_Value else CCL.VM.Object_Value));
                  Program.Local_Types (Local) := Resource_Tags (Initializer.Static_Kind);
                  if Initializer.Static_Kind = CCL.Language.Handler_Type
                  then
                     Fail (Unsupported_Form, Index, Item.Source_Position);
                  elsif Initializer.Static_Kind = CCL.Language.Invalid_Type then
                     Fail (Malformed_Typed_Tree, Index, Item.Source_Position);
                  else
                     if Initializer.Static_Kind not in CCL.Language.Integer_Type | CCL.Language.Boolean_Type then
                        Program.Local_Data_Types (Local) := Initializer.Static_Kind;
                     end if;
                     Next_Local := Next_Local + 1;
                     Program.Locals_Length := Next_Local;
                     Program.Dynamic_Locals_Length := Next_Local;
                     CCL.Debug_Maps.Set_Local_Name
                       (Debug, Local, Item.Identifier);
                     Emit_Node
                       (Item.First, Depth + 1, In_Conditional_Branch, Stack_Base);
                     Environment_Length := Entry_Environment_Length;
                     Emit (CCL.VM.Initialize_Local, Local => Local);
                     if Status = Compilation_Succeeded then
                        Environment (Environment_Length) :=
                          (Identifier => Item.Identifier, Local => Local, others => <>);
                        Environment_Length := Environment_Length + 1;
                        Emit_Node
                          (Item.Second, Depth + 1,
                           In_Conditional_Branch, Stack_Base);
                        Environment_Length := Entry_Environment_Length;
                     end if;
                  end if;
               end if;

            when CCL.Language.Host_Import_Form =>
               if Item.Host_Call.Parameters = 1 then
                  Argument_Index := (if CCL.Host_Values.Has_Receiver (Item.Host_Call.Import)
                                     then Item.Second else Item.First);
                  if Argument_Index >= CCL.Language.Analysis_Node_Count (Analysis) then
                     Fail (Malformed_Typed_Tree, Index, Item.Source_Position); return;
                  end if;
                  Read_Node (Argument_Index, Argument_Node);
               end if;
               if Item.Host_Call.Import.Argument = CCL.Host_Values.Resource_Value or
                 CCL.Host_Values.Has_Receiver (Item.Host_Call.Import)
               then
                  if Item.Host_Call.Import.Argument = CCL.Host_Values.Resource_Value and then
                    Item.Host_Call.Parameters /= 1
                  then
                     Fail (Malformed_Typed_Tree, Index); return;
                  end if;
                  Read_Node (Item.First, Receiver_Node);
                  if Receiver_Node.Kind = CCL.Language.Name_Reference then
                     Find_Local (Receiver_Node.Identifier, Binding, Found);
                     if not Found or else Binding.On_Stack then
                        Fail (Unsupported_Form, Index, Item.Source_Position); return;
                     end if;
                     Local := Binding.Local;
                  else
                     -- An expression receiver needs a real owned local, too.
                     -- A temporary borrowed resource cannot disappear: the
                     -- ordinary ownership verifier still checks its scope.
                     if Next_Local = CCL.Ownership.MAX_BINDINGS then
                        Fail (Too_Many_Locals, Index); return;
                     end if;
                     Local := Next_Local;
                     Next_Local := Next_Local + 1;
                     Program.Locals_Length := Next_Local;
                     Program.Dynamic_Locals_Length := Next_Local;
                     Program.Local_Kinds (Local) := CCL.VM.Resource_Value;
                     Program.Local_Data_Types (Local) := Receiver_Node.Static_Kind;
                     Program.Local_Types (Local) := Resource_Tags (Receiver_Node.Static_Kind);
                     Emit_Node (Item.First, Depth + 1, In_Conditional_Branch, Stack_Base);
                     Emit (CCL.VM.Initialize_Local, Local => Local);
                  end if;
               end if;
               CCL.Host_Values.To_Bytecode
                 (Item.Host_Call.Import, Program.Data_Types,
                  (if Item.Host_Call.Parameters = 0 then CCL.Types.Integer_Type else Argument_Node.Static_Kind),
                  Item.Static_Kind, Bytecode_Import, Import_Lowered,
                  Result_Type_Tag => Resource_Tags (Item.Static_Kind),
                  Receiver_Type => (if CCL.Host_Values.Has_Receiver (Item.Host_Call.Import)
                                    then Receiver_Node.Static_Kind else CCL.Types.Invalid_Type));
               if Item.Host_Call.Import.Argument = CCL.Host_Values.Resource_Value or
                 CCL.Host_Values.Has_Receiver (Item.Host_Call.Import)
               then
                  Bytecode_Import.Local := Local;
               end if;
               if not Import_Lowered then
                  Fail (Unsupported_Form, Index, Item.Source_Position);
               elsif (Item.Host_Call.Import.Result = CCL.Host_Values.Integer_Value and then
                   Item.Static_Kind /= CCL.Language.Integer_Type) or else
                 (Item.Host_Call.Import.Result = CCL.Host_Values.Boolean_Value and then
                   Item.Static_Kind /= CCL.Language.Boolean_Type)
               then
                  Fail (Malformed_Typed_Tree, Index, Item.Source_Position);
               else
                  CCL.Catalog.Intern
                    (Linkage, Item.Host_Call, Import_Position, Interned,
                     Local => Bytecode_Import.Local);
                  if Interned = CCL.Catalog.Linkage_Full then
                     Fail (Too_Many_Imports, Index, Item.Source_Position);
                  elsif Interned = CCL.Catalog.Linkage_Added then
                     Program.Imports (Import_Position) :=
                       Bytecode_Import;
                     Program.Imports_Length := CCL.Catalog.Length (Linkage);
                  end if;

                  if Status = Compilation_Succeeded then
                     if Item.Host_Call.Import.Argument = CCL.Host_Values.Resource_Value then
                        null; -- Invoke_Import borrows/moves the owned local.
                     elsif Item.Host_Call.Parameters = 0 then
                        --  CCLB has no standalone Unit value. The catalog validates
                        --  that a zero-parameter operation uses this canonical
                        --  scalar sentinel without ownership transfer.
                        Emit (CCL.VM.Push_Integer, Immediate => 0);
                     elsif Argument_Index >=
                       CCL.Language.Analysis_Node_Count (Analysis)
                     then
                        Fail
                          (Malformed_Typed_Tree, Index,
                           Item.Source_Position);
                     else
                        Read_Node (Argument_Index, Argument_Node);
                        if (Item.Host_Call.Import.Argument =
                              CCL.Host_Values.Integer_Value and then
                            Argument_Node.Static_Kind /=
                              CCL.Language.Integer_Type) or else
                          (Item.Host_Call.Import.Argument =
                              CCL.Host_Values.Boolean_Value and then
                            Argument_Node.Static_Kind /=
                              CCL.Language.Boolean_Type)
                        then
                           Fail
                             (Malformed_Typed_Tree, Index,
                              Item.Source_Position);
                        else
                           Emit_Node
                             (Argument_Index, Depth + 1,
                              In_Conditional_Branch, Stack_Base);
                        end if;
                     end if;
                  end if;
                  if Status = Compilation_Succeeded then
                     Emit
                       (CCL.VM.Invoke_Import,
                        Import => Import_Position);
                  end if;
               end if;

            when CCL.Language.Function_Definition | CCL.Language.Function_Call | CCL.Language.Handler_Form |
                 CCL.Language.Record_Construct =>
               --  No CCLB call-frame representation yet. Never silently inline
               --  functions or drop their type/effect admission requirements.
               Fail (Unsupported_Form, Index, Item.Source_Position);

            when CCL.Language.String_Literal |
                 CCL.Language.String_Length_Form |
                 CCL.Language.String_Index_Form |
                 CCL.Language.String_Concat_Form |
                 CCL.Language.To_String_Form =>
               --  The source semantics are implemented and exercised by the
               --  direct interpreter. CCLB has no string constant pool or
               --  variable-sized value kind, so lowering fails explicitly.
               Fail (Unsupported_Form, Index, Item.Source_Position);

            when CCL.Language.Invalid_Node =>
               Fail (Malformed_Typed_Tree, Index, Item.Source_Position);
         end case;

         if Status = Compilation_Succeeded and then Program.Length > First_PC
         then
            CCL.Debug_Maps.Add
              (Debug,
               (First_PC => First_PC,
                End_PC => Program.Length,
                Node => Index,
                Source_First => Item.Source_Position,
                Source_End => Item.Source_End_Position),
               Map_Result);
            if Map_Result = CCL.Debug_Maps.Map_Full then
               Fail (Debug_Map_Full, Index, Item.Source_Position);
            end if;
         end if;
      end Emit_Node;
   begin
      Result := (others => <>);
      CCL.Debug_Maps.Initialize (Debug);
      CCL.Catalog.Initialize (Linkage);
      if CCL.Language.Analysis_Status_Of (Analysis) /=
        CCL.Language.Analysis_Succeeded
      then
         Result.Status := Analysis_Failed;
         return;
      end if;

      Program.Data_Types := CCL.Language.Analysis_Types (Analysis);
      for I in 0 .. CCL.Language.Analysis_Node_Count (Analysis) - 1 loop
         declare
            Kind : constant CCL.Types.Type_Reference := CCL.Language.Analysis_Node (Analysis, I).Static_Kind;
         begin
            if CCL.Types.Describe (Program.Data_Types, Kind).Form = CCL.Types.Resource then
               Resource_Roots (Kind) := True;
            end if;
         end;
      end loop;
      CCL.Resource_Policies.Layout
        (Program.Data_Types, CCL.Language.Analysis_Resource_Policies (Analysis), Resource_Roots,
         Resource_Tags, Program.Types, Program.Types_Length, Layout_Result);
      if Layout_Result /= CCL.Resource_Policies.Ready then
         Result.Status := Unsupported_Form; return;
      end if;
      Emit_Node (CCL.Language.Analysis_Root (Analysis), 0, False);
      Emit (CCL.VM.Halt);
      if Status = Compilation_Succeeded and then (for some Needed of Resource_Roots => Needed) then
         CCL.VM.Verify (Program, Checked, Validation);
         if Validation /= CCL.VM.Valid then Status := Ownership_Check_Failed; end if;
      end if;
      Result :=
        (Status => Status,
         Diagnostic_Node => Failed_Node,
         Source_Position => Failed_Position,
         Program => Program,
         Linkage => Linkage,
         Debug => Debug);
   end Compile;
end CCL.Compiler;

with Interfaces;
with CCL.Ownership;
with CCL.Host_Values;
with CCL.Types;

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
         Scalar_Import : Boolean;
         Interned        : CCL.Catalog.Intern_Result;
         Argument_Node   : CCL.Language.Node;
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

         Item := CCL.Language.Analysis_Node
           (Analysis, CCL.Language.Node_Index (Index));
         case Item.Kind is
            when CCL.Language.Type_Definition =>
               Emit_Node (Item.Second, Depth + 1, In_Conditional_Branch, Stack_Base);
               return; -- a declaration has no executable source-map range
            when CCL.Language.Variant_Literal | CCL.Language.Variant_Construct =>
               if Item.Kind = CCL.Language.Variant_Construct then
                  Emit_Node (Item.First, Depth + 1, In_Conditional_Branch, Stack_Base);
               end if;
               Emit (CCL.VM.Make_Variant, Data_Type => Item.Declared_Kind, Alternative => Item.Alternative);
            when CCL.Language.Match_Form =>
               if Program.Matches_Length = CCL.VM.Maximum_Matches then
                  Fail (Too_Many_Matches, Index); return;
               end if;
               declare
                  M : constant CCL.VM.Match_Index := Program.Matches_Length;
                  Scrutinee : constant CCL.Language.Node := CCL.Language.Analysis_Node
                    (Analysis, CCL.Language.Node_Index (Item.First));
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
                     N := CCL.Language.Analysis_Node (Analysis, CCL.Language.Node_Index (Arm));
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
                  Initializer := CCL.Language.Analysis_Node (Analysis, CCL.Language.Node_Index (Item.First));
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
               if Environment_Length > 0 then
                  for Position in reverse 0 .. Environment_Length - 1 loop
                     if CCL.Language.Names_Equal
                       (Environment (Position).Identifier, Item.Identifier)
                     then
                        Local := Environment (Position).Local;
                        Binding := Environment (Position);
                        Found := True;
                        exit;
                     end if;
                  end loop;
               end if;
               if Found then
                  if Binding.On_Stack then
                     if Stack_Base <= Binding.Stack_Level then Fail (Malformed_Typed_Tree, Index);
                     else Emit (CCL.VM.Copy_Stack, Immediate => Interfaces.Integer_64 (Stack_Base - Binding.Stack_Level - 1));
                     end if;
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
                  Initializer := CCL.Language.Analysis_Node
                    (Analysis, CCL.Language.Node_Index (Item.First));
                  Local := CCL.Ownership.Binding_Id (Next_Local);
                  Program.Local_Kinds (Local) :=
                    (case Initializer.Static_Kind is
                        when CCL.Language.Integer_Type => CCL.VM.Integer_Value,
                        when CCL.Language.Boolean_Type => CCL.VM.Boolean_Value,
                        when CCL.Types.Declared_Type => CCL.VM.Variant_Value,
                        when others => CCL.VM.Integer_Value);
                  if Initializer.Static_Kind in
                    CCL.Language.String_Type | CCL.Language.Character_Type | CCL.Language.Handler_Type | CCL.Language.Unit_Type
                  then
                     Fail (Unsupported_Form, Index, Item.Source_Position);
                  elsif Initializer.Static_Kind = CCL.Language.Invalid_Type then
                     Fail (Malformed_Typed_Tree, Index, Item.Source_Position);
                  else
                     if Initializer.Static_Kind in CCL.Types.Declared_Type then
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
               CCL.Host_Values.To_Bytecode (Item.Host_Call.Import, Bytecode_Import, Scalar_Import);
               if not Scalar_Import then
                  Fail (Unsupported_Form, Index, Item.Source_Position);
               elsif (Item.Host_Call.Import.Result = CCL.Host_Values.Integer_Value and then
                   Item.Static_Kind /= CCL.Language.Integer_Type) or else
                 (Item.Host_Call.Import.Result = CCL.Host_Values.Boolean_Value and then
                   Item.Static_Kind /= CCL.Language.Boolean_Type)
               then
                  Fail (Malformed_Typed_Tree, Index, Item.Source_Position);
               else
                  CCL.Catalog.Intern
                    (Linkage, Item.Host_Call, Import_Position, Interned);
                  if Interned = CCL.Catalog.Linkage_Full then
                     Fail (Too_Many_Imports, Index, Item.Source_Position);
                  elsif Interned = CCL.Catalog.Linkage_Added then
                     Program.Imports (Import_Position) :=
                       Bytecode_Import;
                     Program.Imports_Length := CCL.Catalog.Length (Linkage);
                  end if;

                  if Status = Compilation_Succeeded then
                     if Item.Host_Call.Parameters = 0 then
                        --  CCLB has no standalone Unit value. The catalog validates
                        --  that a zero-parameter operation uses this canonical
                        --  scalar sentinel without ownership transfer.
                        Emit (CCL.VM.Push_Integer, Immediate => 0);
                     elsif Item.First >=
                       CCL.Language.Analysis_Node_Count (Analysis)
                     then
                        Fail
                          (Malformed_Typed_Tree, Index,
                           Item.Source_Position);
                     else
                        Argument_Node := CCL.Language.Analysis_Node
                          (Analysis, CCL.Language.Node_Index (Item.First));
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
                             (Item.First, Depth + 1,
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

            when CCL.Language.Function_Definition | CCL.Language.Function_Call | CCL.Language.Handler_Form =>
               --  No CCLB call-frame representation yet. Never silently inline
               --  functions or drop their type/effect admission requirements.
               Fail (Unsupported_Form, Index, Item.Source_Position);

            when CCL.Language.String_Literal |
                 CCL.Language.String_Length_Form |
                 CCL.Language.String_Index_Form |
                 CCL.Language.String_Concat_Form |
                 CCL.Language.To_String_Form =>
               --  The source semantics are implemented and exercised by the
               --  direct interpreter. CCLB v4 has no string constant pool or
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

      Program.Types_Length := 1;
      Program.Data_Types := CCL.Language.Analysis_Types (Analysis);
      Emit_Node (CCL.Language.Analysis_Root (Analysis), 0, False);
      Emit (CCL.VM.Halt);
      Result :=
        (Status => Status,
         Diagnostic_Node => Failed_Node,
         Source_Position => Failed_Position,
         Program => Program,
         Linkage => Linkage,
         Debug => Debug);
   end Compile;
end CCL.Compiler;

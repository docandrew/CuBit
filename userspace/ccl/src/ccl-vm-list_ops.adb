with CCL.Checked_Arithmetic;
with CCL.Text_Operations;
with CCL.VM.Operations;
with CCL.VM.Text_Ops;

package body CCL.VM.List_Ops with SPARK_Mode => On is
   package L renames CCL.List_Operations;
   use type L.Operation;
   use type Runtime_Stacks.Operation_Result;
   use type List_Regions.Operation_Result;
   use type Text_Regions.Operation_Result;
   use type CCL.Execution_Budgets.Consume_Result;

   --  The interpreter keeps a split piece as a region string of at most a
   --  host text value (CCL.Host_Values.Maximum_Text_Length); a longer piece
   --  fills the list region.
   MAX_SPLIT_PIECE : constant := MAX_HOST_TEXT;

   procedure Run_List_Builtin
     (Lists  : in out List_Regions.Stack;
      Texts  : in out Text_Regions.Stack;
      Budget : in out CCL.Execution_Budgets.Budget;
      Arena  : Value_Arena;
      Types  : CCL.Types.Registry;
      Item   : CCL.List_Operations.Operation;
      List_Type : CCL.Types.Type_Reference;
      Subject, A, B : Value;
      Result : out Value;
      Status : out Execution_Status)
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
                    (if Kind = Text_Value then Text_Ops.Text_Less (Texts, X.Text, Y.Text)
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
                           Text_Ops.Equal_Texts (Texts, Member, A, Same, Compared);
                           if Compared /= Text_Regions.Operation_Ok then
                              Fail (Operations.Text_Failure (Compared));
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
                     Fail (Operations.Text_Failure (Copied));
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
                        Fail (Operations.Text_Failure (Copied));
                     else
                        Used := Used + Piece_Length;
                     end if;
                  end if;
               end loop;
               if Good then
                  Result := (Kind => Text_Value, others => <>);
                  Text_Regions.Allocate_String (Texts, Joined (1 .. Used), Result.Text, Copied);
                  if Copied /= Text_Regions.Operation_Ok then
                     Fail (Operations.Text_Failure (Copied));
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
               Separator : String (1 .. Text_Ops.MAX_PATTERN_BYTES) := [others => ' '];
               Text_Length : constant Natural range 0 .. MAX_STRING_BYTES :=
                 Natural'Min (Text_Regions.Length (Subject.Text), MAX_STRING_BYTES);
               Separator_Length : constant Natural range 0 .. Text_Ops.MAX_PATTERN_BYTES :=
                 Natural'Min (Text_Regions.Length (A.Text), Text_Ops.MAX_PATTERN_BYTES);
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
                 Text_Regions.Length (A.Text) > Text_Ops.MAX_PATTERN_BYTES
               then
                  Fail (Text_Storage_Exhausted);
               else
                  Text_Regions.Copy_To (Texts, Subject.Text, Text (1 .. Text_Length), Copied);
                  if Copied = Text_Regions.Operation_Ok then
                     Text_Regions.Copy_To (Texts, A.Text, Separator (1 .. Separator_Length), Copied);
                  end if;
                  if Copied /= Text_Regions.Operation_Ok then
                     Fail (Operations.Text_Failure (Copied));
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
                        Fail (Operations.Text_Failure (Copied));
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

   procedure Run_List (Item : Validated_Program; State : in out Machine_State; Regs : in out Registers) is
      Ins : constant Instruction := Item.Content.Code (Regs.PC);
      Left_Value, Right_Value, Joined : Value;
      Stack_Result : Runtime_Stacks.Operation_Result := Runtime_Stacks.Stack_Ok;
      List_Status : List_Regions.Operation_Result;
   begin
      case Ins.Op is
         when New_List =>
            Joined := (Kind => List_Value, Data_Type => Ins.Data_Type, others => <>);
            if Ins.Immediate not in 0 .. MAX_LIST_ELEMENTS then
               Trap (Regs, State, Invalid_Bytecode);
            else
               List_Regions.Reserve (State.Lists, Natural (Ins.Immediate), Joined.Items, List_Status);
               if List_Status /= List_Regions.Operation_Ok then
                  Trap (Regs, State, List_Storage_Exhausted);
               else
                  Push_Next (Item, Regs, State, Joined);
               end if;
            end if;

         when Fill_List =>
            Runtime_Stacks.Pop (Regs.Stack, Right_Value, Stack_Result);
            if Stack_Result = Runtime_Stacks.Stack_Ok then
               Runtime_Stacks.Peek_Top (Regs.Stack, Left_Value, Stack_Result);
            end if;
            if Stack_Result /= Runtime_Stacks.Stack_Ok or else
              Left_Value.Kind /= List_Value or else
              Left_Value.Data_Type /= Ins.Data_Type or else
              Right_Value.Kind /= Element_Kind (Item.Content.Data_Types, Left_Value.Data_Type) or else
              Right_Value.Data_Type /= Element_Data_Type (Item.Content.Data_Types, Left_Value.Data_Type) or else
              Ins.Immediate not in 1 .. Integer_64 (List_Regions.Length (Left_Value.Items)) or else
              Program_Length (Regs.PC) + 1 >= Item.Content.Length
            then
               Trap (Regs, State, Invalid_Bytecode);
            else
               List_Regions.Write
                 (State.Lists, Left_Value.Items, List_Regions.Array_Index (Ins.Immediate),
                  To_Element (Right_Value), List_Status);
               if List_Status /= List_Regions.Operation_Ok then
                  Trap (Regs, State, Invalid_Bytecode);
               else
                  Regs.PC := Regs.PC + 1;
               end if;
            end if;

         when Length_List =>
            Runtime_Stacks.Pop (Regs.Stack, Left_Value, Stack_Result);
            if Stack_Result /= Runtime_Stacks.Stack_Ok or else Left_Value.Kind /= List_Value then
               Trap (Regs, State, Invalid_Bytecode);
            else
               Push_Next (Item, Regs, State, Integer_Constant (Integer_64 (List_Regions.Length (Left_Value.Items))));
            end if;

         when List_At =>
            Runtime_Stacks.Pop (Regs.Stack, Right_Value, Stack_Result);
            if Stack_Result = Runtime_Stacks.Stack_Ok then
               Runtime_Stacks.Pop (Regs.Stack, Left_Value, Stack_Result);
            end if;
            if Stack_Result /= Runtime_Stacks.Stack_Ok or else Right_Value.Kind /= Integer_Value or else
              Left_Value.Kind /= List_Value or else
              Left_Value.Data_Type /= Ins.Data_Type
            then
               Trap (Regs, State, Invalid_Bytecode);
            elsif Right_Value.Integer < 1 or else
              Right_Value.Integer > Integer_64 (List_Regions.Length (Left_Value.Items))
            then
               Trap (Regs, State, Index_Out_Of_Range);
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
                     Trap (Regs, State, Invalid_Bytecode);
                  else
                     Push_Next (Item, Regs, State, Joined);
                  end if;
               end;
            end if;

         when List_Builtin =>
            declare
               Operation : L.Operation;
               Known : Boolean;
               Subject, A, B, Answer : Value;
               Outcome : Execution_Status;

               --  Pop an operand of Kind (and list type, for lists), or trap.
               procedure Take (Kind : Value_Kind; Operand : out Value) is
               begin
                  Operand := (others => <>);
                  if Regs.Done then return; end if;
                  Runtime_Stacks.Pop (Regs.Stack, Operand, Stack_Result);
                  if Stack_Result /= Runtime_Stacks.Stack_Ok or else Operand.Kind /= Kind or else
                    (Kind = List_Value and then Operand.Data_Type /= Ins.Data_Type)
                  then
                     Trap (Regs, State, Invalid_Bytecode);
                  end if;
               end Take;
            begin
               Operations.Find_List_Operation (Ins.Immediate, Operation, Known);
               if not Known or else
                 not Operations.List_Builtin_Applies (Item.Content.Data_Types, Operation, Ins.Data_Type)
               then
                  Trap (Regs, State, Invalid_Bytecode);
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
                  if not Regs.Done then
                     Run_List_Builtin
                       (State.Lists, State.Text, State.Execution_Budget, State.Arena, Item.Content.Data_Types,
                        Operation, Ins.Data_Type, Subject, A, B, Answer, Outcome);
                     if Outcome /= Completed then
                        Trap (Regs, State, Outcome);
                     else
                        Push_Next (Item, Regs, State, Answer);
                     end if;
                  end if;
               end if;
            end;

         when Make_Node =>
            declare
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
                  Runtime_Stacks.Pop (Regs.Stack, Parts (P), Stack_Result);
                  if Stack_Result /= Runtime_Stacks.Stack_Ok then
                     Good := False;
                     exit;
                  end if;
               end loop;
               if not Good then
                  Trap (Regs, State, Invalid_Bytecode);
               elsif (D.Form = CCL.Types.Product or else Count > 0) and then
                 (State.Arena.Nodes_Used = Node_Count'Last or else
                  Slot_Count'Last - State.Arena.Slots_Used < Slot_Count (Count))
               then
                  Trap (Regs, State, Object_Storage_Exhausted);
               else
                  Allocate_Node (State.Arena, Item.Content.Data_Types, Ins.Data_Type, Ins.Alternative,
                                 Parts, Count, Built, Good);
                  if Good then
                     Push_Next (Item, Regs, State, Built);
                  else
                     Trap (Regs, State, Invalid_Bytecode);
                  end if;
               end if;
            end;

         when Check_Range =>
            Runtime_Stacks.Pop (Regs.Stack, Right_Value, Stack_Result);
            if Stack_Result /= Runtime_Stacks.Stack_Ok or else Right_Value.Kind /= Integer_Value or else
              not CCL.Types.Is_Range (Item.Content.Data_Types, Ins.Data_Type)
            then
               Trap (Regs, State, Invalid_Bytecode);
            elsif Right_Value.Integer < CCL.Types.Low_Of (Item.Content.Data_Types, Ins.Data_Type) or else
              Right_Value.Integer > CCL.Types.High_Of (Item.Content.Data_Types, Ins.Data_Type)
            then
               Trap (Regs, State, Range_Error);
            else
               Push_Next (Item, Regs, State, Right_Value);
            end if;

         when others => null;
      end case;
   end Run_List;
end CCL.VM.List_Ops;

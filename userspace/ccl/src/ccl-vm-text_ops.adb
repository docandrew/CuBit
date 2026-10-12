with CCL.VM.Operations;

package body CCL.VM.Text_Ops with SPARK_Mode => On is
   package T renames CCL.Text_Operations;
   use type T.Operation;
   use type T.Outcome;
   use type T.Operand_Kind;
   use type Text_Regions.Operation_Result;
   use type Runtime_Stacks.Operation_Result;

   procedure Push_Constant
     (Content : Program; Index : Constant_Index;
      Region  : in out Text_Regions.Stack; Item : out Value;
      Status  : out Text_Regions.Operation_Result)
   is
      C : constant Text_Constant := Content.Constants (Index);
   begin
      Item := (Kind => Text_Value, others => <>);
      Status := Text_Regions.Invalid_Bounds;
      if not In_Pool (C) then
         return;
      end if;
      Text_Regions.Allocate_String
        (Region, Content.Constant_Text (Natural (C.First) .. Natural (C.First) + Natural (C.Length) - 1),
         Item.Text, Status);
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

   procedure Run_Text_Builtin
     (Region   : in out Text_Regions.Stack;
      Item     : CCL.Text_Operations.Operation;
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
                    else Operations.Text_Failure (Region_Status));
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

   procedure Run_Text (Item : Validated_Program; State : in out Machine_State; Regs : in out Registers) is
      Ins : constant Instruction := Item.Content.Code (Regs.PC);
      Left_Value, Right_Value, Joined : Value;
      Popped : Runtime_Stacks.Operation_Result;
      Text_Status : Text_Regions.Operation_Result;
      Same_Text : Boolean;
      Element : Character;
   begin
      case Ins.Op is
         when Push_Text =>
            if Ins.Immediate not in 0 .. Integer_64 (Item.Content.Constants_Length) - 1 then
               Trap (Regs, State, Invalid_Bytecode);
            else
               Push_Constant (Item.Content, Operations.Constant_Operand (Ins.Immediate), State.Text, Left_Value, Text_Status);
               if Text_Status /= Text_Regions.Operation_Ok then
                  Trap (Regs, State, Operations.Text_Failure (Text_Status));
               else
                  Push_Next (Item, Regs, State, Left_Value);
               end if;
            end if;

         when Concat_Text =>
            Pop_Text (Regs, State, Right_Value);
            if not Regs.Done then Pop_Text (Regs, State, Left_Value); end if;
            if not Regs.Done then
               Concat_Texts (State.Text, Left_Value, Right_Value, Joined, Text_Status);
               if Text_Status /= Text_Regions.Operation_Ok then
                  Trap (Regs, State, Operations.Text_Failure (Text_Status));
               else
                  Push_Next (Item, Regs, State, Joined);
               end if;
            end if;

         when Text_Builtin =>
            declare
               Operation : T.Operation;
               Known : Boolean;
               Operands : Text_Operands := [others => (others => <>)];
               Subject, Answer : Value;
               Outcome : Execution_Status;
            begin
               Operations.Find_Operation (Ins.Immediate, Operation, Known);
               if not Known then
                  Trap (Regs, State, Invalid_Bytecode);
               else
                  Pop_Text (Regs, State, Subject);
                  for I in reverse 1 .. T.Signature_Of (Operation).Count loop
                     exit when Regs.Done;
                     Runtime_Stacks.Pop (Regs.Stack, Operands (I), Popped);
                     if Popped /= Runtime_Stacks.Stack_Ok or else
                       Operands (I).Kind /= Operations.Kind_Of (T.Signature_Of (Operation).Operands (I))
                     then
                        Trap (Regs, State, Invalid_Bytecode);
                     end if;
                  end loop;
                  if not Regs.Done then
                     Run_Text_Builtin (State.Text, Operation, Operands, Subject, Answer, Outcome);
                     if Outcome /= Completed then
                        Trap (Regs, State, Outcome);
                     else
                        Push_Next (Item, Regs, State, Answer);
                     end if;
                  end if;
               end if;
            end;

         when Length_Text =>
            Pop_Text (Regs, State, Left_Value);
            if not Regs.Done then
               Push_Next (Item, Regs, State, Integer_Constant
                 (Integer_64 (Text_Regions.Length (Left_Value.Text))));
            end if;

         when Equal_Text =>
            Pop_Text (Regs, State, Right_Value);
            if not Regs.Done then Pop_Text (Regs, State, Left_Value); end if;
            if not Regs.Done then
               Equal_Texts (State.Text, Left_Value, Right_Value, Same_Text, Text_Status);
               if Text_Status /= Text_Regions.Operation_Ok then
                  Trap (Regs, State, Operations.Text_Failure (Text_Status));
               else
                  Push_Next (Item, Regs, State, Boolean_Constant (Same_Text));
               end if;
            end if;

         when Text_At =>
            Runtime_Stacks.Pop (Regs.Stack, Right_Value, Popped);
            if Popped /= Runtime_Stacks.Stack_Ok or else Right_Value.Kind /= Integer_Value then
               Trap (Regs, State, Invalid_Bytecode);
            end if;
            if not Regs.Done then Pop_Text (Regs, State, Left_Value); end if;
            if not Regs.Done then
               if Right_Value.Integer < 1 or else
                 Right_Value.Integer > Integer_64 (Text_Regions.Length (Left_Value.Text))
               then
                  Trap (Regs, State, Index_Out_Of_Range);
               else
                  Text_Regions.Read
                    (State.Text, Left_Value.Text, Text_Regions.String_Index (Right_Value.Integer),
                     Element, Text_Status);
                  if Text_Status /= Text_Regions.Operation_Ok then
                     Trap (Regs, State, Operations.Text_Failure (Text_Status));
                  else
                     Push_Next (Item, Regs, State, Character_Constant (Element));
                  end if;
               end if;
            end if;

         when Integer_To_Text | Variant_To_Text =>
            Runtime_Stacks.Pop (Regs.Stack, Left_Value, Popped);
            if Popped /= Runtime_Stacks.Stack_Ok or else
              Left_Value.Kind /= (if Ins.Op = Integer_To_Text then Integer_Value else Variant_Value) or else
              (Left_Value.Kind = Variant_Value and then
               (Left_Value.Data_Type /= Ins.Data_Type or else
                not CCL.Types.Is_Enumeration (Item.Content.Data_Types, Left_Value.Data_Type) or else
                Left_Value.Alternative > CCL.Types.Describe (Item.Content.Data_Types, Left_Value.Data_Type).Count))
            then
               Trap (Regs, State, Invalid_Bytecode);
            else
               Joined := (Kind => Text_Value, others => <>);
               Text_Regions.Allocate_String
                 (State.Text,
                  (if Left_Value.Kind = Integer_Value then T.Decimal_Image (Left_Value.Integer)
                   else CCL.Types.Image (CCL.Types.Describe (Item.Content.Data_Types, Left_Value.Data_Type)
                     .Parts (Left_Value.Alternative).Identifier)),
                  Joined.Text, Text_Status);
               if Text_Status /= Text_Regions.Operation_Ok then
                  Trap (Regs, State, Operations.Text_Failure (Text_Status));
               else
                  Push_Next (Item, Regs, State, Joined);
               end if;
            end if;

         when others => null;
      end case;
   end Run_Text;
end CCL.VM.Text_Ops;

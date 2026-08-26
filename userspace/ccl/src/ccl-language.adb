with Interfaces; use Interfaces;

package body CCL.Language with
   SPARK_Mode => On
is
   subtype Node_Index is Natural range 0 .. MAX_AST_NODES - 1;
   NO_NODE : constant Natural := MAX_AST_NODES;

   type Name_Buffer is array (Positive range 1 .. MAX_NAME_LENGTH) of Character;

   type Name is record
      Length : Natural range 0 .. MAX_NAME_LENGTH := 0;
      Data   : Name_Buffer := [others => ' '];
   end record;

   type Node_Kind is
     (Invalid_Node,
      Integer_Literal,
      Boolean_Literal,
      Name_Reference,
      Add_Form,
      Equal_Form,
      Not_Form,
      If_Form,
      Let_Form);

   type Node is record
      Kind          : Node_Kind := Invalid_Node;
      Integer_Value : Integer_64 := 0;
      Boolean_Value : Boolean := False;
      Identifier    : Name;
      First         : Natural range 0 .. NO_NODE := NO_NODE;
      Second        : Natural range 0 .. NO_NODE := NO_NODE;
      Third         : Natural range 0 .. NO_NODE := NO_NODE;
   end record;

   type Node_Array is array (Node_Index) of Node;

   type Syntax_Tree is record
      Length : Natural range 0 .. MAX_AST_NODES := 0;
      Nodes  : Node_Array := [others => (others => <>)];
   end record;

   type Static_Type is (Invalid_Type, Integer_Type, Boolean_Type);

   type Type_Binding is record
      Identifier : Name;
      Kind       : Static_Type := Invalid_Type;
   end record;

   type Type_Environment is
     array (Natural range 0 .. MAX_BINDINGS - 1) of Type_Binding;

   type Value_Binding is record
      Identifier : Name;
      Item       : CCL.VM.Value := (others => <>);
   end record;

   type Value_Environment is
     array (Natural range 0 .. MAX_BINDINGS - 1) of Value_Binding;

   function Is_Name_Character (Item : Character) return Boolean is
     ((Item >= 'a' and then Item <= 'z') or else
      (Item >= 'A' and then Item <= 'Z') or else
      (Item >= '0' and then Item <= '9') or else
      Item = '-' or else Item = '_' or else Item = '.' or else Item = '?' or else
      Item = '+' or else Item = '=');

   function Names_Equal (Left, Right : Name) return Boolean is
   begin
      if Left.Length /= Right.Length then
         return False;
      end if;

      if Left.Length > 0 then
         for Position in 1 .. Left.Length loop
            if Left.Data (Position) /= Right.Data (Position) then
               return False;
            end if;
         end loop;
      end if;
      return True;
   end Names_Equal;

   function Name_Is (Item : Name; Text : String) return Boolean is
   begin
      if Item.Length /= Text'Length or else Text'Length > MAX_NAME_LENGTH then
         return False;
      end if;

      if Item.Length > 0 then
         for Position in 1 .. Item.Length loop
            if Item.Data (Position) /= Text (Text'First + Position - 1) then
               return False;
            end if;
         end loop;
      end if;
      return True;
   end Name_Is;

   function Addition_Overflows (Left, Right : Integer_64) return Boolean is
     (if Right > 0 then
         Left > Integer_64'Last - Right
      elsif Right < 0 then
         Left < Integer_64'First - Right
      else False);

   procedure Interpret
     (Source : String;
      Fuel   : Natural;
      Result : out Interpretation_Result)
   is
      Tree       : Syntax_Tree;
      Cursor     : Natural := 0;
      Root       : Natural range 0 .. NO_NODE := NO_NODE;
      Diagnostic : Diagnostic_Code := No_Diagnostic;

      procedure Skip_Whitespace is
      begin
         while Cursor < Source'Length loop
            exit when Source (Source'First + Cursor) /= ' ' and then
              Source (Source'First + Cursor) /= ASCII.HT and then
              Source (Source'First + Cursor) /= ASCII.LF and then
              Source (Source'First + Cursor) /= ASCII.CR;
            Cursor := Cursor + 1;
         end loop;
      end Skip_Whitespace;

      procedure Read_Name (Item : out Name; Ok : out Boolean) is
      begin
         Item := (others => <>);
         Ok := False;
         Skip_Whitespace;
         while Cursor < Source'Length and then
           Is_Name_Character (Source (Source'First + Cursor))
         loop
            if Item.Length = MAX_NAME_LENGTH then
               Diagnostic := Expected_Name;
               return;
            end if;
            Item.Length := Item.Length + 1;
            Item.Data (Item.Length) := Source (Source'First + Cursor);
            Cursor := Cursor + 1;
         end loop;
         Ok := Item.Length > 0;
         if not Ok then
            Diagnostic := Expected_Name;
         end if;
      end Read_Name;

      procedure Add_Node
        (Item  : Node;
         Index : out Natural)
      is
      begin
         if Tree.Length = MAX_AST_NODES then
            Diagnostic := AST_Full;
            Index := NO_NODE;
         else
            Index := Tree.Length;
            Tree.Nodes (Node_Index (Tree.Length)) := Item;
            Tree.Length := Tree.Length + 1;
         end if;
      end Add_Node;

      procedure Expect (Item : Character; Ok : out Boolean) is
      begin
         Skip_Whitespace;
         Ok := Cursor < Source'Length and then
           Source (Source'First + Cursor) = Item;
         if Ok then
            Cursor := Cursor + 1;
         else
            Diagnostic := Expected_Close;
         end if;
      end Expect;

      procedure Parse_Expression
        (Depth : Natural;
         Index : out Natural);

      procedure Parse_Integer (Index : out Natural) is
         Negative  : Boolean := False;
         Magnitude : Unsigned_64 := 0;
         Digit     : Unsigned_64;
         Started   : Boolean := False;
         Limit     : Unsigned_64 := Unsigned_64 (Integer_64'Last);
         Item      : Integer_64;
      begin
         if Cursor < Source'Length and then
           Source (Source'First + Cursor) = '-'
         then
            Negative := True;
            Limit := Limit + 1;
            Cursor := Cursor + 1;
         end if;

         while Cursor < Source'Length and then
           Source (Source'First + Cursor) >= '0' and then
           Source (Source'First + Cursor) <= '9'
         loop
            Started := True;
            Digit := Unsigned_64
              (Character'Pos (Source (Source'First + Cursor)) -
               Character'Pos ('0'));
            if Magnitude > (Limit - Digit) / 10 then
               Diagnostic := Invalid_Integer;
               Index := NO_NODE;
               return;
            end if;
            Magnitude := Magnitude * 10 + Digit;
            Cursor := Cursor + 1;
         end loop;

         if not Started then
            Diagnostic := Invalid_Integer;
            Index := NO_NODE;
            return;
         elsif Negative and then Magnitude = Limit then
            Item := Integer_64'First;
         elsif Negative then
            Item := -Integer_64 (Magnitude);
         else
            Item := Integer_64 (Magnitude);
         end if;

         Add_Node
           ((Kind => Integer_Literal, Integer_Value => Item, others => <>),
            Index);
      end Parse_Integer;

      procedure Parse_List (Depth : Natural; Index : out Natural) is
         Operator_Name : Name;
         Binding_Name  : Name;
         Ok            : Boolean;
         A             : Natural range 0 .. NO_NODE := NO_NODE;
         B             : Natural range 0 .. NO_NODE := NO_NODE;
         C             : Natural range 0 .. NO_NODE := NO_NODE;
      begin
         Read_Name (Operator_Name, Ok);
         if not Ok then
            Index := NO_NODE;
            return;
         end if;

         if Name_Is (Operator_Name, "+") or else
           Name_Is (Operator_Name, "add")
         then
            Parse_Expression (Depth + 1, A);
            Parse_Expression (Depth + 1, B);
            Expect (')', Ok);
            if Diagnostic = No_Diagnostic and then Ok then
               Add_Node ((Kind => Add_Form, First => A, Second => B,
                          others => <>), Index);
            else
               Index := NO_NODE;
            end if;
         elsif Name_Is (Operator_Name, "=") or else
           Name_Is (Operator_Name, "equal")
         then
            Parse_Expression (Depth + 1, A);
            Parse_Expression (Depth + 1, B);
            Expect (')', Ok);
            if Diagnostic = No_Diagnostic and then Ok then
               Add_Node ((Kind => Equal_Form, First => A, Second => B,
                          others => <>), Index);
            else
               Index := NO_NODE;
            end if;
         elsif Name_Is (Operator_Name, "not") then
            Parse_Expression (Depth + 1, A);
            Expect (')', Ok);
            if Diagnostic = No_Diagnostic and then Ok then
               Add_Node ((Kind => Not_Form, First => A, others => <>), Index);
            else
               Index := NO_NODE;
            end if;
         elsif Name_Is (Operator_Name, "if") then
            Parse_Expression (Depth + 1, A);
            Parse_Expression (Depth + 1, B);
            Parse_Expression (Depth + 1, C);
            Expect (')', Ok);
            if Diagnostic = No_Diagnostic and then Ok then
               Add_Node ((Kind => If_Form, First => A, Second => B, Third => C,
                          others => <>), Index);
            else
               Index := NO_NODE;
            end if;
         elsif Name_Is (Operator_Name, "let") then
            Expect ('(', Ok);
            if Diagnostic = No_Diagnostic then
               Expect ('(', Ok);
            end if;
            if Diagnostic = No_Diagnostic then
               Read_Name (Binding_Name, Ok);
            end if;
            if Diagnostic = No_Diagnostic then
               Parse_Expression (Depth + 1, A);
            end if;
            if Diagnostic = No_Diagnostic then
               Expect (')', Ok);
            end if;
            if Diagnostic = No_Diagnostic then
               Expect (')', Ok);
            end if;
            if Diagnostic = No_Diagnostic then
               Parse_Expression (Depth + 1, B);
            end if;
            if Diagnostic = No_Diagnostic then
               Expect (')', Ok);
            end if;
            if Diagnostic = No_Diagnostic then
               Add_Node ((Kind => Let_Form, Identifier => Binding_Name,
                          First => A, Second => B, others => <>), Index);
            else
               Index := NO_NODE;
            end if;
         else
            Diagnostic := Unknown_Form;
            Index := NO_NODE;
         end if;
      end Parse_List;

      procedure Parse_Expression
        (Depth : Natural;
         Index : out Natural)
      is
         Item : Name;
         Ok   : Boolean;
      begin
         Index := NO_NODE;
         if Diagnostic /= No_Diagnostic then
            return;
         elsif Depth >= MAX_NESTING then
            Diagnostic := Nesting_Too_Deep;
            return;
         end if;

         Skip_Whitespace;
         if Cursor >= Source'Length then
            Diagnostic := Unexpected_End;
         elsif Source (Source'First + Cursor) = '(' then
            Cursor := Cursor + 1;
            Parse_List (Depth, Index);
         elsif Source (Source'First + Cursor) = '-' or else
           (Source (Source'First + Cursor) >= '0' and then
            Source (Source'First + Cursor) <= '9')
         then
            Parse_Integer (Index);
         elsif Is_Name_Character (Source (Source'First + Cursor)) then
            Read_Name (Item, Ok);
            if Ok and then Name_Is (Item, "true") then
               Add_Node ((Kind => Boolean_Literal, Boolean_Value => True,
                          others => <>), Index);
            elsif Ok and then Name_Is (Item, "false") then
               Add_Node ((Kind => Boolean_Literal, Boolean_Value => False,
                          others => <>), Index);
            elsif Ok then
               Add_Node ((Kind => Name_Reference, Identifier => Item,
                          others => <>), Index);
            end if;
         else
            Diagnostic := Unexpected_Token;
         end if;
      end Parse_Expression;

      Type_Env : Type_Environment := [others => (others => <>)];
      Type_Env_Length : Natural range 0 .. MAX_BINDINGS := 0;

      procedure Check_Node
        (Index : Natural;
         Depth : Natural;
         Kind  : out Static_Type)
      is
         Left_Type  : Static_Type := Invalid_Type;
         Right_Type : Static_Type := Invalid_Type;
         Third_Type : Static_Type := Invalid_Type;
         Found      : Boolean := False;
      begin
         Kind := Invalid_Type;
         if Diagnostic /= No_Diagnostic then
            return;
         elsif Depth >= MAX_NESTING or else Index >= Tree.Length then
            Diagnostic := Nesting_Too_Deep;
            return;
         end if;

         case Tree.Nodes (Node_Index (Index)).Kind is
            when Integer_Literal => Kind := Integer_Type;
            when Boolean_Literal => Kind := Boolean_Type;
            when Name_Reference =>
               if Type_Env_Length > 0 then
                  for Position in reverse 0 .. Type_Env_Length - 1 loop
                     if Names_Equal
                       (Type_Env (Position).Identifier,
                        Tree.Nodes (Node_Index (Index)).Identifier)
                     then
                        Kind := Type_Env (Position).Kind;
                        Found := True;
                        exit;
                     end if;
                  end loop;
               end if;
               if not Found then
                  Diagnostic := Unknown_Name;
               end if;
            when Add_Form | Equal_Form =>
               Check_Node (Tree.Nodes (Node_Index (Index)).First,
                           Depth + 1, Left_Type);
               Check_Node (Tree.Nodes (Node_Index (Index)).Second,
                           Depth + 1, Right_Type);
               if Diagnostic = No_Diagnostic and then
                 (Left_Type /= Integer_Type or else Right_Type /= Integer_Type)
               then
                  Diagnostic := Expected_Integer;
               elsif Tree.Nodes (Node_Index (Index)).Kind = Add_Form then
                  Kind := Integer_Type;
               else
                  Kind := Boolean_Type;
               end if;
            when Not_Form =>
               Check_Node (Tree.Nodes (Node_Index (Index)).First,
                           Depth + 1, Left_Type);
               if Diagnostic = No_Diagnostic and then Left_Type /= Boolean_Type
               then
                  Diagnostic := Expected_Boolean;
               else
                  Kind := Boolean_Type;
               end if;
            when If_Form =>
               Check_Node (Tree.Nodes (Node_Index (Index)).First,
                           Depth + 1, Left_Type);
               Check_Node (Tree.Nodes (Node_Index (Index)).Second,
                           Depth + 1, Right_Type);
               Check_Node (Tree.Nodes (Node_Index (Index)).Third,
                           Depth + 1, Third_Type);
               if Diagnostic = No_Diagnostic and then Left_Type /= Boolean_Type
               then
                  Diagnostic := Expected_Boolean;
               elsif Diagnostic = No_Diagnostic and then
                 Right_Type /= Third_Type
               then
                  Diagnostic := Branch_Type_Mismatch;
               else
                  Kind := Right_Type;
               end if;
            when Let_Form =>
               Check_Node (Tree.Nodes (Node_Index (Index)).First,
                           Depth + 1, Left_Type);
               if Diagnostic = No_Diagnostic and then
                 Type_Env_Length = MAX_BINDINGS
               then
                  Diagnostic := Too_Many_Bindings;
               elsif Diagnostic = No_Diagnostic then
                  Type_Env (Type_Env_Length) :=
                    (Identifier => Tree.Nodes (Node_Index (Index)).Identifier,
                     Kind => Left_Type);
                  Type_Env_Length := Type_Env_Length + 1;
                  Check_Node (Tree.Nodes (Node_Index (Index)).Second,
                              Depth + 1, Kind);
                  Type_Env_Length := Type_Env_Length - 1;
               end if;
            when Invalid_Node => Diagnostic := Unexpected_Token;
         end case;
      end Check_Node;

      Value_Env : Value_Environment := [others => (others => <>)];
      Value_Env_Length : Natural range 0 .. MAX_BINDINGS := 0;
      Fuel_Left : Natural := Fuel;
      Eval_Status : Interpretation_Status := Succeeded;

      procedure Evaluate_Node
        (Index : Natural;
         Depth : Natural;
         Item  : out CCL.VM.Value;
         Ok    : out Boolean)
      is
         Left  : CCL.VM.Value := (others => <>);
         Right : CCL.VM.Value := (others => <>);
         Good  : Boolean;
         Found : Boolean := False;
      begin
         Item := (others => <>);
         Ok := False;
         if Eval_Status /= Succeeded then
            return;
         elsif Fuel_Left = 0 then
            Eval_Status := Evaluation_Fuel_Exhausted;
            return;
         elsif Depth >= MAX_NESTING or else Index >= Tree.Length then
            Eval_Status := Parse_Failed;
            return;
         end if;
         Fuel_Left := Fuel_Left - 1;

         case Tree.Nodes (Node_Index (Index)).Kind is
            when Integer_Literal =>
               Item := CCL.VM.Integer_Constant
                 (Tree.Nodes (Node_Index (Index)).Integer_Value);
               Ok := True;
            when Boolean_Literal =>
               Item := CCL.VM.Boolean_Constant
                 (Tree.Nodes (Node_Index (Index)).Boolean_Value);
               Ok := True;
            when Name_Reference =>
               if Value_Env_Length > 0 then
                  for Position in reverse 0 .. Value_Env_Length - 1 loop
                     if Names_Equal
                       (Value_Env (Position).Identifier,
                        Tree.Nodes (Node_Index (Index)).Identifier)
                     then
                        Item := Value_Env (Position).Item;
                        Found := True;
                        exit;
                     end if;
                  end loop;
               end if;
               Ok := Found;
            when Add_Form =>
               Evaluate_Node (Tree.Nodes (Node_Index (Index)).First,
                              Depth + 1, Left, Good);
               if Good then
                  Evaluate_Node (Tree.Nodes (Node_Index (Index)).Second,
                                 Depth + 1, Right, Good);
               end if;
               if Good and then Addition_Overflows
                 (Left.Integer, Right.Integer)
               then
                  Eval_Status := Evaluation_Overflow;
                  Good := False;
               elsif Good then
                  Item := CCL.VM.Integer_Constant
                    (Left.Integer + Right.Integer);
               end if;
               Ok := Good;
            when Equal_Form =>
               Evaluate_Node (Tree.Nodes (Node_Index (Index)).First,
                              Depth + 1, Left, Good);
               if Good then
                  Evaluate_Node (Tree.Nodes (Node_Index (Index)).Second,
                                 Depth + 1, Right, Good);
               end if;
               if Good then
                  Item := CCL.VM.Boolean_Constant
                    (Left.Integer = Right.Integer);
               end if;
               Ok := Good;
            when Not_Form =>
               Evaluate_Node (Tree.Nodes (Node_Index (Index)).First,
                              Depth + 1, Left, Good);
               if Good then
                  Item := CCL.VM.Boolean_Constant (not Left.Boolean);
               end if;
               Ok := Good;
            when If_Form =>
               Evaluate_Node (Tree.Nodes (Node_Index (Index)).First,
                              Depth + 1, Left, Good);
               if Good and then Left.Boolean then
                  Evaluate_Node (Tree.Nodes (Node_Index (Index)).Second,
                                 Depth + 1, Item, Good);
               elsif Good then
                  Evaluate_Node (Tree.Nodes (Node_Index (Index)).Third,
                                 Depth + 1, Item, Good);
               end if;
               Ok := Good;
            when Let_Form =>
               Evaluate_Node (Tree.Nodes (Node_Index (Index)).First,
                              Depth + 1, Left, Good);
               if Good and then Value_Env_Length < MAX_BINDINGS then
                  Value_Env (Value_Env_Length) :=
                    (Identifier => Tree.Nodes (Node_Index (Index)).Identifier,
                     Item => Left);
                  Value_Env_Length := Value_Env_Length + 1;
                  Evaluate_Node (Tree.Nodes (Node_Index (Index)).Second,
                                 Depth + 1, Item, Good);
                  Value_Env_Length := Value_Env_Length - 1;
               else
                  Good := False;
               end if;
               Ok := Good;
            when Invalid_Node => Ok := False;
         end case;
      end Evaluate_Node;

      Root_Type : Static_Type;
      Value     : CCL.VM.Value;
      Ok        : Boolean;
   begin
      Result :=
        (Status => Parse_Failed, Diagnostic => No_Diagnostic,
         Has_Value => False, Result_Value => (others => <>),
         Fuel_Remaining => Fuel);

      if Source'Length > MAX_SOURCE_LENGTH then
         Result.Diagnostic := Source_Too_Long;
         return;
      end if;

      Parse_Expression (0, Root);
      Skip_Whitespace;
      if Diagnostic = No_Diagnostic and then Cursor /= Source'Length then
         Diagnostic := Trailing_Input;
      end if;
      if Diagnostic /= No_Diagnostic then
         Result.Diagnostic := Diagnostic;
         return;
      end if;

      Check_Node (Root, 0, Root_Type);
      if Diagnostic /= No_Diagnostic or else Root_Type = Invalid_Type then
         Result.Status := Type_Check_Failed;
         Result.Diagnostic := Diagnostic;
         return;
      end if;

      Evaluate_Node (Root, 0, Value, Ok);
      Result.Status := Eval_Status;
      Result.Fuel_Remaining := Fuel_Left;
      if Ok and then Eval_Status = Succeeded then
         Result.Has_Value := True;
         Result.Result_Value := Value;
      end if;
   end Interpret;
end CCL.Language;

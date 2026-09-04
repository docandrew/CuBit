with Interfaces; use Interfaces;
with CCL.Checked_Arithmetic;
with CCL.Secondary_Stacks;

package body CCL.Language with
   SPARK_Mode => On
is
   use type CCL.VM.Value_Kind;
   use type CCL.Checked_Arithmetic.Arithmetic_Error;

   package Text_Regions is new CCL.Secondary_Stacks
     (Capacity => MAX_TEXT_BYTES * 4,
      Max_Values => MAX_AST_NODES);
   use type Text_Regions.Operation_Result;

   type Runtime_Value is record
      Kind      : Static_Type := Invalid_Type;
      Scalar    : CCL.VM.Value := (others => <>);
      Text      : Text_Regions.String_Value;
      Character_Item : Character := Character'Val (0);
   end record;

   function Analysis_Status_Of
     (Result : Analysis_Result) return Analysis_Status is (Result.Status);

   function Analysis_Diagnostic
     (Result : Analysis_Result) return Diagnostic_Code is (Result.Diagnostic);

   function Analysis_Diagnostic_Position
     (Result : Analysis_Result) return Natural is
     (Result.Diagnostic_Position);

   function Analysis_Node_Count
     (Result : Analysis_Result) return Node_Count is (Result.Tree.Length);

   function Analysis_Root
     (Result : Analysis_Result) return Node_Reference is (Result.Tree.Root);

   function Analysis_Node
     (Result : Analysis_Result;
      Index  : Node_Index) return Node is (Result.Tree.Nodes (Index));

   type Type_Binding is record
      Identifier : Name;
      Kind       : Static_Type := Invalid_Type;
   end record;

   type Type_Environment is
     array (Natural range 0 .. MAX_BINDINGS - 1) of Type_Binding;

   type Value_Binding is record
      Identifier : Name;
      Item       : Runtime_Value := (others => <>);
   end record;

   type Value_Environment is
     array (Natural range 0 .. MAX_BINDINGS - 1) of Value_Binding;

   function Is_Name_Character (Item : Character) return Boolean is
     ((Item >= 'a' and then Item <= 'z') or else
      (Item >= 'A' and then Item <= 'Z') or else
      (Item >= '0' and then Item <= '9') or else
      Item = '-' or else Item = '_' or else Item = '.' or else Item = '?' or else
      Item = '+' or else Item = '=' or else Item = '*' or else
      Item = '/' or else Item = '%');

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
      return Item.Data (1 .. Item.Length) = Text;
   end Name_Is;

   function Addition_Overflows (Left, Right : Integer_64) return Boolean is
     (if Right > 0 then
         Left > Integer_64'Last - Right
      elsif Right < 0 then
         Left < Integer_64'First - Right
      else False);

   procedure Process_Source
     (Source : String;
      Fuel   : Natural;
      Visible_Interfaces : CCL.Catalog.Interface_Catalog;
      Analyze_Input : Boolean;
      Evaluate : Boolean;
      Result : out Interpretation_Result;
      Tree   : in out Syntax_Tree)
   with
      Post => Result.Fuel_Remaining <= Fuel
   is
      Cursor     : Natural := 0;
      Root       : Node_Reference := NO_NODE;
      Diagnostic : Diagnostic_Code := No_Diagnostic;
      Diagnostic_Position : Natural range 0 .. MAX_SOURCE_LENGTH + 1 := 0;
      subtype Diagnostic_Source_Position is
        Positive range 1 .. MAX_SOURCE_LENGTH + 1;

      function To_Diagnostic_Position
        (Offset : Natural) return Diagnostic_Source_Position
      is
        (if Offset >= MAX_SOURCE_LENGTH then MAX_SOURCE_LENGTH + 1
         else Offset + 1);

      function Is_Whitespace (Item : Character) return Boolean is
        (Item = ' ' or else Item = ASCII.HT or else
         Item = ASCII.LF or else Item = ASCII.CR);

      procedure Skip_Trivia is
      begin
         while Cursor < Source'Length loop
            if Is_Whitespace (Source (Source'First + Cursor)) then
               Cursor := Cursor + 1;
            elsif Source (Source'First + Cursor) = '#' then
               while Cursor < Source'Length and then
                 Source (Source'First + Cursor) /= ASCII.LF and then
                 Source (Source'First + Cursor) /= ASCII.CR
               loop
                  Cursor := Cursor + 1;
               end loop;
            else
               exit;
            end if;
         end loop;
      end Skip_Trivia;

      procedure Read_Name (Item : out Name; Ok : out Boolean) is
      begin
         Item := (others => <>);
         Ok := False;
         Skip_Trivia;
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
         Index : out Node_Reference)
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
         Skip_Trivia;
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
         Index : out Node_Reference);

      procedure Parse_Integer (Index : out Node_Reference) is
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

      procedure Parse_String (Index : out Node_Reference) is
         Start  : constant Natural := Tree.Text_Bytes_Used;
         Closed : Boolean := False;
         Item   : Character;

         procedure Append (Value : Character) is
         begin
            if Tree.Text_Bytes_Used = MAX_TEXT_BYTES then
               Diagnostic := Text_Storage_Full;
            else
               Tree.Text_Bytes_Used := Tree.Text_Bytes_Used + 1;
               Tree.Text_Data (Tree.Text_Bytes_Used) := Value;
            end if;
         end Append;
      begin
         Index := NO_NODE;
         Cursor := Cursor + 1;
         while Cursor < Source'Length and then
           Diagnostic = No_Diagnostic
         loop
            Item := Source (Source'First + Cursor);
            if Item = '"' then
               Cursor := Cursor + 1;
               Closed := True;
               exit;
            elsif Item = '\' then
               Cursor := Cursor + 1;
               if Cursor >= Source'Length then
                  Diagnostic := Unterminated_String;
               else
                  Item := Source (Source'First + Cursor);
                  case Item is
                     when '"' | '\' => Append (Item);
                     when 'n' => Append (ASCII.LF);
                     when 'r' => Append (ASCII.CR);
                     when 't' => Append (ASCII.HT);
                     when others => Diagnostic := Invalid_String_Escape;
                  end case;
                  Cursor := Cursor + 1;
               end if;
            elsif Item = ASCII.LF or else Item = ASCII.CR then
               Diagnostic := Unterminated_String;
            else
               Append (Item);
               Cursor := Cursor + 1;
            end if;
         end loop;

         if Diagnostic = No_Diagnostic and then not Closed then
            Diagnostic := Unterminated_String;
         elsif Diagnostic = No_Diagnostic then
            Add_Node
              ((Kind => String_Literal,
                Text_Offset => Start,
                Text_Length => Tree.Text_Bytes_Used - Start,
                others => <>),
               Index);
         end if;
      end Parse_String;

      procedure Parse_List
        (Depth : Natural;
         Index : out Node_Reference)
      is
         Operator_Name : Name;
         Binding_Name  : Name;
         Ok            : Boolean;
         A             : Node_Reference := NO_NODE;
         B             : Node_Reference := NO_NODE;
         C             : Node_Reference := NO_NODE;
         Host_Call     : CCL.Catalog.Resolved_Operation;
         Host_Found    : Boolean;
      begin
         Read_Name (Operator_Name, Ok);
         if not Ok then
            Index := NO_NODE;
            return;
         end if;

         if Name_Is (Operator_Name, "+") or else
           Name_Is (Operator_Name, "add") or else
           Name_Is (Operator_Name, "*") or else
           Name_Is (Operator_Name, "multiply") or else
           Name_Is (Operator_Name, "/") or else
           Name_Is (Operator_Name, "divide") or else
           Name_Is (Operator_Name, "%") or else
           Name_Is (Operator_Name, "mod") or else
           Name_Is (Operator_Name, "modulo")
         then
            Parse_Expression (Depth + 1, A);
            Parse_Expression (Depth + 1, B);
            Expect (')', Ok);
            if Diagnostic = No_Diagnostic and then Ok then
               Add_Node
                 ((Kind =>
                     (if Name_Is (Operator_Name, "+") or else
                         Name_Is (Operator_Name, "add")
                      then Add_Form
                      elsif Name_Is (Operator_Name, "*") or else
                        Name_Is (Operator_Name, "multiply")
                      then Multiply_Form
                      elsif Name_Is (Operator_Name, "/") or else
                        Name_Is (Operator_Name, "divide")
                      then Divide_Form
                      else Modulo_Form),
                   First => A, Second => B, others => <>),
                  Index);
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
         elsif Name_Is (Operator_Name, "length") then
            Parse_Expression (Depth + 1, A);
            Expect (')', Ok);
            if Diagnostic = No_Diagnostic and then Ok then
               Add_Node
                 ((Kind => String_Length_Form, First => A, others => <>),
                  Index);
            else
               Index := NO_NODE;
            end if;
         elsif Name_Is (Operator_Name, "at") then
            Parse_Expression (Depth + 1, A);
            Parse_Expression (Depth + 1, B);
            Expect (')', Ok);
            if Diagnostic = No_Diagnostic and then Ok then
               Add_Node
                 ((Kind => String_Index_Form, First => A, Second => B,
                   others => <>), Index);
            else
               Index := NO_NODE;
            end if;
         elsif Name_Is (Operator_Name, "concat") then
            Parse_Expression (Depth + 1, A);
            Parse_Expression (Depth + 1, B);
            Expect (')', Ok);
            if Diagnostic = No_Diagnostic and then Ok then
               Add_Node
                 ((Kind => String_Concat_Form, First => A, Second => B,
                   others => <>), Index);
            else
               Index := NO_NODE;
            end if;
         elsif Name_Is (Operator_Name, "to-string") then
            Parse_Expression (Depth + 1, A);
            Expect (')', Ok);
            if Diagnostic = No_Diagnostic and then Ok then
               Add_Node
                 ((Kind => To_String_Form, First => A, others => <>),
                  Index);
            else
               Index := NO_NODE;
            end if;
         else
            CCL.Catalog.Resolve
              (Visible_Interfaces,
               Operator_Name.Data (1 .. Operator_Name.Length),
               Host_Call,
               Host_Found);
            if not Host_Found then
               Diagnostic := Unknown_Form;
               Index := NO_NODE;
            else
               if Host_Call.Parameters = 1 then
                  Parse_Expression (Depth + 1, A);
               end if;
               if Diagnostic = No_Diagnostic then
                  Expect (')', Ok);
               end if;
               if Diagnostic = No_Diagnostic and then Ok then
                  Add_Node
                    ((Kind => Host_Import_Form,
                      First => A,
                      Host_Call => Host_Call,
                      others => <>),
                     Index);
               else
                  Index := NO_NODE;
               end if;
            end if;
         end if;
      end Parse_List;

      procedure Parse_Expression
        (Depth : Natural;
         Index : out Node_Reference)
      is
         Item : Name;
         Ok   : Boolean;
         Start : Natural := Cursor;
      begin
         Index := NO_NODE;
         if Diagnostic /= No_Diagnostic then
            return;
         elsif Depth >= MAX_NESTING then
            Diagnostic := Nesting_Too_Deep;
            return;
         end if;

         Skip_Trivia;
         Start := Cursor;
         if Cursor >= Source'Length then
            Diagnostic := Unexpected_End;
         elsif Source (Source'First + Cursor) = '(' then
            Cursor := Cursor + 1;
            Parse_List (Depth, Index);
            if Index < Tree.Length then
               Tree.Nodes (Node_Index (Index)).Source_Position :=
                 To_Diagnostic_Position (Start);
            end if;
         elsif Source (Source'First + Cursor) = '"' then
            Parse_String (Index);
            if Index < Tree.Length then
               Tree.Nodes (Node_Index (Index)).Source_Position :=
                 To_Diagnostic_Position (Start);
            end if;
         elsif Source (Source'First + Cursor) = '-' or else
           (Source (Source'First + Cursor) >= '0' and then
            Source (Source'First + Cursor) <= '9')
         then
            Parse_Integer (Index);
            if Index < Tree.Length then
               Tree.Nodes (Node_Index (Index)).Source_Position :=
                 To_Diagnostic_Position (Start);
            end if;
         elsif Is_Name_Character (Source (Source'First + Cursor)) then
            Read_Name (Item, Ok);
            if Ok and then Name_Is (Item, "true") then
               Add_Node ((Kind => Boolean_Literal, Boolean_Value => True,
                          Source_Position => To_Diagnostic_Position (Start),
                          others => <>), Index);
            elsif Ok and then Name_Is (Item, "false") then
               Add_Node ((Kind => Boolean_Literal, Boolean_Value => False,
                          Source_Position => To_Diagnostic_Position (Start),
                          others => <>), Index);
            elsif Ok then
               Add_Node ((Kind => Name_Reference, Identifier => Item,
                          Source_Position => To_Diagnostic_Position (Start),
                          others => <>), Index);
            end if;
         else
            Diagnostic := Unexpected_Token;
         end if;
         if Index < Tree.Length then
            Tree.Nodes (Node_Index (Index)).Source_Position :=
              To_Diagnostic_Position (Start);
            Tree.Nodes (Node_Index (Index)).Source_End_Position :=
              To_Diagnostic_Position (Cursor);
         end if;
         if Diagnostic /= No_Diagnostic and then Diagnostic_Position = 0 then
            Diagnostic_Position := To_Diagnostic_Position (Start);
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
         Entry_Environment_Length : constant Natural range 0 .. MAX_BINDINGS :=
           Type_Env_Length;
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
            when String_Literal => Kind := String_Type;
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
            when Add_Form | Multiply_Form | Divide_Form | Modulo_Form |
                 Equal_Form =>
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
               elsif Tree.Nodes (Node_Index (Index)).Kind in
                 Multiply_Form | Divide_Form | Modulo_Form
               then
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
                  Type_Env_Length := Entry_Environment_Length;
               end if;
            when String_Length_Form =>
               Check_Node (Tree.Nodes (Node_Index (Index)).First,
                           Depth + 1, Left_Type);
               if Diagnostic = No_Diagnostic and then
                 Left_Type /= String_Type
               then
                  Diagnostic := Expected_String;
               else
                  Kind := Integer_Type;
               end if;
            when String_Index_Form =>
               Check_Node (Tree.Nodes (Node_Index (Index)).First,
                           Depth + 1, Left_Type);
               Check_Node (Tree.Nodes (Node_Index (Index)).Second,
                           Depth + 1, Right_Type);
               if Diagnostic = No_Diagnostic and then
                 Left_Type /= String_Type
               then
                  Diagnostic := Expected_String;
               elsif Diagnostic = No_Diagnostic and then
                 Right_Type /= Integer_Type
               then
                  Diagnostic := Expected_Integer;
               else
                  Kind := Character_Type;
               end if;
            when String_Concat_Form =>
               Check_Node (Tree.Nodes (Node_Index (Index)).First,
                           Depth + 1, Left_Type);
               Check_Node (Tree.Nodes (Node_Index (Index)).Second,
                           Depth + 1, Right_Type);
               if Diagnostic = No_Diagnostic and then
                 (Left_Type /= String_Type or else Right_Type /= String_Type)
               then
                  Diagnostic := Expected_String;
               else
                  Kind := String_Type;
               end if;
            when To_String_Form =>
               Check_Node (Tree.Nodes (Node_Index (Index)).First,
                           Depth + 1, Left_Type);
               if Diagnostic = No_Diagnostic and then
                 Left_Type /= Integer_Type
               then
                  Diagnostic := Expected_Integer;
               else
                  Kind := String_Type;
               end if;
            when Host_Import_Form =>
               if Tree.Nodes (Node_Index (Index)).Host_Call.Parameters = 1
               then
                  Check_Node
                    (Tree.Nodes (Node_Index (Index)).First,
                     Depth + 1,
                     Left_Type);
                  if Diagnostic = No_Diagnostic and then
                    ((Tree.Nodes (Node_Index (Index)).Host_Call.Import.Argument =
                        CCL.VM.Integer_Value and then
                      Left_Type /= Integer_Type) or else
                     (Tree.Nodes (Node_Index (Index)).Host_Call.Import.Argument =
                        CCL.VM.Boolean_Value and then
                      Left_Type /= Boolean_Type))
                  then
                     Diagnostic :=
                       (if Tree.Nodes (Node_Index (Index)).Host_Call.Import.Argument =
                           CCL.VM.Integer_Value
                        then Expected_Integer
                        else Expected_Boolean);
                  end if;
               end if;
               if Diagnostic = No_Diagnostic then
                  Kind :=
                    (if Tree.Nodes (Node_Index (Index)).Host_Call.Import.Result =
                        CCL.VM.Integer_Value
                     then Integer_Type
                     else Boolean_Type);
               end if;
            when Invalid_Node => Diagnostic := Unexpected_Token;
         end case;
         if Diagnostic = No_Diagnostic then
            Tree.Nodes (Node_Index (Index)).Static_Kind := Kind;
         end if;
         if Diagnostic /= No_Diagnostic and then Diagnostic_Position = 0 then
            Diagnostic_Position :=
              Tree.Nodes (Node_Index (Index)).Source_Position;
         end if;
      end Check_Node;

      Value_Env : Value_Environment := [others => (others => <>)];
      Value_Env_Length : Natural range 0 .. MAX_BINDINGS := 0;
      Text_Region : Text_Regions.Stack;
      subtype Remaining_Fuel is Natural range 0 .. Fuel;
      Fuel_Left : Remaining_Fuel := Fuel;
      Eval_Status : Interpretation_Status := Succeeded;

      procedure Evaluate_Node
        (Index : Natural;
         Depth : Natural;
         Item  : out Runtime_Value;
         Ok    : out Boolean)
      is
         Left  : Runtime_Value := (others => <>);
         Right : Runtime_Value := (others => <>);
         Good  : Boolean;
         Found : Boolean := False;
         Arithmetic_Value : Integer_64 := 0;
         Overflowed : Boolean := False;
         Arithmetic_Error : CCL.Checked_Arithmetic.Arithmetic_Error :=
           CCL.Checked_Arithmetic.Arithmetic_Ok;
         Region_Result : Text_Regions.Operation_Result;
         Text_Length : Natural;
         Scratch : String (1 .. MAX_TEXT_BYTES) :=
           [others => Character'Val (0)];
         Character_Item : Character;
         Entry_Environment_Length : constant Natural range 0 .. MAX_BINDINGS :=
           Value_Env_Length;
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
               Item.Kind := Integer_Type;
               Item.Scalar := CCL.VM.Integer_Constant
                 (Tree.Nodes (Node_Index (Index)).Integer_Value);
               Ok := True;
            when Boolean_Literal =>
               Item.Kind := Boolean_Type;
               Item.Scalar := CCL.VM.Boolean_Constant
                 (Tree.Nodes (Node_Index (Index)).Boolean_Value);
               Ok := True;
            when String_Literal =>
               Item.Kind := String_Type;
               if Tree.Nodes (Node_Index (Index)).Text_Length = 0 then
                  Text_Regions.Allocate_String
                    (Text_Region, "", Item.Text, Region_Result);
               else
                  Text_Regions.Allocate_String
                    (Text_Region,
                     Tree.Text_Data
                       (Tree.Nodes (Node_Index (Index)).Text_Offset + 1 ..
                        Tree.Nodes (Node_Index (Index)).Text_Offset +
                          Tree.Nodes (Node_Index (Index)).Text_Length),
                     Item.Text, Region_Result);
               end if;
               if Region_Result = Text_Regions.Operation_Ok then
                  Ok := True;
               else
                  Eval_Status := Evaluation_Text_Storage_Exhausted;
               end if;
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
                 (Left.Scalar.Integer, Right.Scalar.Integer)
               then
                  Eval_Status := Evaluation_Overflow;
                  Good := False;
               elsif Good then
                  Item.Kind := Integer_Type;
                  Item.Scalar := CCL.VM.Integer_Constant
                    (Left.Scalar.Integer + Right.Scalar.Integer);
               end if;
               Ok := Good;
            when Multiply_Form | Divide_Form | Modulo_Form =>
               Evaluate_Node (Tree.Nodes (Node_Index (Index)).First,
                              Depth + 1, Left, Good);
               if Good then
                  Evaluate_Node (Tree.Nodes (Node_Index (Index)).Second,
                                 Depth + 1, Right, Good);
               end if;
               if Good then
                  case Tree.Nodes (Node_Index (Index)).Kind is
                     when Multiply_Form =>
                        CCL.Checked_Arithmetic.Multiply
                          (Left.Scalar.Integer, Right.Scalar.Integer,
                           Arithmetic_Value, Overflowed);
                        Arithmetic_Error :=
                          (if Overflowed then
                              CCL.Checked_Arithmetic.Arithmetic_Overflow
                           else CCL.Checked_Arithmetic.Arithmetic_Ok);
                     when Divide_Form =>
                        CCL.Checked_Arithmetic.Divide
                          (Left.Scalar.Integer, Right.Scalar.Integer,
                           Arithmetic_Value, Arithmetic_Error);
                     when Modulo_Form =>
                        CCL.Checked_Arithmetic.Modulo
                          (Left.Scalar.Integer, Right.Scalar.Integer,
                           Arithmetic_Value, Arithmetic_Error);
                     when others => null;
                  end case;
                  if Arithmetic_Error =
                    CCL.Checked_Arithmetic.Arithmetic_Overflow
                  then
                     Eval_Status := Evaluation_Overflow;
                     Good := False;
                  elsif Arithmetic_Error =
                    CCL.Checked_Arithmetic.Division_By_Zero
                  then
                     Eval_Status := Evaluation_Division_By_Zero;
                     Good := False;
                  else
                     Item.Kind := Integer_Type;
                     Item.Scalar := CCL.VM.Integer_Constant (Arithmetic_Value);
                  end if;
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
                  Item.Kind := Boolean_Type;
                  Item.Scalar := CCL.VM.Boolean_Constant
                    (Left.Scalar.Integer = Right.Scalar.Integer);
               end if;
               Ok := Good;
            when Not_Form =>
               Evaluate_Node (Tree.Nodes (Node_Index (Index)).First,
                              Depth + 1, Left, Good);
               if Good then
                  Item.Kind := Boolean_Type;
                  Item.Scalar := CCL.VM.Boolean_Constant
                    (not Left.Scalar.Boolean);
               end if;
               Ok := Good;
            when If_Form =>
               Evaluate_Node (Tree.Nodes (Node_Index (Index)).First,
                              Depth + 1, Left, Good);
               if Good and then Left.Scalar.Boolean then
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
                  Value_Env_Length := Entry_Environment_Length;
               else
                  Good := False;
               end if;
               Ok := Good;
            when String_Length_Form =>
               Evaluate_Node (Tree.Nodes (Node_Index (Index)).First,
                              Depth + 1, Left, Good);
               if Good then
                  Item.Kind := Integer_Type;
                  Item.Scalar := CCL.VM.Integer_Constant
                    (Integer_64 (Text_Regions.Length (Left.Text)));
               end if;
               Ok := Good;
            when String_Index_Form =>
               Evaluate_Node (Tree.Nodes (Node_Index (Index)).First,
                              Depth + 1, Left, Good);
               if Good then
                  Evaluate_Node (Tree.Nodes (Node_Index (Index)).Second,
                                 Depth + 1, Right, Good);
               end if;
               if Good and then
                 (Right.Scalar.Integer < 1 or else
                  Right.Scalar.Integer >
                    Integer_64 (Text_Regions.Last_Index (Left.Text)) or else
                  Right.Scalar.Integer >
                    Integer_64 (Text_Regions.String_Index'Last))
               then
                  Eval_Status := Evaluation_Index_Error;
                  Good := False;
               elsif Good then
                  Text_Regions.Read
                    (Text_Region, Left.Text,
                     Text_Regions.String_Index (Right.Scalar.Integer),
                     Character_Item, Region_Result);
                  if Region_Result = Text_Regions.Operation_Ok then
                     Item.Kind := Character_Type;
                     Item.Character_Item := Character_Item;
                  else
                     Eval_Status := Evaluation_Index_Error;
                     Good := False;
                  end if;
               end if;
               Ok := Good;
            when String_Concat_Form =>
               Evaluate_Node (Tree.Nodes (Node_Index (Index)).First,
                              Depth + 1, Left, Good);
               if Good then
                  Evaluate_Node (Tree.Nodes (Node_Index (Index)).Second,
                                 Depth + 1, Right, Good);
               end if;
               if Good and then
                 Text_Regions.Length (Left.Text) >
                   MAX_TEXT_BYTES - Text_Regions.Length (Right.Text)
               then
                  Eval_Status := Evaluation_Text_Storage_Exhausted;
                  Good := False;
               elsif Good then
                  Text_Length := Text_Regions.Length (Left.Text) +
                    Text_Regions.Length (Right.Text);
                  if Text_Regions.Length (Left.Text) > 0 then
                     for Position in
                       0 .. Text_Regions.Length (Left.Text) - 1
                     loop
                        Text_Regions.Read
                          (Text_Region, Left.Text,
                           Text_Regions.First_Index (Left.Text) + Position,
                           Scratch (Position + 1), Region_Result);
                        if Region_Result /= Text_Regions.Operation_Ok then
                           Good := False;
                           exit;
                        end if;
                     end loop;
                  end if;
                  if Good and then Text_Regions.Length (Right.Text) > 0 then
                     for Position in
                       0 .. Text_Regions.Length (Right.Text) - 1
                     loop
                        Text_Regions.Read
                          (Text_Region, Right.Text,
                           Text_Regions.First_Index (Right.Text) + Position,
                           Scratch
                             (Text_Regions.Length (Left.Text) + Position + 1),
                           Region_Result);
                        if Region_Result /= Text_Regions.Operation_Ok then
                           Good := False;
                           exit;
                        end if;
                     end loop;
                  end if;
                  if not Good then
                     Eval_Status := Evaluation_Index_Error;
                  elsif Text_Length = 0 then
                     Text_Regions.Allocate_String
                       (Text_Region, "", Item.Text, Region_Result);
                  else
                     Text_Regions.Allocate_String
                       (Text_Region, Scratch (1 .. Text_Length), Item.Text,
                        Region_Result);
                  end if;
                  if Good and then
                    Region_Result = Text_Regions.Operation_Ok
                  then
                     Item.Kind := String_Type;
                  elsif Good then
                     Eval_Status := Evaluation_Text_Storage_Exhausted;
                     Good := False;
                  end if;
               end if;
               Ok := Good;
            when To_String_Form =>
               Evaluate_Node (Tree.Nodes (Node_Index (Index)).First,
                              Depth + 1, Left, Good);
               if Good then
                  declare
                     Image_Buffer : String (1 .. 20) := [others => '0'];
                     Image_First  : Positive := Image_Buffer'Last;
                     Magnitude    : Unsigned_64;
                     Digit        : Unsigned_64;
                  begin
                     if Left.Scalar.Integer < 0 then
                        Magnitude :=
                          Unsigned_64 (-(Left.Scalar.Integer + 1)) + 1;
                     else
                        Magnitude := Unsigned_64 (Left.Scalar.Integer);
                     end if;
                     loop
                        Digit := Magnitude mod 10;
                        Image_Buffer (Image_First) := Character'Val
                          (Character'Pos ('0') + Natural (Digit));
                        Magnitude := Magnitude / 10;
                        exit when Magnitude = 0;
                        Image_First := Image_First - 1;
                     end loop;
                     if Left.Scalar.Integer < 0 then
                        Image_First := Image_First - 1;
                        Image_Buffer (Image_First) := '-';
                     end if;
                     Text_Regions.Allocate_String
                       (Text_Region,
                        Image_Buffer (Image_First .. Image_Buffer'Last),
                        Item.Text, Region_Result);
                     if Region_Result = Text_Regions.Operation_Ok then
                        Item.Kind := String_Type;
                     else
                        Eval_Status := Evaluation_Text_Storage_Exhausted;
                        Good := False;
                     end if;
                  end;
               end if;
               Ok := Good;
            when Host_Import_Form =>
               --  The direct evaluator deliberately has no host.  Catalog
               --  visibility permits analysis; only a linked VM host can
               --  satisfy the operation's declared authority requirement.
               Eval_Status := Host_Import_Required;
               Ok := False;
            when Invalid_Node => Ok := False;
         end case;
      end Evaluate_Node;

      Root_Type : Static_Type;
      Value     : Runtime_Value;
      Ok        : Boolean;
      Region_Result : Text_Regions.Operation_Result;
   begin
      Result :=
        (Status => Parse_Failed, Diagnostic => No_Diagnostic,
         Diagnostic_Position => 0,
         Fuel_Remaining => Fuel, others => <>);

      if Analyze_Input then
         Tree := (others => <>);
         if Source'Length > MAX_SOURCE_LENGTH then
            Result.Diagnostic := Source_Too_Long;
            Result.Diagnostic_Position := MAX_SOURCE_LENGTH + 1;
            return;
         end if;

         Parse_Expression (0, Root);
         Tree.Root := Root;
         Skip_Trivia;
         if Diagnostic = No_Diagnostic and then Cursor /= Source'Length then
            Diagnostic := Trailing_Input;
         end if;
         if Diagnostic /= No_Diagnostic then
            Result.Diagnostic := Diagnostic;
            Result.Diagnostic_Position :=
              (if Diagnostic_Position > 0 then Diagnostic_Position
               else To_Diagnostic_Position (Cursor));
            return;
         end if;

         Check_Node (Root, 0, Root_Type);
         if Diagnostic /= No_Diagnostic or else Root_Type = Invalid_Type then
            Result.Status := Type_Check_Failed;
            Result.Diagnostic := Diagnostic;
            Result.Diagnostic_Position := Diagnostic_Position;
            return;
         end if;
      else
         Root := Tree.Root;
      end if;

      if not Evaluate then
         Result.Status := Succeeded;
         return;
      end if;

      Text_Regions.Initialize (Text_Region);
      Evaluate_Node (Root, 0, Value, Ok);
      Result.Status := Eval_Status;
      Result.Fuel_Remaining := Fuel_Left;
      if Ok and then Eval_Status = Succeeded then
         Result.Has_Value := True;
         case Value.Kind is
            when String_Type =>
               Result.Has_Text := True;
               Result.Result_Text.Length := Text_Regions.Length (Value.Text);
               if Result.Result_Text.Length > 0 then
                  Text_Regions.Copy_To
                    (Text_Region, Value.Text,
                     Result.Result_Text.Data
                       (1 .. Result.Result_Text.Length),
                     Region_Result);
                  if Region_Result /= Text_Regions.Operation_Ok then
                     Result.Status := Evaluation_Index_Error;
                     Result.Has_Value := False;
                     Result.Has_Text := False;
                  end if;
               end if;
            when Character_Type =>
               Result.Has_Character := True;
               Result.Result_Character := Value.Character_Item;
            when Integer_Type | Boolean_Type =>
               Result.Result_Value := Value.Scalar;
            when Invalid_Type =>
               Result.Status := Type_Check_Failed;
               Result.Has_Value := False;
         end case;
      end if;
      Text_Regions.Clear (Text_Region);
   end Process_Source;

   procedure Analyze
     (Source : String;
      Result : out Analysis_Result)
   is
      Empty   : CCL.Catalog.Interface_Catalog;
   begin
      CCL.Catalog.Initialize (Empty);
      Analyze (Source, Empty, Result);
   end Analyze;

   procedure Analyze
     (Source             : String;
      Visible_Interfaces : CCL.Catalog.Interface_Catalog;
      Result             : out Analysis_Result)
   is
      Tree    : Syntax_Tree;
      Outcome : Interpretation_Result;
   begin
      Process_Source
         (Source   => Source,
          Fuel     => 0,
          Visible_Interfaces => Visible_Interfaces,
          Analyze_Input => True,
         Evaluate => False,
         Result   => Outcome,
         Tree     => Tree);

      Result :=
        (Status =>
           (case Outcome.Status is
               when Succeeded => Analysis_Succeeded,
               when Type_Check_Failed => Analysis_Type_Check_Failed,
               when others => Analysis_Parse_Failed),
         Diagnostic => Outcome.Diagnostic,
         Diagnostic_Position => Outcome.Diagnostic_Position,
         Tree => Tree);
   end Analyze;

   procedure Interpret
     (Source : String;
      Fuel   : Natural;
      Result : out Interpretation_Result)
   is
      Empty : CCL.Catalog.Interface_Catalog;
   begin
      CCL.Catalog.Initialize (Empty);
      Interpret (Source, Fuel, Empty, Result);
   end Interpret;

   procedure Interpret
     (Source             : String;
      Fuel               : Natural;
      Visible_Interfaces : CCL.Catalog.Interface_Catalog;
      Result             : out Interpretation_Result)
   is
      Analysis : Analysis_Result;
      Tree     : Syntax_Tree;
   begin
      Analyze (Source, Visible_Interfaces, Analysis);
      if Analysis.Status /= Analysis_Succeeded then
         Result :=
           (Status =>
              (if Analysis.Status = Analysis_Type_Check_Failed then
                  Type_Check_Failed
               else Parse_Failed),
            Diagnostic => Analysis.Diagnostic,
            Diagnostic_Position => Analysis.Diagnostic_Position,
            Fuel_Remaining => Fuel,
            others => <>);
         return;
      end if;

      Tree := Analysis.Tree;
      Process_Source
         (Source   => Source,
          Fuel     => Fuel,
          Visible_Interfaces => Visible_Interfaces,
          Analyze_Input => False,
         Evaluate => True,
         Result   => Result,
         Tree     => Tree);
   end Interpret;
end CCL.Language;

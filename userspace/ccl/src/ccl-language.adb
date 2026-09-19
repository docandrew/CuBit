with Interfaces; use Interfaces;
with CCL.Checked_Arithmetic;
with CCL.Secondary_Stacks;
with CCL.Imports;
with CCL.Handler_References;

package body CCL.Language with
   SPARK_Mode => On
is
   use type CCL.Host_Values.Value_Kind;
   use type CCL.Checked_Arithmetic.Arithmetic_Error;
   use type CCL.Imports.Transfer_Mode;
   use type CCL.Imports.Cancellation_Mode;

   package Text_Regions is new CCL.Secondary_Stacks
     (Capacity => MAX_TEXT_BYTES * 4,
      Max_Values => MAX_AST_NODES,
      Max_String_Length => MAX_TEXT_BYTES);
   use type Text_Regions.Operation_Result;

   type Runtime_Value is record
      Kind      : Static_Type := Invalid_Type;
      Scalar    : CCL.VM.Value := (others => <>);
      Text      : Text_Regions.String_Value;
      Character_Item : Character := Character'Val (0);
      Handler_Id : Function_Reference := NO_FUNCTION;
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

   function Decimal_Image (Value : Integer_64) return String is
      Buffer : String (1 .. 20) := [others => '0'];
      First : Positive range 2 .. 20 := 20;
      Signed_First : Positive range 1 .. 20;
      Magnitude : Unsigned_64;
      Digit : Unsigned_64 range 0 .. 9;
   begin
      Magnitude := (if Value < 0 then Unsigned_64 (-(Value + 1)) + 1
                    else Unsigned_64 (Value));
      --  Nineteen magnitude digits, plus a reserved sign position. The
      --  bounded reverse loop cannot underflow, even for Integer_64'First.
      for Position in reverse 2 .. 20 loop
         First := Position;
         Digit := Magnitude mod 10;
         Buffer (First) := Character'Val (Character'Pos ('0') + Natural (Digit));
         Magnitude := Magnitude / 10;
         exit when Magnitude = 0;
      end loop;
      Signed_First := First;
      if Value < 0 then
         Signed_First := First - 1;
         Buffer (Signed_First) := '-';
      end if;
      return Buffer (Signed_First .. Buffer'Last);
   end Decimal_Image;

   procedure Process_Source_With_Host
     (Source : String; Fuel : Natural;
      Visible_Interfaces : CCL.Catalog.Interface_Catalog;
      Grants : CCL.Catalog.Granted_Bindings;
      Context : in out Host_Context;
      Host_Enabled : Boolean;
      Analyze_Input : Boolean; Evaluate : Boolean;
      Result : out Interpretation_Result; Tree : in out Syntax_Tree)
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
                Text_First => Start + 1,
                Text_Last => Tree.Text_Bytes_Used,
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
         Arguments     : Argument_Array := [others => NO_NODE];
         Count         : Parameter_Count := 0;
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
         elsif Name_Is (Operator_Name, "handler") then
            Read_Name (Binding_Name, Ok);
            if Ok then Expect (')', Ok); end if;
            if Diagnostic = No_Diagnostic and then Ok then
               Add_Node ((Kind => Handler_Form, Identifier => Binding_Name, others => <>), Index);
            else Index := NO_NODE; end if;
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
            if not Host_Found and then
              (for some C of Operator_Name.Data (1 .. Operator_Name.Length) => C = '.')
            then
               --  Qualified names belong to advertised services, never local
               --  functions. Preserve fail-closed catalog resolution here.
               Diagnostic := Unknown_Form;
               Index := NO_NODE;
            elsif not Host_Found then
               Index := NO_NODE;
               Skip_Trivia;
               while Diagnostic = No_Diagnostic and then Cursor < Source'Length
                 and then Source (Source'First + Cursor) /= ')'
               loop
                  if Count = MAX_PARAMETERS then
                     Diagnostic := Too_Many_Parameters;
                     return;
                  end if;
                  Count := Count + 1;
                  Parse_Expression (Depth + 1, Arguments (Count));
                  Skip_Trivia;
               end loop;
               if Diagnostic = No_Diagnostic then
                  Expect (')', Ok);
                  if Ok then
                     Add_Node
                       ((Kind => Function_Call, Identifier => Operator_Name,
                         Arguments => Arguments, Argument_Count => Count,
                         others => <>), Index);
                  end if;
               end if;
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
                      Identifier => Operator_Name,
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
            Cursor := Cursor + 1;
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

      --  A program is zero or more declarations followed by one expression.
      --  Definitions are not expressions and cannot capture a surrounding let.
      procedure Parse_Program (Depth : Natural; Index : out Node_Reference) is
         Start : Natural;
         Token : Name;
         Ok : Boolean;
         Decl : Function_Declaration;
         Id : Function_Reference;
         Tail : Node_Reference;
         procedure Read_Type (Kind : out Static_Type) is
            Token : Name;
            Good : Boolean;
         begin
            Read_Name (Token, Good);
            Kind := Invalid_Type;
            if Good then
               if Name_Is (Token, "Integer") then Kind := Integer_Type;
               elsif Name_Is (Token, "Boolean") then Kind := Boolean_Type;
               elsif Name_Is (Token, "String") then Kind := String_Type;
               elsif Name_Is (Token, "Character") then Kind := Character_Type;
               else Diagnostic := Expected_Type_Name;
               end if;
            end if;
         end Read_Type;
      begin
         Index := NO_NODE;
         if Diagnostic /= No_Diagnostic then return; end if;
         Skip_Trivia;
         Start := Cursor;
         if Depth >= MAX_NESTING then Diagnostic := Nesting_Too_Deep; return; end if;
         if Cursor < Source'Length and then Source (Source'First + Cursor) = '(' then
            Cursor := Cursor + 1;
            Read_Name (Token, Ok);
            if not Ok then return; end if;
         end if;
         if not Name_Is (Token, "define") then
            Cursor := Start;
            Parse_Expression (Depth, Index);
            return;
         end if;
         if Tree.Function_Count = MAX_FUNCTIONS then
            Diagnostic := Too_Many_Functions; return;
         end if;
         Id := Tree.Function_Count;
         Expect ('(', Ok);
         if not Ok then return; end if;
         Read_Name (Decl.Identifier, Ok);
         if not Ok then return; end if;
         Skip_Trivia;
         while Diagnostic = No_Diagnostic and then Cursor < Source'Length
           and then Source (Source'First + Cursor) = '('
         loop
            if Decl.Count = MAX_PARAMETERS then Diagnostic := Too_Many_Parameters; return; end if;
            Decl.Count := Decl.Count + 1;
            Cursor := Cursor + 1;
            Read_Name (Decl.Parameters (Decl.Count).Identifier, Ok);
            if not Ok then return; end if;
            Read_Type (Decl.Parameters (Decl.Count).Kind);
            if Diagnostic /= No_Diagnostic then return; end if;
            Expect (')', Ok);
            Skip_Trivia;
         end loop;
         if Diagnostic /= No_Diagnostic then return; end if;
         Expect (')', Ok);
         if not Ok then return; end if;
         Read_Type (Decl.Result_Kind);
         if Diagnostic /= No_Diagnostic then return; end if;
         Parse_Expression (Depth + 1, Decl.Body_Node);
         if Diagnostic /= No_Diagnostic then return; end if;
         Expect (')', Ok);
         if not Ok then return; end if;
         Tree.Functions (Id) := Decl;
         Tree.Function_Count := Tree.Function_Count + 1;
         Add_Node
           ((Kind => Function_Definition, Function_Id => Id,
             First => Decl.Body_Node,
             Source_Position => To_Diagnostic_Position (Start),
             Source_End_Position => To_Diagnostic_Position (Cursor), others => <>), Index);
         if Diagnostic /= No_Diagnostic then return; end if;
         Parse_Program (Depth + 1, Tail);
         Tree.Nodes (Index).Second := Tail;
      end Parse_Program;

      Type_Env : Type_Environment := [others => (others => <>)];
      Type_Env_Length : Natural range 0 .. MAX_BINDINGS := 0;
      Visible_Functions : Natural range 0 .. MAX_FUNCTIONS := 0;

      function Referenceable (Item : Name) return Boolean is
        (Item.Length > 0 and then Item.Data (1) not in '-' | '0' .. '9' and then
         not Name_Is (Item, "true") and then not Name_Is (Item, "false"));

      function Reserved (Item : Name) return Boolean is
        (not Referenceable (Item) or else
         Name_Is (Item, "define") or else Name_Is (Item, "handler") or else Name_Is (Item, "let") or else
         Name_Is (Item, "if") or else Name_Is (Item, "not") or else
         Name_Is (Item, "true") or else Name_Is (Item, "false") or else
         Name_Is (Item, "+") or else Name_Is (Item, "add") or else
         Name_Is (Item, "*") or else Name_Is (Item, "multiply") or else
         Name_Is (Item, "/") or else Name_Is (Item, "divide") or else
         Name_Is (Item, "%") or else Name_Is (Item, "mod") or else
         Name_Is (Item, "modulo") or else Name_Is (Item, "=") or else
         Name_Is (Item, "equal") or else Name_Is (Item, "length") or else
         Name_Is (Item, "at") or else Name_Is (Item, "concat") or else
         Name_Is (Item, "to-string") or else
         (for some C of Item.Data (1 .. Item.Length) => C = '.'));

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
            when Function_Definition =>
               declare
                  Decl : constant Function_Declaration :=
                    Tree.Functions (Tree.Nodes (Index).Function_Id);
               begin
                  if Reserved (Decl.Identifier) then
                     Diagnostic := Duplicate_Declaration;
                  end if;
                  for F in 1 .. Visible_Functions loop
                     if Names_Equal (Decl.Identifier, Tree.Functions (F - 1).Identifier) then
                        Diagnostic := Duplicate_Declaration;
                     end if;
                  end loop;
                  for P in 1 .. Decl.Count loop
                     if not Referenceable (Decl.Parameters (P).Identifier)
                     then Diagnostic := Duplicate_Declaration; end if;
                     for Q in 1 .. P - 1 loop
                        if Names_Equal (Decl.Parameters (P).Identifier,
                                        Decl.Parameters (Q).Identifier)
                        then Diagnostic := Duplicate_Declaration; end if;
                     end loop;
                     Type_Env (P - 1) :=
                       (Identifier => Decl.Parameters (P).Identifier,
                        Kind => Decl.Parameters (P).Kind);
                  end loop;
                  Type_Env_Length := Decl.Count;
                  Check_Node (Decl.Body_Node, Depth + 1, Left_Type);
                  Type_Env_Length := Entry_Environment_Length;
                  if Diagnostic = No_Diagnostic and then Left_Type /= Decl.Result_Kind then
                     Diagnostic := Function_Result_Mismatch;
                  end if;
                  if Diagnostic = No_Diagnostic then
                     --  Publish only after checking the body: no self calls or
                     --  forward calls, and therefore no recursive call graph.
                     Visible_Functions := Visible_Functions + 1;
                     Check_Node (Tree.Nodes (Index).Second, Depth + 1, Kind);
                  end if;
               end;
            when Function_Call | Handler_Form =>
               for F in 1 .. Visible_Functions loop
                  if Names_Equal (Tree.Nodes (Index).Identifier,
                                  Tree.Functions (F - 1).Identifier)
                  then
                     Tree.Nodes (Index).Function_Id := F - 1;
                     Found := True;
                     exit;
                  end if;
               end loop;
               if not Found then Diagnostic := Unknown_Form;
               else
                  declare
                     Decl : constant Function_Declaration :=
                       Tree.Functions (Tree.Nodes (Index).Function_Id);
                  begin
                     if Tree.Nodes (Index).Kind = Handler_Form then
                        if Decl.Count /= 0 or else Decl.Result_Kind /= Boolean_Type then
                           Diagnostic := Invalid_Handler_Profile;
                        else Kind := Handler_Type; end if;
                     elsif Tree.Nodes (Index).Argument_Count /= Decl.Count then
                        Diagnostic := Function_Arity_Mismatch;
                     else
                        for P in 1 .. Decl.Count loop
                           Check_Node (Tree.Nodes (Index).Arguments (P), Depth + 1, Left_Type);
                           if Diagnostic = No_Diagnostic and then Left_Type /= Decl.Parameters (P).Kind then
                              Diagnostic := Function_Argument_Mismatch;
                           end if;
                        end loop;
                        Kind := Decl.Result_Kind;
                     end if;
                  end;
               end if;
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
                        CCL.Host_Values.Integer_Value and then
                      Left_Type /= Integer_Type) or else
                     (Tree.Nodes (Node_Index (Index)).Host_Call.Import.Argument =
                        CCL.Host_Values.Boolean_Value and then
                      Left_Type /= Boolean_Type) or else
                     (Tree.Nodes (Node_Index (Index)).Host_Call.Import.Argument =
                        CCL.Host_Values.Text_Value and then Left_Type /= String_Type) or else
                     (Tree.Nodes (Node_Index (Index)).Host_Call.Import.Argument =
                        CCL.Host_Values.Handler_Value and then Left_Type /= Handler_Type))
                  then
                     Diagnostic :=
                       (if Tree.Nodes (Node_Index (Index)).Host_Call.Import.Argument =
                           CCL.Host_Values.Integer_Value
                        then Expected_Integer
                        elsif Tree.Nodes (Node_Index (Index)).Host_Call.Import.Argument =
                          CCL.Host_Values.Boolean_Value then Expected_Boolean
                        elsif Tree.Nodes (Node_Index (Index)).Host_Call.Import.Argument =
                          CCL.Host_Values.Handler_Value then Expected_Handler
                        else Expected_String);
                  end if;
               end if;
               if Diagnostic = No_Diagnostic then
                  Kind :=
                    (if Tree.Nodes (Node_Index (Index)).Host_Call.Import.Result =
                        CCL.Host_Values.Integer_Value
                     then Integer_Type
                     elsif Tree.Nodes (Node_Index (Index)).Host_Call.Import.Result =
                       CCL.Host_Values.Boolean_Value then Boolean_Type
                     else String_Type);
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
         elsif Depth >= MAX_NESTING then
            Eval_Status := Evaluation_Depth_Exhausted;
            return;
         elsif Index >= Tree.Length then
            Eval_Status := Parse_Failed;
            return;
         end if;
         Fuel_Left := Fuel_Left - 1;

         case Tree.Nodes (Node_Index (Index)).Kind is
            when Handler_Form =>
               Item.Kind := Handler_Type;
               Item.Handler_Id := Tree.Nodes (Index).Function_Id;
               Ok := True;
            when Function_Definition =>
               --  Definitions are checked before execution; only the final
               --  expression executes, never an unused function body.
               Evaluate_Node (Tree.Nodes (Index).Second, Depth + 1, Item, Ok);
            when Function_Call =>
               declare
                  Decl : constant Function_Declaration :=
                    Tree.Functions (Tree.Nodes (Index).Function_Id);
                  type Parameter_Values is array (Parameter_Index) of Runtime_Value;
                  Arguments : Parameter_Values := [others => (others => <>)];
               begin
                  Good := True;
                  --  Left-to-right, exactly once, in the caller's environment.
                  for P in 1 .. Decl.Count loop
                     Evaluate_Node (Tree.Nodes (Index).Arguments (P), Depth + 1, Arguments (P), Good);
                     exit when not Good;
                  end loop;
                  if Good then
                     declare
                        Saved : constant Value_Environment := Value_Env;
                     begin
                        for P in 1 .. Decl.Count loop
                           Value_Env (P - 1) :=
                             (Identifier => Decl.Parameters (P).Identifier, Item => Arguments (P));
                        end loop;
                        Value_Env_Length := Decl.Count;
                        Evaluate_Node (Decl.Body_Node, Depth + 1, Item, Good);
                        Value_Env := Saved;
                        Value_Env_Length := Entry_Environment_Length;
                     end;
                  end if;
                  Ok := Good;
               end;
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
               Text_Regions.Allocate_String
                 (Text_Region,
                  Tree.Text_Data
                    (Tree.Nodes (Node_Index (Index)).Text_First ..
                     Tree.Nodes (Node_Index (Index)).Text_Last),
                  Item.Text, Region_Result);
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
                  Text_Regions.Copy_To
                    (Text_Region, Left.Text,
                     Scratch (1 .. Text_Regions.Length (Left.Text)),
                     Region_Result);
                  Good := Region_Result = Text_Regions.Operation_Ok;
                  if Good then
                     Text_Regions.Copy_To
                       (Text_Region, Right.Text,
                        Scratch (Text_Regions.Length (Left.Text) + 1 ..
                                   Text_Length), Region_Result);
                     Good := Region_Result = Text_Regions.Operation_Ok;
                  end if;
                  if not Good then
                     Eval_Status := Evaluation_Index_Error;
                  else
                     Text_Regions.Allocate_String
                       (Text_Region, Scratch (1 .. Text_Length), Item.Text,
                        Region_Result);
                     if Region_Result = Text_Regions.Operation_Ok then
                        Item.Kind := String_Type;
                     else
                        Eval_Status := Evaluation_Text_Storage_Exhausted;
                        Good := False;
                     end if;
                  end if;
               end if;
               Ok := Good;
            when To_String_Form =>
               Evaluate_Node (Tree.Nodes (Node_Index (Index)).First,
                              Depth + 1, Left, Good);
               if Good then
                  Text_Regions.Allocate_String
                    (Text_Region, Decimal_Image (Left.Scalar.Integer),
                     Item.Text, Region_Result);
                  if Region_Result = Text_Regions.Operation_Ok then
                     Item.Kind := String_Type;
                  else
                     Eval_Status := Evaluation_Text_Storage_Exhausted;
                     Good := False;
                  end if;
               end if;
               Ok := Good;
            when Host_Import_Form =>
               if not Host_Enabled then
                  Eval_Status := Host_Import_Required; Ok := False;
               else
                  declare
                     Operation : constant CCL.Catalog.Resolved_Operation :=
                       Tree.Nodes (Node_Index (Index)).Host_Call;
                     Binding : Unsigned_32;
                     Granted, Called : Boolean;
                     Argument : CCL.Host_Values.Value := CCL.Host_Values.Integer_Constant (0);
                     Returned : CCL.Host_Values.Value;
                  begin
                     Good := True;
                     if Operation.Parameters = 1 then
                        Evaluate_Node (Tree.Nodes (Node_Index (Index)).First,
                                       Depth + 1, Left, Good);
                        if Good then
                           if Left.Kind = String_Type then
                              Argument := (Kind => CCL.Host_Values.Text_Value, Content =>
                                (Length => Text_Regions.Length (Left.Text), others => <>));
                              Text_Regions.Copy_To
                                (Text_Region, Left.Text,
                                 Argument.Content.Data (1 .. Argument.Content.Length), Region_Result);
                              Good := Region_Result = Text_Regions.Operation_Ok;
                              if not Good then Eval_Status := Evaluation_Text_Storage_Exhausted; end if;
                           else Argument := CCL.Host_Values.From_Scalar (Left.Scalar);
                           end if;
                        end if;
                     end if;
                     if Good and then Left.Kind = Handler_Type then
                        declare
                           Ref : CCL.Handler_References.Reference;
                           Decl : constant Function_Declaration := Tree.Functions (Left.Handler_Id);
                        begin
                           CCL.Handler_References.Create
                             (Source, Decl.Identifier.Data (1 .. Decl.Identifier.Length), Ref, Good);
                           Argument := CCL.Host_Values.Handler_Constant (Ref);
                           if not Good then Eval_Status := Host_Contract_Unsupported; end if;
                        end;
                     end if;
                     if Good and then not CCL.Host_Values.Matches
                       (Argument, Operation.Import.Argument, Operation.Import.Argument_Text_Limit)
                     then
                        Eval_Status := Host_Argument_Out_Of_Bounds;
                        Good := False;
                     end if;
                     if Good then
                        CCL.Catalog.Find_Granted_Binding (Grants, Operation, Binding, Granted);
                        if not Granted then
                           Eval_Status := Host_Authority_Denied; Good := False;
                        else
                           Invoke (Context, Binding, Argument, Returned, Called);
                           if not Called then
                              Eval_Status := Host_Call_Failed; Good := False;
                           elsif not CCL.Host_Values.Matches
                             (Returned, Operation.Import.Result, Operation.Import.Result_Text_Limit)
                           then
                              Eval_Status := Host_Result_Type_Mismatch; Good := False;
                           else
                              case Returned.Kind is
                                 when CCL.Host_Values.Integer_Value =>
                                    Item.Kind := Integer_Type;
                                    Item.Scalar := CCL.VM.Integer_Constant (Returned.Integer);
                                 when CCL.Host_Values.Boolean_Value =>
                                    Item.Kind := Boolean_Type;
                                    Item.Scalar := CCL.VM.Boolean_Constant (Returned.Boolean);
                                 when CCL.Host_Values.Text_Value =>
                                    Item.Kind := String_Type;
                                    Text_Regions.Allocate_String
                                      (Text_Region, Returned.Content.Data (1 .. Returned.Content.Length),
                                       Item.Text, Region_Result);
                                    Good := Region_Result = Text_Regions.Operation_Ok;
                                    if not Good then Eval_Status := Evaluation_Text_Storage_Exhausted; end if;
                                 when CCL.Host_Values.Handler_Value =>
                                    Good := False; Eval_Status := Host_Result_Type_Mismatch;
                              end case;
                           end if;
                        end if;
                     end if;
                     if not Good then
                        Result.Diagnostic_Position := Tree.Nodes (Node_Index (Index)).Source_Position;
                     end if;
                     Ok := Good;
                  end;
               end if;
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

         Parse_Program (0, Root);
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
         if Diagnostic = No_Diagnostic and then Root_Type = Handler_Type then
            Diagnostic := Handler_Result_Not_Exportable;
            Diagnostic_Position := Tree.Nodes (Root).Source_Position;
         end if;
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
            when Invalid_Type | Handler_Type =>
               Result.Status := Type_Check_Failed;
               Result.Has_Value := False;
         end case;
      end if;
      Text_Regions.Clear (Text_Region);
   end Process_Source_With_Host;

   type No_Host is null record;
   procedure Deny_Host
     (Context : in out No_Host; Binding : Unsigned_32;
      Argument : CCL.Host_Values.Value; Value : out CCL.Host_Values.Value; Success : out Boolean)
   is
      pragma Unreferenced (Context, Binding, Argument);
   begin
      Value := CCL.Host_Values.Integer_Constant (0); Success := False;
   end Deny_Host;
   procedure Process_Without_Host is new Process_Source_With_Host (No_Host, Deny_Host);

   procedure Process_Source
     (Source : String; Fuel : Natural;
      Visible_Interfaces : CCL.Catalog.Interface_Catalog;
      Analyze_Input : Boolean; Evaluate : Boolean;
      Result : out Interpretation_Result; Tree : in out Syntax_Tree)
     with Post => Result.Fuel_Remaining <= Fuel
   is
      Grants : CCL.Catalog.Granted_Bindings;
      Context : No_Host;
   begin
      CCL.Catalog.Initialize (Grants);
      Process_Without_Host (Source, Fuel, Visible_Interfaces, Grants, Context,
                            False, Analyze_Input, Evaluate, Result, Tree);
   end Process_Source;

   procedure Admit
     (Tree : Syntax_Tree; Grants : CCL.Catalog.Granted_Bindings;
      Allow_Text : Boolean; Status : out Interpretation_Status;
      Position : out Source_Position) is
      Binding : Unsigned_32;
      Granted : Boolean;
   begin
      Status := Succeeded;
      Position := 0;
      -- Like VM linkage: reject the whole program before executing anything.
      for N of Tree.Nodes loop
         if N.Kind = Host_Import_Form then
            if (not Allow_Text and then not CCL.Host_Values.Scalar_Only (N.Host_Call.Import)) or else
              N.Host_Call.Import.Ownership_Argument or else
              N.Host_Call.Import.Transfer /= CCL.Imports.Copy_Argument or else
              N.Host_Call.Import.Cancellation /= CCL.Imports.Not_Cancellable or else
              N.Host_Call.Import.Success_Verb /= 0 or else
              N.Host_Call.Import.Failure_Verb /= 0 or else N.Host_Call.Import.Cancel_Verb /= 0
            then
               Status := Host_Contract_Unsupported;
               Position := N.Source_Position; return;
            end if;
            CCL.Catalog.Find_Granted_Binding (Grants, N.Host_Call, Binding, Granted);
            if not Granted then
               Status := Host_Authority_Denied;
               Position := N.Source_Position; return;
            end if;
         end if;
      end loop;
   end Admit;

   procedure Interpret_With_Values
     (Source : String; Fuel : Natural;
      Visible_Interfaces : CCL.Catalog.Interface_Catalog;
      Grants : CCL.Catalog.Granted_Bindings;
      Context : in out Host_Context;
      Result : out Interpretation_Result)
   is
      Analysis : Analysis_Result;
      Tree : Syntax_Tree;
      procedure Run is new Process_Source_With_Host (Host_Context, Invoke);
   begin
      Analyze (Source, Visible_Interfaces, Analysis);
      Result := (Fuel_Remaining => Fuel, others => <>);
      if Analysis.Status /= Analysis_Succeeded then
         Result.Status := (if Analysis.Status = Analysis_Type_Check_Failed then Type_Check_Failed else Parse_Failed);
         Result.Diagnostic := Analysis.Diagnostic;
         Result.Diagnostic_Position := Analysis.Diagnostic_Position;
         return;
      end if;
      Tree := Analysis.Tree;
      Admit (Tree, Grants, Allow_Text, Result.Status, Result.Diagnostic_Position);
      if Result.Status /= Succeeded then return; end if;
      Run (Source, Fuel, Visible_Interfaces, Grants, Context, True, False, True, Result, Tree);
   end Interpret_With_Values;

   procedure Interpret_With_Host
     (Source : String; Fuel : Natural;
      Visible_Interfaces : CCL.Catalog.Interface_Catalog;
      Grants : CCL.Catalog.Granted_Bindings;
      Context : in out Host_Context; Result : out Interpretation_Result)
   is
      procedure Invoke_Scalar
        (Context : in out Host_Context; Binding : Unsigned_32;
         Argument : CCL.Host_Values.Value; Value : out CCL.Host_Values.Value;
         Success : out Boolean)
      is
         A, R : CCL.VM.Value;
      begin
         CCL.Host_Values.To_Scalar (Argument, A, Success);
         Value := CCL.Host_Values.Integer_Constant (0);
         if Success then Invoke (Context, Binding, A, R, Success); Value := CCL.Host_Values.From_Scalar (R); end if;
      end Invoke_Scalar;
      procedure Run is new Interpret_With_Values (Host_Context, Invoke_Scalar, Allow_Text => False);
   begin
      Run (Source, Fuel, Visible_Interfaces, Grants, Context, Result);
   end Interpret_With_Host;

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
         Tree => Tree, others => <>);
      if Source'Length <= MAX_SOURCE_LENGTH then
         Result.Source_Length := Source'Length;
         Result.Source_Text (1 .. Source'Length) := Source;
      end if;
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

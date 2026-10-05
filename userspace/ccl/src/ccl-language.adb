with Interfaces; use Interfaces;
with CCL.Ownership;

package body CCL.Language with
   SPARK_Mode => On
is
   use type CCL.Types.Type_Reference;
   use type CCL.Types.Definition_Result;
   use type CCL.Host_Values.Value_Kind;
   use type CCL.Streams.View_Kind;
   use type CCL.Types.Shape;
   use type CCL.Ownership.Ownership_Mode;

   function Analysis_Status_Of
     (Result : Analysis_Result) return Analysis_Status is (Result.Status);

   function Analysis_Diagnostic
     (Result : Analysis_Result) return Diagnostic_Code is (Result.Diagnostic);

   function Analysis_Diagnostic_Subject
     (Result : Analysis_Result) return Name is (Result.Diagnostic_Subject);
   function Analysis_Diagnostic_Expected (Result : Analysis_Result) return Name is
     (Result.Diagnostic_Expected);
   function Analysis_Diagnostic_Found (Result : Analysis_Result) return Name is
     (Result.Diagnostic_Found);

   function Analysis_Diagnostic_Position
     (Result : Analysis_Result) return Natural is
     (Result.Diagnostic_Position);

   function Analysis_Node_Count
     (Result : Analysis_Result) return Node_Count is (Result.Tree.Length);

   function Analysis_Root
     (Result : Analysis_Result) return Node_Reference is (Result.Tree.Root);
   function Analysis_Literal
     (Result : Analysis_Result;
      Index  : Node_Index) return String is
     (if Index < Result.Tree.Length and then Result.Tree.Nodes (Index).Kind = String_Literal
      then Result.Tree.Text_Data (Result.Tree.Nodes (Index).Text_First ..
                                  Result.Tree.Nodes (Index).Text_Last)
      else "");
   function Analysis_Types (Result : Analysis_Result) return CCL.Types.Registry is (Result.Tree.Types);
   function Analysis_Resource_Policies (Result : Analysis_Result)
     return CCL.Resource_Policies.Policy_Table is (Result.Resource_Policies);

   function Analysis_Function_Count (Result : Analysis_Result) return Natural is
     (Result.Tree.Function_Count);
   function Analysis_Function
     (Result : Analysis_Result; Id : Function_Index) return Function_Declaration is
     (Result.Tree.Functions (Id));

   function Analysis_Node
     (Result : Analysis_Result;
      Index  : Node_Index) return Node is (Result.Tree.Nodes (Index));

   type Type_Binding is record
      Identifier : Name;
      Kind       : Static_Type := Invalid_Type;
   end record;

   type Type_Environment is
     array (Natural range 0 .. MAX_BINDINGS - 1) of Type_Binding;


   function Is_Name_Character (Item : Character) return Boolean is
     ((Item >= 'a' and then Item <= 'z') or else
      (Item >= 'A' and then Item <= 'Z') or else
      (Item >= '0' and then Item <= '9') or else
      Item = '-' or else Item = '_' or else Item = '.' or else Item = '?' or else
      Item = '+' or else Item = '=' or else Item = '*' or else
      Item = '/' or else Item = '%' or else Item = '<' or else Item = '>');

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


   --  Parse and type-check Source: the front end every program goes
   --  through before it is compiled (CCL.Compiler) and run (CCL.Evaluation).
   procedure Check_Source
     (Source : String;
      Visible_Interfaces : CCL.Catalog.Interface_Catalog;
      Result : out Interpretation_Result; Tree : out Syntax_Tree)
   is
      Cursor     : Natural := 0;
      Root       : Node_Reference := NO_NODE;
      Diagnostic : Diagnostic_Code := No_Diagnostic;
      --  The type whose declaration is being read: inside it, its own name
      --  may appear only as a list element ((List Self)), which Read_Type
      --  reports through Self_List_Read.
      Declaring : Name;
      Self_List_Read : Boolean := False;
      Diagnostic_Position : Natural range 0 .. MAX_SOURCE_LENGTH + 1 := 0;
      Diagnostic_Subject : Name;
      Diagnostic_Expected, Diagnostic_Found : Name;
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

      --  The elements of [a b c] or (list a b c), up to Close. Both are the
      --  same List_Construct: brackets are the collection spelling.
      procedure Parse_Elements
        (Depth : Natural; Close : Character; Index : out Node_Reference);

      --  An integer literal's value: optional '-', decimal digits, no
      --  overflow. Sets Invalid_Integer and Ok := False otherwise.
      procedure Read_Integer_Value (Item : out Integer_64; Ok : out Boolean) is
         Negative  : Boolean := False;
         Magnitude : Unsigned_64 := 0;
         Digit     : Unsigned_64;
         Started   : Boolean := False;
         Limit     : Unsigned_64 := Unsigned_64 (Integer_64'Last);
      begin
         Item := 0;
         Ok := False;
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
               return;
            end if;
            Magnitude := Magnitude * 10 + Digit;
            Cursor := Cursor + 1;
         end loop;

         if not Started then
            Diagnostic := Invalid_Integer;
            return;
         elsif Negative and then Magnitude = Limit then
            Item := Integer_64'First;
         elsif Negative then
            Item := -Integer_64 (Magnitude);
         else
            Item := Integer_64 (Magnitude);
         end if;
         Ok := True;
      end Read_Integer_Value;

      --  A record field's default: an integer, true or false, a qualified
      --  nullary alternative (Color.Red), or [] for an empty list. Define
      --  checks that it is a constant of the field's type.
      procedure Read_Default (Payload : Static_Type; Default : out CCL.Types.Field_Default) is
         Value : Integer_64;
         Item : Name;
         Ok : Boolean;
         Owner : Static_Type;
         Choice : CCL.Types.Component_Count;
      begin
         Default := CCL.Types.No_Field_Default;
         if Source (Source'First + Cursor) = '[' then
            Cursor := Cursor + 1;
            Expect (']', Ok);
            if Ok then
               Default := (Kind => CCL.Types.Empty_List_Default, Value => 0);
            end if;
         elsif Source (Source'First + Cursor) = '-' or else
           Source (Source'First + Cursor) in '0' .. '9'
         then
            Read_Integer_Value (Value, Ok);
            if Ok then
               Default := (Kind => CCL.Types.Integer_Default, Value => Value);
            end if;
         else
            Read_Name (Item, Ok);
            if not Ok then
               Diagnostic := Invalid_Field_Default;
            elsif Name_Is (Item, "true") or else Name_Is (Item, "false") then
               Default := (Kind => CCL.Types.Boolean_Default, Value => (if Name_Is (Item, "true") then 1 else 0));
            else
               CCL.Types.Resolve_Alternative (Tree.Types, Item, Owner, Choice);
               if Choice = 0 or else Owner /= Payload then
                  Diagnostic := Invalid_Field_Default;
               else
                  Default := (Kind => CCL.Types.Alternative_Default, Value => Integer_64 (Choice));
               end if;
            end if;
         end if;
      end Read_Default;

      --  Whether the next tokens are a name and =>, as in (Limits name => "a"):
      --  the start of a named association. Moves nothing.
      function Named_Association_Ahead return Boolean is
         Probe : Natural := Cursor;
      begin
         if Probe >= Source'Length or else not Is_Name_Character (Source (Source'First + Probe)) then
            return False;
         end if;
         while Probe < Source'Length and then Is_Name_Character (Source (Source'First + Probe)) loop
            Probe := Probe + 1;
         end loop;
         while Probe < Source'Length and then Source (Source'First + Probe) in ' ' | ASCII.HT | ASCII.LF | ASCII.CR loop
            Probe := Probe + 1;
         end loop;
         return Probe + 1 < Source'Length and then Source (Source'First + Probe .. Source'First + Probe + 1) = "=>"
           and then (Probe + 2 = Source'Length or else not Is_Name_Character (Source (Source'First + Probe + 2)));
      end Named_Association_Ahead;

      procedure Parse_Integer (Index : out Node_Reference) is
         Item : Integer_64;
         Ok : Boolean;
      begin
         Index := NO_NODE;
         Read_Integer_Value (Item, Ok);
         if Ok then
            Add_Node
              ((Kind => Integer_Literal, Integer_Value => Item, others => <>),
               Index);
         end if;
      end Parse_Integer;

      procedure Parse_String (Index : out Node_Reference) is
         Start  : constant Natural := Tree.Text_Bytes_Used;
         Closed : Boolean := False;
         Item   : Character;

         procedure Append (Value : Character) is
         begin
            if Tree.Text_Bytes_Used = MAX_PROGRAM_TEXT then
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

      --  A type name, or a function type (Function (Parameter ...) Result).
      procedure Read_Type
        (Kind : out Static_Type; Allow_Unit : Boolean := False; Depth : Natural := 0) is
         Token : Name;
         Good : Boolean;
      begin
         Kind := Invalid_Type;
         Skip_Trivia;
         if Cursor < Source'Length and then Source (Source'First + Cursor) = '(' then
            if Depth >= MAX_NESTING then Diagnostic := Nesting_Too_Deep; return; end if;
            Cursor := Cursor + 1;
            Read_Name (Token, Good);
            if Good and then Name_Is (Token, "List") then
               --  (List T): the list of T elements.
               declare
                  Element : Static_Type;
                  Specialized : CCL.Types.List_Result;
                  Saved : constant Natural := Cursor;
                  Self : Name;
               begin
                  if Declaring.Length > 0 then
                     --  (List Self) inside Self's declaration: completed once
                     --  Self is defined (CCL.Types.Complete_Self_List).
                     Skip_Trivia;
                     if Cursor < Source'Length and then
                       Is_Name_Character (Source (Source'First + Cursor))
                     then
                        Read_Name (Self, Good);
                        if Good and then Names_Equal (Self, Declaring) then
                           Expect (')', Good);
                           if Good then
                              Kind := Unit_Type;
                              Self_List_Read := True;
                           end if;
                           return;
                        end if;
                     end if;
                     Cursor := Saved;
                  end if;
                  Read_Type (Element, Depth => Depth + 1);
                  if Diagnostic /= No_Diagnostic then return; end if;
                  Expect (')', Good);
                  if not Good then return; end if;
                  CCL.Types.Specialize_List (Tree.Types, Element, Kind, Specialized);
                  if Specialized not in CCL.Types.List_Specialized |
                    CCL.Types.List_Already_Specialized
                  then
                     Diagnostic := Unsupported_List_Element;
                  end if;
               end;
               return;
            elsif Good and then Name_Is (Token, "Stream") then
               --  (Stream T): a live source of T elements (docs/ccl-streams.md).
               declare
                  Element : Static_Type;
                  Specialized : CCL.Types.Stream_Result;
               begin
                  Read_Type (Element, Depth => Depth + 1);
                  if Diagnostic /= No_Diagnostic then return; end if;
                  Expect (')', Good);
                  if not Good then return; end if;
                  CCL.Types.Specialize_Stream (Tree.Types, Element, Kind, Specialized);
                  if Specialized not in CCL.Types.Stream_Specialized |
                    CCL.Types.Stream_Already_Specialized
                  then
                     Diagnostic := Unsupported_Stream_Element;
                  end if;
               end;
               return;
            elsif Good and then Name_Is (Token, "Task") then
               --  (Task T): a T that arrives later (docs/control-language.md).
               declare
                  Result_Type : Static_Type;
                  Specialized : CCL.Types.Stream_Result;
               begin
                  Read_Type (Result_Type, Depth => Depth + 1);
                  if Diagnostic /= No_Diagnostic then return; end if;
                  Expect (')', Good);
                  if not Good then return; end if;
                  CCL.Types.Specialize_Task (Tree.Types, Result_Type, Kind, Specialized);
                  if Specialized not in CCL.Types.Stream_Specialized |
                    CCL.Types.Stream_Already_Specialized
                  then
                     Diagnostic := Unsupported_Stream_Element;
                  end if;
               end;
               return;
            elsif not Good or else not Name_Is (Token, "Function") then
               Diagnostic := Expected_Type_Name; return;
            end if;
            Expect ('(', Good);
            if not Good then return; end if;
            declare
               Parameters : CCL.Types.Function_Parameters := [others => Invalid_Type];
               Count : CCL.Types.Function_Parameter_Count := 0;
               Result_Kind : Static_Type;
               Specialized : CCL.Types.Function_Result;
            begin
               loop
                  Skip_Trivia;
                  exit when Cursor >= Source'Length or else Source (Source'First + Cursor) = ')';
                  if Count = CCL.Types.Maximum_Function_Parameters then
                     Diagnostic := Too_Many_Parameters; return;
                  end if;
                  Count := Count + 1;
                  Read_Type (Parameters (Count), Depth => Depth + 1);
                  if Diagnostic /= No_Diagnostic then return; end if;
               end loop;
               Expect (')', Good);
               if not Good then return; end if;
               Read_Type (Result_Kind, Allow_Unit => True, Depth => Depth + 1);
               if Diagnostic /= No_Diagnostic then return; end if;
               Expect (')', Good);
               if not Good then return; end if;
               CCL.Types.Specialize_Function
                 (Tree.Types, Parameters, Count, Result_Kind, Kind, Specialized);
               if Specialized not in CCL.Types.Function_Specialized |
                 CCL.Types.Function_Already_Specialized
               then
                  Diagnostic := Expected_Type_Name;
               end if;
            end;
            return;
         end if;
         Read_Name (Token, Good);
         if Good then
            Kind := CCL.Types.Find (Tree.Types, Token);
            if Kind in Invalid_Type | Handler_Type or else (Kind = Unit_Type and not Allow_Unit) then
               Diagnostic := Expected_Type_Name;
            end if;
         end if;
      end Read_Type;

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
         Variant_Type : Static_Type;
         Choice : CCL.Types.Component_Count;
         Record_Type : Static_Type;
         Components : Component_Node_Array := [others => NO_NODE];
         List_Start : constant Natural := Cursor;
      begin
         Index := NO_NODE;
         if Depth >= MAX_NESTING then
            Diagnostic := Nesting_Too_Deep;
            return;
         end if;
         Read_Name (Operator_Name, Ok);
         if not Ok then
            return;
         end if;

         Index := NO_NODE;
         if Name_Is (Operator_Name, "field") then
            Parse_Expression (Depth + 1, A);
            if Diagnostic /= No_Diagnostic then return; end if;
            Read_Name (Binding_Name, Ok); if not Ok then return; end if;
            Expect (')', Ok);
            if Diagnostic = No_Diagnostic and then Ok then
               Add_Node ((Kind => Field_Form, First => A, Identifier => Binding_Name, others => <>), Index);
            end if;
         elsif Name_Is (Operator_Name, "match") then
            Parse_Expression (Depth + 1, A);
            declare
               Previous, Arm : Node_Reference := NO_NODE;
               Pattern_Name, Bound_Name : Name;
               Arms : CCL.Types.Component_Count := 0;
            begin
               Skip_Trivia;
               while Diagnostic = No_Diagnostic and then Cursor < Source'Length and then
                 Source (Source'First + Cursor) /= ')'
               loop
                  if Arms = CCL.Types.Maximum_Components then
                     Diagnostic := Invalid_Match_Pattern; return;
                  end if;
                  Arms := Arms + 1;
                  Expect ('(', Ok); if not Ok then return; end if;
                  Expect ('(', Ok); if not Ok then return; end if;
                  Read_Name (Pattern_Name, Ok); if not Ok then return; end if;
                  CCL.Types.Resolve_Alternative (Tree.Types, Pattern_Name, Variant_Type, Choice);
                  if Choice = 0 then Diagnostic := Invalid_Match_Pattern; return; end if;
                  Bound_Name := (others => <>);
                  Skip_Trivia;
                  if Cursor < Source'Length and then Source (Source'First + Cursor) /= ')' then
                     Read_Name (Bound_Name, Ok); if not Ok then return; end if;
                  end if;
                  Expect (')', Ok); if not Ok then return; end if;
                  Parse_Expression (Depth + 1, C);
                  if Diagnostic /= No_Diagnostic then return; end if;
                  Expect (')', Ok); if not Ok then return; end if;
                  Add_Node ((Kind => Match_Arm, Identifier => Bound_Name, Pattern => Pattern_Name,
                    Declared_Kind => Variant_Type, Alternative => Choice, First => C, others => <>), Arm);
                  if Diagnostic /= No_Diagnostic then return; end if;
                  if Previous = NO_NODE then B := Arm;
                  else Tree.Nodes (Previous).Second := Arm; end if;
                  Previous := Arm;
                  Skip_Trivia;
               end loop;
               Expect (')', Ok);
               if Diagnostic = No_Diagnostic and then Ok then
                  Add_Node ((Kind => Match_Form, First => A, Second => B, others => <>), Index);
               end if;
            end;
         elsif Name_Is (Operator_Name, "+") or else
           Name_Is (Operator_Name, "add") or else
           Name_Is (Operator_Name, "-") or else
           Name_Is (Operator_Name, "subtract") or else
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
                      elsif Name_Is (Operator_Name, "-") or else
                        Name_Is (Operator_Name, "subtract")
                      then Subtract_Form
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
         elsif Name_Is (Operator_Name, "/=") or else
           Name_Is (Operator_Name, "not-equal") or else
           Name_Is (Operator_Name, "<") or else
           Name_Is (Operator_Name, "less") or else
           Name_Is (Operator_Name, "<=") or else
           Name_Is (Operator_Name, "less-equal") or else
           Name_Is (Operator_Name, ">") or else
           Name_Is (Operator_Name, "greater") or else
           Name_Is (Operator_Name, ">=") or else
           Name_Is (Operator_Name, "greater-equal") or else
           Name_Is (Operator_Name, "and") or else
           Name_Is (Operator_Name, "or")
         then
            Parse_Expression (Depth + 1, A);
            Parse_Expression (Depth + 1, B);
            Expect (')', Ok);
            if Diagnostic = No_Diagnostic and then Ok then
               Add_Node
                 ((Kind =>
                     (if Name_Is (Operator_Name, "/=") or else
                         Name_Is (Operator_Name, "not-equal")
                      then Not_Equal_Form
                      elsif Name_Is (Operator_Name, "<") or else
                        Name_Is (Operator_Name, "less")
                      then Less_Form
                      elsif Name_Is (Operator_Name, "<=") or else
                        Name_Is (Operator_Name, "less-equal")
                      then Less_Equal_Form
                      elsif Name_Is (Operator_Name, ">") or else
                        Name_Is (Operator_Name, "greater")
                      then Greater_Form
                      elsif Name_Is (Operator_Name, ">=") or else
                        Name_Is (Operator_Name, "greater-equal")
                      then Greater_Equal_Form
                      elsif Name_Is (Operator_Name, "and")
                      then And_Form
                      else Or_Form),
                   First => A, Second => B, others => <>),
                  Index);
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
         elsif (for some Operation in Builtin_Operation range Each_Builtin .. Builtin_Operation'Last =>
                  Name_Is (Operator_Name, Builtin_Name (Operation))) and then
           --  A function the program defines shadows a builtin of that name.
           not (for some F in 0 .. Tree.Function_Count - 1 =>
                  Names_Equal (Tree.Functions (F).Identifier, Operator_Name))
         then
            declare
               Operation : Builtin_Operation := No_Builtin;
               Operands : Argument_Array := [others => NO_NODE];
               Count : Parameter_Count := 0;
               Good : Boolean;
            begin
               for Candidate in Builtin_Operation range Each_Builtin .. Builtin_Operation'Last loop
                  if Name_Is (Operator_Name, Builtin_Name (Candidate)) then
                     Operation := Candidate;
                  end if;
               end loop;
               loop
                  Skip_Trivia;
                  exit when Cursor >= Source'Length or else Source (Source'First + Cursor) = ')';
                  if Count = MAX_PARAMETERS then Diagnostic := Too_Many_Parameters; return; end if;
                  Count := Count + 1;
                  Parse_Expression (Depth + 1, Operands (Count));
                  if Diagnostic /= No_Diagnostic then return; end if;
               end loop;
               Expect (')', Good);
               if not Good then return; end if;
               Add_Node ((Kind => Builtin_Form, Builtin => Operation, Arguments => Operands,
                          Argument_Count => Count, others => <>), Index);
            end;
         elsif Name_Is (Operator_Name, "->>") then
            --  (->> x stage ...): thread x through each stage as its last
            --  argument; a bare name f is the stage (f x).
            declare
               Piped_Value, Stage : Node_Reference := NO_NODE;
               Stage_Name : Name;
               Name_Start : Natural := 0;
               Good : Boolean;
               Operation : Builtin_Operation;
            begin
               Parse_Expression (Depth + 1, Piped_Value);
               if Diagnostic /= No_Diagnostic then return; end if;
               loop
                  Skip_Trivia;
                  exit when Cursor >= Source'Length or else Source (Source'First + Cursor) = ')';
                  if Source (Source'First + Cursor) = '(' then
                     Parse_Expression (Depth + 1, Stage);
                     if Diagnostic /= No_Diagnostic then return; end if;
                     if Stage < Tree.Length and then
                       Tree.Nodes (Stage).Kind in Builtin_Form | Function_Call and then
                       Tree.Nodes (Stage).Argument_Count < MAX_PARAMETERS
                     then
                        Tree.Nodes (Stage).Argument_Count := Tree.Nodes (Stage).Argument_Count + 1;
                        Tree.Nodes (Stage).Arguments (Tree.Nodes (Stage).Argument_Count) := Piped_Value;
                        Tree.Nodes (Stage).Piped := True;
                     else
                        --  A stage is a call that can take one more argument.
                        Diagnostic := Unexpected_Token; return;
                     end if;
                  else
                     Name_Start := Cursor;
                     Read_Name (Stage_Name, Good);
                     if not Good then return; end if;
                     Operation := No_Builtin;
                     for Candidate in Builtin_Operation range Each_Builtin .. Builtin_Operation'Last loop
                        if Name_Is (Stage_Name, Builtin_Name (Candidate)) then
                           Operation := Candidate;
                        end if;
                     end loop;
                     if Operation /= No_Builtin and then
                       not (for some F in 0 .. Tree.Function_Count - 1 =>
                              Names_Equal (Tree.Functions (F).Identifier, Stage_Name))
                     then
                        Add_Node ((Kind => Builtin_Form, Builtin => Operation,
                                   Arguments => [1 => Piped_Value, others => NO_NODE],
                                   Argument_Count => 1, Piped => True, others => <>), Stage);
                     elsif Name_Is (Stage_Name, "length") then
                        Add_Node ((Kind => String_Length_Form, First => Piped_Value, Piped => True,
                                   others => <>), Stage);
                     elsif Name_Is (Stage_Name, "to-string") then
                        Add_Node ((Kind => To_String_Form, First => Piped_Value, Piped => True,
                                   others => <>), Stage);
                     else
                        Add_Node ((Kind => Function_Call, Identifier => Stage_Name,
                                   Arguments => [1 => Piped_Value, others => NO_NODE],
                                   Argument_Count => 1, Piped => True, others => <>), Stage);
                     end if;
                     if Diagnostic /= No_Diagnostic then return; end if;
                     --  A bare stage spans its name.
                     if Stage < Tree.Length then
                        Tree.Nodes (Stage).Source_Position := To_Diagnostic_Position (Name_Start);
                        Tree.Nodes (Stage).Source_End_Position := To_Diagnostic_Position (Cursor);
                     end if;
                  end if;
                  Piped_Value := Stage;
               end loop;
               Expect (')', Good);
               if not Good then return; end if;
               Index := Piped_Value;
            end;
         elsif Name_Is (Operator_Name, "fn") then
            --  (fn ((x T) ...) body): lifted into a generated function, named
            --  so that source can never spell it ('#' begins a comment).
            declare
               Decl : Function_Declaration;
               Id : Function_Index;
               Good : Boolean;
            begin
               if Tree.Function_Count = MAX_FUNCTIONS then
                  Diagnostic := Too_Many_Functions; return;
               end if;
               Id := Tree.Function_Count;
               Tree.Function_Count := Id + 1;
               Decl.Identifier := CCL.Types.Named ("fn#" & Natural'Image (Id));
               Expect ('(', Good);
               if not Good then return; end if;
               loop
                  Skip_Trivia;
                  exit when Cursor >= Source'Length or else Source (Source'First + Cursor) = ')';
                  if Decl.Count = MAX_PARAMETERS then Diagnostic := Too_Many_Parameters; return; end if;
                  Decl.Count := Decl.Count + 1;
                  if Source (Source'First + Cursor) = '(' then
                     Cursor := Cursor + 1;
                     Read_Name (Decl.Parameters (Decl.Count).Identifier, Good);
                     if not Good then return; end if;
                     Read_Type (Decl.Parameters (Decl.Count).Kind);
                     if Diagnostic /= No_Diagnostic then return; end if;
                     Expect (')', Good);
                     if not Good then return; end if;
                  else
                     --  Untyped: inferred from where the function is passed.
                     Read_Name (Decl.Parameters (Decl.Count).Identifier, Good);
                     if not Good then return; end if;
                     Decl.Parameters (Decl.Count).Declared := False;
                  end if;
               end loop;
               Expect (')', Good);
               if not Good then return; end if;
               Parse_Expression (Depth + 1, Decl.Body_Node);
               if Diagnostic /= No_Diagnostic then return; end if;
               Expect (')', Good);
               if not Good then return; end if;
               Tree.Functions (Id) := Decl;
               Add_Node ((Kind => Lambda_Form, Function_Id => Id,
                          First => Decl.Body_Node, others => <>), Index);
            end;
         elsif Name_Is (Operator_Name, "list") then
            Parse_Elements (Depth, ')', Index);
         elsif Name_Is (Operator_Name, "list-of") then
            --  (list-of T): the empty List<T>, the spelling an empty list
            --  needs when nothing else gives its element type.
            declare
               Element, List_Type : Static_Type;
               Specialized : CCL.Types.List_Result;
            begin
               Read_Type (Element, Depth => Depth + 1);
               if Diagnostic /= No_Diagnostic then return; end if;
               Expect (')', Ok);
               if not Ok then return; end if;
               CCL.Types.Specialize_List (Tree.Types, Element, List_Type, Specialized);
               if Specialized not in CCL.Types.List_Specialized |
                 CCL.Types.List_Already_Specialized
               then
                  Diagnostic := Unsupported_List_Element; return;
               end if;
               Add_Node ((Kind => List_Construct, Element_Count => 0,
                          Declared_Kind => List_Type, others => <>), Index);
            end;
         elsif Name_Is (Operator_Name, "task") then
            --  (task T n): task n of this session, whose result is a T; as
            --  (stream T n), n names an entry of the session's table and
            --  grants nothing.
            declare
               Result_Type, Task_Type : Static_Type;
               Specialized : CCL.Types.Stream_Result;
            begin
               Read_Type (Result_Type, Depth => Depth + 1);
               if Diagnostic /= No_Diagnostic then return; end if;
               CCL.Types.Specialize_Task (Tree.Types, Result_Type, Task_Type, Specialized);
               if Specialized not in CCL.Types.Stream_Specialized |
                 CCL.Types.Stream_Already_Specialized
               then
                  Diagnostic := Unsupported_Stream_Element; return;
               end if;
               Parse_Expression (Depth + 1, A);
               if Diagnostic /= No_Diagnostic then return; end if;
               if A = NO_NODE or else Tree.Nodes (A).Kind /= Integer_Literal then
                  Diagnostic := Expected_Stream; return;
               elsif Tree.Nodes (A).Integer_Value not in 1 .. CCL.Streams.Maximum_Handle then
                  Diagnostic := Value_Out_Of_Range; return;
               end if;
               Expect (')', Ok);
               if not Ok then return; end if;
               Add_Node ((Kind => Stream_Reference, Declared_Kind => Task_Type,
                          Integer_Value => Tree.Nodes (A).Integer_Value, others => <>), Index);
            end;
         elsif Name_Is (Operator_Name, "stream") then
            --  (stream T n): stream n of this session, whose elements are T.
            --  n names an entry in the session's own table and grants
            --  nothing; every element read is checked against T.
            declare
               Element, Stream_Type : Static_Type;
               Specialized : CCL.Types.Stream_Result;
            begin
               Read_Type (Element, Depth => Depth + 1);
               if Diagnostic /= No_Diagnostic then return; end if;
               CCL.Types.Specialize_Stream (Tree.Types, Element, Stream_Type, Specialized);
               if Specialized not in CCL.Types.Stream_Specialized |
                 CCL.Types.Stream_Already_Specialized
               then
                  Diagnostic := Unsupported_Stream_Element; return;
               end if;
               Parse_Expression (Depth + 1, A);
               if Diagnostic /= No_Diagnostic then return; end if;
               if A = NO_NODE or else Tree.Nodes (A).Kind /= Integer_Literal then
                  Diagnostic := Expected_Stream; return;
               elsif Tree.Nodes (A).Integer_Value not in 1 .. CCL.Streams.Maximum_Handle then
                  Diagnostic := Value_Out_Of_Range; return;
               end if;
               Expect (')', Ok);
               if not Ok then return; end if;
               Add_Node ((Kind => Stream_Reference, Declared_Kind => Stream_Type,
                          Integer_Value => Tree.Nodes (A).Integer_Value, others => <>), Index);
            end;
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
            CCL.Types.Resolve_Alternative (Tree.Types, Operator_Name, Variant_Type, Choice);
            Record_Type := CCL.Types.Find (Tree.Types, Operator_Name);
            if CCL.Types.Describe (Tree.Types, Record_Type).Form = CCL.Types.Product then
               if Host_Found then Diagnostic := Duplicate_Declaration; return; end if;
               --  Positional fields first, then :name value pairs in any
               --  order; a field left out takes its default. The node is
               --  positional either way (docs/ccl-launch-parameters.md).
               declare
                  Shape : constant CCL.Types.Description := CCL.Types.Describe (Tree.Types, Record_Type);
                  Given, Named_Fields, Defaulted_Fields : Field_Flags := [others => False];
                  Position : CCL.Types.Component_Count := 0;
                  Field_Name : Name;
                  Field : CCL.Types.Component_Count;
                  Field_Start : Natural;
               begin
                  --  Positional fields, then field => value pairs (Ada's named
                  --  association; positional ones may not follow).
                  loop
                     Skip_Trivia;
                     exit when Cursor >= Source'Length or else Source (Source'First + Cursor) = ')';
                     Field_Start := Cursor;
                     if Named_Association_Ahead then
                        Read_Name (Field_Name, Ok);
                        Skip_Trivia;
                        Cursor := Cursor + 2;
                        Field := 0;
                        for P in 1 .. Shape.Count loop
                           if Names_Equal (Shape.Parts (P).Identifier, Field_Name) then Field := P; end if;
                        end loop;
                        if Field = 0 or else Given (Field) then
                           Diagnostic := (if Field = 0 then Unknown_Field_Argument else Repeated_Field_Argument);
                           Diagnostic_Position := To_Diagnostic_Position (Field_Start);
                           Diagnostic_Subject := Field_Name;
                           return;
                        end if;
                        Parse_Expression (Depth + 1, Components (Field));
                        if Diagnostic /= No_Diagnostic then return; end if;
                        Given (Field) := True;
                        Named_Fields (Field) := True;
                     else
                        if (for some F of Named_Fields => F) then
                           Diagnostic := Positional_After_Named;
                           Diagnostic_Position := To_Diagnostic_Position (Field_Start);
                           return;
                        end if;
                        exit when Position = Shape.Count;
                        Position := Position + 1;
                        Parse_Expression (Depth + 1, Components (Position));
                        if Diagnostic /= No_Diagnostic then return; end if;
                        Given (Position) := True;
                     end if;
                  end loop;
                  for P in 1 .. Shape.Count loop
                     if not Given (P) then
                        Defaulted_Fields (P) := True;
                        case CCL.Types.Default_Of (Tree.Types, Record_Type, P).Kind is
                           when CCL.Types.No_Default =>
                              Diagnostic := Missing_Field_Argument;
                              Diagnostic_Position := To_Diagnostic_Position (List_Start);
                              Diagnostic_Subject := Shape.Parts (P).Identifier;
                              return;
                           when CCL.Types.Integer_Default =>
                              Add_Node ((Kind => Integer_Literal,
                                         Integer_Value => CCL.Types.Default_Of (Tree.Types, Record_Type, P).Value,
                                         others => <>), Components (P));
                           when CCL.Types.Boolean_Default =>
                              Add_Node ((Kind => Boolean_Literal,
                                         Boolean_Value => CCL.Types.Default_Of (Tree.Types, Record_Type, P).Value = 1,
                                         others => <>), Components (P));
                           when CCL.Types.Alternative_Default =>
                              declare
                                 Owner : constant CCL.Types.Description :=
                                   CCL.Types.Describe (Tree.Types, Shape.Parts (P).Payload);
                                 Member : constant Name := Owner.Parts (CCL.Types.Component_Index
                                   (CCL.Types.Default_Of (Tree.Types, Record_Type, P).Value)).Identifier;
                              begin
                                 if Owner.Identifier.Length + 1 + Member.Length > MAX_NAME_LENGTH then
                                    Diagnostic := Invalid_Field_Default; return;
                                 end if;
                                 Add_Node ((Kind => Name_Reference,
                                            Identifier => CCL.Types.Named
                                              (CCL.Types.Image (Owner.Identifier) & "." & CCL.Types.Image (Member)),
                                            others => <>), Components (P));
                              end;
                           when CCL.Types.Empty_List_Default =>
                              Add_Node ((Kind => List_Construct, Element_Count => 0,
                                         Declared_Kind => Shape.Parts (P).Payload, others => <>), Components (P));
                        end case;
                        if Diagnostic /= No_Diagnostic then return; end if;
                     end if;
                  end loop;
                  Expect (')', Ok);
                  if Ok then
                     Add_Node ((Kind => Record_Construct, Identifier => Operator_Name,
                       Declared_Kind => Record_Type, Components => Components,
                       Named_Fields => Named_Fields, Defaulted_Fields => Defaulted_Fields, others => <>), Index);
                  end if;
               end;
            elsif Choice > 0 then
               if Host_Found then Diagnostic := Duplicate_Declaration; return; end if;
               if CCL.Types.Describe (Tree.Types, Variant_Type).Parts (Choice).Payload = Unit_Type then
                  -- Nullary alternatives are values, not function calls.
                  Diagnostic := Unknown_Form; return;
               end if;
               Parse_Expression (Depth + 1, A);
               if Diagnostic /= No_Diagnostic then return; end if;
               Expect (')', Ok);
               if Ok then
                  Add_Node ((Kind => Variant_Construct, Identifier => Operator_Name,
                    Declared_Kind => Variant_Type, Alternative => Choice, First => A, others => <>), Index);
               end if;
            elsif not Host_Found and then
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
               if CCL.Host_Values.Has_Receiver (Host_Call.Import) then
                  Parse_Expression (Depth + 1, A);
                  if Diagnostic = No_Diagnostic and then Host_Call.Parameters = 1 then
                     Parse_Expression (Depth + 1, B);
                  end if;
               elsif Host_Call.Parameters = 1 then
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
                      Second => B,
                      Host_Call => Host_Call,
                      others => <>),
                     Index);
               else
                  Index := NO_NODE;
               end if;
            end if;
         end if;
      end Parse_List;

      procedure Parse_Elements
        (Depth : Natural; Close : Character; Index : out Node_Reference)
      is
         Elements : Component_Node_Array := [others => NO_NODE];
         Count : CCL.Types.Component_Count := 0;
         --  A literal longer than one node's components continues in a
         --  chained chunk (Second); the chain is one list.
         Rest : Node_Reference := NO_NODE;
      begin
         Index := NO_NODE;
         if Depth >= MAX_NESTING then
            Diagnostic := Nesting_Too_Deep;
            return;
         end if;
         loop
            Skip_Trivia;
            if Cursor >= Source'Length then
               Diagnostic := Unexpected_End;
               return;
            elsif Source (Source'First + Cursor) = Close then
               Cursor := Cursor + 1;
               exit;
            elsif Count = CCL.Types.Maximum_Components then
               Parse_Elements (Depth + 1, Close, Rest);
               if Diagnostic /= No_Diagnostic then
                  return;
               end if;
               exit;
            end if;
            Count := Count + 1;
            Parse_Expression (Depth + 1, Elements (Count));
            if Diagnostic /= No_Diagnostic then
               return;
            end if;
         end loop;
         Add_Node ((Kind => List_Construct, Components => Elements,
                    Element_Count => Count, Second => Rest, others => <>), Index);
      end Parse_Elements;

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
         elsif Source (Source'First + Cursor) = '[' then
            Cursor := Cursor + 1;
            Parse_Elements (Depth, ']', Index);
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
         Id : Function_Index;
         Tail : Node_Reference;
         Definition : CCL.Types.Description;
         Defined_Type : Static_Type;
         Definition_Status : CCL.Types.Definition_Result;
         Is_Enum, Is_Record : Boolean;
         Self_Parts : array (CCL.Types.Component_Index) of Boolean := [others => False];
         Defaults : array (CCL.Types.Component_Index) of CCL.Types.Field_Default :=
           [others => CCL.Types.No_Field_Default];
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
         if Name_Is (Token, "type") then
            Read_Name (Definition.Identifier, Ok);
            if not Ok then return; end if;
            Expect ('(', Ok);
            if not Ok then return; end if;
            Read_Name (Token, Ok);
            if not Ok then return; end if;
            if Name_Is (Token, "range") then
               --  (type P (range Low High)): a range subtype of Integer.
               declare
                  Low, High : Integer_64;
                  Range_Status : CCL.Types.Definition_Result;
               begin
                  Skip_Trivia;
                  Read_Integer_Value (Low, Ok);
                  if not Ok then return; end if;
                  Skip_Trivia;
                  Read_Integer_Value (High, Ok);
                  if not Ok then return; end if;
                  Expect (')', Ok);
                  if not Ok then return; end if;
                  Expect (')', Ok);
                  if not Ok then return; end if;
                  CCL.Types.Define_Range
                    (Tree.Types, Definition.Identifier, Low, High, Defined_Type, Range_Status);
                  if Range_Status /= CCL.Types.Defined then
                     Diagnostic := Invalid_Type_Declaration; return;
                  end if;
                  Add_Node
                    ((Kind => Type_Definition, Declared_Kind => Defined_Type,
                      Source_Position => To_Diagnostic_Position (Start),
                      Source_End_Position => To_Diagnostic_Position (Cursor), others => <>), Index);
                  if Diagnostic /= No_Diagnostic then return; end if;
                  --  Declarations are siblings: the rest of the program is not nested.
                  Parse_Program (Depth, Tail);
                  Tree.Nodes (Index).Second := Tail;
                  return;
               end;
            end if;
            Is_Enum := Name_Is (Token, "enum");
            Is_Record := Name_Is (Token, "record");
            if not Is_Enum and then not Is_Record and then not Name_Is (Token, "variant") then
               Diagnostic := Invalid_Type_Declaration; return;
            end if;
            Definition.Form := (if Is_Record then CCL.Types.Product else CCL.Types.Sum);
            Declaring := Definition.Identifier;
            Skip_Trivia;
            while Cursor < Source'Length and then
              Source (Source'First + Cursor) /= ')'
            loop
               if Definition.Count = CCL.Types.Maximum_Components then
                  Diagnostic := Invalid_Type_Declaration; return;
               end if;
               if not Is_Enum then
                  Expect ('(', Ok);
                  if not Ok then return; end if;
               end if;
               Definition.Count := Definition.Count + 1;
               Read_Name (Definition.Parts (Definition.Count).Identifier, Ok);
               if not Ok then return; end if;
               if not Is_Record and then Definition.Identifier.Length + 1 +
                 Definition.Parts (Definition.Count).Identifier.Length > MAX_NAME_LENGTH
               then Diagnostic := Invalid_Type_Declaration; return; end if;
               Definition.Parts (Definition.Count).Payload := Unit_Type;
               if not Is_Enum then
                  Skip_Trivia;
                  if Cursor < Source'Length and then Source (Source'First + Cursor) /= ')' then
                     Self_List_Read := False;
                     Read_Type (Definition.Parts (Definition.Count).Payload, Allow_Unit => True);
                     if Diagnostic /= No_Diagnostic then return; end if;
                     if Self_List_Read then
                        Self_Parts (Definition.Count) := True;
                        Self_List_Read := False;
                     elsif not CCL.Objects.Storable (Tree.Types, Definition.Parts (Definition.Count).Payload) then
                        Diagnostic := Invalid_Variant_Payload; return;
                     end if;
                     Skip_Trivia;
                     if Is_Record and then Cursor < Source'Length and then
                       Source (Source'First + Cursor) /= ')'
                     then
                        Read_Default (Definition.Parts (Definition.Count).Payload, Defaults (Definition.Count));
                        if Diagnostic /= No_Diagnostic then return; end if;
                     end if;
                  elsif Is_Record then
                     Diagnostic := Expected_Type_Name; return;
                  end if;
                  Expect (')', Ok);
                  if not Ok then return; end if;
               end if;
               Skip_Trivia;
            end loop;
            Declaring := (others => <>);
            Expect (')', Ok);
            if not Ok then return; end if;
            Expect (')', Ok);
            if not Ok then return; end if;
            --  A stream handle is never part of a record or variant.
            for P in 1 .. Definition.Count loop
               if CCL.Types.Is_Handle (Tree.Types, Definition.Parts (P).Payload) then
                  Diagnostic := Stream_Not_Data; return;
               end if;
            end loop;
            CCL.Types.Define (Tree.Types, Definition, Defined_Type, Definition_Status);
            if Definition_Status /= CCL.Types.Defined then
               Diagnostic := Invalid_Type_Declaration; return;
            end if;
            for P in 1 .. Definition.Count loop
               if CCL.Types."/=" (Defaults (P).Kind, CCL.Types.No_Default) then
                  declare
                     Set : CCL.Types.Default_Result;
                  begin
                     CCL.Types.Set_Default (Tree.Types, Defined_Type, P, Defaults (P), Set);
                     if CCL.Types."/=" (Set, CCL.Types.Default_Set) then
                        Diagnostic := Invalid_Field_Default; return;
                     end if;
                  end;
               end if;
            end loop;
            --  Complete each (List Self) field now that Self exists.
            for P in 1 .. Definition.Count loop
               if Self_Parts (P) then
                  declare
                     List_Type : Static_Type;
                     Specialized : CCL.Types.List_Result;
                     Completed : Boolean;
                  begin
                     CCL.Types.Specialize_List (Tree.Types, Defined_Type, List_Type, Specialized);
                     if Specialized not in CCL.Types.List_Specialized |
                       CCL.Types.List_Already_Specialized
                     then
                        Diagnostic := Unsupported_List_Element; return;
                     end if;
                     CCL.Types.Complete_Self_List (Tree.Types, Defined_Type, P, List_Type, Completed);
                     if not Completed then
                        Diagnostic := Invalid_Type_Declaration; return;
                     end if;
                  end;
               end if;
            end loop;
            Add_Node
              ((Kind => Type_Definition, Declared_Kind => Defined_Type,
                Source_Position => To_Diagnostic_Position (Start),
                Source_End_Position => To_Diagnostic_Position (Cursor), others => <>), Index);
            if Diagnostic /= No_Diagnostic then return; end if;
            --  Declarations are siblings: the rest of the program is not nested.
                  Parse_Program (Depth, Tail);
            Tree.Nodes (Index).Second := Tail;
            return;
         elsif not Name_Is (Token, "define") then
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
         --  Publish the reservation before the body: a lambda inside it takes
         --  the next slot.
         Tree.Function_Count := Id + 1;
         Parse_Expression (Depth + 1, Decl.Body_Node);
         if Diagnostic /= No_Diagnostic then return; end if;
         Expect (')', Ok);
         if not Ok then return; end if;
         Tree.Functions (Id) := Decl;
         --  The body's lambdas took the slots after Id: never hand them out
         --  again (a later lambda would overwrite one).
         Tree.Function_Count := Natural'Max (Tree.Function_Count, Id + 1);
         Add_Node
           ((Kind => Function_Definition, Function_Id => Id,
             First => Decl.Body_Node,
             Source_Position => To_Diagnostic_Position (Start),
             Source_End_Position => To_Diagnostic_Position (Cursor), others => <>), Index);
         if Diagnostic /= No_Diagnostic then return; end if;
         --  Declarations are siblings: the rest of the program is not nested.
                  Parse_Program (Depth, Tail);
         Tree.Nodes (Index).Second := Tail;
      end Parse_Program;

      Type_Env : Type_Environment := [others => (others => <>)];
      Type_Env_Length : Natural range 0 .. MAX_BINDINGS := 0;
      Visible_Functions : Natural range 0 .. MAX_FUNCTIONS := 0;
      Visible_Types : Static_Type := Unit_Type;

      function Referenceable (Item : Name) return Boolean is
        (Item.Length > 0 and then Item.Data (1) not in '-' | '0' .. '9' and then
         not Name_Is (Item, "true") and then not Name_Is (Item, "false"));

      function Reserved (Item : Name) return Boolean is
        (not Referenceable (Item) or else
         Name_Is (Item, "type") or else Name_Is (Item, "define") or else Name_Is (Item, "handler") or else Name_Is (Item, "let") or else
         Name_Is (Item, "match") or else
         Name_Is (Item, "field") or else
         Name_Is (Item, "fn") or else Name_Is (Item, "list") or else Name_Is (Item, "stream") or else Name_Is (Item, "task") or else
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

      function Resource_Type (Name : CCL.Types.Name) return Static_Type is
         Types : constant CCL.Types.Registry := CCL.Catalog.Visible_Types (Visible_Interfaces);
         Ref : constant Static_Type := CCL.Types.Find (Types, Name);
      begin
         if CCL.Types.Known (Types, Ref) and then
           CCL.Types.Describe (Types, Ref).Form = CCL.Types.Resource and then
           CCL.Catalog.Resource_Policy (Visible_Interfaces, Ref).Mode /= CCL.Ownership.Unrestricted
         then
            return Ref;
         end if;
         return Invalid_Type;
      end Resource_Type;

      function Host_Type
        (Kind : CCL.Host_Values.Value_Kind; Schema : CCL.Objects.Schema_Key;
         Resource_Name : CCL.Types.Name) return Static_Type is
        (case Kind is
           when CCL.Host_Values.Integer_Value => Integer_Type,
           when CCL.Host_Values.Boolean_Value => Boolean_Type,
           when CCL.Host_Values.Text_Value => String_Type,
           when CCL.Host_Values.Handler_Value => Handler_Type,
           when CCL.Host_Values.Object_Value => CCL.Catalog.Schema_Type (Visible_Interfaces, Schema),
           when CCL.Host_Values.Resource_Value => Resource_Type (Resource_Name));

      --  Anonymous functions being checked, innermost last. A body sees the
      --  enclosing bindings; one it resolves below its frame's Floor is a
      --  capture of that function (and of every enclosing function whose
      --  floor is also above it, so the value reaches the inner one).
      type Lambda_Frame is record
         Id : Function_Index := 0;
         Floor : Natural range 0 .. MAX_BINDINGS := 0;
      end record;
      type Lambda_Frame_Array is array (1 .. MAX_FUNCTIONS) of Lambda_Frame;
      Frames : Lambda_Frame_Array := [others => (others => <>)];
      Frame_Count : Natural range 0 .. MAX_FUNCTIONS := 0;

      --  Captured values are stored as list elements (the rule of
      --  CCL.Types.Specialize_List): scalars, String, Character, enumerations.
      function Capturable (Kind : Static_Type) return Boolean is
        (CCL.Types.Known (Tree.Types, Kind) and then Kind /= Handler_Type and then
         Kind /= Unit_Type and then
         (CCL.Types.Describe (Tree.Types, Kind).Form = CCL.Types.Primitive or else
          CCL.Types.Is_Enumeration (Tree.Types, Kind)));

      procedure Note_Capture (Position : Natural) is
      begin
         if Position >= Type_Env_Length then return; end if;
         for F in 1 .. Frame_Count loop
            if Position < Frames (F).Floor then
               declare
                  Id : constant Function_Index := Frames (F).Id;
                  Binding : constant Type_Binding := Type_Env (Position);
                  Known : Boolean := False;
               begin
                  for C in 1 .. Tree.Functions (Id).Captured loop
                     if Names_Equal (Tree.Functions (Id).Captures (C).Identifier, Binding.Identifier) then
                        Known := True;
                     end if;
                  end loop;
                  if not Known then
                     if not Capturable (Binding.Kind) then
                        Diagnostic := Lambda_Capture_Unsupported; return;
                     elsif Tree.Functions (Id).Captured = MAX_CAPTURES then
                        Diagnostic := Too_Many_Captures; return;
                     end if;
                     Tree.Functions (Id).Captured := Tree.Functions (Id).Captured + 1;
                     Tree.Functions (Id).Captures (Tree.Functions (Id).Captured) :=
                       (Identifier => Binding.Identifier, Kind => Binding.Kind, Declared => True);
                  end if;
               end;
            end if;
         end loop;
      end Note_Capture;

      --  An anonymous function with a parameter written without a type.
      function Untyped_Lambda (Index : Node_Reference) return Boolean is
        (Index < Tree.Length and then Tree.Nodes (Index).Kind = Lambda_Form and then
         (for some P in 1 .. Tree.Functions (Tree.Nodes (Index).Function_Id).Count =>
            Tree.Functions (Tree.Nodes (Index).Function_Id).Parameters (P).Kind = Invalid_Type));

      --  Give an untyped anonymous function's first two parameters these types
      --  (written types are kept; the checker reports any mismatch).
      procedure Infer_Parameters (Index : Node_Reference; First, Second : Static_Type) is
      begin
         if Untyped_Lambda (Index) then
            declare
               Id : constant Function_Index := Tree.Nodes (Index).Function_Id;
            begin
               for P in 1 .. Tree.Functions (Id).Count loop
                  if Tree.Functions (Id).Parameters (P).Kind = Invalid_Type then
                     Tree.Functions (Id).Parameters (P).Kind :=
                       (if P = 1 then First elsif P = 2 then Second else Invalid_Type);
                  end if;
               end loop;
            end;
         end if;
      end Infer_Parameters;

      --  The same, from an expected function type (Function (A B ...) R).
      procedure Infer_From_Type (Index : Node_Reference; Expected : Static_Type) is
         D : CCL.Types.Description;
      begin
         if Untyped_Lambda (Index) and then CCL.Types.Is_Function (Tree.Types, Expected) then
            D := CCL.Types.Describe (Tree.Types, Expected);
            Infer_Parameters (Index, (if D.Count >= 2 then D.Parts (1).Payload else Invalid_Type),
                              (if D.Count >= 3 then D.Parts (2).Payload else Invalid_Type));
         end if;
      end Infer_From_Type;

      --  A value of type Source, written by node From, fills a position of
      --  type Target: the same type, or an Integer filling a range type
      --  (a literal is checked against the bounds here, anything else when
      --  it runs). Otherwise Mismatch.
      --  A type's name as code writes it, for messages.
      function Type_Name (T : Static_Type) return Name is
        (if T = Integer_Type then CCL.Types.Named ("Integer")
         elsif T = Boolean_Type then CCL.Types.Named ("Boolean")
         elsif T = String_Type then CCL.Types.Named ("String")
         elsif T = Character_Type then CCL.Types.Named ("Character")
         elsif T = Unit_Type then CCL.Types.Named ("Unit")
         elsif T in CCL.Types.Declared_Type then CCL.Types.Describe (Tree.Types, T).Identifier
         else CCL.Types.Named (""));

      procedure Conform
        (Source, Target : Static_Type; From : Node_Reference; Mismatch : Diagnostic_Code) is
      begin
         if Diagnostic /= No_Diagnostic or else Source = Target then
            return;
         elsif Source = Integer_Type and then CCL.Types.Is_Range (Tree.Types, Target) then
            if From < Tree.Length and then Tree.Nodes (From).Kind = Integer_Literal and then
              (Tree.Nodes (From).Integer_Value < CCL.Types.Low_Of (Tree.Types, Target) or else
               Tree.Nodes (From).Integer_Value > CCL.Types.High_Of (Tree.Types, Target))
            then
               Diagnostic := Value_Out_Of_Range;
            end if;
         else
            Diagnostic := Mismatch;
         end if;
      end Conform;

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
            when Type_Definition =>
               if CCL.Types.Describe (Tree.Types, Tree.Nodes (Index).Declared_Kind).Form = CCL.Types.Product then
                  declare
                     Name : constant CCL.Types.Name := CCL.Types.Describe (Tree.Types, Tree.Nodes (Index).Declared_Kind).Identifier;
                  begin
                     if Reserved (Name) or else
                       (for some F in 1 .. Visible_Functions => Names_Equal (Name, Tree.Functions (F - 1).Identifier))
                     then Diagnostic := Duplicate_Declaration; return; end if;
                  end;
               end if;
               Visible_Types := Tree.Nodes (Index).Declared_Kind;
               --  Declarations are siblings: the rest of the program is not nested.
               Check_Node (Tree.Nodes (Index).Second, Depth, Kind);
            when Variant_Literal =>
               Kind := Tree.Nodes (Index).Declared_Kind;
            when Variant_Construct =>
               Check_Node (Tree.Nodes (Index).First, Depth + 1, Left_Type);
               Kind := Tree.Nodes (Index).Declared_Kind;
               if Kind > Visible_Types or else not CCL.Objects.Storable (Tree.Types, Kind) then
                  Diagnostic := Invalid_Variant_Payload;
               else
                  Conform (Left_Type, CCL.Types.Describe (Tree.Types, Kind).
                             Parts (Tree.Nodes (Index).Alternative).Payload,
                           Tree.Nodes (Index).First, Invalid_Variant_Payload);
               end if;
            when Lambda_Form =>
               --  The body is checked on top of the enclosing bindings, with
               --  its parameters innermost; enclosing names it uses become
               --  captures (Note_Capture). Its result type is the body's.
               declare
                  Id : constant Function_Index := Tree.Nodes (Index).Function_Id;
                  Decl : Function_Declaration := Tree.Functions (Id);
                  Saved_Types : constant Type_Environment := Type_Env;
                  Saved_Frames : constant Natural range 0 .. MAX_FUNCTIONS := Frame_Count;
                  Floor : constant Natural range 0 .. MAX_BINDINGS := Type_Env_Length;
                  Parameters : CCL.Types.Function_Parameters := [others => Invalid_Type];
                  Specialized : CCL.Types.Function_Result;
                  Body_Type : Static_Type := Invalid_Type;
               begin
                  if Frame_Count = MAX_FUNCTIONS then
                     Diagnostic := Too_Many_Functions; return;
                  elsif Decl.Count > MAX_BINDINGS - Floor then
                     Diagnostic := Too_Many_Bindings; return;
                  end if;
                  Tree.Functions (Id).Captured := 0;
                  for P in 1 .. Decl.Count loop
                     if Decl.Parameters (P).Kind = Invalid_Type then
                        --  Not passed where its parameter types are known.
                        Diagnostic := Lambda_Parameter_Needs_Type; return;
                     end if;
                     if not Referenceable (Decl.Parameters (P).Identifier) then
                        Diagnostic := Duplicate_Declaration; return;
                     end if;
                     Type_Env (Floor + P - 1) := (Identifier => Decl.Parameters (P).Identifier,
                                                  Kind => Decl.Parameters (P).Kind);
                     Parameters (P) := Decl.Parameters (P).Kind;
                  end loop;
                  Type_Env_Length := Floor + Decl.Count;
                  Frames (Frame_Count + 1) := (Id => Id, Floor => Floor);
                  Frame_Count := Frame_Count + 1;
                  Check_Node (Decl.Body_Node, Depth + 1, Body_Type);
                  Frame_Count := Saved_Frames;
                  Type_Env := Saved_Types;
                  Type_Env_Length := Entry_Environment_Length;
                  if Diagnostic /= No_Diagnostic then return; end if;
                  Decl.Result_Kind := Body_Type;
                  Decl.Captures := Tree.Functions (Id).Captures;
                  Decl.Captured := Tree.Functions (Id).Captured;
                  Tree.Functions (Id) := Decl;
                  CCL.Types.Specialize_Function
                    (Tree.Types, Parameters, Decl.Count, Body_Type, Kind, Specialized);
                  if Specialized not in CCL.Types.Function_Specialized |
                    CCL.Types.Function_Already_Specialized
                  then
                     Diagnostic := Invalid_Type_Declaration;
                  end if;
               end;
            when Builtin_Form =>
               --  Operands first (left to right), then the builtin's profile.
               declare
                  Operation : constant Builtin_Operation := Tree.Nodes (Index).Builtin;
                  Operand_Types : array (Parameter_Index) of Static_Type := [others => Invalid_Type];
                  Count : constant Parameter_Count := Tree.Nodes (Index).Argument_Count;
                  List_Type, Element_Type, Function_Type : Static_Type := Invalid_Type;
                  Specialized : CCL.Types.List_Result;
                  --  Whether Kind is a function of the given parameters and result.
                  function Profile_Is
                    (Kind : Static_Type; First, Second : Static_Type; Arity : Parameter_Count;
                     Result_Kind : Static_Type) return Boolean
                  is
                     D : CCL.Types.Description;
                  begin
                     if not CCL.Types.Is_Function (Tree.Types, Kind) then return False; end if;
                     D := CCL.Types.Describe (Tree.Types, Kind);
                     --  Range-typed parameters and results match their base
                     --  Integer; Apply checks the bounds when the call runs.
                     return D.Count = Arity + 1 and then
                       CCL.Types.Base_Of (Tree.Types, D.Parts (1).Payload) = First and then
                       (Arity < 2 or else CCL.Types.Base_Of (Tree.Types, D.Parts (2).Payload) = Second) and then
                       (Result_Kind = Invalid_Type or else
                        CCL.Types.Base_Of (Tree.Types, D.Parts (D.Count).Payload) = Result_Kind);
                  end Profile_Is;
                  function Result_Of (Kind : Static_Type) return Static_Type is
                    (if CCL.Types.Is_Function (Tree.Types, Kind) and then
                        CCL.Types.Describe (Tree.Types, Kind).Count >= 1
                     then
                        CCL.Types.Base_Of (Tree.Types, CCL.Types.Describe (Tree.Types, Kind).Parts
                          (CCL.Types.Describe (Tree.Types, Kind).Count).Payload)
                     else Invalid_Type);
               begin
                  if Count /= Builtin_Arity (Operation) then
                     Diagnostic := Function_Arity_Mismatch; return;
                  end if;
                  for P in 1 .. Count loop
                     --  An untyped function operand waits for the subject.
                     if P > 1 or else not Untyped_Lambda (Tree.Nodes (Index).Arguments (1)) then
                        Check_Node (Tree.Nodes (Index).Arguments (P), Depth + 1, Operand_Types (P));
                        if Diagnostic /= No_Diagnostic then return; end if;
                     end if;
                  end loop;
                  --  The subject is the last operand (except for range): a
                  --  list, or for Takes_Text builtins also a String. The parser
                  --  enforces each builtin's arity.
                  if Count = 0 then
                     Diagnostic := Function_Arity_Mismatch; return;
                  end if;
                  if Operation /= Range_Builtin and then not Is_Stream_View (Operation) then
                     List_Type := Operand_Types (Count);
                     if List_Type = String_Type and then Takes_Text (Operation) then
                        Element_Type := Character_Type;
                     elsif Operation in Upper_Builtin .. Index_Of_Builtin | Replace_Builtin |
                       Split_Builtin | Parse_Int_Builtin
                     then
                        Diagnostic := Expected_String; return;
                     elsif not CCL.Types.Is_List (Tree.Types, List_Type) then
                        Diagnostic := Function_Argument_Mismatch; return;
                     else
                        Element_Type := CCL.Types.Element_Of (Tree.Types, List_Type);
                     end if;
                  end if;
                  if Untyped_Lambda (Tree.Nodes (Index).Arguments (1)) then
                     --  (fold f init xs): f takes (init's type, element);
                     --  the others take one element.
                     Infer_Parameters
                       (Tree.Nodes (Index).Arguments (1),
                        (if Operation = Fold_Builtin then Operand_Types (2) else Element_Type),
                        (if Operation = Fold_Builtin then Element_Type else Invalid_Type));
                     Check_Node (Tree.Nodes (Index).Arguments (1), Depth + 1, Operand_Types (1));
                     if Diagnostic /= No_Diagnostic then return; end if;
                  end if;
                  Function_Type := Operand_Types (1);
                  case Operation is
                     when Each_Builtin =>
                        if not Profile_Is (Function_Type, Element_Type, Invalid_Type, 1, Invalid_Type) then
                           Diagnostic := Function_Argument_Mismatch; return;
                        end if;
                        CCL.Types.Specialize_List (Tree.Types, Result_Of (Function_Type), Kind, Specialized);
                        if Specialized not in CCL.Types.List_Specialized |
                          CCL.Types.List_Already_Specialized
                        then Diagnostic := Unsupported_List_Element; end if;
                     when Where_Builtin | Any_Builtin | All_Builtin | Count_Builtin =>
                        if not Profile_Is (Function_Type, Element_Type, Invalid_Type, 1, Boolean_Type) then
                           Diagnostic := Function_Argument_Mismatch; return;
                        end if;
                        Kind := (if Operation = Where_Builtin then List_Type
                                 elsif Operation = Count_Builtin then Integer_Type
                                 else Boolean_Type);
                     when Fold_Builtin =>
                        --  (fold f init xs), f : (Accumulator, Element) -> Accumulator.
                        if not Profile_Is (Function_Type, Operand_Types (2), Element_Type, 2, Operand_Types (2)) then
                           Diagnostic := Function_Argument_Mismatch; return;
                        end if;
                        Kind := Operand_Types (2);
                     when First_Builtin | Last_Builtin | Skip_Builtin =>
                        if Operand_Types (1) /= Integer_Type then
                           Diagnostic := Expected_Integer; return;
                        end if;
                        Kind := List_Type;
                     when Reverse_Builtin =>
                        Kind := List_Type;
                     when Sum_Builtin | Min_Builtin | Max_Builtin =>
                        if Element_Type /= Integer_Type then
                           Diagnostic := Expected_Integer; return;
                        end if;
                        Kind := Integer_Type;
                     when Sort_Builtin =>
                        --  Ordered elements: integers, text, characters, enumerations
                        --  (by declaration order).
                        if Element_Type not in Integer_Type | String_Type | Character_Type and then
                          not CCL.Types.Is_Enumeration (Tree.Types, Element_Type)
                        then
                           Diagnostic := Expected_Comparable; return;
                        end if;
                        Kind := List_Type;
                     when Sort_By_Builtin =>
                        --  (sort-by key xs): key : Element -> Integer or String.
                        if not Profile_Is (Function_Type, Element_Type, Invalid_Type, 1, Integer_Type) and then
                          not Profile_Is (Function_Type, Element_Type, Invalid_Type, 1, String_Type)
                        then
                           Diagnostic := Function_Argument_Mismatch; return;
                        end if;
                        Kind := List_Type;
                     when Contains_Builtin =>
                        --  (contains x xs) membership; (contains needle s) substring.
                        if List_Type = String_Type then
                           if Operand_Types (1) /= String_Type then
                              Diagnostic := Expected_String; return;
                           end if;
                        elsif Operand_Types (1) /= Element_Type then
                           Diagnostic := Function_Argument_Mismatch; return;
                        elsif Element_Type not in Integer_Type | Boolean_Type | String_Type | Character_Type and then
                          not CCL.Types.Is_Enumeration (Tree.Types, Element_Type)
                        then
                           Diagnostic := Expected_Comparable; return;
                        end if;
                        Kind := Boolean_Type;
                     when Upper_Builtin | Lower_Builtin | Trim_Builtin =>
                        Kind := String_Type;
                     when Starts_With_Builtin | Ends_With_Builtin | Index_Of_Builtin | Split_Builtin =>
                        if Operand_Types (1) /= String_Type then
                           Diagnostic := Expected_String; return;
                        end if;
                        if Operation = Index_Of_Builtin then
                           Kind := Integer_Type;
                        elsif Operation = Split_Builtin then
                           CCL.Types.Specialize_List (Tree.Types, String_Type, Kind, Specialized);
                        else
                           Kind := Boolean_Type;
                        end if;
                     when Replace_Builtin =>
                        if Operand_Types (1) /= String_Type or else Operand_Types (2) /= String_Type then
                           Diagnostic := Expected_String; return;
                        end if;
                        Kind := String_Type;
                     when Join_Builtin =>
                        --  (join separator xs), xs : List<String>.
                        if Operand_Types (1) /= String_Type or else Element_Type /= String_Type then
                           Diagnostic := Expected_String; return;
                        end if;
                        Kind := String_Type;
                     when Parse_Int_Builtin =>
                        Kind := Integer_Type;
                     when Range_Builtin =>
                        if Operand_Types (1) /= Integer_Type or else Operand_Types (2) /= Integer_Type then
                           Diagnostic := Expected_Integer; return;
                        end if;
                        CCL.Types.Specialize_List (Tree.Types, Integer_Type, Kind, Specialized);
                     when Wait_Builtin =>
                        --  (wait t) : T, for a task t of T.
                        if not CCL.Types.Is_Task (Tree.Types, Operand_Types (Count)) then
                           Diagnostic := Expected_Task; return;
                        end if;
                        Kind := CCL.Types.Task_Result (Tree.Types, Operand_Types (Count));
                     when Latest_Builtin .. Lost_Builtin =>
                        --  (latest s) : T, (window n s) : List<T>,
                        --  (arrived s), (lost s) : Integer.
                        if not CCL.Types.Is_Stream (Tree.Types, Operand_Types (Count)) then
                           Diagnostic := Expected_Stream; return;
                        end if;
                        Element_Type := CCL.Types.Stream_Element (Tree.Types, Operand_Types (Count));
                        case Stream_View_Of (Operation) is
                           when CCL.Streams.Latest_View => Kind := Element_Type;
                           when CCL.Streams.Window_View =>
                              if Operand_Types (1) /= Integer_Type then
                                 Diagnostic := Expected_Integer; return;
                              end if;
                              CCL.Types.Specialize_List (Tree.Types, Element_Type, Kind, Specialized);
                              if Specialized not in CCL.Types.List_Specialized |
                                CCL.Types.List_Already_Specialized
                              then Diagnostic := Unsupported_List_Element; end if;
                           when CCL.Streams.Arrived_View | CCL.Streams.Lost_View =>
                              Kind := Integer_Type;
                           when CCL.Streams.Wait_View => Diagnostic := Expected_Stream;
                        end case;
                     when No_Builtin => Diagnostic := Unknown_Form;
                  end case;
               end;
            when List_Construct =>
               --  Every element has the first element's type; the result is
               --  List<that type>. An empty list needs a declared type
               --  ((list-of T)). Chunks chained through Second continue it.
               if Tree.Nodes (Index).Element_Count = 0 then
                  if Tree.Nodes (Index).Declared_Kind /= Invalid_Type and then
                    CCL.Types.Is_List (Tree.Types, Tree.Nodes (Index).Declared_Kind)
                  then
                     Kind := Tree.Nodes (Index).Declared_Kind;
                  else
                     Diagnostic := Empty_List_Needs_Type; return;
                  end if;
               else
               declare
                  Element_Type, Next_Type : Static_Type := Invalid_Type;
                  Specialized : CCL.Types.List_Result;
                  Chunk : Node_Reference := Index;
                  First_Element : Boolean := True;
               begin
                  for Step in 0 .. MAX_NESTING loop
                     exit when Chunk >= Tree.Length;
                     for P in 1 .. Tree.Nodes (Chunk).Element_Count loop
                        Check_Node (Tree.Nodes (Chunk).Components (P), Depth + 1, Next_Type);
                        if Diagnostic /= No_Diagnostic then return; end if;
                        if First_Element then
                           Element_Type := Next_Type;
                           First_Element := False;
                        elsif Next_Type /= Element_Type then
                           Diagnostic := List_Element_Mismatch; return;
                        end if;
                     end loop;
                     Chunk := Tree.Nodes (Chunk).Second;
                  end loop;
                  CCL.Types.Specialize_List (Tree.Types, Element_Type, Kind, Specialized);
                  if Specialized not in CCL.Types.List_Specialized |
                    CCL.Types.List_Already_Specialized
                  then
                     Diagnostic := Unsupported_List_Element; return;
                  end if;
               end;
               end if;
            when Record_Construct =>
               Kind := Tree.Nodes (Index).Declared_Kind;
               if Kind > Visible_Types or else not CCL.Objects.Storable (Tree.Types, Kind) then
                  Diagnostic := Invalid_Type_Declaration; return;
               end if;
               for P in 1 .. CCL.Types.Describe (Tree.Types, Kind).Count loop
                  Check_Node (Tree.Nodes (Index).Components (P), Depth + 1, Left_Type);
                  if Diagnostic /= No_Diagnostic then return; end if;
                  Conform (Left_Type, CCL.Types.Describe (Tree.Types, Kind).Parts (P).Payload,
                           Tree.Nodes (Index).Components (P), Field_Type_Mismatch);
                  if Diagnostic = Field_Type_Mismatch then
                     --  Which field, and the type it wants and was given.
                     Diagnostic_Subject := CCL.Types.Describe (Tree.Types, Kind).Parts (P).Identifier;
                     Diagnostic_Expected := Type_Name (CCL.Types.Describe (Tree.Types, Kind).Parts (P).Payload);
                     Diagnostic_Found := Type_Name (Left_Type);
                     if Tree.Nodes (Index).Components (P) < Tree.Length then
                        Diagnostic_Position :=
                          Tree.Nodes (Tree.Nodes (Index).Components (P)).Source_Position;
                     end if;
                  end if;
                  if Diagnostic /= No_Diagnostic then return; end if;
               end loop;
            when Field_Form =>
               Check_Node (Tree.Nodes (Index).First, Depth + 1, Left_Type);
               if Diagnostic /= No_Diagnostic then return; end if;
               declare
                  D : constant CCL.Types.Description := CCL.Types.Describe (Tree.Types, Left_Type);
               begin
                  if D.Form = CCL.Types.Product then
                     for P in 1 .. D.Count loop
                        if Names_Equal (Tree.Nodes (Index).Identifier, D.Parts (P).Identifier) then
                           --  Reading a range-typed field gives an Integer.
                           Kind := CCL.Types.Base_Of (Tree.Types, D.Parts (P).Payload);
                           Tree.Nodes (Index).Alternative := P;
                           exit;
                        end if;
                     end loop;
                  end if;
                  if Kind = Invalid_Type then Diagnostic := Unknown_Name; end if;
               end;
            when Match_Form =>
               Check_Node (Tree.Nodes (Index).First, Depth + 1, Left_Type);
               if Diagnostic /= No_Diagnostic then return; end if;
               if CCL.Types.Describe (Tree.Types, Left_Type).Form /= CCL.Types.Sum then
                  Diagnostic := Invalid_Match_Pattern; return;
               end if;
               declare
                  D : constant CCL.Types.Description := CCL.Types.Describe (Tree.Types, Left_Type);
                  Seen : array (CCL.Types.Component_Index) of Boolean := [others => False];
                  Arm : Node_Reference := Tree.Nodes (Index).Second;
                  N : Node;
                  Payload : Static_Type;
               begin
                  while Arm < Tree.Length and then Diagnostic = No_Diagnostic loop
                     N := Tree.Nodes (Arm);
                     if N.Kind /= Match_Arm or else N.Declared_Kind /= Left_Type then
                        Diagnostic := Invalid_Match_Pattern; exit;
                     elsif Seen (N.Alternative) then Diagnostic := Duplicate_Match_Arm; exit;
                     end if;
                     Seen (N.Alternative) := True;
                     Payload := D.Parts (N.Alternative).Payload;
                     if Payload = Unit_Type then
                        if N.Identifier.Length /= 0 then Diagnostic := Invalid_Match_Pattern; exit; end if;
                     elsif not Referenceable (N.Identifier) then
                        Diagnostic := Invalid_Match_Pattern; exit;
                     elsif Type_Env_Length = MAX_BINDINGS then
                        Diagnostic := Too_Many_Bindings; exit;
                     else
                        Type_Env (Type_Env_Length) :=
                          (N.Identifier, CCL.Types.Base_Of (Tree.Types, Payload));
                        Type_Env_Length := Type_Env_Length + 1;
                     end if;
                     Check_Node (N.First, Depth + 1, Right_Type);
                     Type_Env_Length := Entry_Environment_Length;
                     if Kind = Invalid_Type then Kind := Right_Type;
                     elsif Diagnostic = No_Diagnostic and then Kind /= Right_Type then
                        Diagnostic := Branch_Type_Mismatch;
                     end if;
                     Tree.Nodes (Arm).Static_Kind := Right_Type;
                     Arm := N.Second;
                  end loop;
                  if Diagnostic = No_Diagnostic and then
                    (for some A in 1 .. D.Count => not Seen (A))
                  then Diagnostic := Nonexhaustive_Match; end if;
               end;
            when Match_Arm => Diagnostic := Invalid_Match_Pattern;
            when Function_Definition =>
               declare
                  Decl : constant Function_Declaration :=
                    Tree.Functions (Tree.Nodes (Index).Function_Id);
               begin
                  if Reserved (Decl.Identifier) or else
                    CCL.Types.Describe (Tree.Types, CCL.Types.Find (Tree.Types, Decl.Identifier)).Form = CCL.Types.Product
                  then
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
                     --  A range-typed parameter reads as an Integer.
                     Type_Env (P - 1) :=
                       (Identifier => Decl.Parameters (P).Identifier,
                        Kind => CCL.Types.Base_Of (Tree.Types, Decl.Parameters (P).Kind));
                  end loop;
                  Type_Env_Length := Decl.Count;
                  Check_Node (Decl.Body_Node, Depth + 1, Left_Type);
                  Type_Env_Length := Entry_Environment_Length;
                  Conform (Left_Type, Decl.Result_Kind, Decl.Body_Node, Function_Result_Mismatch);
                  if Diagnostic = No_Diagnostic then
                     --  Publish only after checking the body: no self calls or
                     --  forward calls, and therefore no recursive call graph.
                     Visible_Functions := Tree.Nodes (Index).Function_Id + 1;
                     Check_Node (Tree.Nodes (Index).Second, Depth, Kind);
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
               --  A call through a function-typed binding: (f 3) with f a
               --  parameter or let of function type.
               if not Found and then Tree.Nodes (Index).Kind = Function_Call and then
                 Type_Env_Length > 0
               then
                  for Position in reverse 0 .. Type_Env_Length - 1 loop
                     if Names_Equal (Type_Env (Position).Identifier, Tree.Nodes (Index).Identifier) then
                        if CCL.Types.Is_Function (Tree.Types, Type_Env (Position).Kind) then
                           declare
                              D : constant CCL.Types.Description :=
                                CCL.Types.Describe (Tree.Types, Type_Env (Position).Kind);
                           begin
                              Found := True;
                              Tree.Nodes (Index).Calls_Value := True;
                              Note_Capture (Position);
                              if Tree.Nodes (Index).Argument_Count /= D.Count - 1 then
                                 Diagnostic := Function_Arity_Mismatch;
                              else
                                 for P in 1 .. D.Count - 1 loop
                                    Infer_From_Type (Tree.Nodes (Index).Arguments (P), D.Parts (P).Payload);
                                    Check_Node (Tree.Nodes (Index).Arguments (P), Depth + 1, Left_Type);
                                    if Diagnostic = No_Diagnostic and then
                                      CCL.Types.Base_Of (Tree.Types, Left_Type) /=
                                        CCL.Types.Base_Of (Tree.Types, D.Parts (P).Payload)
                                    then
                                       Diagnostic := Function_Argument_Mismatch;
                                    end if;
                                 end loop;
                                 Kind := CCL.Types.Base_Of (Tree.Types, D.Parts (D.Count).Payload);
                              end if;
                           end;
                        end if;
                        exit;
                     end if;
                  end loop;
               end if;
               if not Found then Diagnostic := Unknown_Form;
               elsif Tree.Nodes (Index).Calls_Value then null;
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
                           Infer_From_Type (Tree.Nodes (Index).Arguments (P), Decl.Parameters (P).Kind);
                           Check_Node (Tree.Nodes (Index).Arguments (P), Depth + 1, Left_Type);
                           Conform (Left_Type, Decl.Parameters (P).Kind,
                                    Tree.Nodes (Index).Arguments (P), Function_Argument_Mismatch);
                        end loop;
                        Kind := CCL.Types.Base_Of (Tree.Types, Decl.Result_Kind);
                     end if;
                  end;
               end if;
            when Integer_Literal => Kind := Integer_Type;
            when Stream_Reference => Kind := Tree.Nodes (Index).Declared_Kind;
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
                        Note_Capture (Position);
                        exit;
                     end if;
                  end loop;
               end if;
               if not Found then
                  declare
                     Identifier : constant Name := Tree.Nodes (Index).Identifier;
                     Ref : Static_Type;
                     D : CCL.Types.Description;
                  begin
                     for Dot in 2 .. Identifier.Length loop
                        if Identifier.Data (Dot) = '.' then
                           Ref := CCL.Types.Find (Tree.Types,
                             CCL.Types.Named (Identifier.Data (1 .. Dot - 1)));
                           if Ref <= Visible_Types and then
                             CCL.Types.Describe (Tree.Types, Ref).Form = CCL.Types.Sum and then
                             CCL.Objects.Storable (Tree.Types, Ref)
                           then
                              D := CCL.Types.Describe (Tree.Types, Ref);
                              for I in 1 .. D.Count loop
                                 if CCL.Types.Image (D.Parts (I).Identifier) =
                                   Identifier.Data (Dot + 1 .. Identifier.Length)
                                 then
                                    if D.Parts (I).Payload /= Unit_Type then
                                       Diagnostic := Invalid_Variant_Payload;
                                       return;
                                    end if;
                                    Tree.Nodes (Index).Kind := Variant_Literal;
                                    Tree.Nodes (Index).Declared_Kind := Ref;
                                    Tree.Nodes (Index).Alternative := I;
                                    Kind := Ref; Found := True;
                                    exit;
                                 end if;
                              end loop;
                           end if;
                           exit;
                        end if;
                     end loop;
                  end;
               end if;
               --  A defined function's name is a value of its function type.
               if not Found then
                  for F in 1 .. Visible_Functions loop
                     if Names_Equal (Tree.Nodes (Index).Identifier, Tree.Functions (F - 1).Identifier) then
                        declare
                           Decl : constant Function_Declaration := Tree.Functions (F - 1);
                           Parameters : CCL.Types.Function_Parameters := [others => Invalid_Type];
                           Specialized : CCL.Types.Function_Result;
                        begin
                           for P in 1 .. Decl.Count loop
                              Parameters (P) := Decl.Parameters (P).Kind;
                           end loop;
                           CCL.Types.Specialize_Function
                             (Tree.Types, Parameters, Decl.Count, Decl.Result_Kind, Kind, Specialized);
                           if Specialized in CCL.Types.Function_Specialized |
                             CCL.Types.Function_Already_Specialized
                           then
                              Tree.Nodes (Index).Names_Function := True;
                              Tree.Nodes (Index).Function_Id := F - 1;
                              Found := True;
                           end if;
                        end;
                        exit;
                     end if;
                  end loop;
               end if;
               if not Found then
                  Diagnostic := Unknown_Name;
               end if;
            when Add_Form | Subtract_Form | Multiply_Form | Divide_Form |
                 Modulo_Form | Equal_Form | Not_Equal_Form | Less_Form |
                 Less_Equal_Form | Greater_Form | Greater_Equal_Form =>
               Check_Node (Tree.Nodes (Node_Index (Index)).First,
                           Depth + 1, Left_Type);
               Check_Node (Tree.Nodes (Node_Index (Index)).Second,
                           Depth + 1, Right_Type);
               if Tree.Nodes (Index).Kind in Equal_Form | Not_Equal_Form then
                  if Diagnostic = No_Diagnostic and then
                    (Left_Type /= Right_Type or else
                     (Left_Type /= Integer_Type and then
                      Left_Type /= Boolean_Type and then
                      Left_Type /= String_Type and then
                      Left_Type /= Character_Type and then
                      not CCL.Types.Is_Enumeration (Tree.Types, Left_Type)))
                  then Diagnostic := Expected_Comparable; end if;
                  Kind := Boolean_Type;
               elsif Diagnostic = No_Diagnostic and then
                 (Left_Type /= Integer_Type or else Right_Type /= Integer_Type)
               then
                  Diagnostic := Expected_Integer;
               elsif Tree.Nodes (Node_Index (Index)).Kind in
                 Add_Form | Subtract_Form
               then
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
            when And_Form | Or_Form =>
               Check_Node (Tree.Nodes (Node_Index (Index)).First,
                           Depth + 1, Left_Type);
               Check_Node (Tree.Nodes (Node_Index (Index)).Second,
                           Depth + 1, Right_Type);
               if Diagnostic = No_Diagnostic and then
                 (Left_Type /= Boolean_Type or else Right_Type /= Boolean_Type)
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
                 Left_Type /= String_Type and then
                 not CCL.Types.Is_List (Tree.Types, Left_Type)
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
                 Left_Type /= String_Type and then
                 not CCL.Types.Is_List (Tree.Types, Left_Type)
               then
                  Diagnostic := Expected_String;
               elsif Diagnostic = No_Diagnostic and then
                 Right_Type /= Integer_Type
               then
                  Diagnostic := Expected_Integer;
               elsif CCL.Types.Is_List (Tree.Types, Left_Type) then
                  Kind := CCL.Types.Element_Of (Tree.Types, Left_Type);
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
                 Left_Type /= Integer_Type and then
                 not CCL.Types.Is_Enumeration (Tree.Types, Left_Type)
               then
                  Diagnostic := Expected_Printable;
               else
                  Kind := String_Type;
               end if;
            when Host_Import_Form =>
               declare
                  Op : constant CCL.Catalog.Resolved_Operation := Tree.Nodes (Index).Host_Call;
                  Argument_Type : constant Static_Type := Host_Type
                    (Op.Import.Argument, Op.Import.Argument_Schema, Op.Import.Argument_Resource);
                  Result_Type : constant Static_Type := Host_Type
                    (Op.Import.Result, Op.Import.Result_Schema, Op.Import.Result_Resource);
                  Has_Receiver : constant Boolean := CCL.Host_Values.Has_Receiver (Op.Import);
                  Receiver_Type : constant Static_Type :=
                    (if Has_Receiver then Resource_Type (Op.Import.Receiver_Resource) else Unit_Type);
               begin
                  if Argument_Type = Invalid_Type or Result_Type = Invalid_Type or Receiver_Type = Invalid_Type then
                     Diagnostic := Host_Schema_Unavailable;
                  elsif (Op.Import.Argument = CCL.Host_Values.Object_Value and then
                     not CCL.Objects.Persistable (Tree.Types, Argument_Type)) or else
                    (Op.Import.Result = CCL.Host_Values.Object_Value and then
                     not CCL.Objects.Persistable (Tree.Types, Result_Type))
                  then
                     Diagnostic := Unsupported_Host_Object;
                  else
                     if Has_Receiver then
                        Check_Node (Tree.Nodes (Index).First, Depth + 1, Left_Type);
                        if Diagnostic = No_Diagnostic and then Left_Type /= Receiver_Type then
                           Diagnostic := Host_Object_Type_Mismatch;
                        end if;
                     end if;
                     if Diagnostic = No_Diagnostic and then Op.Parameters = 1 then
                        Check_Node ((if Has_Receiver then Tree.Nodes (Index).Second else Tree.Nodes (Index).First),
                          Depth + 1, Left_Type);
                        if Diagnostic = No_Diagnostic and then Left_Type /= Argument_Type then
                           Diagnostic := (case Op.Import.Argument is
                             when CCL.Host_Values.Integer_Value => Expected_Integer,
                             when CCL.Host_Values.Boolean_Value => Expected_Boolean,
                             when CCL.Host_Values.Text_Value => Expected_String,
                             when CCL.Host_Values.Handler_Value => Expected_Handler,
                             when CCL.Host_Values.Object_Value | CCL.Host_Values.Resource_Value =>
                               Argument_Type_Mismatch);
                           if Diagnostic = Argument_Type_Mismatch then
                              --  Which operation, and the type it takes and was given.
                              Diagnostic_Subject := Tree.Nodes (Index).Identifier;
                              Diagnostic_Expected := Type_Name (Argument_Type);
                              Diagnostic_Found := Type_Name (Left_Type);
                           end if;
                        end if;
                     end if;
                     if Diagnostic = No_Diagnostic and then Op.Import.Result_Task then
                        --  The result arrives later: a task of Result_Type.
                        declare
                           Specialized : CCL.Types.Stream_Result;
                        begin
                           CCL.Types.Specialize_Task (Tree.Types, Result_Type, Kind, Specialized);
                           if Specialized not in CCL.Types.Stream_Specialized |
                             CCL.Types.Stream_Already_Specialized
                           then Diagnostic := Unsupported_Stream_Element; end if;
                        end;
                     elsif Diagnostic = No_Diagnostic and then Op.Import.Result_Stream then
                        --  A source: the result is a stream of Result_Type.
                        declare
                           Specialized : CCL.Types.Stream_Result;
                        begin
                           CCL.Types.Specialize_Stream (Tree.Types, Result_Type, Kind, Specialized);
                           if Specialized not in CCL.Types.Stream_Specialized |
                             CCL.Types.Stream_Already_Specialized
                           then Diagnostic := Unsupported_Stream_Element; end if;
                        end;
                     elsif Diagnostic = No_Diagnostic then
                        Kind := Result_Type;
                     end if;
                  end if;
               end;
            when Invalid_Node => Diagnostic := Unexpected_Token;
         end case;
         -- Export admission belongs to the checked root node. Keeping it here
         -- also keeps its diagnostic within the established node-index bounds;
         -- the caller only receives a type, not a validated node reference.
         if Depth = 0 and then Diagnostic = No_Diagnostic and then Kind = Handler_Type then
            Diagnostic := Handler_Result_Not_Exportable;
         end if;
         if Diagnostic = No_Diagnostic then
            Tree.Nodes (Node_Index (Index)).Static_Kind := Kind;
         end if;
         if Diagnostic /= No_Diagnostic and then Diagnostic_Position = 0 then
            Diagnostic_Position :=
              Tree.Nodes (Node_Index (Index)).Source_Position;
         end if;
      end Check_Node;

      Root_Type : Static_Type;
   begin
      Result :=
        (Status => Parse_Failed, Diagnostic => No_Diagnostic,
         Diagnostic_Position => 0, others => <>);

      Tree := (others => <>);
      Tree.Types := CCL.Catalog.Visible_Types (Visible_Interfaces);
      Visible_Types := CCL.Types.Last (Tree.Types);
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
         Result.Diagnostic_Subject := Diagnostic_Subject;
         return;
      end if;

      Check_Node (Root, 0, Root_Type);
      if Diagnostic /= No_Diagnostic or else Root_Type = Invalid_Type then
         Result.Status := Type_Check_Failed;
         Result.Diagnostic := Diagnostic;
         Result.Diagnostic_Position := Diagnostic_Position;
         Result.Diagnostic_Subject := Diagnostic_Subject;
         Result.Diagnostic_Expected := Diagnostic_Expected;
         Result.Diagnostic_Found := Diagnostic_Found;
         return;
      end if;
      Result.Status := Succeeded;
   end Check_Source;



   function Analysis_Source (Result : Analysis_Result) return String is
     (Result.Source_Text (1 .. Result.Source_Length));

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
      Check_Source (Source, Visible_Interfaces, Outcome, Tree);

      Result :=
        (Status =>
           (case Outcome.Status is
               when Succeeded => Analysis_Succeeded,
               when Type_Check_Failed => Analysis_Type_Check_Failed,
               when others => Analysis_Parse_Failed),
         Diagnostic => Outcome.Diagnostic,
         Diagnostic_Position => Outcome.Diagnostic_Position,
         Diagnostic_Subject => Outcome.Diagnostic_Subject,
         Diagnostic_Expected => Outcome.Diagnostic_Expected,
         Diagnostic_Found => Outcome.Diagnostic_Found,
         Tree => Tree, others => <>);
      for Ref in CCL.Types.Type_Reference loop
         Result.Resource_Policies (Ref) := CCL.Catalog.Resource_Policy (Visible_Interfaces, Ref);
      end loop;
      if Source'Length <= MAX_SOURCE_LENGTH then
         Result.Source_Length := Source'Length;
         Result.Source_Text (1 .. Source'Length) := Source;
      end if;
   end Analyze;


end CCL.Language;

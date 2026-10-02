with Interfaces; use Interfaces;
with CCL.Checked_Arithmetic;
with CCL.Secondary_Stacks;
with CCL.Secondary_Arrays;
with CCL.Imports;
with CCL.Handler_References;
with CCL.Objects.Values;
with CCL.Objects.Views;
with CCL.Ownership;

package body CCL.Language with
   SPARK_Mode => On
is
   use type CCL.Types.Type_Reference;
   use type CCL.Types.Definition_Result;
   use type CCL.Host_Values.Value_Kind;
   use type CCL.VM.Value_Kind;
   use type CCL.Checked_Arithmetic.Arithmetic_Error;
   use type CCL.Imports.Transfer_Mode;
   use type CCL.Imports.Cancellation_Mode;
   use type CCL.Types.Shape;
   use type CCL.Objects.Build_Result;
   use type CCL.Ownership.Ownership_Mode;
   package Object_Views renames CCL.Objects.Views;
   package T renames CCL.Text_Operations;
   package L renames CCL.List_Operations;
   use type T.Outcome;
   subtype Object_Count is Natural range 0 .. MAX_OBJECT_VALUES;
   subtype Object_Index is Positive range 1 .. MAX_OBJECT_VALUES;
   subtype Value_Node_Count is Natural range 0 .. MAX_VALUE_NODES;
   subtype Value_Node_Index is Positive range 1 .. MAX_VALUE_NODES;
   subtype Value_Slot_Count is Natural range 0 .. MAX_VALUE_SLOTS;
   subtype Value_Slot_Index is Positive range 1 .. MAX_VALUE_SLOTS;

   --  Strings of the longest short-string size one evaluation can hold at
   --  once (splitting a long text makes many).
   TEXT_REGION_STRINGS : constant := 32;
   package Text_Regions is new CCL.Secondary_Stacks
     (Capacity => MAX_TEXT_BYTES * TEXT_REGION_STRINGS,
      Max_Values => MAX_AST_NODES,
      Max_String_Length => MAX_TEXT_BYTES);
   use type Text_Regions.Operation_Result;

   --  An interpreter scalar payload is not a VM ownership/variant value.
   --  Nominal identity and alternative belong to Runtime_Value; a payload
   --  can contain only Integer or Boolean, never resource-transfer metadata.
   type Scalar_Value is record
      Kind : CCL.VM.Scalar_Kind := CCL.VM.Integer_Value;
      Integer : Integer_64 := 0;
      Boolean : Standard.Boolean := False;
   end record;
   function Integer_Scalar (Item : Integer_64) return Scalar_Value is
     ((Kind => CCL.VM.Integer_Value, Integer => Item, others => <>));
   function Boolean_Scalar (Item : Standard.Boolean) return Scalar_Value is
     ((Kind => CCL.VM.Boolean_Value, Boolean => Item, others => <>));
   function To_VM (Item : Scalar_Value) return CCL.VM.Value is
     (case Item.Kind is
        when CCL.VM.Integer_Value => CCL.VM.Integer_Constant (Item.Integer),
        when CCL.VM.Boolean_Value => CCL.VM.Boolean_Constant (Item.Boolean));
   function To_Host (Item : Scalar_Value) return CCL.Host_Values.Value is
     (case Item.Kind is
        when CCL.VM.Integer_Value => CCL.Host_Values.Integer_Constant (Item.Integer),
        when CCL.VM.Boolean_Value => CCL.Host_Values.Boolean_Constant (Item.Boolean));

   --  One list element: the scalar or text descriptor of its element type.
   --  Lists live in the list region (CCL.Secondary_Arrays), strings in the
   --  text region; values hold checked descriptors, never pointers.
   type List_Element is record
      Scalar : Scalar_Value := (others => <>);
      Text : Text_Regions.String_Value;
      Character_Item : Character := Character'Val (0);
      Alternative : CCL.Types.Component_Index := 1;
      --  A record or payload-variant element: its value-arena node.
      Node : Value_Node_Count := 0;
   end record;
   Null_List_Element : constant List_Element := (others => <>);
   type List_Element_Array is array (Positive range <>) of List_Element;
   package List_Regions is new CCL.Secondary_Arrays
     (Element_Type => List_Element, Null_Element => Null_List_Element,
      Element_Array => List_Element_Array,
      Capacity => MAX_LIST_ELEMENTS, Max_Values => MAX_AST_NODES);
   use type List_Regions.Operation_Result;

   type Runtime_Value is record
      Kind      : Static_Type := Invalid_Type;
      Scalar    : Scalar_Value := (others => <>);
      Text      : Text_Regions.String_Value;
      Character_Item : Character := Character'Val (0);
      Handler_Id : Function_Index := Function_Index'First;
      Alternative : CCL.Types.Component_Index := 1;
      Object_Owner : Object_Count := 0;
      Object_Position : Object_Views.Cursor;
      Items : List_Regions.Array_Value;
      --  A record or payload variant built by this evaluation: its arena
      --  node. Components only refer to earlier nodes (acyclic).
      Node : Value_Node_Count := 0;
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

   function Addition_Overflows (Left, Right : Integer_64) return Boolean is
     (if Right > 0 then
         Left > Integer_64'Last - Right
      elsif Right < 0 then
         Left < Integer_64'First - Right
      else False);

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
      --  The type whose declaration is being read: inside it, its own name
      --  may appear only as a list element ((List Self)), which Read_Type
      --  reports through Self_List_Read.
      Declaring : Name;
      Self_List_Read : Boolean := False;
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
               for P in 1 .. CCL.Types.Describe (Tree.Types, Record_Type).Count loop
                  Parse_Expression (Depth + 1, Components (P));
                  if Diagnostic /= No_Diagnostic then return; end if;
               end loop;
               Expect (')', Ok);
               if Ok then
                  Add_Node ((Kind => Record_Construct, Identifier => Operator_Name,
                    Declared_Kind => Record_Type, Components => Components, others => <>), Index);
               end if;
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
                  Parse_Program (Depth + 1, Tail);
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
            CCL.Types.Define (Tree.Types, Definition, Defined_Type, Definition_Status);
            if Definition_Status /= CCL.Types.Defined then
               Diagnostic := Invalid_Type_Declaration; return;
            end if;
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
            Parse_Program (Depth + 1, Tail);
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
         --  Publish the slot reserved before parsing the body, rather than
         --  reading a count through the recursively updated syntax tree.
         Tree.Function_Count := Id + 1;
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
      Visible_Types : Static_Type := Unit_Type;

      function Referenceable (Item : Name) return Boolean is
        (Item.Length > 0 and then Item.Data (1) not in '-' | '0' .. '9' and then
         not Name_Is (Item, "true") and then not Name_Is (Item, "false"));

      function Reserved (Item : Name) return Boolean is
        (not Referenceable (Item) or else
         Name_Is (Item, "type") or else Name_Is (Item, "define") or else Name_Is (Item, "handler") or else Name_Is (Item, "let") or else
         Name_Is (Item, "match") or else
         Name_Is (Item, "field") or else
         Name_Is (Item, "fn") or else Name_Is (Item, "list") or else
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

      function Supported_Object (Kind : Static_Type) return Boolean is
        (Kind in Integer_Type | Boolean_Type or else
         CCL.Types.Is_Scalar_Sum (Tree.Types, Kind));

      function Matches_Host
        (Value : CCL.Host_Values.Value; Kind : CCL.Host_Values.Value_Kind;
         Limit : CCL.Host_Values.Text_Length; Schema : CCL.Objects.Schema_Key) return Boolean
      is
         Contract : CCL.Objects.Binding;
      begin
         if Kind /= CCL.Host_Values.Object_Value then
            return CCL.Host_Values.Matches (Value, Kind, Limit);
         end if;
         CCL.Catalog.Resolve_Schema (Visible_Interfaces, Schema, Contract);
         return CCL.Host_Values.Matches (Value, Contract);
      end Matches_Host;

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
               Check_Node (Tree.Nodes (Index).Second, Depth + 1, Kind);
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
                  if Operation /= Range_Builtin then
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
                           Tree.Nodes (Index).Components (P), Host_Object_Type_Mismatch);
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
                             when CCL.Host_Values.Object_Value | CCL.Host_Values.Resource_Value => Host_Object_Type_Mismatch);
                        end if;
                     end if;
                     if Diagnostic = No_Diagnostic then Kind := Result_Type; end if;
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

      Value_Env : Value_Environment := [others => (others => <>)];
      Value_Env_Length : Natural range 0 .. MAX_BINDINGS := 0;
      Text_Region : Text_Regions.Stack;
      List_Region : List_Regions.Stack;
      type Object_Array is array (Object_Index) of Object_Views.Snapshot;
      Objects : Object_Array;
      Objects_Used : Object_Count := 0;
      --  The value arena. A node's components are Count slots from First;
      --  every node its slots refer to was allocated before it.
      type Value_Node is record
         Kind : Static_Type := Invalid_Type;
         Alternative : CCL.Types.Component_Index := 1;
         First : Positive range 1 .. MAX_VALUE_SLOTS + 1 := 1;
         Count : CCL.Types.Component_Count := 0;
      end record;
      type Value_Node_Array is array (Value_Node_Index) of Value_Node;
      type Value_Slot_Array is array (Value_Slot_Index) of Runtime_Value;
      Value_Nodes : Value_Node_Array;
      Value_Slots : Value_Slot_Array;
      Nodes_Used : Value_Node_Count := 0;
      Slots_Used : Value_Slot_Count := 0;

      --  Component P of Owner's node, or False when it has none.
      procedure Component
        (Owner : Runtime_Value; P : CCL.Types.Component_Index;
         Item : out Runtime_Value; Good : out Boolean)
      is
      begin
         Item := (others => <>);
         Good := False;
         if Owner.Node /= 0 then
            declare
               N : constant Value_Node := Value_Nodes (Owner.Node);
            begin
               if P <= N.Count and then N.First <= MAX_VALUE_SLOTS - (P - 1) then
                  Item := Value_Slots (N.First + (P - 1));
                  Good := True;
               end if;
            end;
         end if;
      end Component;
      subtype Remaining_Fuel is Natural range 0 .. Fuel;
      Fuel_Left : Remaining_Fuel := Fuel;
      Eval_Status : Interpretation_Status := Succeeded;

      procedure Load_View
        (Owner : Object_Index; Position : Object_Views.Cursor;
         Item : out Runtime_Value; Good : out Boolean)
      is
         Choice : CCL.Types.Component_Count;
      begin
         Item := (others => <>); Good := False;
         Item.Kind := Object_Views.Local_Type (Objects (Owner), Position, Tree.Types);
         case Item.Kind is
            when Integer_Type =>
               Item.Scalar := Integer_Scalar (CCL.Objects.Integer_Of (Object_Views.Scalar (Objects (Owner), Position)));
            when Boolean_Type =>
               Item.Scalar := Boolean_Scalar (Object_Views.Scalar (Objects (Owner), Position).First = 1);
            when String_Type =>
               Item.Object_Owner := Owner; Item.Object_Position := Position;
            when Character_Type =>
               declare
                  C : constant Unsigned_64 := Object_Views.Scalar (Objects (Owner), Position).First;
               begin
                  if C > 255 then return; end if;
                  Item.Character_Item := Character'Val (C);
               end;
            when Unit_Type => null;
            when CCL.Types.Declared_Type =>
               Item.Object_Owner := Owner; Item.Object_Position := Position;
               if CCL.Types.Describe (Tree.Types, Item.Kind).Form = CCL.Types.Sum then
                  Choice := Object_Views.Alternative (Objects (Owner), Position);
                  if Choice = 0 then return; end if;
                  Item.Alternative := Choice;
                  if CCL.Types.Is_Scalar_Sum (Tree.Types, Item.Kind) then
                     declare
                        Payload : constant CCL.Objects.Cell := Object_Views.Scalar
                          (Objects (Owner), Object_Views.Payload (Objects (Owner), Position));
                     begin
                        Item.Scalar := (if CCL.Types.Describe (Tree.Types, Item.Kind).Parts (Choice).Payload = Boolean_Type
                          then Boolean_Scalar (Payload.First = 1) else Integer_Scalar (CCL.Objects.Integer_Of (Payload)));
                     end;
                  end if;
               end if;
            when others => return;
         end case;
         Good := True;
      end Load_View;

      function String_Length (Item : Runtime_Value) return Object_Views.Text_Size is
        (if Item.Object_Owner = 0 then Text_Regions.Length (Item.Text)
         else Object_Views.Text_Length (Objects (Item.Object_Owner), Item.Object_Position));

      procedure Copy_String (Item : Runtime_Value; Target : out String; Good : out Boolean) is
         Status : Text_Regions.Operation_Result;
      begin
         if Item.Object_Owner /= 0 then
            Object_Views.Copy_Text (Objects (Item.Object_Owner), Item.Object_Position, Target, Good);
         else
            Text_Regions.Copy_To (Text_Region, Item.Text, Target, Status);
            Good := Status = Text_Regions.Operation_Ok;
         end if;
      end Copy_String;

      procedure Append_Runtime
        (Value : in out CCL.Objects.Image; Item : Runtime_Value; Good : out Boolean)
        with Subprogram_Variant => (Decreases => Item.Node)
      is
         Built : CCL.Objects.Build_Result := CCL.Objects.Invalid_Image;
         Text : String (1 .. MAX_TEXT_BYTES) := [others => ' '];
         Length : constant Natural := Text_Regions.Length (Item.Text);
         Copied : Text_Regions.Operation_Result;
         D : CCL.Types.Description;
      begin
         if Item.Node /= 0 then
            --  An arena node: its cell, then its components depth first.
            --  Components are older nodes, so the recursion ends.
            declare
               N : constant Value_Node := Value_Nodes (Item.Node);
               Part : Runtime_Value;
            begin
               CCL.Objects.Append
                 (Value,
                  (if CCL.Types.Describe (Tree.Types, Item.Kind).Form = CCL.Types.Product
                   then CCL.Objects.Product_Cell (N.Count)
                   else CCL.Objects.Variant_Cell (N.Alternative)), Built);
               Good := Built = CCL.Objects.Added;
               for P in 1 .. N.Count loop
                  exit when not Good;
                  Component (Item, P, Part, Good);
                  exit when not Good;
                  if Part.Node >= Item.Node then
                     Good := False;
                     exit;
                  end if;
                  Append_Runtime (Value, Part, Good);
               end loop;
            end;
            if not Good and then Eval_Status = Succeeded then
               Eval_Status := Evaluation_Object_Storage_Exhausted;
            end if;
            return;
         end if;
         if Item.Object_Owner /= 0 then
            Object_Views.Append_Value (Objects (Item.Object_Owner), Item.Object_Position, Value, Built);
         else
            case Item.Kind is
               when Integer_Type => CCL.Objects.Append (Value, CCL.Objects.Integer_Cell (Item.Scalar.Integer), Built);
               when Boolean_Type => CCL.Objects.Append (Value, CCL.Objects.Boolean_Cell (Item.Scalar.Boolean), Built);
               when Character_Type => CCL.Objects.Append (Value, CCL.Objects.Character_Cell (Item.Character_Item), Built);
               when Unit_Type => CCL.Objects.Append (Value, CCL.Objects.Unit_Cell, Built);
               when String_Type =>
                  Text_Regions.Copy_To (Text_Region, Item.Text, Text (1 .. Length), Copied);
                  if Copied = Text_Regions.Operation_Ok then
                     CCL.Objects.Append_Text (Value, Text (1 .. Length), Built);
                  end if;
               when CCL.Types.Declared_Type =>
                  D := CCL.Types.Describe (Tree.Types, Item.Kind);
                  if D.Form = CCL.Types.Sum and then Item.Alternative <= D.Count then
                     CCL.Objects.Append (Value, CCL.Objects.Variant_Cell (Item.Alternative), Built);
                     if Built = CCL.Objects.Added then
                        case D.Parts (Item.Alternative).Payload is
                           when Integer_Type => CCL.Objects.Append (Value, CCL.Objects.Integer_Cell (Item.Scalar.Integer), Built);
                           when Boolean_Type => CCL.Objects.Append (Value, CCL.Objects.Boolean_Cell (Item.Scalar.Boolean), Built);
                           when Unit_Type => CCL.Objects.Append (Value, CCL.Objects.Unit_Cell, Built);
                           when others => Built := CCL.Objects.Invalid_Image;
                        end case;
                     end if;
                  end if;
               when others => null;
            end case;
         end if;
         Good := Built = CCL.Objects.Added;
         if not Good and then Eval_Status = Succeeded then
            Eval_Status := Evaluation_Object_Storage_Exhausted;
         end if;
      end Append_Runtime;

      --  The canonical literal of Item, appended to Output: source the reader
      --  accepts back as the same value. Values without a literal spelling
      --  (characters, functions, lists inside records for now) fail.
      --  A value entering a position of type Target (a field, payload,
      --  parameter or result): outside a range type's bounds it is a typed
      --  Evaluation_Range_Error, never wrapped or clamped.
      procedure Check_Range
        (Value : Runtime_Value; Target : Static_Type; Good : in out Boolean) is
      begin
         if Good and then CCL.Types.Is_Range (Tree.Types, Target) and then
           (Value.Scalar.Integer < CCL.Types.Low_Of (Tree.Types, Target) or else
            Value.Scalar.Integer > CCL.Types.High_Of (Tree.Types, Target))
         then
            Eval_Status := Evaluation_Range_Error;
            Good := False;
         end if;
      end Check_Range;

      --  Termination: every node Item reaches is below Bound. A record steps
      --  to its components with Bound := its own (older) node; a list steps
      --  to its elements at the same Bound but Level 0, and an element is
      --  never itself a list.
      subtype Print_Bound is Natural range 0 .. MAX_VALUE_NODES + 1;
      subtype Print_Level is Natural range 0 .. 1;
      procedure Print_Value
        (Item : Runtime_Value; Bound : Print_Bound; Level : Print_Level;
         Output : in out Text_Result; Good : in out Boolean)
        with Subprogram_Variant => (Decreases => Bound, Decreases => Level)
      is
         procedure Add (Text : String) is
         begin
            if Good and then Text'Length <= MAX_TEXT_BYTES - Output.Length then
               Output.Data (Output.Length + 1 .. Output.Length + Text'Length) := Text;
               Output.Length := Output.Length + Text'Length;
            else
               Good := False;
            end if;
         end Add;
         procedure Print_Type (Kind : Static_Type) is
         begin
            Add (CCL.Types.Image (CCL.Types.Describe (Tree.Types, Kind).Identifier));
         end Print_Type;
         function Trimmed (Image : String) return String is
           (if Image'Length >= 2 and then Image (Image'First) = ' '
            then Image (Image'Last - (Image'Length - 2) .. Image'Last) else Image);
         D : CCL.Types.Description;
         Part : Runtime_Value;
         C : Character := ' ';
         Region_Result : Text_Regions.Operation_Result;
         Raw : List_Element;
         Read_Status : List_Regions.Operation_Result;
      begin
         if not Good then return; end if;
         if Item.Node >= Bound then
            Good := False;
            return;
         end if;
         case Item.Kind is
            when Integer_Type => Add (Trimmed (Item.Scalar.Integer'Image));
            when Boolean_Type => Add ((if Item.Scalar.Boolean then "true" else "false"));
            when String_Type =>
               Add ("""");
               for I in 1 .. String_Length (Item) loop
                  exit when not Good;
                  if Item.Object_Owner /= 0 then
                     Object_Views.Read_Text (Objects (Item.Object_Owner), Item.Object_Position, I, C, Good);
                  elsif I <= Text_Regions.String_Index'Last then
                     Text_Regions.Read (Text_Region, Item.Text, Text_Regions.String_Index (I), C, Region_Result);
                     Good := Region_Result = Text_Regions.Operation_Ok;
                  else
                     Good := False;
                  end if;
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
            when CCL.Types.Declared_Type =>
               D := CCL.Types.Describe (Tree.Types, Item.Kind);
               if D.Form = CCL.Types.Sequence then
                  --  [e1 e2 ...], or (list-of T) when empty.
                  if Level = 0 then Good := False; return; end if;
                  if List_Regions.Length (Item.Items) = 0 then
                     Add ("(list-of ");
                     Print_Type (CCL.Types.Element_Of (Tree.Types, Item.Kind));
                     Add (")");
                     return;
                  end if;
                  Add ("[");
                  for I in 1 .. List_Regions.Length (Item.Items) loop
                     exit when not Good;
                     List_Regions.Read
                       (List_Region, Item.Items, List_Regions.Array_Index (I), Raw, Read_Status);
                     if Read_Status /= List_Regions.Operation_Ok then Good := False; exit; end if;
                     if I > 1 then Add (" "); end if;
                     Print_Value
                       ((Kind => CCL.Types.Element_Of (Tree.Types, Item.Kind),
                         Scalar => Raw.Scalar, Text => Raw.Text,
                         Character_Item => Raw.Character_Item,
                         Alternative => Raw.Alternative, Node => Raw.Node, others => <>),
                        Bound, 0, Output, Good);
                  end loop;
                  Add ("]");
               elsif D.Form = CCL.Types.Product and then Item.Node /= 0 then
                  Add ("(" & CCL.Types.Image (D.Identifier));
                  for P in 1 .. Value_Nodes (Item.Node).Count loop
                     exit when not Good;
                     Component (Item, P, Part, Good);
                     exit when not Good;
                     Add (" ");
                     Print_Value (Part, Item.Node, 1, Output, Good);
                  end loop;
                  Add (")");
               elsif D.Form = CCL.Types.Sum and then Item.Alternative <= D.Count then
                  if D.Parts (Item.Alternative).Payload = Unit_Type then
                     Add (CCL.Types.Image (D.Identifier) & "." &
                          CCL.Types.Image (D.Parts (Item.Alternative).Identifier));
                  else
                     Add ("(" & CCL.Types.Image (D.Identifier) & "." &
                          CCL.Types.Image (D.Parts (Item.Alternative).Identifier) & " ");
                     if Item.Node /= 0 then
                        Component (Item, 1, Part, Good);
                        if Good then Print_Value (Part, Item.Node, 1, Output, Good); end if;
                     else
                        --  A scalar payload carried inline: printed here, not
                        --  by recursion (it has no node to decrease).
                        case D.Parts (Item.Alternative).Payload is
                           when Integer_Type => Add (Trimmed (Item.Scalar.Integer'Image));
                           when Boolean_Type => Add ((if Item.Scalar.Boolean then "true" else "false"));
                           when others => Good := False;
                        end case;
                     end if;
                     Add (")");
                  end if;
               else
                  Good := False;
               end if;
            when others => Good := False;
         end case;
      end Print_Value;

      procedure Join_Strings (Left, Right : Runtime_Value; Item : out Runtime_Value; Good : out Boolean) is
         L : constant Object_Views.Text_Size := String_Length (Left);
         R : constant Object_Views.Text_Size := String_Length (Right);
         Data : String (1 .. CCL.Objects.Maximum_Text_Bytes) := [others => ' '];
         Region_Result : Text_Regions.Operation_Result;
         Native : CCL.Objects.Image;
         Built : CCL.Objects.Build_Result;
      begin
         Item := (others => <>); Good := False;
         if L > CCL.Objects.Maximum_Text_Bytes - R then
            Eval_Status := Evaluation_Text_Storage_Exhausted; return;
         end if;
         if L + R > MAX_TEXT_BYTES and then Objects_Used = MAX_OBJECT_VALUES then
            Eval_Status := Evaluation_Object_Storage_Exhausted; return;
         end if;
         Copy_String (Left, Data (1 .. L), Good);
         if Good then Copy_String (Right, Data (L + 1 .. L + R), Good); end if;
         if not Good then Eval_Status := Evaluation_Index_Error; return; end if;
         if L + R <= MAX_TEXT_BYTES then
            Text_Regions.Allocate_String (Text_Region, Data (1 .. L + R), Item.Text, Region_Result);
            Good := Region_Result = Text_Regions.Operation_Ok;
            if Good then Item.Kind := String_Type;
            else Eval_Status := Evaluation_Text_Storage_Exhausted; end if;
         else
            Objects_Used := Objects_Used + 1;
            CCL.Objects.Append_Text (Native, Data (1 .. L + R), Built);
            Good := Built = CCL.Objects.Added;
            if Good then Object_Views.Capture_Local (Objects (Objects_Used), Tree.Types, String_Type, Native, Good); end if;
            if Good then Load_View (Objects_Used, Object_Views.Root (Objects (Objects_Used)), Item, Good); end if;
            if not Good then Eval_Status := Evaluation_Text_Storage_Exhausted; end if;
         end if;
      end Join_Strings;

      --  Every element visited and every TEXT_BYTES_PER_FUEL bytes scanned by
      --  a builtin costs fuel, as nodes do: fuel stays the unit of work.
      TEXT_BYTES_PER_FUEL : constant := 64;

      procedure Spend (Amount : Natural; Good : out Boolean) is
      begin
         if Fuel_Left < Amount then
            Fuel_Left := 0;
            Eval_Status := Evaluation_Fuel_Exhausted;
            Good := False;
         else
            Fuel_Left := Fuel_Left - Amount;
            Good := True;
         end if;
      end Spend;

      --  A String value holding Data: in the text region when it fits, else a
      --  native object (as concatenation does).
      procedure Make_String (Data : String; Item : out Runtime_Value; Good : out Boolean) is
         Region_Result : Text_Regions.Operation_Result;
         Native : CCL.Objects.Image;
         Built : CCL.Objects.Build_Result;
      begin
         Item := (others => <>); Good := False;
         if Data'Length <= MAX_TEXT_BYTES then
            Text_Regions.Allocate_String (Text_Region, Data, Item.Text, Region_Result);
            Good := Region_Result = Text_Regions.Operation_Ok;
            if Good then Item.Kind := String_Type;
            else Eval_Status := Evaluation_Text_Storage_Exhausted; end if;
         elsif Data'Length > CCL.Objects.Maximum_Text_Bytes then
            Eval_Status := Evaluation_Text_Storage_Exhausted;
         elsif Objects_Used = MAX_OBJECT_VALUES then
            Eval_Status := Evaluation_Object_Storage_Exhausted;
         else
            Objects_Used := Objects_Used + 1;
            CCL.Objects.Append_Text (Native, Data, Built);
            Good := Built = CCL.Objects.Added;
            if Good then Object_Views.Capture_Local (Objects (Objects_Used), Tree.Types, String_Type, Native, Good); end if;
            if Good then Load_View (Objects_Used, Object_Views.Root (Objects (Objects_Used)), Item, Good); end if;
            if not Good then Eval_Status := Evaluation_Text_Storage_Exhausted; end if;
         end if;
      end Make_String;

      --  A host image's value as this evaluation's own (docs/ccl-repl.md,
      --  "Lists"): strings into the text region, records and payload
      --  variants into the arena (components first, so nodes point
      --  backwards), lists into the list region. Persistable types refer only
      --  to earlier types, so Kind decreases.
      procedure Copy_In
        (Owner : Object_Index; Position : Object_Views.Cursor; Kind : Static_Type;
         Item : out Runtime_Value; Good : out Boolean)
        with Subprogram_Variant => (Decreases => Kind)
      is
         D : constant CCL.Types.Description := CCL.Types.Describe (Tree.Types, Kind);
         Region_Result : Text_Regions.Operation_Result;
      begin
         Item := (others => <>);
         Good := Object_Views.Local_Type (Objects (Owner), Position, Tree.Types) = Kind;
         if not Good then return; end if;
         if Kind = String_Type then
            declare
               Length : constant Object_Views.Text_Size := Object_Views.Text_Length (Objects (Owner), Position);
               Buffer : String (1 .. CCL.Objects.Maximum_Text_Bytes) := [others => ' '];
            begin
               Good := Length <= MAX_TEXT_BYTES;
               if Good then
                  Object_Views.Copy_Text (Objects (Owner), Position, Buffer (1 .. Length), Good);
               end if;
               if Good then
                  Text_Regions.Allocate_String (Text_Region, Buffer (1 .. Length), Item.Text, Region_Result);
                  Good := Region_Result = Text_Regions.Operation_Ok;
                  Item.Kind := String_Type;
               end if;
               if not Good then Eval_Status := Evaluation_Text_Storage_Exhausted; end if;
            end;
         elsif Kind not in CCL.Types.Declared_Type or else CCL.Types.Is_Scalar_Sum (Tree.Types, Kind) then
            --  Scalars and scalar variants hold no reference to the image.
            Load_View (Owner, Position, Item, Good);
         elsif D.Form = CCL.Types.Sequence then
            declare
               Count : constant Object_Views.Element_Count := Object_Views.Length (Objects (Owner), Position);
               Items : List_Element_Array (1 .. CCL.Objects.Maximum_Cells) := [others => Null_List_Element];
               Part : Runtime_Value;
               Placed : List_Regions.Operation_Result;
            begin
               Good := D.Count = 1 and then D.Parts (1).Payload < Kind;
               --  Guarded on the type order itself: elements are earlier types.
               if D.Count = 1 and then D.Parts (1).Payload < Kind then
                  for E in 1 .. Count loop
                     Copy_In (Owner, Object_Views.Element (Objects (Owner), Position, E), D.Parts (1).Payload,
                              Part, Good);
                     exit when not Good;
                     Items (E) := (Scalar => Part.Scalar, Text => Part.Text, Character_Item => Part.Character_Item,
                                   Alternative => Part.Alternative, Node => Part.Node);
                  end loop;
               end if;
               if Good then
                  List_Regions.Allocate (List_Region, Items (1 .. Count), Item.Items, Placed);
                  Good := Placed = List_Regions.Operation_Ok;
                  Item.Kind := Kind;
                  if not Good then Eval_Status := Evaluation_List_Storage_Exhausted; end if;
               end if;
            end;
         else
            --  A record (every field) or a payload variant (its payload).
            declare
               Choice : constant CCL.Types.Component_Count :=
                 (if D.Form = CCL.Types.Sum then Object_Views.Alternative (Objects (Owner), Position) else 0);
               Count : constant CCL.Types.Component_Count :=
                 (if D.Form = CCL.Types.Product then D.Count
                  elsif Choice in 1 .. D.Count and then D.Parts (Choice).Payload /= Unit_Type then 1 else 0);
               Parts : array (CCL.Types.Component_Index) of Runtime_Value := [others => (others => <>)];
            begin
               Good := D.Form = CCL.Types.Product or else Choice in 1 .. D.Count;
               for P in 1 .. Count loop
                  exit when not Good;
                  declare
                     Part_Kind : constant Static_Type :=
                       D.Parts (if D.Form = CCL.Types.Product then P else Choice).Payload;
                  begin
                     Good := Part_Kind < Kind;
                     if Good then
                        Copy_In (Owner,
                                 (if D.Form = CCL.Types.Product then Object_Views.Field (Objects (Owner), Position, P)
                                  else Object_Views.Payload (Objects (Owner), Position)),
                                 Part_Kind, Parts (P), Good);
                     end if;
                  end;
               end loop;
               if Good and then D.Form = CCL.Types.Sum and then Count = 0 then
                  --  A unit member: no node.
                  Item := (Kind => Kind, Alternative => Choice, others => <>);
               elsif Good then
                  if Nodes_Used = MAX_VALUE_NODES or else MAX_VALUE_SLOTS - Slots_Used < Count then
                     Eval_Status := Evaluation_Object_Storage_Exhausted;
                     Good := False;
                  else
                     for P in 1 .. Count loop
                        Value_Slots (Slots_Used + P) := Parts (P);
                     end loop;
                     Nodes_Used := Nodes_Used + 1;
                     Value_Nodes (Nodes_Used) :=
                       (Kind => Kind, Alternative => (if Choice in CCL.Types.Component_Index then Choice else 1),
                        First => Slots_Used + 1, Count => Count);
                     Slots_Used := Slots_Used + Count;
                     Item := (Kind => Kind, Alternative => (if Choice in CCL.Types.Component_Index then Choice else 1),
                              Node => Nodes_Used, others => <>);
                  end if;
               end if;
            end;
         end if;
      end Copy_In;

      --  Order of two region strings (list elements), compared character by
      --  character without copying.
      function Text_Less (Left, Right : Text_Regions.String_Value) return Boolean is
         L : constant Natural := Text_Regions.Length (Left);
         R : constant Natural := Text_Regions.Length (Right);
         A, B : Character;
         Status_A, Status_B : Text_Regions.Operation_Result;
      begin
         for I in 1 .. Natural'Min (L, R) loop
            if I - 1 > Text_Regions.String_Index'Last - Text_Regions.First_Index (Left) or else
              I - 1 > Text_Regions.String_Index'Last - Text_Regions.First_Index (Right)
            then
               return False;
            end if;
            Text_Regions.Read (Text_Region, Left, Text_Regions.First_Index (Left) + (I - 1), A, Status_A);
            Text_Regions.Read (Text_Region, Right, Text_Regions.First_Index (Right) + (I - 1), B, Status_B);
            if Status_A /= Text_Regions.Operation_Ok or else Status_B /= Text_Regions.Operation_Ok then
               return False;
            elsif A /= B then
               return A < B;
            end if;
         end loop;
         return L < R;
      end Text_Less;

      --  Equality of two String values, wherever each is stored.
      function Equal_Strings (Left, Right : Runtime_Value) return Boolean is
         L : constant Object_Views.Text_Size := String_Length (Left);
         A, B : String (1 .. CCL.Objects.Maximum_Text_Bytes) := [others => ' '];
         Good_A, Good_B : Boolean;
      begin
         if L /= String_Length (Right) then return False; end if;
         Copy_String (Left, A (1 .. L), Good_A);
         Copy_String (Right, B (1 .. L), Good_B);
         return Good_A and then Good_B and then A (1 .. L) = B (1 .. L);
      end Equal_Strings;

      --  Builtins whose subject (last operand) is a String. Kept out of
      --  Evaluate_Node so the text buffers live only during this call, not on
      --  every level of the evaluation recursion.
      procedure Text_Builtin
        (Operation : Builtin_Operation; First, Second, Subject : Runtime_Value;
         Result_Kind : Static_Type; Item : out Runtime_Value; Good : out Boolean)
      is
         Max : constant := CCL.Objects.Maximum_Text_Bytes;
         S_Length : constant Object_Views.Text_Size := String_Length (Subject);
         S : String (1 .. Max) := [others => ' '];
         --  Needles, separators and replacements are short strings.
         A_Length, B_Length : Natural range 0 .. MAX_TEXT_BYTES := 0;
         A, B : String (1 .. MAX_TEXT_BYTES) := [others => ' '];
         Result : String (1 .. Max) := [others => ' '];
         Result_Length : T.String_Length := 0;
      begin
         Item := (others => <>);
         Copy_String (Subject, S (1 .. S_Length), Good);
         if Good and then Operation in Contains_Builtin | Starts_With_Builtin | Ends_With_Builtin |
           Index_Of_Builtin | Replace_Builtin | Split_Builtin
         then
            if String_Length (First) > MAX_TEXT_BYTES then
               Eval_Status := Evaluation_Text_Storage_Exhausted; Good := False; return;
            end if;
            A_Length := String_Length (First);
            Copy_String (First, A (1 .. A_Length), Good);
         end if;
         if Good and then Operation = Replace_Builtin then
            if String_Length (Second) > MAX_TEXT_BYTES then
               Eval_Status := Evaluation_Text_Storage_Exhausted; Good := False; return;
            end if;
            B_Length := String_Length (Second);
            Copy_String (Second, B (1 .. B_Length), Good);
         end if;
         if not Good then
            if Eval_Status = Succeeded then Eval_Status := Evaluation_Index_Error; end if;
            return;
         end if;
         Spend (1 + S_Length / TEXT_BYTES_PER_FUEL, Good);
         if not Good then return; end if;

         case Operation is
            when Upper_Builtin | Lower_Builtin | Reverse_Builtin =>
               T.Transform (Text_Operation_Of (Operation), S (1 .. S_Length), Result (1 .. S_Length));
               Make_String (Result (1 .. S_Length), Item, Good);
            when Trim_Builtin | First_Builtin | Last_Builtin | Skip_Builtin =>
               declare
                  Low : Positive;
                  High : Natural;
               begin
                  T.Slice (Text_Operation_Of (Operation), S (1 .. S_Length),
                           (if Operation = Trim_Builtin then 0 else First.Scalar.Integer), Low, High);
                  Make_String (S (Low .. High), Item, Good);
               end;
            when Contains_Builtin =>
               Item := (Kind => Boolean_Type,
                        Scalar => Boolean_Scalar (T.Test (T.Contains, S (1 .. S_Length), A (1 .. A_Length))),
                        others => <>);
            when Index_Of_Builtin =>
               Item := (Kind => Integer_Type,
                        Scalar => Integer_Scalar (Integer_64
                          (if A_Length = 0 then 1 else T.Find (S (1 .. S_Length), A (1 .. A_Length), 1))),
                        others => <>);
            when Starts_With_Builtin | Ends_With_Builtin =>
               Item := (Kind => Boolean_Type,
                        Scalar => Boolean_Scalar
                          (T.Test (Text_Operation_Of (Operation), S (1 .. S_Length), A (1 .. A_Length))),
                        others => <>);
            when Replace_Builtin =>
               declare
                  Status : T.Outcome;
               begin
                  T.Replace_All (S (1 .. S_Length), A (1 .. A_Length), B (1 .. B_Length),
                                 Result, Result_Length, Status);
                  if Status /= T.Done then
                     Eval_Status := Evaluation_Text_Storage_Exhausted; Good := False;
                  else
                     Make_String (Result (1 .. Result_Length), Item, Good);
                  end if;
               end;
            when Split_Builtin =>
               --  On each separator; an empty separator splits on runs of
               --  blanks and drops empty pieces (words).
               declare
                  Pieces : List_Regions.Array_Value;
                  Placed : List_Regions.Operation_Result;
                  Count : Natural range 0 .. MAX_LIST_ELEMENTS := 0;
                  Capacity : constant Natural range 0 .. MAX_LIST_ELEMENTS := Natural'Min (S_Length + 1, MAX_LIST_ELEMENTS);
                  Region_Result : Text_Regions.Operation_Result;
                  Piece : Text_Regions.String_Value;

                  procedure Add (Low, High : Natural) is
                  begin
                     if not Good then return; end if;
                     if Count >= Capacity or else (High >= Low and then High - Low + 1 > MAX_TEXT_BYTES) then
                        Eval_Status := Evaluation_List_Storage_Exhausted; Good := False; return;
                     end if;
                     Text_Regions.Allocate_String
                       (Text_Region, (if High >= Low then S (Low .. High) else ""), Piece, Region_Result);
                     if Region_Result /= Text_Regions.Operation_Ok then
                        Eval_Status := Evaluation_Text_Storage_Exhausted; Good := False; return;
                     end if;
                     Count := Count + 1;
                     List_Regions.Write (List_Region, Pieces, Count, (Null_List_Element with delta Text => Piece), Placed);
                     if Placed /= List_Regions.Operation_Ok then
                        Eval_Status := Evaluation_Index_Error; Good := False;
                     end if;
                  end Add;
               begin
                  List_Regions.Reserve (List_Region, Capacity, Pieces, Placed);
                  if Placed /= List_Regions.Operation_Ok then
                     Eval_Status := Evaluation_List_Storage_Exhausted; Good := False; return;
                  end if;
                  declare
                     P : Positive := 1;
                     Finished : Boolean := False;
                     Low : Positive;
                     High : Natural;
                     Found : Boolean;
                  begin
                     --  At most S_Length + 1 pieces; one more step reports a
                     --  full list when Capacity is smaller.
                     for Step in 0 .. Capacity loop
                        pragma Loop_Invariant (P <= S_Length + 1);
                        exit when not Good;
                        T.Next_Piece (S (1 .. S_Length), A (1 .. A_Length), P, Finished, Low, High, Found);
                        exit when not Found;
                        Add (Low, High);
                     end loop;
                  end;
                  if Good then
                     List_Regions.Shrink (List_Region, Pieces, Count, Placed);
                     if Placed = List_Regions.Operation_Ok then
                        Item.Items := Pieces;
                        Item.Kind := Result_Kind;
                     else
                        Eval_Status := Evaluation_List_Storage_Exhausted; Good := False;
                     end if;
                  end if;
               end;
            when Parse_Int_Builtin =>
               declare
                  Value : Integer_64;
                  Status : T.Outcome;
               begin
                  T.Parse_Integer (S (1 .. S_Length), Value, Status);
                  case Status is
                     when T.Done =>
                        Item := (Kind => Integer_Type, Scalar => Integer_Scalar (Value), others => <>);
                     when T.Overflow => Eval_Status := Evaluation_Overflow; Good := False;
                     when others => Eval_Status := Evaluation_Invalid_Number; Good := False;
                  end case;
               end;
            when others =>
               Eval_Status := Host_Contract_Unsupported; Good := False;
         end case;
      end Text_Builtin;

      --  (join separator xs) over a List<String>.
      procedure Join_List
        (Separator, Source : Runtime_Value; Item : out Runtime_Value; Good : out Boolean)
      is
         Max : constant := CCL.Objects.Maximum_Text_Bytes;
         Sep_Length : constant Object_Views.Text_Size := String_Length (Separator);
         Sep : String (1 .. Max) := [others => ' '];
         Result : String (1 .. Max) := [others => ' '];
         Result_Length : Natural := 0;
         Raw : List_Element;
         Read_Status : List_Regions.Operation_Result;
         Copied : Text_Regions.Operation_Result;
         Piece_Length : Natural;
         Count : constant Natural := List_Regions.Length (Source.Items);
      begin
         Item := (others => <>);
         Copy_String (Separator, Sep (1 .. Sep_Length), Good);
         if not Good then Eval_Status := Evaluation_Index_Error; return; end if;
         for I in 1 .. Count loop
            Spend (1, Good);
            exit when not Good;
            List_Regions.Read (List_Region, Source.Items, List_Regions.Array_Index (I), Raw, Read_Status);
            if Read_Status /= List_Regions.Operation_Ok then
               Eval_Status := Evaluation_Index_Error; Good := False; exit;
            end if;
            Piece_Length := Text_Regions.Length (Raw.Text);
            if (I > 1 and then Sep_Length > Max - Result_Length) or else
              Piece_Length > Max - Result_Length - (if I > 1 then Sep_Length else 0)
            then
               Eval_Status := Evaluation_Text_Storage_Exhausted; Good := False; exit;
            end if;
            if I > 1 then
               Result (Result_Length + 1 .. Result_Length + Sep_Length) := Sep (1 .. Sep_Length);
               Result_Length := Result_Length + Sep_Length;
            end if;
            Text_Regions.Copy_To
              (Text_Region, Raw.Text, Result (Result_Length + 1 .. Result_Length + Piece_Length), Copied);
            if Copied /= Text_Regions.Operation_Ok then
               Eval_Status := Evaluation_Index_Error; Good := False; exit;
            end if;
            Result_Length := Result_Length + Piece_Length;
         end loop;
         if Good then Make_String (Result (1 .. Result_Length), Item, Good); end if;
      end Join_List;

      --  Starts a call frame of Decl: its captured values (stored in the
      --  function value Callee) at the bottom, parameters to follow.
      procedure Bind_Captures
        (Callee : Runtime_Value; Decl : Function_Declaration; Good : out Boolean)
      is
         Raw : List_Element;
         Read_Status : List_Regions.Operation_Result;
      begin
         Good := True;
         Value_Env_Length := 0;
         for C in 1 .. Decl.Captured loop
            List_Regions.Read (List_Region, Callee.Items, C, Raw, Read_Status);
            if Read_Status /= List_Regions.Operation_Ok then
               Eval_Status := Evaluation_Index_Error;
               Good := False;
               return;
            end if;
            Value_Env (C - 1) :=
              (Identifier => Decl.Captures (C).Identifier,
               Item => (Kind => Decl.Captures (C).Kind, Scalar => Raw.Scalar, Text => Raw.Text,
                        Character_Item => Raw.Character_Item, Alternative => Raw.Alternative,
                        Node => Raw.Node, others => <>));
            Value_Env_Length := C;
         end loop;
      end Bind_Captures;

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
         Character_Item : Character;
         Entry_Environment_Length : constant Natural range 0 .. MAX_BINDINGS :=
           Value_Env_Length;
         Reserved_Object : Object_Count := 0;
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
            when Type_Definition | Function_Definition =>
               --  Definitions are checked before execution; only the final
               --  expression executes, never an unused function body.
               Evaluate_Node (Tree.Nodes (Index).Second, Depth + 1, Item, Ok);
            when Function_Call =>
               declare
                  --  A call through a value takes its function from the binding.
                  function Callee return Runtime_Value is
                  begin
                     if Tree.Nodes (Index).Calls_Value and then Value_Env_Length > 0 then
                        for Position in reverse 0 .. Value_Env_Length - 1 loop
                           if Names_Equal (Value_Env (Position).Identifier, Tree.Nodes (Index).Identifier) then
                              return Value_Env (Position).Item;
                           end if;
                        end loop;
                     end if;
                     return (Handler_Id => Tree.Nodes (Index).Function_Id, others => <>);
                  end Callee;
                  Target : constant Runtime_Value := Callee;
                  Decl : constant Function_Declaration := Tree.Functions (Target.Handler_Id);
                  type Parameter_Values is array (Parameter_Index) of Runtime_Value;
                  Arguments : Parameter_Values := [others => (others => <>)];
               begin
                  Good := True;
                  --  Left-to-right, exactly once, in the caller's environment.
                  for P in 1 .. Decl.Count loop
                     Evaluate_Node (Tree.Nodes (Index).Arguments (P), Depth + 1, Arguments (P), Good);
                     Check_Range (Arguments (P), Decl.Parameters (P).Kind, Good);
                     exit when not Good;
                  end loop;
                  if Good then
                     declare
                        Saved : constant Value_Environment := Value_Env;
                     begin
                        Bind_Captures (Target, Decl, Good);
                        if Good then
                           for P in 1 .. Decl.Count loop
                              Value_Env (Decl.Captured + P - 1) :=
                                (Identifier => Decl.Parameters (P).Identifier, Item => Arguments (P));
                           end loop;
                           Value_Env_Length := Decl.Captured + Decl.Count;
                           Evaluate_Node (Decl.Body_Node, Depth + 1, Item, Good);
                           Check_Range (Item, Decl.Result_Kind, Good);
                        end if;
                        Value_Env := Saved;
                        Value_Env_Length := Entry_Environment_Length;
                     end;
                  end if;
                  Ok := Good;
               end;
            when Integer_Literal =>
               Item.Kind := Integer_Type;
               Item.Scalar := Integer_Scalar
                 (Tree.Nodes (Node_Index (Index)).Integer_Value);
               Ok := True;
            when Variant_Literal =>
               Item.Kind := Tree.Nodes (Index).Declared_Kind;
               Item.Alternative := Tree.Nodes (Index).Alternative;
               Ok := True;
            when Variant_Construct | Record_Construct =>
               if Tree.Nodes (Index).Kind = Variant_Construct and then
                 CCL.Types.Is_Scalar_Sum (Tree.Types, Tree.Nodes (Index).Declared_Kind)
               then
                  Evaluate_Node (Tree.Nodes (Index).First, Depth + 1, Left, Good);
                  if Good then
                     Item.Kind := Tree.Nodes (Index).Declared_Kind;
                     Item.Alternative := Tree.Nodes (Index).Alternative;
                     Item.Scalar := Left.Scalar;
                  end if;
                  Ok := Good;
               else
                  --  An arena node: reserve its slots, evaluate the
                  --  components into them (they may allocate nodes of their
                  --  own), then allocate the node, after all of theirs.
                  declare
                     D : constant CCL.Types.Description :=
                       CCL.Types.Describe (Tree.Types, Tree.Nodes (Index).Declared_Kind);
                     Count : constant CCL.Types.Component_Count :=
                       (if D.Form = CCL.Types.Product then D.Count else 1);
                     First : Positive range 1 .. MAX_VALUE_SLOTS + 1;
                  begin
                     if MAX_VALUE_SLOTS - Slots_Used < Count then
                        Eval_Status := Evaluation_Object_Storage_Exhausted; return;
                     end if;
                     First := Slots_Used + 1;
                     Slots_Used := Slots_Used + Count;
                     Good := True;
                     for P in 1 .. Count loop
                        Evaluate_Node
                          ((if D.Form = CCL.Types.Product then Tree.Nodes (Index).Components (P)
                            else Tree.Nodes (Index).First), Depth + 1, Left, Good);
                        Check_Range
                          (Left, D.Parts (if D.Form = CCL.Types.Product then P
                                          else Tree.Nodes (Index).Alternative).Payload, Good);
                        exit when not Good;
                        if First > MAX_VALUE_SLOTS - (P - 1) then Good := False; exit; end if;
                        Value_Slots (First + (P - 1)) := Left;
                     end loop;
                     if Good then
                        if Nodes_Used = MAX_VALUE_NODES then
                           Eval_Status := Evaluation_Object_Storage_Exhausted; return;
                        end if;
                        Nodes_Used := Nodes_Used + 1;
                        Value_Nodes (Nodes_Used) :=
                          (Kind => Tree.Nodes (Index).Declared_Kind,
                           Alternative => Tree.Nodes (Index).Alternative,
                           First => First, Count => Count);
                        Item := (Kind => Tree.Nodes (Index).Declared_Kind,
                                 Alternative => Tree.Nodes (Index).Alternative,
                                 Node => Nodes_Used, others => <>);
                     end if;
                     if not Good and Eval_Status = Succeeded then Eval_Status := Host_Result_Type_Mismatch; end if;
                     Ok := Good;
                  end;
               end if;
            when Field_Form =>
               Evaluate_Node (Tree.Nodes (Index).First, Depth + 1, Left, Good);
               if Good and then Left.Node /= 0 then
                  Component (Left, Tree.Nodes (Index).Alternative, Item, Good);
               elsif Good and then Left.Object_Owner /= 0 then
                  Load_View (Left.Object_Owner,
                    Object_Views.Field (Objects (Left.Object_Owner), Left.Object_Position, Tree.Nodes (Index).Alternative),
                    Item, Good);
               else Good := False;
               end if;
               if not Good and Eval_Status = Succeeded then Eval_Status := Host_Result_Type_Mismatch; end if;
               Ok := Good;
            when Match_Form =>
               Evaluate_Node (Tree.Nodes (Index).First, Depth + 1, Left, Good);
               if Good then
                  declare
                     Arm : Node_Reference := Tree.Nodes (Index).Second;
                     N : Node;
                     Payload : constant Static_Type := CCL.Types.Describe (Tree.Types, Left.Kind).
                       Parts (Left.Alternative).Payload;
                  begin
                     while Arm < Tree.Length loop
                        N := Tree.Nodes (Arm);
                        if N.Alternative = Left.Alternative then
                           if Payload /= Unit_Type then
                              if Value_Env_Length = MAX_BINDINGS then
                                 Eval_Status := Evaluation_Depth_Exhausted; return;
                              end if;
                              if Left.Node /= 0 then
                                 Component (Left, 1, Right, Good);
                                 if not Good then
                                    if Eval_Status = Succeeded then Eval_Status := Host_Result_Type_Mismatch; end if;
                                    return;
                                 end if;
                              elsif Left.Object_Owner /= 0 then
                                 Load_View (Left.Object_Owner,
                                   Object_Views.Payload (Objects (Left.Object_Owner), Left.Object_Position), Right, Good);
                                 if not Good then
                                    if Eval_Status = Succeeded then Eval_Status := Host_Result_Type_Mismatch; end if;
                                    return;
                                 end if;
                              else Right := (Kind => Payload, Scalar => Left.Scalar, others => <>);
                              end if;
                              Value_Env (Value_Env_Length) := (Identifier => N.Identifier, Item => Right);
                              Value_Env_Length := Value_Env_Length + 1;
                           end if;
                           Evaluate_Node (N.First, Depth + 1, Item, Ok);
                           Value_Env_Length := Entry_Environment_Length;
                           exit;
                        end if;
                        Arm := N.Second;
                     end loop;
                  end;
               end if;
            when Match_Arm => Eval_Status := Type_Check_Failed;
            when Boolean_Literal =>
               Item.Kind := Boolean_Type;
               Item.Scalar := Boolean_Scalar
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
               if not Found and then Tree.Nodes (Index).Names_Function then
                  Item.Kind := Tree.Nodes (Index).Static_Kind;
                  Item.Handler_Id := Tree.Nodes (Index).Function_Id;
                  Found := True;
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
                  Item.Scalar := Integer_Scalar
                    (Left.Scalar.Integer + Right.Scalar.Integer);
               end if;
               Ok := Good;
            when Subtract_Form =>
               Evaluate_Node (Tree.Nodes (Node_Index (Index)).First,
                              Depth + 1, Left, Good);
               if Good then
                  Evaluate_Node (Tree.Nodes (Node_Index (Index)).Second,
                                 Depth + 1, Right, Good);
               end if;
               if Good then
                  CCL.Checked_Arithmetic.Subtract
                    (Left.Scalar.Integer, Right.Scalar.Integer,
                     Arithmetic_Value, Overflowed);
                  if Overflowed then
                     Eval_Status := Evaluation_Overflow;
                     Good := False;
                  else
                     Item.Kind := Integer_Type;
                     Item.Scalar := Integer_Scalar (Arithmetic_Value);
                  end if;
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
                     Item.Scalar := Integer_Scalar (Arithmetic_Value);
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
                  Item.Scalar := Boolean_Scalar
                    (if Left.Kind = Integer_Type then
                        Left.Scalar.Integer = Right.Scalar.Integer
                     elsif Left.Kind = Boolean_Type then
                        Left.Scalar.Boolean = Right.Scalar.Boolean
                     elsif Left.Kind = String_Type then
                        Equal_Strings (Left, Right)
                     elsif Left.Kind = Character_Type then
                        Left.Character_Item = Right.Character_Item
                     else Left.Alternative = Right.Alternative);
               end if;
               Ok := Good;
            when Not_Equal_Form | Less_Form | Less_Equal_Form |
                 Greater_Form | Greater_Equal_Form =>
               Evaluate_Node (Tree.Nodes (Node_Index (Index)).First,
                              Depth + 1, Left, Good);
               if Good then
                  Evaluate_Node (Tree.Nodes (Node_Index (Index)).Second,
                                 Depth + 1, Right, Good);
               end if;
               if Good then
                  Item.Kind := Boolean_Type;
                  Item.Scalar := Boolean_Scalar
                    (case Tree.Nodes (Node_Index (Index)).Kind is
                        when Not_Equal_Form =>
                          (if Left.Kind = Integer_Type then
                              Left.Scalar.Integer /= Right.Scalar.Integer
                           elsif Left.Kind = Boolean_Type then
                              Left.Scalar.Boolean /= Right.Scalar.Boolean
                           elsif Left.Kind = String_Type then
                              not Equal_Strings (Left, Right)
                           elsif Left.Kind = Character_Type then
                              Left.Character_Item /= Right.Character_Item
                           else Left.Alternative /= Right.Alternative),
                        when Less_Form =>
                          Left.Scalar.Integer < Right.Scalar.Integer,
                        when Less_Equal_Form =>
                          Left.Scalar.Integer <= Right.Scalar.Integer,
                        when Greater_Form =>
                          Left.Scalar.Integer > Right.Scalar.Integer,
                        when others =>
                          Left.Scalar.Integer >= Right.Scalar.Integer);
               end if;
               Ok := Good;
            -- Short-circuit: the right operand runs only when it decides.
            when And_Form | Or_Form =>
               Evaluate_Node (Tree.Nodes (Node_Index (Index)).First,
                              Depth + 1, Left, Good);
               if Good and then
                 Left.Scalar.Boolean = (Tree.Nodes (Node_Index (Index)).Kind = Or_Form)
               then
                  Item := Left;
               elsif Good then
                  Evaluate_Node (Tree.Nodes (Node_Index (Index)).Second,
                                 Depth + 1, Item, Good);
               end if;
               Ok := Good;
            when Not_Form =>
               Evaluate_Node (Tree.Nodes (Node_Index (Index)).First,
                              Depth + 1, Left, Good);
               if Good then
                  Item.Kind := Boolean_Type;
                  Item.Scalar := Boolean_Scalar
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
               if Good and then CCL.Types.Is_List (Tree.Types, Left.Kind) then
                  Item.Kind := Integer_Type;
                  Item.Scalar := Integer_Scalar
                    (Integer_64 (List_Regions.Length (Left.Items)));
               elsif Good then
                  Item.Kind := Integer_Type;
                  Item.Scalar := Integer_Scalar
                    (Integer_64 (String_Length (Left)));
               end if;
               Ok := Good;
            when String_Index_Form =>
               Evaluate_Node (Tree.Nodes (Node_Index (Index)).First,
                              Depth + 1, Left, Good);
               if Good then
                  Evaluate_Node (Tree.Nodes (Node_Index (Index)).Second,
                                 Depth + 1, Right, Good);
               end if;
               if Good and then CCL.Types.Is_List (Tree.Types, Left.Kind) then
                  declare
                     Element : List_Element;
                     Read_Status : List_Regions.Operation_Result;
                  begin
                     if Right.Scalar.Integer < 1 or else
                       Right.Scalar.Integer >
                         Integer_64 (List_Regions.Length (Left.Items))
                     then
                        Eval_Status := Evaluation_Index_Error;
                        Good := False;
                     else
                        List_Regions.Read
                          (List_Region, Left.Items,
                           List_Regions.Array_Index (Right.Scalar.Integer),
                           Element, Read_Status);
                        if Read_Status = List_Regions.Operation_Ok then
                           Item.Kind := CCL.Types.Element_Of (Tree.Types, Left.Kind);
                           Item.Scalar := Element.Scalar;
                           Item.Text := Element.Text;
                           Item.Character_Item := Element.Character_Item;
                           Item.Alternative := Element.Alternative;
                           Item.Node := Element.Node;
                        else
                           Eval_Status := Evaluation_Index_Error;
                           Good := False;
                        end if;
                     end if;
                  end;
                  Ok := Good;
                  return;
               end if;
               if Good and then
                 (Right.Scalar.Integer < 1 or else
                  Right.Scalar.Integer >
                    Integer_64 (String_Length (Left)) or else
                  Right.Scalar.Integer >
                    Integer_64 (Text_Regions.String_Index'Last))
               then
                  Eval_Status := Evaluation_Index_Error;
                  Good := False;
               elsif Good then
                  if Left.Object_Owner /= 0 then
                     Object_Views.Read_Text (Objects (Left.Object_Owner), Left.Object_Position,
                       Positive (Right.Scalar.Integer), Character_Item, Good);
                  else
                     Text_Regions.Read
                       (Text_Region, Left.Text, Text_Regions.String_Index (Right.Scalar.Integer),
                        Character_Item, Region_Result);
                     Good := Region_Result = Text_Regions.Operation_Ok;
                  end if;
                  if Good then
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
               if Good then Join_Strings (Left, Right, Item, Good); end if;
               Ok := Good;
            when To_String_Form =>
               Evaluate_Node (Tree.Nodes (Node_Index (Index)).First,
                              Depth + 1, Left, Good);
               if Good then
                  Text_Regions.Allocate_String
                    (Text_Region,
                     (if Left.Kind = Integer_Type then T.Decimal_Image (Left.Scalar.Integer)
                      else CCL.Types.Image (CCL.Types.Describe
                        (Tree.Types, Left.Kind).Parts (Left.Alternative).Identifier)),
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
                     Granted : Boolean;
                     Argument : CCL.Host_Values.Value := CCL.Host_Values.Integer_Constant (0);
                     Reply : CCL.Host_Values.Call_Result;
                     Contract : CCL.Objects.Binding;
                     Native_Object : CCL.Objects.Image;
                     VM_Value : CCL.VM.Value;
                  begin
                     Good := True;
                     if Operation.Parameters = 1 then
                        Evaluate_Node (Tree.Nodes (Node_Index (Index)).First,
                                       Depth + 1, Left, Good);
                        if Good then
                           if Operation.Import.Argument = CCL.Host_Values.Object_Value then
                              CCL.Catalog.Resolve_Schema
                                (Visible_Interfaces, Operation.Import.Argument_Schema, Contract);
                              if Left.Object_Owner /= 0 then
                                 Object_Views.Copy_Value
                                   (Objects (Left.Object_Owner), Left.Object_Position,
                                    Contract, Native_Object, Good);
                              else
                                 Native_Object := CCL.Objects.Empty (Contract);
                                 Good := Left.Kind = Host_Type (Operation.Import.Argument,
                                   Operation.Import.Argument_Schema, Operation.Import.Argument_Resource);
                                 if Good then Append_Runtime (Native_Object, Left, Good); end if;
                                 Good := Good and then CCL.Objects.Validate (Native_Object, Contract);
                              end if;
                              if Good then Argument := CCL.Host_Values.Object_Constant (Native_Object);
                              else Eval_Status := Host_Argument_Out_Of_Bounds; end if;
                           elsif Left.Kind = String_Type then
                              if String_Length (Left) > Operation.Import.Argument_Text_Limit then
                                 Eval_Status := Host_Argument_Out_Of_Bounds; Good := False;
                              else
                                 Argument := (Kind => CCL.Host_Values.Text_Value, Content =>
                                   (Length => String_Length (Left), others => <>));
                                 Copy_String (Left, Argument.Content.Data (1 .. Argument.Content.Length), Good);
                                 if not Good then Eval_Status := Evaluation_Index_Error; end if;
                              end if;
                           else
                              Argument := To_Host (Left.Scalar);
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
                     if Good and then not Matches_Host
                       (Argument, Operation.Import.Argument, Operation.Import.Argument_Text_Limit,
                        Operation.Import.Argument_Schema)
                     then
                        Eval_Status := Host_Argument_Out_Of_Bounds;
                        Good := False;
                     end if;
                     if Good then
                        CCL.Catalog.Find_Granted_Binding (Grants, Operation, Binding, Granted);
                        if not Granted then
                           Eval_Status := Host_Authority_Denied; Good := False;
                        elsif Operation.Import.Result = CCL.Host_Values.Object_Value and then
                          not Supported_Object (Tree.Nodes (Index).Static_Kind)
                        then
                           if Objects_Used = MAX_OBJECT_VALUES then
                              Eval_Status := Evaluation_Object_Storage_Exhausted; Good := False;
                           else
                              Objects_Used := Objects_Used + 1; Reserved_Object := Objects_Used;
                           end if;
                        end if;
                        if Good then
                           Invoke (Context, Binding, Argument, Reply);
                           if not Reply.Success then
                              Eval_Status := Host_Call_Failed; Good := False;
                           elsif not Matches_Host
                             (Reply.Value, Operation.Import.Result, Operation.Import.Result_Text_Limit,
                              Operation.Import.Result_Schema)
                           then
                              Eval_Status := Host_Result_Type_Mismatch; Good := False;
                           else
                              case Reply.Value.Kind is
                                 when CCL.Host_Values.Integer_Value =>
                                    Item.Kind := Integer_Type;
                                    Item.Scalar := Integer_Scalar (Reply.Value.Integer);
                                 when CCL.Host_Values.Boolean_Value =>
                                    Item.Kind := Boolean_Type;
                                    Item.Scalar := Boolean_Scalar (Reply.Value.Boolean);
                                 when CCL.Host_Values.Text_Value =>
                                    Item.Kind := String_Type;
                                    Text_Regions.Allocate_String
                                      (Text_Region, Reply.Value.Content.Data (1 .. Reply.Value.Content.Length),
                                       Item.Text, Region_Result);
                                    Good := Region_Result = Text_Regions.Operation_Ok;
                                    if not Good then Eval_Status := Evaluation_Text_Storage_Exhausted; end if;
                                 when CCL.Host_Values.Handler_Value | CCL.Host_Values.Resource_Value =>
                                    Good := False; Eval_Status := Host_Result_Type_Mismatch;
                                 when CCL.Host_Values.Object_Value =>
                                    CCL.Catalog.Resolve_Schema
                                      (Visible_Interfaces, Operation.Import.Result_Schema, Contract);
                                    if Reserved_Object /= 0 then
                                       Object_Views.Capture (Objects (Reserved_Object), Contract, Reply.Value.Object, Good);
                                       if Good then
                                          if CCL.Types.Is_List (Tree.Types, Tree.Nodes (Node_Index (Index)).Static_Kind) then
                                             --  A list is copied in: its elements are this
                                             --  evaluation's values, not views of the image.
                                             Copy_In (Reserved_Object, Object_Views.Root (Objects (Reserved_Object)),
                                                      Tree.Nodes (Node_Index (Index)).Static_Kind, Item, Good);
                                          else
                                             Load_View (Reserved_Object, Object_Views.Root (Objects (Reserved_Object)), Item, Good);
                                          end if;
                                       end if;
                                    else
                                       CCL.Objects.Values.To_VM (Contract, Tree.Types, Reply.Value.Object, VM_Value, Good);
                                       if Good then
                                          Item.Kind := (case VM_Value.Kind is
                                            when CCL.VM.Integer_Value => Integer_Type,
                                            when CCL.VM.Boolean_Value => Boolean_Type,
                                            when CCL.VM.Variant_Value | CCL.VM.Object_Value => VM_Value.Data_Type,
                                            when CCL.VM.Resource_Value | CCL.VM.Text_Value |
                                                 CCL.VM.Character_Value | CCL.VM.List_Value |
                                                 CCL.VM.Function_Value => Invalid_Type);
                                          Item.Alternative := VM_Value.Alternative;
                                          Item.Scalar :=
                                            (if Item.Kind = Boolean_Type or else
                                              (VM_Value.Kind = CCL.VM.Variant_Value and then
                                               CCL.Types.Describe (Tree.Types, Item.Kind).Parts (Item.Alternative).Payload = Boolean_Type)
                                             then Boolean_Scalar (VM_Value.Boolean)
                                             else Integer_Scalar (VM_Value.Integer));
                                       end if;
                                    end if;
                                    if not Good then Eval_Status := Host_Result_Type_Mismatch; end if;
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
            when Builtin_Form =>
               declare
                  Operation : constant Builtin_Operation := Tree.Nodes (Index).Builtin;
                  Count : constant Parameter_Count := Tree.Nodes (Index).Argument_Count;
                  type Operand_Values is array (Parameter_Index) of Runtime_Value;
                  Operands : Operand_Values := [others => (others => <>)];
                  Source_List : Runtime_Value;
                  Length : Natural := 0;
                  Read_Status : List_Regions.Operation_Result;
                  Placed : List_Regions.Operation_Result;

                  function Packed (Value : Runtime_Value) return List_Element is
                    ((Scalar => Value.Scalar, Text => Value.Text,
                      Character_Item => Value.Character_Item,
                      Alternative => Value.Alternative, Node => Value.Node));

                  --  Element Position of Source_List as a value of Kind.
                  procedure Element
                    (Position : Positive; Kind : Static_Type;
                     Value : out Runtime_Value; Good : out Boolean)
                  is
                     Raw : List_Element;
                  begin
                     Value := (others => <>);
                     Spend (1, Good);
                     if not Good then return; end if;
                     List_Regions.Read
                       (List_Region, Source_List.Items, List_Regions.Array_Index (Position),
                        Raw, Read_Status);
                     Good := Read_Status = List_Regions.Operation_Ok;
                     if Good then
                        Value.Kind := Kind;
                        Value.Scalar := Raw.Scalar;
                        Value.Text := Raw.Text;
                        Value.Character_Item := Raw.Character_Item;
                        Value.Alternative := Raw.Alternative;
                        Value.Node := Raw.Node;
                     else
                        Eval_Status := Evaluation_Index_Error;
                     end if;
                  end Element;

                  --  Call function value Callee with Arity arguments.
                  procedure Apply
                    (Callee : Runtime_Value; First, Second : Runtime_Value; Arity : Positive;
                     Result : out Runtime_Value; Good : out Boolean)
                  is
                     Decl : constant Function_Declaration := Tree.Functions (Callee.Handler_Id);
                     Saved : constant Value_Environment := Value_Env;
                  begin
                     if Decl.Count /= Arity then
                        Result := (others => <>); Good := False;
                        Eval_Status := Host_Contract_Unsupported;
                        return;
                     end if;
                     Bind_Captures (Callee, Decl, Good);
                     if not Good then
                        Result := (others => <>);
                        Value_Env := Saved;
                        Value_Env_Length := Entry_Environment_Length;
                        return;
                     end if;
                     Good := True;
                     Check_Range (First, Decl.Parameters (1).Kind, Good);
                     if Arity = 2 then
                        Check_Range (Second, Decl.Parameters (2).Kind, Good);
                     end if;
                     if not Good then
                        Result := (others => <>);
                        Value_Env := Saved;
                        Value_Env_Length := Entry_Environment_Length;
                        return;
                     end if;
                     Value_Env (Decl.Captured) := (Identifier => Decl.Parameters (1).Identifier, Item => First);
                     if Arity = 2 then
                        Value_Env (Decl.Captured + 1) := (Identifier => Decl.Parameters (2).Identifier, Item => Second);
                     end if;
                     Value_Env_Length := Decl.Captured + Arity;
                     Evaluate_Node (Decl.Body_Node, Depth + 1, Result, Good);
                     Check_Range (Result, Decl.Result_Kind, Good);
                     Value_Env := Saved;
                     Value_Env_Length := Entry_Environment_Length;
                  end Apply;

                  Current, Mapped : Runtime_Value;
               begin
                  Good := True;
                  for P in 1 .. Count loop
                     Evaluate_Node (Tree.Nodes (Index).Arguments (P), Depth + 1, Operands (P), Good);
                     exit when not Good;
                  end loop;
                  if not Good or else Count = 0 then Ok := False; return; end if;
                  if Operation /= Range_Builtin and then Operands (Count).Kind = String_Type then
                     Text_Builtin (Operation, Operands (1), Operands (2), Operands (Count),
                                   Tree.Nodes (Index).Static_Kind, Item, Good);
                     Ok := Good;
                     return;
                  end if;
                  if Operation /= Range_Builtin then
                     Source_List := Operands (Count);
                     Length := List_Regions.Length (Source_List.Items);
                  end if;
                  declare
                     Element_Kind : constant Static_Type :=
                       (if Operation = Range_Builtin then Integer_Type
                        else CCL.Types.Element_Of (Tree.Types, Source_List.Kind));
                     Kept : Natural := 0;

                     --  Results are built in place in the list region: reserve
                     --  an upper bound, write, then shrink to what was kept.
                     procedure Reserve (Size : Natural) is
                     begin
                        List_Regions.Reserve (List_Region, Size, Item.Items, Placed);
                        Good := Placed = List_Regions.Operation_Ok;
                        if not Good then Eval_Status := Evaluation_List_Storage_Exhausted; end if;
                     end Reserve;

                     procedure Keep (Value : Runtime_Value) is
                     begin
                        if Kept >= MAX_LIST_ELEMENTS then
                           Eval_Status := Evaluation_List_Storage_Exhausted; Good := False; return;
                        end if;
                        Kept := Kept + 1;
                        List_Regions.Write (List_Region, Item.Items, Kept, Packed (Value), Placed);
                        Good := Placed = List_Regions.Operation_Ok;
                        if not Good then Eval_Status := Evaluation_Index_Error; end if;
                     end Keep;

                     procedure Finish (Kind : Static_Type) is
                     begin
                        List_Regions.Shrink (List_Region, Item.Items, Kept, Placed);
                        Good := Placed = List_Regions.Operation_Ok;
                        if not Good then Eval_Status := Evaluation_List_Storage_Exhausted; end if;
                        Item.Kind := Kind;
                     end Finish;
                  begin
                     case Operation is
                        when Each_Builtin | Where_Builtin =>
                           Reserve (Length);
                           if Good then
                              for I in 1 .. Length loop
                                 Element (I, Element_Kind, Current, Good);
                                 exit when not Good;
                                 Apply (Operands (1), Current, Current, 1, Mapped, Good);
                                 exit when not Good;
                                 if Operation = Each_Builtin then
                                    Keep (Mapped);
                                 elsif Mapped.Scalar.Boolean then
                                    Keep (Current);
                                 end if;
                                 exit when not Good;
                              end loop;
                           end if;
                           if Good then Finish (Tree.Nodes (Index).Static_Kind); end if;
                        when Any_Builtin | All_Builtin =>
                           --  Short-circuit: stop at the first decisive element.
                           Item.Kind := Boolean_Type;
                           Item.Scalar := Boolean_Scalar (Operation = All_Builtin);
                           for I in 1 .. Length loop
                              Element (I, Element_Kind, Current, Good);
                              exit when not Good;
                              Apply (Operands (1), Current, Current, 1, Mapped, Good);
                              exit when not Good;
                              if Mapped.Scalar.Boolean = (Operation = Any_Builtin) then
                                 Item.Scalar := Boolean_Scalar (Operation = Any_Builtin);
                                 exit;
                              end if;
                           end loop;
                        when Fold_Builtin =>
                           Item := Operands (2);
                           for I in 1 .. Length loop
                              Element (I, Element_Kind, Current, Good);
                              exit when not Good;
                              Apply (Operands (1), Item, Current, 2, Mapped, Good);
                              exit when not Good;
                              Item := Mapped;
                           end loop;
                        when First_Builtin | Last_Builtin | Skip_Builtin =>
                           declare
                              From : Positive;
                              To : Natural;
                           begin
                              L.Take_Bounds (List_Operation_Of (Operation), Operands (1).Scalar.Integer,
                                             Length, From, To);
                              Reserve (if To >= From then To - From + 1 else 0);
                              if Good then
                                 for I in From .. To loop
                                    Element (I, Element_Kind, Current, Good);
                                    exit when not Good;
                                    Keep (Current);
                                    exit when not Good;
                                 end loop;
                              end if;
                              if Good then Finish (Source_List.Kind); end if;
                           end;
                        when Sum_Builtin =>
                           Item.Kind := Integer_Type;
                           Item.Scalar := Integer_Scalar (0);
                           for I in 1 .. Length loop
                              Element (I, Element_Kind, Current, Good);
                              exit when not Good;
                              CCL.Checked_Arithmetic.Add
                                (Item.Scalar.Integer, Current.Scalar.Integer, Arithmetic_Value, Overflowed);
                              if Overflowed then
                                 Eval_Status := Evaluation_Overflow; Good := False; exit;
                              end if;
                              Item.Scalar := Integer_Scalar (Arithmetic_Value);
                           end loop;
                        when Range_Builtin =>
                           --  [a .. b], empty when b < a, bounded by the list region.
                           declare
                              Low : constant Integer_64 := Operands (1).Scalar.Integer;
                              Size : Natural;
                              Fits : Boolean;
                           begin
                              L.Range_Length (Low, Operands (2).Scalar.Integer, MAX_LIST_ELEMENTS, Size, Fits);
                              if not Fits then
                                 Eval_Status := Evaluation_List_Storage_Exhausted; Good := False;
                              else
                                 Reserve (Size);
                                 if Good then
                                    for I in 1 .. Size loop
                                       declare
                                          Value : Integer_64;
                                          Overflowed : Boolean;
                                       begin
                                          CCL.Checked_Arithmetic.Add (Low, Integer_64 (I - 1), Value, Overflowed);
                                          if Overflowed then
                                             Eval_Status := Evaluation_Overflow; Good := False;
                                          else
                                             Current := (Kind => Integer_Type, Scalar => Integer_Scalar (Value),
                                                         others => <>);
                                             Keep (Current);
                                          end if;
                                       end;
                                       exit when not Good;
                                    end loop;
                                 end if;
                                 if Good then Finish (Tree.Nodes (Index).Static_Kind); end if;
                              end if;
                           end;
                        when Reverse_Builtin =>
                           Reserve (Length);
                           if Good then
                              for I in reverse 1 .. Length loop
                                 Element (I, Element_Kind, Current, Good);
                                 exit when not Good;
                                 Keep (Current);
                                 exit when not Good;
                              end loop;
                           end if;
                           if Good then Finish (Source_List.Kind); end if;
                        when Count_Builtin =>
                           Item := (Kind => Integer_Type, Scalar => Integer_Scalar (0), others => <>);
                           for I in 1 .. Length loop
                              Element (I, Element_Kind, Current, Good);
                              exit when not Good;
                              Apply (Operands (1), Current, Current, 1, Mapped, Good);
                              exit when not Good;
                              if Mapped.Scalar.Boolean and then Kept < Natural'Last then
                                 Kept := Kept + 1;
                                 Item.Scalar := Integer_Scalar (Integer_64 (Kept));
                              end if;
                           end loop;
                        when Min_Builtin | Max_Builtin =>
                           if Length = 0 then
                              Eval_Status := Evaluation_Index_Error; Good := False;
                           else
                              Element (1, Element_Kind, Item, Good);
                              for I in 2 .. Length loop
                                 exit when not Good;
                                 Element (I, Element_Kind, Current, Good);
                                 exit when not Good;
                                 if (Operation = Min_Builtin and then Current.Scalar.Integer < Item.Scalar.Integer) or else
                                   (Operation = Max_Builtin and then Current.Scalar.Integer > Item.Scalar.Integer)
                                 then
                                    Item := Current;
                                 end if;
                              end loop;
                           end if;
                        when Contains_Builtin =>
                           Item := (Kind => Boolean_Type, Scalar => Boolean_Scalar (False), others => <>);
                           for I in 1 .. Length loop
                              Element (I, Element_Kind, Current, Good);
                              exit when not Good;
                              if (if Element_Kind = String_Type then Equal_Strings (Current, Operands (1))
                                  elsif Element_Kind = Integer_Type then Current.Scalar.Integer = Operands (1).Scalar.Integer
                                  elsif Element_Kind = Boolean_Type then Current.Scalar.Boolean = Operands (1).Scalar.Boolean
                                  elsif Element_Kind = Character_Type then Current.Character_Item = Operands (1).Character_Item
                                  else Current.Alternative = Operands (1).Alternative)
                              then
                                 Item.Scalar := Boolean_Scalar (True);
                                 exit;
                              end if;
                           end loop;
                        when Join_Builtin =>
                           Join_List (Operands (1), Source_List, Item, Good);
                        when Sort_Builtin | Sort_By_Builtin =>
                           --  Copy, then heapsort in place (n log n comparisons,
                           --  no scratch beyond the copy). Sort-by sorts the keys
                           --  and carries the elements with them.
                           declare
                              Keys : List_Regions.Array_Value;
                              Key_Kind : constant Static_Type :=
                                (if Operation = Sort_Builtin then Element_Kind
                                 elsif CCL.Types.Describe (Tree.Types, Operands (1).Kind).Count >= 1 then
                                    CCL.Types.Describe (Tree.Types, Operands (1).Kind).Parts
                                      (CCL.Types.Describe (Tree.Types, Operands (1).Kind).Count).Payload
                                 else Invalid_Type);

                              procedure Get (List : List_Regions.Array_Value; I : Positive; E : out List_Element) is
                              begin
                                 List_Regions.Read (List_Region, List, List_Regions.Array_Index (I), E, Placed);
                                 if Placed /= List_Regions.Operation_Ok then
                                    E := Null_List_Element;
                                    Eval_Status := Evaluation_Index_Error; Good := False;
                                 end if;
                              end Get;
                              procedure Put (List : List_Regions.Array_Value; I : Positive; E : List_Element) is
                              begin
                                 List_Regions.Write (List_Region, List, List_Regions.Array_Index (I), E, Placed);
                                 if Placed /= List_Regions.Operation_Ok then
                                    Eval_Status := Evaluation_Index_Error; Good := False;
                                 end if;
                              end Put;
                              function Less (A, B : List_Element) return Boolean is
                                (if Key_Kind = Integer_Type then A.Scalar.Integer < B.Scalar.Integer
                                 elsif Key_Kind = String_Type then Text_Less (A.Text, B.Text)
                                 elsif Key_Kind = Character_Type then A.Character_Item < B.Character_Item
                                 else A.Alternative < B.Alternative);
                              --  Whether key I sorts before key J (one fuel each).
                              procedure Key_Less (I, J : Positive; Before : out Boolean) is
                                 A, B : List_Element;
                              begin
                                 Before := False;
                                 Spend (1, Good);
                                 if not Good then return; end if;
                                 Get (Keys, I, A);
                                 Get (Keys, J, B);
                                 Before := Good and then Less (A, B);
                              end Key_Less;
                              procedure Swap (I, J : Positive) is
                                 A, B : List_Element;
                              begin
                                 Get (Keys, I, A); Get (Keys, J, B);
                                 Put (Keys, I, B); Put (Keys, J, A);
                                 if Operation = Sort_By_Builtin then
                                    Get (Item.Items, I, A); Get (Item.Items, J, B);
                                    Put (Item.Items, I, B); Put (Item.Items, J, A);
                                 end if;
                              end Swap;
                              procedure Sort_Less (I, J : Positive; Before : out Boolean; Ok : out Boolean) is
                              begin
                                 Key_Less (I, J, Before);
                                 Ok := Good;
                              end Sort_Less;
                              procedure Sort_Swap (I, J : Positive; Ok : out Boolean) is
                              begin
                                 Swap (I, J);
                                 Ok := Good;
                              end Sort_Swap;
                              procedure Sort is new L.Heap_Sort (Sort_Less, Sort_Swap);
                              Sorted : Boolean;
                           begin
                              Reserve (Length);
                              for I in 1 .. Length loop
                                 exit when not Good;
                                 Element (I, Element_Kind, Current, Good);
                                 exit when not Good;
                                 Keep (Current);
                              end loop;
                              if Good and then Operation = Sort_By_Builtin then
                                 --  One key per element, computed once.
                                 List_Regions.Reserve (List_Region, Length, Keys, Placed);
                                 if Placed /= List_Regions.Operation_Ok then
                                    Eval_Status := Evaluation_List_Storage_Exhausted; Good := False;
                                 end if;
                                 for I in 1 .. Length loop
                                    exit when not Good;
                                    Element (I, Element_Kind, Current, Good);
                                    exit when not Good;
                                    Apply (Operands (1), Current, Current, 1, Mapped, Good);
                                    exit when not Good;
                                    Put (Keys, I, Packed (Mapped));
                                 end loop;
                              else
                                 Keys := Item.Items;
                              end if;
                              if Good then
                                 Sort (Length, Sorted);
                                 Good := Good and then Sorted;
                              end if;
                              if Good then Finish (Source_List.Kind); end if;
                           end;
                        when Upper_Builtin .. Split_Builtin | Parse_Int_Builtin =>
                           --  Text subjects returned above; a list subject is
                           --  a checker error.
                           Eval_Status := Host_Contract_Unsupported; Good := False;
                        when No_Builtin => Good := False;
                     end case;
                  end;
                  Ok := Good;
               end;
            when Lambda_Form =>
               --  A function value: which function, plus a copy of each value
               --  it captures, looked up by name as the body would.
               declare
                  Decl : constant Function_Declaration :=
                    Tree.Functions (Tree.Nodes (Index).Function_Id);
                  Buffer : List_Element_Array (1 .. MAX_CAPTURES) := [others => Null_List_Element];
                  Placed : List_Regions.Operation_Result;
               begin
                  Item.Kind := Tree.Nodes (Index).Static_Kind;
                  Item.Handler_Id := Tree.Nodes (Index).Function_Id;
                  Good := True;
                  for C in 1 .. Decl.Captured loop
                     Found := False;
                     if Value_Env_Length > 0 then
                        for Position in reverse 0 .. Value_Env_Length - 1 loop
                           if Names_Equal (Value_Env (Position).Identifier, Decl.Captures (C).Identifier) then
                              Left := Value_Env (Position).Item;
                              Found := True;
                              exit;
                           end if;
                        end loop;
                     end if;
                     if not Found or else Left.Object_Owner /= 0 then
                        --  Text inside a record image is not copied (as lists).
                        Eval_Status := Host_Contract_Unsupported;
                        Good := False;
                        exit;
                     end if;
                     Buffer (C) := (Scalar => Left.Scalar, Text => Left.Text,
                                    Character_Item => Left.Character_Item,
                                    Alternative => Left.Alternative, Node => Left.Node);
                  end loop;
                  if Good and then Decl.Captured > 0 then
                     List_Regions.Allocate (List_Region, Buffer (1 .. Decl.Captured), Item.Items, Placed);
                     if Placed /= List_Regions.Operation_Ok then
                        Eval_Status := Evaluation_List_Storage_Exhausted;
                        Good := False;
                     end if;
                  end if;
                  Ok := Good;
               end;
            when List_Construct =>
               --  Elements are evaluated left to right into a list reserved
               --  at its full length, across chained chunks.
               declare
                  Total : Natural := 0;
                  Chunk : Node_Reference := Index;
                  Written : Natural := 0;
                  Placed : List_Regions.Operation_Result;
               begin
                  for Step in 0 .. MAX_NESTING loop
                     exit when Chunk >= Tree.Length;
                     if Total <= MAX_LIST_ELEMENTS then
                        Total := Total + Tree.Nodes (Chunk).Element_Count;
                     end if;
                     Chunk := Tree.Nodes (Chunk).Second;
                  end loop;
                  Good := Total <= MAX_LIST_ELEMENTS;
                  if Good then
                     List_Regions.Reserve (List_Region, Total, Item.Items, Placed);
                     Good := Placed = List_Regions.Operation_Ok;
                  end if;
                  if not Good then
                     Eval_Status := Evaluation_List_Storage_Exhausted;
                  end if;
                  Chunk := Index;
                  for Step in 0 .. MAX_NESTING loop
                     exit when not Good or else Chunk >= Tree.Length;
                     for P in 1 .. Tree.Nodes (Chunk).Element_Count loop
                        Evaluate_Node (Tree.Nodes (Chunk).Components (P), Depth + 1, Left, Good);
                        exit when not Good;
                        if Left.Object_Owner /= 0 then
                           --  A host result still viewed in its image: moving
                           --  host results into the arena comes next.
                           Eval_Status := Host_Contract_Unsupported;
                           Good := False;
                           exit;
                        end if;
                        if Written >= Total or else Written >= MAX_LIST_ELEMENTS then
                           Eval_Status := Evaluation_Index_Error;
                           Good := False;
                           exit;
                        end if;
                        Written := Written + 1;
                        List_Regions.Write
                          (List_Region, Item.Items, List_Regions.Array_Index (Written),
                           (Scalar => Left.Scalar, Text => Left.Text,
                            Character_Item => Left.Character_Item,
                            Alternative => Left.Alternative, Node => Left.Node), Placed);
                        if Placed /= List_Regions.Operation_Ok then
                           Eval_Status := Evaluation_Index_Error;
                           Good := False;
                        end if;
                     end loop;
                     Chunk := Tree.Nodes (Chunk).Second;
                  end loop;
                  if Good then
                     Item.Kind := Tree.Nodes (Index).Static_Kind;
                  end if;
                  Ok := Good;
               end;
            when Invalid_Node => Ok := False;
         end case;
      end Evaluate_Node;

      --  Lists whose elements are records or variants with payloads leave
      --  as literals; the flat result form carries scalars, text and
      --  enumeration members.
      function Compound_Elements (Kind : Static_Type) return Boolean is
        (CCL.Types.Describe (Tree.Types, CCL.Types.Element_Of (Tree.Types, Kind)).Form =
           CCL.Types.Product
         or else
           (CCL.Types.Describe (Tree.Types, CCL.Types.Element_Of (Tree.Types, Kind)).Form =
              CCL.Types.Sum
            and then not CCL.Types.Is_Enumeration
                           (Tree.Types, CCL.Types.Element_Of (Tree.Types, Kind))));

      --  Copy a list result out of the list region: values for scalars,
      --  consecutive slices of List_Text for strings.
      procedure Export_List
        (Value : Runtime_Value; Result : in out Interpretation_Result)
      is
         Total : constant Natural := List_Regions.Length (Value.Items);
         Count : constant Natural := Natural'Min (Total, MAX_LIST_RESULT);
         Element_Type : constant Static_Type :=
           CCL.Types.Element_Of (Tree.Types, Value.Kind);
         Element : List_Element;
         Read_Status : List_Regions.Operation_Result;
         Copied : Text_Regions.Operation_Result;
         Text_Used : Natural range 0 .. MAX_TEXT_BYTES := 0;
         Size : Natural;
      begin
         Result.Has_List := True;
         Result.List_Type := Value.Kind;
         Result.List_Element_Type := Element_Type;
         Result.List_Length := Count;
         Result.List_Total := Natural'Min (Total, MAX_LIST_ELEMENTS);
         for I in 1 .. Count loop
            List_Regions.Read
              (List_Region, Value.Items, List_Regions.Array_Index (I), Element, Read_Status);
            if Read_Status /= List_Regions.Operation_Ok then
               Result.Status := Evaluation_Index_Error;
               Result.Has_Value := False; Result.Has_List := False;
               return;
            end if;
            if Element_Type = String_Type then
               Size := Text_Regions.Length (Element.Text);
               if Size > MAX_TEXT_BYTES - Text_Used then
                  --  Carry out the elements that fit.
                  Result.List_Length := I - 1;
                  exit;
               end if;
               Text_Regions.Copy_To
                 (Text_Region, Element.Text,
                  Result.List_Text.Data (Text_Used + 1 .. Text_Used + Size), Copied);
               if Copied /= Text_Regions.Operation_Ok then
                  Result.Status := Evaluation_Index_Error;
                  Result.Has_Value := False; Result.Has_List := False;
                  return;
               end if;
               Text_Used := Text_Used + Size;
               Result.List_Text_Ends (I) := Text_Used;
            elsif Element_Type = Character_Type then
               Result.List_Values (I) := CCL.VM.Integer_Constant
                 (Integer_64 (Character'Pos (Element.Character_Item)));
            elsif Element_Type in Integer_Type | Boolean_Type then
               Result.List_Values (I) := To_VM (Element.Scalar);
            else
               Result.List_Values (I) := CCL.VM.Integer_Constant
                 (Integer_64 (Element.Alternative));
            end if;
         end loop;
         Result.List_Text.Length := Text_Used;
      end Export_List;

      Root_Type : Static_Type;
      Value     : Runtime_Value;
      Ok        : Boolean;
   begin
      Result :=
        (Status => Parse_Failed, Diagnostic => No_Diagnostic,
         Diagnostic_Position => 0,
         Fuel_Remaining => Fuel, others => <>);

      if Analyze_Input then
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
      List_Regions.Initialize (List_Region);
      Evaluate_Node (Root, 0, Value, Ok);
      Result.Status := Eval_Status;
      Result.Fuel_Remaining := Fuel_Left;
      if Ok and then Eval_Status = Succeeded and then Export_Native then
         declare
            Native : CCL.Objects.Image;
         begin
            -- Schema-less, owned native data. The typed result adapter applies
            -- the separately approved identity and validates before publication.
            Append_Runtime (Native, Value, Ok);
            if Ok then Deliver_Native (Native);
            else Result.Status := Eval_Status;
            end if;
         end;
      elsif Ok and then Eval_Status = Succeeded then
         Result.Has_Value := True;
         case Value.Kind is
            when String_Type =>
               if String_Length (Value) > MAX_TEXT_BYTES then
                  Result.Status := Evaluation_Text_Storage_Exhausted;
                  Result.Has_Value := False;
               else
                  Result.Has_Text := True;
                  Result.Result_Text.Length := String_Length (Value);
                  Copy_String (Value, Result.Result_Text.Data (1 .. Result.Result_Text.Length), Ok);
                  if not Ok then
                     Result.Status := Evaluation_Index_Error;
                     Result.Has_Value := False;
                     Result.Has_Text := False;
                  end if;
               end if;
            when Character_Type =>
               Result.Has_Character := True;
               Result.Result_Character := Value.Character_Item;
            when Integer_Type | Boolean_Type =>
               Result.Result_Value := To_VM (Value.Scalar);
            when CCL.Types.Declared_Type =>
               if CCL.Types.Is_List (Tree.Types, Value.Kind) and then
                 not Compound_Elements (Value.Kind)
               then
                  Export_List (Value, Result);
               elsif CCL.Types.Is_List (Tree.Types, Value.Kind) then
                  --  A list of records or payload variants: its literal.
                  Ok := True;
                  Print_Value (Value, MAX_VALUE_NODES + 1, 1, Result.Literal, Ok);
                  if Ok then
                     Result.Has_Literal := True;
                     Result.Literal_Type := Value.Kind;
                     Result.Literal_Type_Name := CCL.Types.Describe (Tree.Types, Value.Kind).Identifier;
                  else
                     Result.Literal := (others => <>);
                     Result.Status := Host_Contract_Unsupported; Result.Has_Value := False;
                  end if;
               elsif CCL.Types.Is_Function (Tree.Types, Value.Kind) then
                  Result.Has_Function := True;
                  Result.Function_Name := Tree.Functions (Value.Handler_Id).Identifier;
               elsif not CCL.Types.Is_Scalar_Sum (Tree.Types, Value.Kind) then
                  --  Records and payload variants come out as their literal.
                  Ok := True;
                  Print_Value (Value, MAX_VALUE_NODES + 1, 1, Result.Literal, Ok);
                  if Ok then
                     Result.Has_Literal := True;
                     Result.Literal_Type := Value.Kind;
                     Result.Literal_Type_Name := CCL.Types.Describe (Tree.Types, Value.Kind).Identifier;
                  else
                     Result.Literal := (others => <>);
                     Result.Status := Host_Contract_Unsupported; Result.Has_Value := False;
                  end if;
               else
               Result.Variant_Type := Value.Kind;
               Result.Variant_Type_Name := CCL.Types.Describe (Tree.Types, Value.Kind).Identifier;
               Result.Variant_Member_Name := CCL.Types.Describe
                 (Tree.Types, Value.Kind).Parts (Value.Alternative).Identifier;
               Result.Variant_Payload_Type := CCL.Types.Describe
                 (Tree.Types, Value.Kind).Parts (Value.Alternative).Payload;
               Result.Result_Value := To_VM (Value.Scalar);
               end if;
            when Invalid_Type | Handler_Type | Unit_Type =>
               Result.Status := Type_Check_Failed;
               Result.Has_Value := False;
         end case;
      end if;
      Text_Regions.Clear (Text_Region);
      List_Regions.Clear (List_Region);
      for I in 1 .. Objects_Used loop Object_Views.Clear (Objects (I)); end loop;
   end Process_Source_With_Host;

   type No_Host is null record;
   procedure Deny_Host
     (Context : in out No_Host; Binding : Unsigned_32;
      Argument : CCL.Host_Values.Value; Reply : out CCL.Host_Values.Call_Result)
   is
      pragma Unreferenced (Context, Binding, Argument);
   begin
      Reply.Value := CCL.Host_Values.Integer_Constant (0); Reply.Success := False;
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
              CCL.Host_Values.Has_Resources (N.Host_Call.Import) or else
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

   procedure Interpret_Object_With_Values
     (Source : String; Fuel : Natural;
      Visible_Interfaces : CCL.Catalog.Interface_Catalog;
      Grants : CCL.Catalog.Granted_Bindings;
      Context : in out Host_Context; Expected : CCL.Objects.Binding;
      Result : out Object_Interpretation_Result)
   is
      Analysis : Analysis_Result;
      Tree : Syntax_Tree;
      Outcome : Interpretation_Result;
      Delivered : Boolean := False;
      procedure Deliver (Value : CCL.Objects.Image) is
         use type CCL.Objects.Schema_Key;
      begin
         if Value.Schema /= CCL.Objects.No_Schema then return; end if;
         Result.Value := Value;
         Result.Value.Schema := CCL.Objects.Identity (Expected);
         Delivered := CCL.Objects.Validate (Result.Value, Expected);
         if not Delivered then Result.Value := CCL.Objects.Empty (Expected); end if;
      end Deliver;
      procedure Run is new Process_Source_With_Host
        (Host_Context, Invoke, Export_Native => True, Deliver_Native => Deliver);
   begin
      Result := (Fuel_Remaining => Fuel, Value => CCL.Objects.Empty (Expected), others => <>);
      Analyze (Source, Visible_Interfaces, Analysis);
      if Analysis.Status /= Analysis_Succeeded then
         Result.Status := (if Analysis.Status = Analysis_Type_Check_Failed then Type_Check_Failed else Parse_Failed);
         Result.Diagnostic := Analysis.Diagnostic;
         Result.Diagnostic_Position := Analysis.Diagnostic_Position;
         return;
      end if;
      Tree := Analysis.Tree;
      if Tree.Root >= Tree.Length or else not CCL.Objects.Matches_Type
        (Expected, Tree.Types, Tree.Nodes (Tree.Root).Static_Kind)
      then
         Result.Status := Type_Check_Failed;
         Result.Diagnostic := Host_Object_Type_Mismatch;
         if Tree.Root < Tree.Length then Result.Diagnostic_Position := Tree.Nodes (Tree.Root).Source_Position; end if;
         return;
      end if;
      Admit (Tree, Grants, True, Result.Status, Result.Diagnostic_Position);
      if Result.Status /= Succeeded then return; end if;
      Run (Source, Fuel, Visible_Interfaces, Grants, Context, True, False, True, Outcome, Tree);
      Result.Status := Outcome.Status;
      Result.Diagnostic := Outcome.Diagnostic;
      Result.Diagnostic_Position := Outcome.Diagnostic_Position;
      Result.Fuel_Remaining := Outcome.Fuel_Remaining;
      Result.Has_Value := Outcome.Status = Succeeded and Delivered;
      if Outcome.Status = Succeeded and not Delivered then Result.Status := Host_Result_Type_Mismatch; end if;
   end Interpret_Object_With_Values;

   procedure Interpret_Object
     (Source : String; Fuel : Natural; Expected : CCL.Objects.Binding;
      Result : out Object_Interpretation_Result)
   is
      Catalog : CCL.Catalog.Interface_Catalog;
      Grants : CCL.Catalog.Granted_Bindings;
      Context : No_Host;
      procedure Run is new Interpret_Object_With_Values (No_Host, Deny_Host);
   begin
      Run (Source, Fuel, Catalog, Grants, Context, Expected, Result);
   end Interpret_Object;

   procedure Interpret_With_Host
     (Source : String; Fuel : Natural;
      Visible_Interfaces : CCL.Catalog.Interface_Catalog;
      Grants : CCL.Catalog.Granted_Bindings;
      Context : in out Host_Context; Result : out Interpretation_Result)
   is
      procedure Invoke_Scalar
        (Context : in out Host_Context; Binding : Unsigned_32;
         Argument : CCL.Host_Values.Value; Reply : out CCL.Host_Values.Call_Result)
      is
         A, R : CCL.VM.Value;
      begin
         CCL.Host_Values.To_Scalar (Argument, A, Reply.Success);
         Reply.Value := CCL.Host_Values.Integer_Constant (0);
         if Reply.Success then
            Invoke (Context, Binding, A, R, Reply.Success);
            --  This boundary admits scalar COPY results only. Do not erase
            --  ownership/linearity metadata by projecting a resource into an
            --  ordinary integer or boolean. Transfer imports use a different
            --  admission path; they cannot be smuggled through this adapter.
            if not Reply.Success or else R.Kind not in CCL.VM.Scalar_Kind or else
              not R.Copyable or else R.Type_Tag /= 0
            then Reply.Success := False;
            else Reply.Value := CCL.Host_Values.From_Scalar (R); end if;
         end if;
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
      for Ref in CCL.Types.Type_Reference loop
         Result.Resource_Policies (Ref) := CCL.Catalog.Resource_Policy (Visible_Interfaces, Ref);
      end loop;
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

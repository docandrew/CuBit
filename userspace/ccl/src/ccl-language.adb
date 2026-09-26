with Interfaces; use Interfaces;
with CCL.Checked_Arithmetic;
with CCL.Secondary_Stacks;
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
   subtype Object_Count is Natural range 0 .. MAX_OBJECT_VALUES;
   subtype Object_Index is Positive range 1 .. MAX_OBJECT_VALUES;

   package Text_Regions is new CCL.Secondary_Stacks
     (Capacity => MAX_TEXT_BYTES * 4,
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

   type Runtime_Value is record
      Kind      : Static_Type := Invalid_Type;
      Scalar    : Scalar_Value := (others => <>);
      Text      : Text_Regions.String_Value;
      Character_Item : Character := Character'Val (0);
      Handler_Id : Function_Index := Function_Index'First;
      Alternative : CCL.Types.Component_Index := 1;
      Object_Owner : Object_Count := 0;
      Object_Position : Object_Views.Cursor;
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
   function Analysis_Types (Result : Analysis_Result) return CCL.Types.Registry is (Result.Tree.Types);
   function Analysis_Resource_Policies (Result : Analysis_Result)
     return CCL.Resource_Policies.Policy_Table is (Result.Resource_Policies);

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
         Variant_Type : Static_Type;
         Choice : CCL.Types.Component_Count;
         Record_Type : Static_Type;
         Components : Component_Node_Array := [others => NO_NODE];
      begin
         Read_Name (Operator_Name, Ok);
         if not Ok then
            Index := NO_NODE;
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
         Id : Function_Index;
         Tail : Node_Reference;
         Definition : CCL.Types.Description;
         Defined_Type : Static_Type;
         Definition_Status : CCL.Types.Definition_Result;
         Is_Enum, Is_Record : Boolean;
         procedure Read_Type (Kind : out Static_Type; Allow_Unit : Boolean := False) is
            Token : Name;
            Good : Boolean;
         begin
            Read_Name (Token, Good);
            Kind := Invalid_Type;
            if Good then
               Kind := CCL.Types.Find (Tree.Types, Token);
               if Kind in Invalid_Type | Handler_Type or else (Kind = Unit_Type and not Allow_Unit) then
                  Diagnostic := Expected_Type_Name;
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
         if Name_Is (Token, "type") then
            Read_Name (Definition.Identifier, Ok);
            if not Ok then return; end if;
            Expect ('(', Ok);
            if not Ok then return; end if;
            Read_Name (Token, Ok);
            if not Ok then return; end if;
            Is_Enum := Name_Is (Token, "enum");
            Is_Record := Name_Is (Token, "record");
            if not Is_Enum and then not Is_Record and then not Name_Is (Token, "variant") then
               Diagnostic := Invalid_Type_Declaration; return;
            end if;
            Definition.Form := (if Is_Record then CCL.Types.Product else CCL.Types.Sum);
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
                     Read_Type (Definition.Parts (Definition.Count).Payload, Allow_Unit => True);
                     if Diagnostic /= No_Diagnostic then return; end if;
                     if not CCL.Objects.Persistable (Tree.Types, Definition.Parts (Definition.Count).Payload) then
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
            Expect (')', Ok);
            if not Ok then return; end if;
            Expect (')', Ok);
            if not Ok then return; end if;
            CCL.Types.Define (Tree.Types, Definition, Defined_Type, Definition_Status);
            if Definition_Status /= CCL.Types.Defined then
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
               if Kind > Visible_Types or else not CCL.Objects.Persistable (Tree.Types, Kind) or else
                 Left_Type /= CCL.Types.Describe (Tree.Types, Kind).
                 Parts (Tree.Nodes (Index).Alternative).Payload
               then Diagnostic := Invalid_Variant_Payload; end if;
            when Record_Construct =>
               Kind := Tree.Nodes (Index).Declared_Kind;
               if Kind > Visible_Types or else not CCL.Objects.Persistable (Tree.Types, Kind) then
                  Diagnostic := Invalid_Type_Declaration; return;
               end if;
               for P in 1 .. CCL.Types.Describe (Tree.Types, Kind).Count loop
                  Check_Node (Tree.Nodes (Index).Components (P), Depth + 1, Left_Type);
                  if Diagnostic /= No_Diagnostic then return; end if;
                  if Left_Type /= CCL.Types.Describe (Tree.Types, Kind).Parts (P).Payload then
                     Diagnostic := Host_Object_Type_Mismatch; return;
                  end if;
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
                           Kind := D.Parts (P).Payload;
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
                        Type_Env (Type_Env_Length) := (N.Identifier, Payload);
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
                             CCL.Objects.Persistable (Tree.Types, Ref)
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
               if not Found then
                  Diagnostic := Unknown_Name;
               end if;
            when Add_Form | Multiply_Form | Divide_Form | Modulo_Form |
                 Equal_Form =>
               Check_Node (Tree.Nodes (Node_Index (Index)).First,
                           Depth + 1, Left_Type);
               Check_Node (Tree.Nodes (Node_Index (Index)).Second,
                           Depth + 1, Right_Type);
               if Tree.Nodes (Index).Kind = Equal_Form then
                  if Diagnostic = No_Diagnostic and then
                    (Left_Type /= Right_Type or else
                     (Left_Type /= Integer_Type and then
                      not CCL.Types.Is_Enumeration (Tree.Types, Left_Type)))
                  then Diagnostic := Expected_Comparable; end if;
                  Kind := Boolean_Type;
               elsif Diagnostic = No_Diagnostic and then
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
      type Object_Array is array (Object_Index) of Object_Views.Snapshot;
      Objects : Object_Array;
      Objects_Used : Object_Count := 0;
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
      is
         Built : CCL.Objects.Build_Result := CCL.Objects.Invalid_Image;
         Text : String (1 .. MAX_TEXT_BYTES);
         Length : constant Natural := Text_Regions.Length (Item.Text);
         Copied : Text_Regions.Operation_Result;
         D : CCL.Types.Description;
      begin
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

      procedure Join_Strings (Left, Right : Runtime_Value; Item : out Runtime_Value; Good : out Boolean) is
         L : constant Object_Views.Text_Size := String_Length (Left);
         R : constant Object_Views.Text_Size := String_Length (Right);
         Data : String (1 .. CCL.Objects.Maximum_Text_Bytes);
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
                  if Objects_Used = MAX_OBJECT_VALUES then
                     Eval_Status := Evaluation_Object_Storage_Exhausted; return;
                  end if;
                  -- Reserve before evaluating effectful constructor arguments.
                  Objects_Used := Objects_Used + 1; Reserved_Object := Objects_Used;
                  declare
                     Native : CCL.Objects.Image;
                     Built : CCL.Objects.Build_Result;
                     D : constant CCL.Types.Description :=
                       CCL.Types.Describe (Tree.Types, Tree.Nodes (Index).Declared_Kind);
                  begin
                     CCL.Objects.Append (Native,
                       (if D.Form = CCL.Types.Product then CCL.Objects.Product_Cell (D.Count)
                        else CCL.Objects.Variant_Cell (Tree.Nodes (Index).Alternative)), Built);
                     Good := Built = CCL.Objects.Added;
                     if D.Form = CCL.Types.Product then
                        for P in 1 .. D.Count loop
                           exit when not Good;
                           Evaluate_Node (Tree.Nodes (Index).Components (P), Depth + 1, Left, Good);
                           if Good then Append_Runtime (Native, Left, Good); end if;
                        end loop;
                     elsif Good then
                        Evaluate_Node (Tree.Nodes (Index).First, Depth + 1, Left, Good);
                        if Good then Append_Runtime (Native, Left, Good); end if;
                     end if;
                     if Good then
                        Object_Views.Capture_Local (Objects (Reserved_Object), Tree.Types,
                          Tree.Nodes (Index).Declared_Kind, Native, Good);
                        if Good then
                           Load_View (Reserved_Object, Object_Views.Root (Objects (Reserved_Object)), Item, Good);
                        end if;
                     end if;
                     if not Good and Eval_Status = Succeeded then Eval_Status := Host_Result_Type_Mismatch; end if;
                     Ok := Good;
                  end;
               end if;
            when Field_Form =>
               Evaluate_Node (Tree.Nodes (Index).First, Depth + 1, Left, Good);
               if Good and then Left.Object_Owner /= 0 then
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
                              if Left.Object_Owner /= 0 then
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
                     else Left.Alternative = Right.Alternative);
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
               if Good then
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
                     (if Left.Kind = Integer_Type then Decimal_Image (Left.Scalar.Integer)
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
                                          Load_View (Reserved_Object, Object_Views.Root (Objects (Reserved_Object)), Item, Good);
                                       end if;
                                    else
                                       CCL.Objects.Values.To_VM (Contract, Tree.Types, Reply.Value.Object, VM_Value, Good);
                                       if Good then
                                          Item.Kind := (case VM_Value.Kind is
                                            when CCL.VM.Integer_Value => Integer_Type,
                                            when CCL.VM.Boolean_Value => Boolean_Type,
                                            when CCL.VM.Variant_Value | CCL.VM.Object_Value => VM_Value.Data_Type,
                                            when CCL.VM.Resource_Value => Invalid_Type);
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
            when Invalid_Node => Ok := False;
         end case;
      end Evaluate_Node;

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
               if not CCL.Types.Is_Scalar_Sum (Tree.Types, Value.Kind) then
                  Result.Status := Host_Contract_Unsupported; Result.Has_Value := False;
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

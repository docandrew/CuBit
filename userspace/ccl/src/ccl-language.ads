with Interfaces;
with CCL.Catalog;
with CCL.VM;
with CCL.Host_Values;
with CCL.Types;
with CCL.Objects;
with CCL.Resource_Policies;

package CCL.Language with
   SPARK_Mode => On
is

   MAX_SOURCE_LENGTH : constant := 1_024;
   MAX_AST_NODES     : constant := 128;
   MAX_NAME_LENGTH   : constant := CCL.Types.Maximum_Name_Length;
   MAX_BINDINGS      : constant := 32;
   MAX_NESTING       : constant := 32;
   MAX_TEXT_BYTES    : constant := CCL.Host_Values.Maximum_Text_Length;
   MAX_FUNCTIONS     : constant := 16;
   MAX_PARAMETERS    : constant := 8;
   --  Enclosing values one anonymous function may capture.
   MAX_CAPTURES      : constant := 4;
   MAX_OBJECT_VALUES : constant := 16;
   --  Lists (docs/ccl-repl.md, "Lists"): elements across all lists of one
   --  evaluation, and elements a result can carry out.
   MAX_LIST_ELEMENTS : constant := 1_024;
   MAX_LIST_RESULT   : constant := 64;

   --  Shared, bounded frontend representation.  Both direct interpretation
   --  and CCLB compilation consume this tree, so syntax and type semantics
   --  cannot silently diverge between the two execution modes.
   subtype Node_Index is Natural range 0 .. MAX_AST_NODES - 1;
   NO_NODE : constant Natural := MAX_AST_NODES;

   subtype Node_Reference is Natural range 0 .. NO_NODE;
   subtype Node_Count is Natural range 0 .. MAX_AST_NODES;
   subtype Source_Position is Natural range 0 .. MAX_SOURCE_LENGTH + 1;

   subtype Name_Buffer is String (1 .. MAX_NAME_LENGTH);

   subtype Name is CCL.Types.Name;

   function Names_Equal (Left, Right : Name) return Boolean;

   type Node_Kind is
     (Invalid_Node,
      Integer_Literal,
      Boolean_Literal,
      String_Literal,
      Name_Reference,
      Add_Form,
      Subtract_Form,
      Multiply_Form,
      Divide_Form,
      Modulo_Form,
      Equal_Form,
      Not_Equal_Form,
      Less_Form,
      Less_Equal_Form,
      Greater_Form,
      Greater_Equal_Form,
      Not_Form,
      And_Form,
      Or_Form,
      If_Form,
      Let_Form,
      String_Length_Form,
      String_Index_Form,
      String_Concat_Form,
      To_String_Form,
      Host_Import_Form,
      Type_Definition,
      Variant_Literal,
      Variant_Construct,
      Record_Construct,
      Field_Form,
      Match_Form,
      Match_Arm,
      Function_Definition,
      Function_Call,
      Handler_Form,
      List_Construct,
      Lambda_Form,
      Builtin_Form);

   subtype Static_Type is CCL.Types.Type_Reference;

   --  Builtins over lists and functions (docs/ccl-repl.md, "Lists"). The
   --  collection is the last argument, so a pipeline a | f x means (f x a).
   type Builtin_Operation is
     (No_Builtin, Each_Builtin, Where_Builtin, Fold_Builtin, Any_Builtin,
      All_Builtin, First_Builtin, Sum_Builtin, Range_Builtin,
      --  Lists, round 2.
      Last_Builtin, Skip_Builtin, Reverse_Builtin, Sort_Builtin, Sort_By_Builtin,
      Count_Builtin, Min_Builtin, Max_Builtin, Contains_Builtin,
      --  Strings: the subject string is the last operand.
      Upper_Builtin, Lower_Builtin, Trim_Builtin, Starts_With_Builtin,
      Ends_With_Builtin, Index_Of_Builtin, Replace_Builtin, Split_Builtin,
      Join_Builtin, Parse_Int_Builtin);
   function Builtin_Name (Operation : Builtin_Operation) return String is
     (case Operation is
        when No_Builtin => "", when Each_Builtin => "each",
        when Where_Builtin => "where", when Fold_Builtin => "fold",
        when Any_Builtin => "any", when All_Builtin => "all",
        when First_Builtin => "first", when Sum_Builtin => "sum",
        when Range_Builtin => "range", when Last_Builtin => "last",
        when Skip_Builtin => "skip", when Reverse_Builtin => "reverse",
        when Sort_Builtin => "sort", when Sort_By_Builtin => "sort-by",
        when Count_Builtin => "count", when Min_Builtin => "min",
        when Max_Builtin => "max", when Contains_Builtin => "contains",
        when Upper_Builtin => "upper", when Lower_Builtin => "lower",
        when Trim_Builtin => "trim", when Starts_With_Builtin => "starts-with",
        when Ends_With_Builtin => "ends-with", when Index_Of_Builtin => "index-of",
        when Replace_Builtin => "replace", when Split_Builtin => "split",
        when Join_Builtin => "join", when Parse_Int_Builtin => "parse-int");
   function Builtin_Arity (Operation : Builtin_Operation) return Natural is
     (case Operation is
        when No_Builtin => 0,
        when Sum_Builtin | Reverse_Builtin | Sort_Builtin | Min_Builtin | Max_Builtin |
             Upper_Builtin | Lower_Builtin | Trim_Builtin | Parse_Int_Builtin => 1,
        when Fold_Builtin | Replace_Builtin => 3,
        when others => 2);
   --  Builtins whose subject (last operand) may be a String as well as a list.
   function Takes_Text (Operation : Builtin_Operation) return Boolean is
     (Operation in First_Builtin | Last_Builtin | Skip_Builtin | Reverse_Builtin |
        Contains_Builtin | Upper_Builtin .. Parse_Int_Builtin);
   Invalid_Type : constant Static_Type := CCL.Types.Invalid_Type;
   Integer_Type : constant Static_Type := CCL.Types.Integer_Type;
   Boolean_Type : constant Static_Type := CCL.Types.Boolean_Type;
   String_Type : constant Static_Type := CCL.Types.String_Type;
   Character_Type : constant Static_Type := CCL.Types.Character_Type;
   Handler_Type : constant Static_Type := CCL.Types.Handler_Type;
   Unit_Type : constant Static_Type := CCL.Types.Unit_Type;

   subtype Parameter_Count is Natural range 0 .. MAX_PARAMETERS;
   subtype Parameter_Index is Positive range 1 .. MAX_PARAMETERS;
   type Parameter is record
      Identifier : Name;
      Kind : Static_Type := Invalid_Type;
   end record;
   type Parameter_Array is array (Parameter_Index) of Parameter;
   subtype Capture_Count is Natural range 0 .. MAX_CAPTURES;
   subtype Capture_Index is Positive range 1 .. MAX_CAPTURES;
   type Capture_Array is array (Capture_Index) of Parameter;
   type Argument_Array is array (Parameter_Index) of Node_Reference;
   type Component_Node_Array is array (CCL.Types.Component_Index) of Node_Reference;
   subtype Function_Index is Natural range 0 .. MAX_FUNCTIONS - 1;
   type Function_Declaration is record
      Identifier : Name;
      Count : Parameter_Count := 0;
      Parameters : Parameter_Array := [others => (others => <>)];
      Result_Kind : Static_Type := Invalid_Type;
      Body_Node : Node_Reference := NO_NODE;
      --  An anonymous function's captured enclosing bindings. They are
      --  bound, by value, ahead of the parameters (lambda lifting).
      Captures : Capture_Array := [others => (others => <>)];
      Captured : Capture_Count := 0;
   end record;
   type Function_Array is array (Function_Index) of Function_Declaration;

   type Node is record
      Kind            : Node_Kind := Invalid_Node;
      Static_Kind     : Static_Type := Invalid_Type;
      Declared_Kind   : Static_Type := Invalid_Type;
      Alternative    : CCL.Types.Component_Index := 1;
      Source_Position : CCL.Language.Source_Position := 0;
      Source_End_Position : CCL.Language.Source_Position := 0;
      Integer_Value   : Interfaces.Integer_64 := 0;
      Boolean_Value   : Boolean := False;
      --  Inclusive slice bounds, including the canonical empty slice 1 .. 0.
      --  Unlike independent offset/length fields, no sum can leave storage.
      Text_First      : Positive range 1 .. MAX_TEXT_BYTES + 1 := 1;
      Text_Last       : Natural range 0 .. MAX_TEXT_BYTES := 0;
      Identifier      : Name;
      Pattern         : Name;
      First           : Node_Reference := NO_NODE;
      Second          : Node_Reference := NO_NODE;
      Third           : Node_Reference := NO_NODE;
      Host_Call       : CCL.Catalog.Resolved_Operation := (others => <>);
      --  Relevant only to function definitions, resolved calls and handlers.
      --  Resolution failure is a diagnostic, not an out-of-table index.
      Function_Id     : Function_Index := Function_Index'First;
      Argument_Count  : Parameter_Count := 0;
      Arguments       : Argument_Array := [others => NO_NODE];
      Components      : Component_Node_Array := [others => NO_NODE];
      --  List_Construct: how many Components are elements.
      Element_Count   : CCL.Types.Component_Count := 0;
      --  Name_Reference naming a defined function (a function value), or a
      --  Function_Call through a function-typed binding (Function_Id is then
      --  taken from the value).
      Names_Function  : Boolean := False;
      Calls_Value     : Boolean := False;
      --  Builtin_Form: which builtin; its operands are Arguments.
      Builtin         : Builtin_Operation := No_Builtin;
   end record;

   type Node_Array is array (Node_Index) of Node;

   type Syntax_Tree is record
      Length : Node_Count := 0;
      Nodes  : Node_Array := [others => (others => <>)];
      Root   : Node_Reference := NO_NODE;
      Function_Count : Natural range 0 .. MAX_FUNCTIONS := 0;
      Functions : Function_Array := [others => (others => <>)];
      Types : CCL.Types.Registry;
      Text_Bytes_Used : Natural range 0 .. MAX_TEXT_BYTES := 0;
      Text_Data : String (1 .. MAX_TEXT_BYTES) :=
        [others => Character'Val (0)];
   end record;

   type Interpretation_Status is
     (Succeeded,
      Parse_Failed,
      Type_Check_Failed,
      Evaluation_Fuel_Exhausted,
      Evaluation_Overflow,
      Evaluation_Division_By_Zero,
      Evaluation_Index_Error,
      Evaluation_Text_Storage_Exhausted,
      Evaluation_Object_Storage_Exhausted,
      Host_Import_Required,
      Host_Authority_Denied,
      Host_Call_Failed,
      Host_Result_Type_Mismatch,
      Host_Argument_Out_Of_Bounds,
      Host_Contract_Unsupported,
      Evaluation_Depth_Exhausted,
      Evaluation_List_Storage_Exhausted,
      --  parse-int on text that is not a decimal integer.
      Evaluation_Invalid_Number);

   type Diagnostic_Code is
     (No_Diagnostic,
      Source_Too_Long,
      Unexpected_End,
      Unexpected_Token,
      Unknown_Form,
      Expected_Close,
      Expected_Name,
      Invalid_Integer,
      Nesting_Too_Deep,
      AST_Full,
      Trailing_Input,
      Unknown_Name,
      Expected_Integer,
      Expected_Boolean,
      Expected_String,
      Expected_Comparable,
      Expected_Printable,
      Invalid_Type_Declaration,
      Invalid_Variant_Payload,
      Invalid_Match_Pattern,
      Nonexhaustive_Match,
      Duplicate_Match_Arm,
      Branch_Type_Mismatch,
      Too_Many_Bindings,
      Unterminated_String,
      Invalid_String_Escape,
      Text_Storage_Full,
      Expected_Type_Name,
      Too_Many_Functions,
      Too_Many_Parameters,
      Duplicate_Declaration,
      Function_Arity_Mismatch,
      Function_Argument_Mismatch,
      Function_Result_Mismatch,
      Expected_Handler,
      Invalid_Handler_Profile,
      Handler_Result_Not_Exportable,
      Host_Schema_Unavailable,
      Unsupported_Host_Object,
      Host_Object_Type_Mismatch,
      List_Element_Mismatch,
      Unsupported_List_Element,
      Empty_List_Needs_Type,
      Too_Many_List_Elements,
      Lambda_Parameter_Needs_Type,
      Lambda_Capture_Unsupported,
      Too_Many_Captures);

   type Text_Result is record
      Length : Natural range 0 .. MAX_TEXT_BYTES := 0;
      Data   : String (1 .. MAX_TEXT_BYTES) :=
        [others => Character'Val (0)];
   end record;

   --  A list result: its elements as values (Integer, Boolean; a Character
   --  as its code; an enumeration member as its position) or, for strings,
   --  as consecutive slices of List_Text ending at List_Text_Ends.
   subtype List_Result_Count is Natural range 0 .. MAX_LIST_RESULT;
   type List_Result_Values is
     array (1 .. MAX_LIST_RESULT) of CCL.VM.Value;
   type List_Result_Ends is
     array (1 .. MAX_LIST_RESULT) of Natural range 0 .. MAX_TEXT_BYTES;

   type Analysis_Status is
     (Analysis_Succeeded,
      Analysis_Parse_Failed,
      Analysis_Type_Check_Failed);

   type Analysis_Result is private;

   function Analysis_Status_Of
     (Result : Analysis_Result) return Analysis_Status;

   function Analysis_Diagnostic
     (Result : Analysis_Result) return Diagnostic_Code;

   function Analysis_Diagnostic_Position
     (Result : Analysis_Result) return Natural;

   function Analysis_Node_Count
     (Result : Analysis_Result) return Node_Count;

   function Analysis_Root
     (Result : Analysis_Result) return Node_Reference;
   function Analysis_Types (Result : Analysis_Result) return CCL.Types.Registry;
   function Analysis_Resource_Policies (Result : Analysis_Result)
     return CCL.Resource_Policies.Policy_Table;

   --  The analysis's function table (named functions and lifted lambdas).
   function Analysis_Function_Count (Result : Analysis_Result) return Natural
     with Post => Analysis_Function_Count'Result <= MAX_FUNCTIONS;
   function Analysis_Function
     (Result : Analysis_Result; Id : Function_Index) return Function_Declaration;

   function Analysis_Node
     (Result : Analysis_Result;
      Index  : Node_Index) return Node;

   procedure Analyze
     (Source : String;
      Result : out Analysis_Result);

   procedure Analyze
     (Source             : String;
      Visible_Interfaces : CCL.Catalog.Interface_Catalog;
      Result             : out Analysis_Result);

   type Interpretation_Result is record
      Status         : Interpretation_Status := Parse_Failed;
      Diagnostic     : Diagnostic_Code := No_Diagnostic;
      --  One-based source position; zero means no source diagnostic.
      Diagnostic_Position : Source_Position := 0;
      Has_Value      : Boolean := False;
      Has_Text       : Boolean := False;
      Has_Character  : Boolean := False;
      Variant_Type : Static_Type := Invalid_Type;
      Variant_Type_Name : Name;
      Variant_Member_Name : Name;
      Variant_Payload_Type : Static_Type := Unit_Type;
      Result_Value   : CCL.VM.Value := (others => <>);
      Result_Text    : Text_Result := (others => <>);
      Result_Character : Character := Character'Val (0);
      --  A function value: the name of the function it refers to.
      Has_Function : Boolean := False;
      Function_Name : Name;
      Has_List : Boolean := False;
      List_Type : Static_Type := Invalid_Type;
      List_Element_Type : Static_Type := Invalid_Type;
      List_Length : List_Result_Count := 0;
      --  The list's full length; List_Length elements (a prefix) are carried
      --  out, so a result never fails just for being long.
      List_Total : Natural range 0 .. MAX_LIST_ELEMENTS := 0;
      List_Values : List_Result_Values := [others => (others => <>)];
      List_Text : Text_Result := (others => <>);
      List_Text_Ends : List_Result_Ends := [others => 0];
      Fuel_Remaining : Natural := 0;
   end record;

   function Has_Scalar (Item : Interpretation_Result) return Boolean is
     (Item.Status = Succeeded and then Item.Has_Value and then
      not Item.Has_Text and then not Item.Has_Character and then
      not Item.Has_List and then not Item.Has_Function and then
      CCL.Types."=" (Item.Variant_Type, Invalid_Type));

   procedure Interpret
     (Source : String;
      Fuel   : Natural;
      Result : out Interpretation_Result)
   with
      Post => Result.Fuel_Remaining <= Fuel;

   procedure Interpret
     (Source             : String;
      Fuel               : Natural;
      Visible_Interfaces : CCL.Catalog.Interface_Catalog;
      Result             : out Interpretation_Result)
   with
      Post => Result.Fuel_Remaining <= Fuel;

   -- Trusted, statically instantiated host adapter. Source cannot supply a
   -- callback or mint bindings. Initial support is synchronous scalar-copy
   -- calls only, with full analysis/admission before any host invocation.
   generic
      type Host_Context is limited private;
      with procedure Invoke
        (Context : in out Host_Context; Binding : Interfaces.Unsigned_32;
         Argument : CCL.VM.Value; Value : out CCL.VM.Value;
         Success : out Boolean);
   procedure Interpret_With_Host
     (Source : String; Fuel : Natural;
      Visible_Interfaces : CCL.Catalog.Interface_Catalog;
      Grants : CCL.Catalog.Granted_Bindings;
      Context : in out Host_Context;
      Result : out Interpretation_Result)
     with Post => Result.Fuel_Remaining <= Fuel;

   generic
      type Host_Context is limited private;
      with procedure Invoke
        (Context : in out Host_Context; Binding : Interfaces.Unsigned_32;
         Argument : CCL.Host_Values.Value; Reply : out CCL.Host_Values.Call_Result);
      Allow_Text : Boolean := True;
   procedure Interpret_With_Values
     (Source : String; Fuel : Natural;
      Visible_Interfaces : CCL.Catalog.Interface_Catalog;
      Grants : CCL.Catalog.Granted_Bindings;
      Context : in out Host_Context; Result : out Interpretation_Result)
     with Post => Result.Fuel_Remaining <= Fuel;

   type Object_Interpretation_Result is record
      Status : Interpretation_Status := Parse_Failed;
      Diagnostic : Diagnostic_Code := No_Diagnostic;
      Diagnostic_Position : Source_Position := 0;
      Fuel_Remaining : Natural := 0;
      Has_Value : Boolean := False;
      Value : CCL.Objects.Image;
   end record;
   -- Separate from the scalar/UI result: ordinary evaluations do not carry
   -- an extra native image. Output is owned and remains valid after evaluation.
   -- Expected is independently approved metadata, never a grant. Mismatch is
   -- rejected before host effects; Has_Value requires validation against it.
   generic
      type Host_Context is limited private;
      with procedure Invoke
        (Context : in out Host_Context; Binding : Interfaces.Unsigned_32;
         Argument : CCL.Host_Values.Value; Reply : out CCL.Host_Values.Call_Result);
   procedure Interpret_Object_With_Values
     (Source : String; Fuel : Natural;
      Visible_Interfaces : CCL.Catalog.Interface_Catalog;
      Grants : CCL.Catalog.Granted_Bindings;
      Context : in out Host_Context; Expected : CCL.Objects.Binding;
      Result : out Object_Interpretation_Result)
     with Post => Result.Fuel_Remaining <= Fuel;

   procedure Interpret_Object
     (Source : String; Fuel : Natural; Expected : CCL.Objects.Binding;
      Result : out Object_Interpretation_Result)
     with Post => Result.Fuel_Remaining <= Fuel;
   -- Pure evaluation: no visible interfaces or host grants. Source must define
   -- its own types (which must match Expected), or return a primitive value.

private
   --  Shared with the retained-handler child package. Only checked, private
   --  frontend results may enter the analyze-free execution path.
   generic
      type Host_Context is limited private;
      with procedure Invoke
        (Context : in out Host_Context; Binding : Interfaces.Unsigned_32;
         Argument : CCL.Host_Values.Value; Reply : out CCL.Host_Values.Call_Result);
      Export_Native : Boolean := False;
      with procedure Deliver_Native (Value : CCL.Objects.Image) is null;
   procedure Process_Source_With_Host
     (Source : String; Fuel : Natural;
      Visible_Interfaces : CCL.Catalog.Interface_Catalog;
      Grants : CCL.Catalog.Granted_Bindings;
      Context : in out Host_Context; Host_Enabled : Boolean;
      Analyze_Input : Boolean; Evaluate : Boolean;
      Result : out Interpretation_Result; Tree : in out Syntax_Tree)
     with Post => Result.Fuel_Remaining <= Fuel;

   procedure Admit
     (Tree : Syntax_Tree; Grants : CCL.Catalog.Granted_Bindings;
      Allow_Text : Boolean; Status : out Interpretation_Status;
      Position : out Source_Position);

   type Analysis_Result is record
      Resource_Policies : CCL.Resource_Policies.Policy_Table := [others => (others => <>)];
      Status              : Analysis_Status := Analysis_Parse_Failed;
      Diagnostic          : Diagnostic_Code := No_Diagnostic;
      Diagnostic_Position : Natural range 0 .. MAX_SOURCE_LENGTH + 1 := 0;
      Tree                : Syntax_Tree;
      Source_Length : Natural range 0 .. MAX_SOURCE_LENGTH := 0;
      Source_Text : String (1 .. MAX_SOURCE_LENGTH) := [others => ' '];
   end record;
end CCL.Language;

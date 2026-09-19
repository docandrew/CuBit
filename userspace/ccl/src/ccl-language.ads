with Interfaces;
with CCL.Catalog;
with CCL.VM;
with CCL.Host_Values;

package CCL.Language with
   SPARK_Mode => On
is

   MAX_SOURCE_LENGTH : constant := 1_024;
   MAX_AST_NODES     : constant := 128;
   MAX_NAME_LENGTH   : constant := 32;
   MAX_BINDINGS      : constant := 32;
   MAX_NESTING       : constant := 32;
   MAX_TEXT_BYTES    : constant := CCL.Host_Values.Maximum_Text_Length;
   MAX_FUNCTIONS     : constant := 16;
   MAX_PARAMETERS    : constant := 8;

   --  Shared, bounded frontend representation.  Both direct interpretation
   --  and CCLB compilation consume this tree, so syntax and type semantics
   --  cannot silently diverge between the two execution modes.
   subtype Node_Index is Natural range 0 .. MAX_AST_NODES - 1;
   NO_NODE : constant Natural := MAX_AST_NODES;

   subtype Node_Reference is Natural range 0 .. NO_NODE;
   subtype Node_Count is Natural range 0 .. MAX_AST_NODES;
   subtype Source_Position is Natural range 0 .. MAX_SOURCE_LENGTH + 1;

   subtype Name_Buffer is String (1 .. MAX_NAME_LENGTH);

   type Name is record
      Length : Natural range 0 .. MAX_NAME_LENGTH := 0;
      Data   : Name_Buffer := [others => ' '];
   end record;

   function Names_Equal (Left, Right : Name) return Boolean;

   type Node_Kind is
     (Invalid_Node,
      Integer_Literal,
      Boolean_Literal,
      String_Literal,
      Name_Reference,
      Add_Form,
      Multiply_Form,
      Divide_Form,
      Modulo_Form,
      Equal_Form,
      Not_Form,
      If_Form,
      Let_Form,
      String_Length_Form,
      String_Index_Form,
      String_Concat_Form,
      To_String_Form,
      Host_Import_Form,
      Function_Definition,
      Function_Call,
      Handler_Form);

   type Static_Type is
     (Invalid_Type, Integer_Type, Boolean_Type, String_Type, Character_Type, Handler_Type);

   subtype Parameter_Count is Natural range 0 .. MAX_PARAMETERS;
   subtype Parameter_Index is Positive range 1 .. MAX_PARAMETERS;
   type Parameter is record
      Identifier : Name;
      Kind : Static_Type := Invalid_Type;
   end record;
   type Parameter_Array is array (Parameter_Index) of Parameter;
   type Argument_Array is array (Parameter_Index) of Node_Reference;
   subtype Function_Index is Natural range 0 .. MAX_FUNCTIONS - 1;
   NO_FUNCTION : constant := MAX_FUNCTIONS;
   subtype Function_Reference is Natural range 0 .. NO_FUNCTION;
   type Function_Declaration is record
      Identifier : Name;
      Count : Parameter_Count := 0;
      Parameters : Parameter_Array := [others => (others => <>)];
      Result_Kind : Static_Type := Invalid_Type;
      Body_Node : Node_Reference := NO_NODE;
   end record;
   type Function_Array is array (Function_Index) of Function_Declaration;

   type Node is record
      Kind            : Node_Kind := Invalid_Node;
      Static_Kind     : Static_Type := Invalid_Type;
      Source_Position : CCL.Language.Source_Position := 0;
      Source_End_Position : CCL.Language.Source_Position := 0;
      Integer_Value   : Interfaces.Integer_64 := 0;
      Boolean_Value   : Boolean := False;
      --  Inclusive slice bounds, including the canonical empty slice 1 .. 0.
      --  Unlike independent offset/length fields, no sum can leave storage.
      Text_First      : Positive range 1 .. MAX_TEXT_BYTES + 1 := 1;
      Text_Last       : Natural range 0 .. MAX_TEXT_BYTES := 0;
      Identifier      : Name;
      First           : Node_Reference := NO_NODE;
      Second          : Node_Reference := NO_NODE;
      Third           : Node_Reference := NO_NODE;
      Host_Call       : CCL.Catalog.Resolved_Operation := (others => <>);
      Function_Id     : Function_Reference := NO_FUNCTION;
      Argument_Count  : Parameter_Count := 0;
      Arguments       : Argument_Array := [others => NO_NODE];
   end record;

   type Node_Array is array (Node_Index) of Node;

   type Syntax_Tree is record
      Length : Node_Count := 0;
      Nodes  : Node_Array := [others => (others => <>)];
      Root   : Node_Reference := NO_NODE;
      Function_Count : Natural range 0 .. MAX_FUNCTIONS := 0;
      Functions : Function_Array := [others => (others => <>)];
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
      Host_Import_Required,
      Host_Authority_Denied,
      Host_Call_Failed,
      Host_Result_Type_Mismatch,
      Host_Argument_Out_Of_Bounds,
      Host_Contract_Unsupported,
      Evaluation_Depth_Exhausted);

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
      Handler_Result_Not_Exportable);

   type Text_Result is record
      Length : Natural range 0 .. MAX_TEXT_BYTES := 0;
      Data   : String (1 .. MAX_TEXT_BYTES) :=
        [others => Character'Val (0)];
   end record;

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
      Result_Value   : CCL.VM.Value := (others => <>);
      Result_Text    : Text_Result := (others => <>);
      Result_Character : Character := Character'Val (0);
      Fuel_Remaining : Natural := 0;
   end record;

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
         Argument : CCL.Host_Values.Value; Value : out CCL.Host_Values.Value;
         Success : out Boolean);
      Allow_Text : Boolean := True;
   procedure Interpret_With_Values
     (Source : String; Fuel : Natural;
      Visible_Interfaces : CCL.Catalog.Interface_Catalog;
      Grants : CCL.Catalog.Granted_Bindings;
      Context : in out Host_Context; Result : out Interpretation_Result)
     with Post => Result.Fuel_Remaining <= Fuel;

private
   --  Shared with the retained-handler child package. Only checked, private
   --  frontend results may enter the analyze-free execution path.
   generic
      type Host_Context is limited private;
      with procedure Invoke
        (Context : in out Host_Context; Binding : Interfaces.Unsigned_32;
         Argument : CCL.Host_Values.Value; Value : out CCL.Host_Values.Value;
         Success : out Boolean);
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
      Status              : Analysis_Status := Analysis_Parse_Failed;
      Diagnostic          : Diagnostic_Code := No_Diagnostic;
      Diagnostic_Position : Natural range 0 .. MAX_SOURCE_LENGTH + 1 := 0;
      Tree                : Syntax_Tree;
      Source_Length : Natural range 0 .. MAX_SOURCE_LENGTH := 0;
      Source_Text : String (1 .. MAX_SOURCE_LENGTH) := [others => ' '];
   end record;
end CCL.Language;

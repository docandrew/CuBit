with Interfaces;
with CCL.VM;

package CCL.Language with
   SPARK_Mode => On
is

   MAX_SOURCE_LENGTH : constant := 1_024;
   MAX_AST_NODES     : constant := 128;
   MAX_NAME_LENGTH   : constant := 32;
   MAX_BINDINGS      : constant := 32;
   MAX_NESTING       : constant := 32;

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
      Name_Reference,
      Add_Form,
      Equal_Form,
      Not_Form,
      If_Form,
      Let_Form);

   type Static_Type is (Invalid_Type, Integer_Type, Boolean_Type);

   type Node is record
      Kind            : Node_Kind := Invalid_Node;
      Static_Kind     : Static_Type := Invalid_Type;
      Source_Position : CCL.Language.Source_Position := 0;
      Source_End_Position : CCL.Language.Source_Position := 0;
      Integer_Value   : Interfaces.Integer_64 := 0;
      Boolean_Value   : Boolean := False;
      Identifier      : Name;
      First           : Node_Reference := NO_NODE;
      Second          : Node_Reference := NO_NODE;
      Third           : Node_Reference := NO_NODE;
   end record;

   type Node_Array is array (Node_Index) of Node;

   type Syntax_Tree is record
      Length : Node_Count := 0;
      Nodes  : Node_Array := [others => (others => <>)];
      Root   : Node_Reference := NO_NODE;
   end record;

   type Interpretation_Status is
     (Succeeded,
      Parse_Failed,
      Type_Check_Failed,
      Evaluation_Fuel_Exhausted,
      Evaluation_Overflow);

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
      Branch_Type_Mismatch,
      Too_Many_Bindings);

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

   type Interpretation_Result is record
      Status         : Interpretation_Status := Parse_Failed;
      Diagnostic     : Diagnostic_Code := No_Diagnostic;
      --  One-based source position; zero means no source diagnostic.
      Diagnostic_Position : Source_Position := 0;
      Has_Value      : Boolean := False;
      Result_Value   : CCL.VM.Value := (others => <>);
      Fuel_Remaining : Natural := 0;
   end record;

   procedure Interpret
     (Source : String;
      Fuel   : Natural;
      Result : out Interpretation_Result)
   with
      Post => Result.Fuel_Remaining <= Fuel;

private
   type Analysis_Result is record
      Status              : Analysis_Status := Analysis_Parse_Failed;
      Diagnostic          : Diagnostic_Code := No_Diagnostic;
      Diagnostic_Position : Natural range 0 .. MAX_SOURCE_LENGTH + 1 := 0;
      Tree                : Syntax_Tree;
   end record;
end CCL.Language;

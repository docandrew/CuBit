package body CCL.Diagnostics with SPARK_Mode is
   function Message (Code : CCL.Language.Diagnostic_Code) return String is
      use CCL.Language;
   begin
      return (case Code is
         when No_Diagnostic => "No diagnostic",
         when Source_Too_Long => "Source exceeds its size limit",
         when Unexpected_End => "Expression ended before it was complete",
         when Unexpected_Token => "Unexpected token",
         when Unknown_Form => "Unknown operation or function; functions must be defined before use",
         when Expected_Close => "Expected a closing parenthesis",
         when Expected_Name => "Expected a binding or operation name (at most 32 characters)",
         when Invalid_Integer => "Invalid or out-of-range integer",
         when Value_Out_Of_Range => "Value is outside the range of its type",
         when Nesting_Too_Deep => "Expression nesting limit exceeded",
         when AST_Full => "Expression has too many syntax nodes",
         when Trailing_Input => "Unexpected text after the expression",
         when Unknown_Name => "Unknown name (keep a value with LET x = ... or (define x ...))",
         when Expected_Integer => "Expected an Integer expression",
         when Expected_Boolean => "Expected a Boolean expression",
         when Expected_String => "Expected a String expression",
         when Expected_Comparable => "Equality requires two Integers or two members of the same enum type",
         when Expected_Printable => "to-string expects an Integer or enum member",
         when Expected_Stream => "Expected a stream: (stream T n), or a name bound to one",
         when Expected_Task => "Expected a task: (task T n), or a name bound to one",
         when Invalid_Type_Declaration => "Invalid type declaration: use 1 to 16 uniquely named alternatives and a unique type name (at most 48 types)",
         when Invalid_Variant_Payload => "Variant payload must match its declared Integer or Boolean type",
         when Invalid_Match_Pattern => "Match patterns must name alternatives of the input type and bind exactly their payloads",
         when Nonexhaustive_Match => "Match must handle every alternative",
         when Duplicate_Match_Arm => "Match contains a duplicate alternative",
         when Branch_Type_Mismatch => "All branches must return the same type",
         when Too_Many_Bindings => "Too many local bindings",
         when Unterminated_String => "String is missing its closing quote",
         when Invalid_String_Escape => "Unsupported escape sequence in string",
         when Text_Storage_Full => "Expression text storage limit exceeded",
         when Expected_Type_Name => "Expected a built-in value type or a previously declared variant type",
         when Too_Many_Functions => "Program exceeds the 16-function limit",
         when Too_Many_Parameters => "Function exceeds the 8-parameter limit",
         when Duplicate_Declaration => "Duplicate or reserved function/parameter name",
         when Function_Arity_Mismatch => "Function argument count does not match its declaration",
         when Function_Argument_Mismatch => "Argument type does not match the function parameter",
         when Function_Result_Mismatch => "Function body does not match its declared return type",
         when Expected_Handler => "Expected a typed handler value",
         when Host_Schema_Unavailable => "The host object's approved schema is not visible",
         when Unsupported_Host_Object => "This object shape is not supported by the interpreter yet",
         when Host_Object_Type_Mismatch => "The argument does not match the host object's declared type",
         when Field_Type_Mismatch => "This field takes another type",
         when Argument_Type_Mismatch => "This operation takes another type",
         when Invalid_Handler_Profile => "Handler must take no arguments and return Boolean",
         when Handler_Result_Not_Exportable => "Pass the handler to a service; it cannot be returned from this invocation",
         when List_Element_Mismatch => "Every element of a list must have the same type",
         when Unsupported_List_Element => "Lists of this element type are not supported yet",
         when Unsupported_Stream_Element =>
            "A stream carries data: scalars, strings, records, variants or lists of them",
         when Stream_Not_Data =>
            "A stream is a live source a session holds, not data: it cannot be a field or payload",
         when Empty_List_Needs_Type => "An empty list needs a declared element type",
         when Lambda_Parameter_Needs_Type => "Give each fn parameter a type: (fn ((x Integer)) ...)",
         when Lambda_Capture_Unsupported => "A fn body can capture only scalar, String, Character or enumeration values; pass lists and functions as parameters",
         when Too_Many_Captures => "A fn body can capture at most 4 enclosing values; pass the rest as parameters",
         when Invalid_Field_Default =>
           "A field's default must be a constant of its type: an Integer within its range, true or false, " &
           "an enum member such as Color.Red, or [] for a list",
         when Unknown_Field_Argument => "This record has no field by that name",
         when Repeated_Field_Argument => "This field is already given, by position or by name",
         when Missing_Field_Argument => "This field has no default, so the construction must give it",
         when Positional_After_Named => "Positional values come before named ones (field => value)",
         when Resource_Ownership_Violation =>
            "A resource is copied, used after it was moved, or never released");
   end Message;

   function Message (Status : CCL.Language.Interpretation_Status) return String is
      use CCL.Language;
   begin
      return (case Status is
         when Succeeded => "Completed",
         when Parse_Failed => "Cannot parse expression",
         when Type_Check_Failed => "Expression does not type-check",
         when Evaluation_Fuel_Exhausted => "Execution fuel exhausted",
         when Evaluation_Overflow => "Integer arithmetic overflow",
         when Evaluation_Division_By_Zero => "Cannot divide by zero",
         when Evaluation_Index_Error => "Index is outside the value's bounds",
         when Evaluation_Range_Error => "A computed value is outside the range of its type",
         when Evaluation_Text_Storage_Exhausted => "Execution text storage exhausted",
         when Evaluation_Object_Storage_Exhausted => "Execution object storage exhausted",
         when Host_Import_Required => "Service call needs a host-enabled interpreter or VM; no service was invoked.",
         when Host_Authority_Denied => "Service operation has no granted runtime binding",
         when Host_Call_Failed => "Service call failed; no value returned",
         when Host_Result_Type_Mismatch => "Service returned a value with the wrong type",
         when Host_Argument_Out_Of_Bounds => "Argument exceeds the service's declared text bound",
         when Host_Contract_Unsupported => "This interpreter host does not support the operation's value or lifecycle contract",
         when Evaluation_Depth_Exhausted => "Execution call/expression depth limit exceeded",
         when Evaluation_List_Storage_Exhausted => "Execution list storage exhausted",
         when Evaluation_Invalid_Number => "Text is not a decimal integer",
         when Session_Value_Not_Kept => "Value cannot be kept in the session (too long or not storable); define a function instead",
         when Stream_Unavailable => "This session holds no such stream",
         when Stream_Empty => "Nothing has arrived on the stream yet",
         when Stream_Window_Out_Of_Range => "A window holds 1 to 255 elements",
         when Stream_Element_Mismatch => "The stream's elements are not of the type named",
         when Waiting_On_Task => "Waiting for the task to complete",
         when Not_Compiled => "This program cannot be compiled to bytecode yet");
   end Message;
   function Message (Status : CCL.VM.Execution_Status) return String is
      use CCL.VM;
   begin
      return (case Status is
         when Completed => "Completed",
         when Paused => "Paused",
         when Stopped => "Stopped",
         when Fuel_Exhausted => "Execution budget exhausted",
         when Arithmetic_Overflow => "Arithmetic overflow",
         when Division_By_Zero => "Division by zero",
         when Object_Storage_Exhausted => "Object storage exhausted",
         when Text_Storage_Exhausted => "Text storage exhausted",
         when Invalid_Number => "Not a number",
         when Index_Out_Of_Range => "Index out of range",
         when List_Storage_Exhausted => "List storage exhausted",
         when Range_Error => "Value outside its range type",
         when Call_Depth_Exhausted => "Calls nested too deeply",
         when Stream_Unavailable => "This session holds no such stream",
         when Stream_Empty => "Nothing has arrived on the stream yet",
         when Stream_Window_Out_Of_Range => "A window holds 1 to 255 elements",
         when Stream_Element_Mismatch => "The stream's elements are not of the type named",
         when Host_Argument_Out_Of_Bounds => "A text argument is longer than the operation accepts",
         when Invalid_Bytecode => "Invalid bytecode",
         when Waiting_For_Host => "Waiting for service",
         when Host_Call_Failed => "Service call failed",
         when No_Result => "No result");
   end Message;
end CCL.Diagnostics;

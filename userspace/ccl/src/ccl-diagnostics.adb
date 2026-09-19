package body CCL.Diagnostics with SPARK_Mode is
   function Message (Code : CCL.Language.Diagnostic_Code) return String is
      use CCL.Language;
   begin
      return (case Code is
         when No_Diagnostic => "No diagnostic",
         when Source_Too_Long => "Source exceeds the 1024-byte limit",
         when Unexpected_End => "Expression ended before it was complete",
         when Unexpected_Token => "Unexpected token",
         when Unknown_Form => "Unknown operation or function; functions must be defined before use",
         when Expected_Close => "Expected a closing parenthesis",
         when Expected_Name => "Expected a binding or operation name",
         when Invalid_Integer => "Invalid or out-of-range integer",
         when Nesting_Too_Deep => "Expression nesting limit exceeded",
         when AST_Full => "Expression has too many syntax nodes",
         when Trailing_Input => "Unexpected text after the expression",
         when Unknown_Name => "Unknown name; bindings do not persist between submissions",
         when Expected_Integer => "Expected an Integer expression",
         when Expected_Boolean => "Expected a Boolean expression",
         when Expected_String => "Expected a String expression",
         when Branch_Type_Mismatch => "Both branches of if must return the same type",
         when Too_Many_Bindings => "Too many local bindings",
         when Unterminated_String => "String is missing its closing quote",
         when Invalid_String_Escape => "Unsupported escape sequence in string",
         when Text_Storage_Full => "Expression text storage limit exceeded",
         when Expected_Type_Name => "Expected Integer, Boolean, String or Character",
         when Too_Many_Functions => "Program exceeds the 16-function limit",
         when Too_Many_Parameters => "Function exceeds the 8-parameter limit",
         when Duplicate_Declaration => "Duplicate or reserved function/parameter name",
         when Function_Arity_Mismatch => "Function argument count does not match its declaration",
         when Function_Argument_Mismatch => "Argument type does not match the function parameter",
         when Function_Result_Mismatch => "Function body does not match its declared return type",
         when Expected_Handler => "Expected a typed handler value",
         when Invalid_Handler_Profile => "Handler must take no arguments and return Boolean",
         when Handler_Result_Not_Exportable => "Pass the handler to a service; it cannot be returned from this invocation");
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
         when Evaluation_Text_Storage_Exhausted => "Execution text storage exhausted",
         when Host_Import_Required => "Service call needs a host-enabled interpreter or VM; no service was invoked.",
         when Host_Authority_Denied => "Service operation has no granted runtime binding",
         when Host_Call_Failed => "Service call failed; no value returned",
         when Host_Result_Type_Mismatch => "Service returned a value with the wrong type",
         when Host_Argument_Out_Of_Bounds => "Argument exceeds the service's declared text bound",
         when Host_Contract_Unsupported => "This interpreter host does not support the operation's value or lifecycle contract",
         when Evaluation_Depth_Exhausted => "Execution call/expression depth limit exceeded");
   end Message;
end CCL.Diagnostics;

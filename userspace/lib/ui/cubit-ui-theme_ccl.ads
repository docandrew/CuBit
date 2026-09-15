package CuBit.UI.Theme_CCL with SPARK_Mode is
   Maximum_Source : constant := 1024;
   type Diagnostic is
     (No_Error, Too_Long, Invalid_Syntax, Unsupported_Version, Invalid_Base,
      Unknown_Field, Duplicate_Field, Invalid_Color, Trailing_Input);
   type Result is record
      Success : Boolean := False;
      Error : Diagnostic := No_Error;
      Position : Positive := 1;
      Value : Theme := CuBit_Alloy;
   end record;
   --  Uses the CCL declaration scanner, never the interpreter or host imports.
   --  Failure leaves the supplied hardcoded fallback, never a partial palette.
   procedure Load (Source : String; Fallback : Theme; Output : out Result)
     with Post => (if not Output.Success then Output.Value = Fallback);
end CuBit.UI.Theme_CCL;

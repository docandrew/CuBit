with CCL.Text_Operations;
with CCL.VM.Machine;

--  Text: the constant pool, joining and comparing strings, the string
--  built-ins (CCL.Text_Operations, shared with the interpreter) and the text
--  opcodes. Each makes its result a new string in the run's region.
private package CCL.VM.Text_Ops with SPARK_Mode => On is
   use CCL.VM.Machine;

   --  Patterns, separators and replacements, and a split piece: the
   --  interpreter's short strings (a host text value).
   MAX_PATTERN_BYTES : constant := MAX_HOST_TEXT;
   type Text_Operands is array (1 .. 2) of Value;

   --  Constant Index of Content's pool, as a new string in Region.
   procedure Push_Constant
     (Content : Program; Index : Constant_Index;
      Region  : in out Text_Regions.Stack; Item : out Value;
      Status  : out Text_Regions.Operation_Result)
     with Global => null;

   --  Left and Right, joined, as a new string.
   procedure Concat_Texts
     (Region : in out Text_Regions.Stack; Left, Right : Value;
      Item   : out Value; Status : out Text_Regions.Operation_Result)
     with Global => null;

   --  Whether Left and Right hold the same characters.
   procedure Equal_Texts
     (Region : Text_Regions.Stack; Left, Right : Value;
      Same   : out Boolean; Status : out Text_Regions.Operation_Result)
     with Global => null;

   --  The interpreter's string order (sort): byte by byte, a prefix first.
   function Text_Less
     (Region : Text_Regions.Stack; Left, Right : Text_Regions.String_Value) return Boolean
     with Global => null;

   --  A string built-in on Subject, with its operands.
   procedure Run_Text_Builtin
     (Region   : in out Text_Regions.Stack;
      Item     : CCL.Text_Operations.Operation;
      Operands : Text_Operands;
      Subject  : Value;
      Result   : out Value;
      Status   : out Execution_Status)
     with Global => null;

   --  The text opcodes.
   procedure Run_Text (Item : Validated_Program; State : in out Machine_State; Regs : in out Registers)
     with Global => null,
          Pre => Ready (Item, Regs) and then
                 Item.Content.Code (Regs.PC).Op in
                   Push_Text | Concat_Text | Text_Builtin | Length_Text | Equal_Text | Text_At |
                   Integer_To_Text | Variant_To_Text,
          Post => Same_Frame (State'Old, State);
   --  Inlined into the dispatch loop (as the nested steps were): one
   --  call per instruction would cost the hot path.
   pragma Inline_Always (Run_Text);
end CCL.VM.Text_Ops;

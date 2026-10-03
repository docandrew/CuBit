with Interfaces;
with CCL.Ownership;
with CCL.Imports;
with CCL.Bounded_Stacks;
with CCL.Execution_Budgets;
with CCL.Types;
with CCL.Types.Shapes;
with CCL.Objects;
with CCL.Resources;
with CCL.Streams;
with CCL.Secondary_Stacks;
with CCL.Secondary_Arrays;
with CCL.List_Operations;

package CCL.VM with
   SPARK_Mode => On
is
   use Interfaces;
   use type CCL.Types.Type_Reference;
   use type CCL.Types.Shape;
   use type CCL.Resources.Reference;

   MAX_INSTRUCTIONS : constant := 256;
   --  Functions per program ("Functions" below).
   MAX_FUNCTIONS    : constant := 16;
   MAX_STACK_DEPTH  : constant := 64;
   MAX_IMPORTS      : constant := 16;

   type Instruction_Index is mod MAX_INSTRUCTIONS;
   type Stack_Index is range 0 .. MAX_STACK_DEPTH - 1;
   type Program_Length is range 0 .. MAX_INSTRUCTIONS;
   type Stack_Depth is range 0 .. MAX_STACK_DEPTH;
   subtype Import_Index is Natural range 0 .. MAX_IMPORTS - 1;
   subtype Import_Count is Natural range 0 .. MAX_IMPORTS;
   subtype Local_Count is Natural range 0 .. CCL.Ownership.MAX_BINDINGS;
   subtype Type_Count is Natural range 0 .. CCL.Ownership.MAX_TYPES;
   type Local_Type_Array is
     array (CCL.Ownership.Binding_Id) of CCL.Ownership.Type_Id;

   type Value_Kind is
     (Integer_Value, Boolean_Value, Variant_Value, Object_Value, Resource_Value, Text_Value,
      Character_Value, List_Value, Function_Value);
   for Value_Kind use
     (Integer_Value => 0, Boolean_Value => 1, Variant_Value => 2, Object_Value => 3,
      Resource_Value => 4, Text_Value => 5, Character_Value => 6, List_Value => 7,
      Function_Value => 8);
   for Value_Kind'Size use 8;
   subtype Scalar_Kind is Value_Kind range Integer_Value .. Boolean_Value;

   --  The value arena (docs/ccl-bytecode-format.md, step 4): the records and
   --  payload variants a run builds or copies in from the host. An
   --  Object_Value holds a node index (0 for a payload variant's unit
   --  alternative). The interpreter's bounds (CCL.Language uses these).
   MAX_VALUE_NODES : constant := 512;
   MAX_VALUE_SLOTS : constant := 2_048;
   subtype Node_Count is Natural range 0 .. MAX_VALUE_NODES;
   subtype Node_Index is Positive range 1 .. MAX_VALUE_NODES;
   subtype Slot_Count is Natural range 0 .. MAX_VALUE_SLOTS;
   subtype Slot_Index is Positive range 1 .. MAX_VALUE_SLOTS;

   --  Text (docs/ccl-bytecode-format.md, "Version 8 plan", step 2): a run's
   --  strings live in a bounded region of its machine state; a Text_Value
   --  holds a checked descriptor into it, never a pointer, so the state
   --  stays serializable. Literals come from the program's constant pool.
   --  The interpreter's bounds (CCL.Language), so a program fails at the
   --  same point in both: a string holds at most 8 KiB (the interpreter
   --  keeps longer-than-1-KiB strings in object images of that size), a
   --  result carries at most 1 KiB out, and as many live strings as a
   --  program has syntax nodes.
   MAX_STRING_BYTES   : constant := 8_192;
   MAX_TEXT_BYTES     : constant := 65_536;
   MAX_TEXT_VALUES    : constant := 512;
   MAX_RESULT_TEXT    : constant := 1_024;
   MAX_CONSTANTS      : constant := 32;
   MAX_CONSTANT_BYTES : constant := 4_096;
   package Text_Regions is new CCL.Secondary_Stacks
     (Capacity => MAX_TEXT_BYTES, Max_Values => MAX_TEXT_VALUES,
      Max_String_Length => MAX_STRING_BYTES);
   subtype Constant_Count is Natural range 0 .. MAX_CONSTANTS;
   subtype Constant_Index is Natural range 0 .. MAX_CONSTANTS - 1;
   subtype Constant_Length is Natural range 0 .. MAX_CONSTANT_BYTES;
   type Text_Constant is record
      First  : Positive range 1 .. MAX_CONSTANT_BYTES + 1 := 1;
      Length : Constant_Length := 0;
   end record;
   type Text_Constants is array (Constant_Index) of Text_Constant;
   subtype Result_Text_Length is Natural range 0 .. MAX_RESULT_TEXT;
   type Result_Text is record
      Length : Result_Text_Length := 0;
      Data   : String (1 .. MAX_RESULT_TEXT) := [others => ' '];
   end record;

   --  Lists (step 3): a run's lists live in a bounded region of its machine
   --  state, as strings do; a List_Value holds a checked descriptor and
   --  names its List<T> type in Data_Type. The interpreter's bounds
   --  (CCL.Language uses these): elements across all lists of a run, live
   --  lists, and elements a result carries out.
   MAX_LIST_ELEMENTS : constant := 4_096;
   MAX_LIST_VALUES   : constant := 512;
   MAX_LIST_RESULT   : constant := 64;
   --  One element: a value's scalar fields, read back by the element type.
   --  Integer holds an Integer, a Character's code or a scalar variant's
   --  Integer payload; Boolean a Boolean or Boolean payload; Alternative a
   --  variant's member; Text a String; Node a record or payload variant.
   type List_Element is record
      Integer : Integer_64 := 0;
      Boolean : Standard.Boolean := False;
      Alternative : CCL.Types.Component_Count := 0;
      Text : Text_Regions.String_Value;
      Node : Node_Count := 0;
   end record;
   Null_List_Element : constant List_Element := (others => <>);
   type List_Element_Array is array (Positive range <>) of List_Element;
   package List_Regions is new CCL.Secondary_Arrays
     (Element_Type => List_Element, Null_Element => Null_List_Element,
      Element_Array => List_Element_Array,
      Capacity => MAX_LIST_ELEMENTS, Max_Values => MAX_LIST_VALUES);

   type Value is record
      Kind    : Value_Kind := Integer_Value;
      Integer : Integer_64 := 0;
      Boolean : Standard.Boolean := False;
      Type_Tag : CCL.Ownership.Type_Id := 0;
      Data_Type : CCL.Types.Type_Reference := CCL.Types.Invalid_Type;
      Alternative : CCL.Types.Component_Index := 1;
      Copyable : Standard.Boolean := True;
      Node : Node_Count := 0;
      Resource : CCL.Resources.Reference := CCL.Resources.No_Reference;
      Text : Text_Regions.String_Value;
      Items : List_Regions.Array_Value;
   end record;

   --  A list result: its first elements as values (Integer, Boolean; a
   --  Character as its code; an enumeration member as its position) or, for
   --  strings, as consecutive slices of List_Text ending at List_Text_Ends.
   subtype List_Result_Count is Natural range 0 .. MAX_LIST_RESULT;
   type List_Result_Values is array (1 .. MAX_LIST_RESULT) of Value;
   type List_Result_Ends is array (1 .. MAX_LIST_RESULT) of Result_Text_Length;

   function Integer_Constant (Item : Integer_64) return Value is
     ((Kind => Integer_Value, Integer => Item, others => <>));

   function Boolean_Constant (Item : Standard.Boolean) return Value is
     ((Kind => Boolean_Value, Boolean => Item, others => <>));

   --  A character is its code in Integer.
   MAX_CHARACTER_CODE : constant := Character'Pos (Character'Last);
   function Character_Constant (Item : Character) return Value is
     ((Kind => Character_Value, Integer => Integer_64 (Character'Pos (Item)), others => <>));

   function With_Type
     (Item : Value; Type_Tag : CCL.Ownership.Type_Id) return Value is
     ((Kind => Item.Kind, Integer => Item.Integer, Boolean => Item.Boolean,
       Type_Tag => Type_Tag, Data_Type => Item.Data_Type, Alternative => Item.Alternative,
       Copyable => Item.Copyable, Node => Item.Node,
       Resource => Item.Resource, Text => Item.Text, Items => Item.Items));

   function Native_Object_Type
     (Types : CCL.Types.Registry; Ref : CCL.Types.Type_Reference) return Boolean is
     (CCL.Objects.Persistable (Types, Ref) and then
      Ref not in CCL.Types.Integer_Type | CCL.Types.Boolean_Type and then
      not CCL.Types.Is_Scalar_Sum (Types, Ref) and then not CCL.Types.Is_List (Types, Ref));
   function Known_Value_Type
     (Types : CCL.Types.Registry; Kind : Value_Kind; Ref : CCL.Types.Type_Reference) return Boolean is
     (case Kind is
        --  An Integer carrying a stream type is a session's stream handle.
        when Integer_Value => Ref = CCL.Types.Invalid_Type or else CCL.Types.Is_Stream (Types, Ref),
        when Boolean_Value => Ref = CCL.Types.Invalid_Type,
        when Variant_Value => CCL.Types.Is_Scalar_Sum (Types, Ref),
        when Object_Value => Native_Object_Type (Types, Ref),
        when Resource_Value => CCL.Types.Known (Types, Ref) and then
          CCL.Types.Describe (Types, Ref).Form = CCL.Types.Resource,
        --  A list crosses as an image (copied into the run's list region).
        when List_Value => CCL.Types.Is_List (Types, Ref) and then CCL.Objects.Persistable (Types, Ref),
        --  Text lives in the run's region, and neither text nor characters
        --  cross the host boundary yet: only compiler-created locals hold them.
        when Text_Value | Character_Value | Function_Value => False);
   --  How a value of type Ref lives in a run: one representation per type.
   --  A range subtype is an Integer; a record or payload variant is an
   --  Object_Value in the arena, whether built here or copied in.
   function Kind_For_Type
     (Types : CCL.Types.Registry; Ref : CCL.Types.Type_Reference) return Value_Kind is
     (if Ref = CCL.Types.Integer_Type then Integer_Value
      elsif Ref = CCL.Types.Boolean_Type then Boolean_Value
      elsif Ref = CCL.Types.String_Type then Text_Value
      elsif Ref = CCL.Types.Character_Type then Character_Value
      elsif CCL.Types.Describe (Types, Ref).Form = CCL.Types.Bounded then Integer_Value
      elsif CCL.Types.Describe (Types, Ref).Form = CCL.Types.Resource then Resource_Value
      --  A stream is its session handle: an Integer carrying its type.
      elsif CCL.Types.Is_Stream (Types, Ref) then Integer_Value
      elsif CCL.Types.Is_List (Types, Ref) then List_Value
      elsif CCL.Types.Is_Function (Types, Ref) then Function_Value
      elsif CCL.Types.Is_Scalar_Sum (Types, Ref) then Variant_Value else Object_Value);
   --  The data type a value of type Ref carries on the stack: none for the
   --  types its kind already says.
   function Reference_For_Type
     (Types : CCL.Types.Registry; Ref : CCL.Types.Type_Reference) return CCL.Types.Type_Reference is
     (if Kind_For_Type (Types, Ref) in Integer_Value | Boolean_Value | Text_Value | Character_Value and then
        not CCL.Types.Is_Stream (Types, Ref)
      then CCL.Types.Invalid_Type else Ref);
   --  What a view of a stream of type Stream_Type yields: its element type
   --  (latest), the list of it (window) or Integer (arrived, lost).
   --  Invalid_Type when Stream_Type is not a stream, or the list does not
   --  exist in Types.
   function Stream_View_Type
     (Types : CCL.Types.Registry; Stream_Type : CCL.Types.Type_Reference;
      View : CCL.Streams.View_Kind) return CCL.Types.Type_Reference is
     (if not CCL.Types.Is_Stream (Types, Stream_Type) then CCL.Types.Invalid_Type
      else
        (case View is
            when CCL.Streams.Latest_View => CCL.Types.Stream_Element (Types, Stream_Type),
            when CCL.Streams.Window_View =>
               CCL.Types.List_Of (Types, CCL.Types.Stream_Element (Types, Stream_Type)),
            when CCL.Streams.Arrived_View | CCL.Streams.Lost_View => CCL.Types.Integer_Type));

   --  A record or payload variant the arena holds.
   function Node_Type
     (Types : CCL.Types.Registry; Ref : CCL.Types.Type_Reference) return Boolean is
     (CCL.Objects.Storable (Types, Ref) and then
      CCL.Types.Describe (Types, Ref).Form in CCL.Types.Product | CCL.Types.Sum and then
      not CCL.Types.Is_Scalar_Sum (Types, Ref));
   --  A List<T> a run holds: any storable element except another list.
   function Supported_List
     (Types : CCL.Types.Registry; Ref : CCL.Types.Type_Reference) return Boolean is
     (CCL.Types.Is_List (Types, Ref) and then CCL.Objects.Storable (Types, Ref));
   function Element_Kind
     (Types : CCL.Types.Registry; Ref : CCL.Types.Type_Reference) return Value_Kind is
     (Kind_For_Type (Types, CCL.Types.Element_Of (Types, Ref)));
   function Element_Data_Type
     (Types : CCL.Types.Registry; Ref : CCL.Types.Type_Reference) return CCL.Types.Type_Reference is
     (Reference_For_Type (Types, CCL.Types.Element_Of (Types, Ref)));

   function Value_Image (Types : CCL.Types.Registry; Item : Value) return String;
   function Well_Typed (Types : CCL.Types.Registry; Item : Value) return Boolean is
     (if Item.Kind = Resource_Value then
         Known_Value_Type (Types, Item.Kind, Item.Data_Type) and then
         not Item.Copyable and then Item.Resource /= CCL.Resources.No_Reference and then
         Item.Node = 0
      elsif Item.Resource /= CCL.Resources.No_Reference then False
      elsif Item.Kind = Object_Value then
         Node_Type (Types, Item.Data_Type) and then
         (Item.Node /= 0 or else
          (CCL.Types.Describe (Types, Item.Data_Type).Form = CCL.Types.Sum and then
           Item.Alternative <= CCL.Types.Describe (Types, Item.Data_Type).Count and then
           CCL.Types.Describe (Types, Item.Data_Type).Parts (Item.Alternative).Payload =
             CCL.Types.Unit_Type))
      elsif Item.Node /= 0 then False
      elsif Item.Kind = List_Value then Supported_List (Types, Item.Data_Type)
      elsif Item.Kind = Function_Value then
         CCL.Types.Is_Function (Types, Item.Data_Type) and then Item.Integer in 0 .. MAX_FUNCTIONS - 1
      elsif Item.Kind = Character_Value then
         Item.Data_Type = CCL.Types.Invalid_Type and then Item.Integer in 0 .. MAX_CHARACTER_CODE
      elsif Item.Kind = Integer_Value and then Item.Data_Type /= CCL.Types.Invalid_Type then
         CCL.Types.Is_Stream (Types, Item.Data_Type) and then
         Item.Integer in 1 .. CCL.Streams.Maximum_Handle
      elsif Item.Kind /= Variant_Value then Item.Data_Type = CCL.Types.Invalid_Type
      else CCL.Types.Is_Scalar_Sum (Types, Item.Data_Type) and then
         Item.Alternative <= CCL.Types.Describe (Types, Item.Data_Type).Count);

   type Local_Value_Array is
     array (CCL.Ownership.Binding_Id) of Value;
   type Local_Kind_Array is
     array (CCL.Ownership.Binding_Id) of Value_Kind;
   type Local_Data_Type_Array is
     array (CCL.Ownership.Binding_Id) of CCL.Types.Type_Reference;

   type Op_Code is
     (Halt,
      Push_Integer,
      Push_Boolean,
      Add_Integer,
      Equal_Integer,
      Not_Boolean,
      Drop,
      Jump,
      Jump_If_False,
      Invoke_Import,
      Copy_Local,
      Move_Local,
      Drop_Local,
      Borrow_Local_RO,
      Return_Local_RO,
      Borrow_Local_RW,
      Return_Local_RW,
      Apply_Local_Disposition,
      Initialize_Local,
      Multiply_Integer,
      Divide_Integer,
      Modulo_Integer,
      Make_Variant,
      Equal_Variant,
      Switch_Variant,
      Copy_Stack,
      Drop_Under_Top,
      Project_Field,
      Subtract_Integer,
      Less_Integer,
      Less_Equal_Integer,
      Equal_Boolean,
      Call_Function,
      Return_Function,
      --  Text: Push_Text pushes constant Immediate; Concat_Text joins the
      --  two top texts (left below right); Length_Text and Equal_Text.
      Push_Text,
      Concat_Text,
      Length_Text,
      Equal_Text,
      --  A string built-in (CCL.Text_Operations): Immediate is the
      --  operation; its operands are below the subject on the stack.
      Text_Builtin,
      --  Characters and to-string. Text_At pops an index (1-based) and the
      --  text below it. Variant_To_Text names its enumeration in Data_Type.
      Equal_Character,
      Text_At,
      Integer_To_Text,
      Variant_To_Text,
      --  Lists; Data_Type names the List<T>. New_List reserves Immediate
      --  elements; Fill_List pops an element into position Immediate of the
      --  list below it, which stays; List_At pops an index and a list.
      New_List,
      Fill_List,
      Length_List,
      List_At,
      --  A list built-in (CCL.List_Operations): Immediate is the operation,
      --  Data_Type the subject's list type (the result's, for range and
      --  split); its operands are below the subject on the stack.
      List_Builtin,
      --  A record (Alternative 0) or payload-variant member built in the
      --  arena (step 4): pops the record's components, or the member's
      --  payload; a unit member pops nothing.
      Make_Node,
      --  An Integer entering a position of the range type in Data_Type
      --  (step 5): kept within its bounds, or Range_Error.
      Check_Range,
      --  Function values (step 6): Make_Closure makes a value of the function
      --  type in Data_Type for function Immediate, popping its captures;
      --  Call_Value pops the arguments and the function value beneath them,
      --  then calls it with the captures ahead of the arguments.
      Make_Closure,
      Call_Value,
      --  each, where, fold, any, all, count and sort-by
      --  (CCL.List_Operations.Apply_Operation in Immediate) over the list
      --  type in Data_Type. Resumable: each element is an ordinary call whose
      --  return comes back to this instruction, with the iteration's state in
      --  the machine (no loop in the code, no recursion in the VM).
      List_Apply,
      --  Streams (docs/ccl-streams.md). Push_Stream pushes the session's
      --  stream Immediate as a value of the stream type in Data_Type: an
      --  Integer handle that no arithmetic or comparison accepts.
      --  Stream_View (CCL.Streams.View_Kind in Immediate) pops the stream
      --  in Data_Type, and for a window the count below it, then suspends
      --  for the host's reader (Waiting_For_Host with Stream_Requested).
      Push_Stream,
      Stream_View);
   for Op_Code use
     (Halt                    => 0,
      Push_Integer            => 1,
      Push_Boolean            => 2,
      Add_Integer             => 3,
      Equal_Integer           => 4,
      Not_Boolean             => 5,
      Drop                    => 6,
      Jump                    => 7,
      Jump_If_False           => 8,
      Invoke_Import           => 9,
      Copy_Local              => 10,
      Move_Local              => 11,
      Drop_Local              => 12,
      Borrow_Local_RO         => 13,
      Return_Local_RO         => 14,
      Borrow_Local_RW         => 15,
      Return_Local_RW         => 16,
      Apply_Local_Disposition => 17,
      Initialize_Local        => 18,
      Multiply_Integer        => 19,
      Divide_Integer          => 20,
      Modulo_Integer          => 21,
      Make_Variant            => 22,
      Equal_Variant           => 23,
      Switch_Variant          => 24,
      Copy_Stack              => 25,
      Drop_Under_Top          => 26,
      Project_Field           => 27,
      Subtract_Integer        => 28,
      Less_Integer            => 29,
      Less_Equal_Integer      => 30,
      Equal_Boolean           => 31,
      Call_Function           => 32,
      Return_Function         => 33,
      Push_Text               => 34,
      Concat_Text             => 35,
      Length_Text             => 36,
      Equal_Text              => 37,
      Text_Builtin            => 38,
      Equal_Character         => 39,
      Text_At                 => 40,
      Integer_To_Text         => 41,
      Variant_To_Text         => 42,
      New_List                => 43,
      Fill_List               => 44,
      Length_List             => 45,
      List_At                 => 46,
      List_Builtin            => 47,
      Make_Node               => 48,
      Check_Range             => 49,
      Make_Closure            => 50,
      Call_Value              => 51,
      List_Apply              => 52,
      Push_Stream             => 53,
      Stream_View             => 54);
   for Op_Code'Size use 8;

   type Authority_Class is
     (No_Authority,
      Observe_Authority,
      Control_Authority,
      Secret_Use_Authority,
      Network_Authority);
   for Authority_Class use
     (No_Authority         => 0,
      Observe_Authority    => 1,
      Control_Authority    => 2,
      Secret_Use_Authority => 3,
      Network_Authority    => 4);
   for Authority_Class'Size use 8;

   type Import_Declaration is record
      Argument  : Value_Kind := Integer_Value;
      Result    : Value_Kind := Integer_Value;
      Argument_Data_Type, Result_Data_Type : CCL.Types.Type_Reference := CCL.Types.Invalid_Type;
      Result_Type_Tag : CCL.Ownership.Type_Id := 0;
      Receiver_Data_Type : CCL.Types.Type_Reference := CCL.Types.Invalid_Type;
      -- When present, Local is an owned resource receiver. Argument is a
      -- separate, unrestricted data operand, never part of the authority.
      -- A resource result is moved onto the operand stack under this declared
      -- ownership type. Data results retain the canonical unrestricted tag 0.
      Authority : Authority_Class := No_Authority;
      Binding   : Unsigned_32 := 0;
      Ownership_Argument : Boolean := False;
      Local       : CCL.Ownership.Binding_Id := 0;
      Transfer    : CCL.Imports.Transfer_Mode := CCL.Imports.Copy_Argument;
      Cancellation : CCL.Imports.Cancellation_Mode :=
        CCL.Imports.Not_Cancellable;
      Success_Verb : CCL.Ownership.Disposition_Id := 0;
      Failure_Verb : CCL.Ownership.Disposition_Id := 0;
      Cancel_Verb  : CCL.Ownership.Disposition_Id := 0;
   end record;

   -- Scalar-only operations need no nominal type references. Schema-bearing
   -- imports require separate pinned linkage; this predicate grants nothing.
   function Scalar_Import (Item : Import_Declaration) return Boolean is
     (Item.Argument in Scalar_Kind and Item.Result in Scalar_Kind and
      Item.Argument_Data_Type = CCL.Types.Invalid_Type and
      Item.Result_Data_Type = CCL.Types.Invalid_Type and Item.Result_Type_Tag = 0 and
      Item.Receiver_Data_Type = CCL.Types.Invalid_Type);

   type Import_Array is array (Import_Index) of Import_Declaration;

   function Has_Receiver (Item : Import_Declaration) return Boolean is
     (Item.Receiver_Data_Type /= CCL.Types.Invalid_Type);
   function Local_Argument_Kind (Item : Import_Declaration) return Value_Kind is
     (if Has_Receiver (Item) then Resource_Value else Item.Argument);
   function Local_Argument_Type (Item : Import_Declaration) return CCL.Types.Type_Reference is
     (if Has_Receiver (Item) then Item.Receiver_Data_Type else Item.Argument_Data_Type);

   type Instruction is record
      Op        : Op_Code := Halt;
      Immediate : Integer_64 := 0;
      Target    : Instruction_Index := 0;
      Import    : Import_Index := 0;
      Local     : CCL.Ownership.Binding_Id := 0;
      Verb      : CCL.Ownership.Disposition_Id := 0;
      Data_Type : CCL.Types.Type_Reference := CCL.Types.Invalid_Type;
      Alternative : CCL.Types.Component_Count := 0;
   end record;

   type Instruction_Array is array (Instruction_Index) of Instruction;

   Maximum_Matches : constant := 16;
   subtype Match_Count is Natural range 0 .. Maximum_Matches;
   subtype Match_Index is Natural range 0 .. Maximum_Matches - 1;
   type Alternative_Targets is array (CCL.Types.Component_Index) of Instruction_Index;
   type Match_Table is record
      Data_Type : CCL.Types.Type_Reference := CCL.Types.Invalid_Type;
      Targets : Alternative_Targets := [others => 0];
   end record;
   type Match_Tables is array (Match_Index) of Match_Table;

   --  Functions (docs/ccl-bytecode-format.md, "Functions"): code after the
   --  main body, one contiguous region each, in declaration order. A function
   --  calls by name only functions declared before it; a call through a
   --  function value is bounded by the verifier's stack analysis and the
   --  frame table, so every program still terminates; at most MAX_FUNCTIONS
   --  frames are live. Parameters and results are any value a run holds. An
   --  anonymous function's captured values are its leading parameters: a
   --  language function's 8 parameters plus 4 captures.
   MAX_PARAMETERS : constant := 12;
   subtype Function_Count is Natural range 0 .. MAX_FUNCTIONS;
   subtype Function_Index is Natural range 0 .. MAX_FUNCTIONS - 1;
   subtype Parameter_Count is Natural range 0 .. MAX_PARAMETERS;
   subtype Parameter_Index is Positive range 1 .. MAX_PARAMETERS;
   type Parameter_Kinds is array (Parameter_Index) of Value_Kind;
   type Parameter_Data_Types is array (Parameter_Index) of CCL.Types.Type_Reference;
   type Function_Declaration is record
      Entry_PC : Instruction_Index := 0;
      --  Parameters, the first Captures of them bound by its function value.
      Count : Parameter_Count := 0;
      Captures : Parameter_Count := 0;
      Kinds : Parameter_Kinds := [others => Integer_Value];
      Data_Types : Parameter_Data_Types := [others => CCL.Types.Invalid_Type];
      Result : Value_Kind := Integer_Value;
      Result_Data_Type : CCL.Types.Type_Reference := CCL.Types.Invalid_Type;
   end record;
   type Function_Array is array (Function_Index) of Function_Declaration;

   type Program is record
      Length : Program_Length := 0;
      Code   : Instruction_Array := [others => (others => <>)];
      Imports_Length : Import_Count := 0;
      Imports : Import_Array := [others => (others => <>)];
      Locals_Length : Local_Count := 0;
      Dynamic_Locals_Length : Local_Count := 0;
      Types_Length : Type_Count := 0;
      Local_Types : Local_Type_Array := [others => 0];
      Local_Kinds : Local_Kind_Array := [others => Integer_Value];
      Types : CCL.Ownership.Type_Table := [others => (others => <>)];
      Data_Types : CCL.Types.Registry;
      Local_Data_Types : Local_Data_Type_Array := [others => CCL.Types.Invalid_Type];
      Matches_Length : Match_Count := 0;
      Matches : Match_Tables := [others => (others => <>)];
      Functions_Length : Function_Count := 0;
      Functions : Function_Array := [others => (others => <>)];
      --  Text literals: Constants (I) is Constant_Text (First .. First + Length - 1).
      Constants_Length : Constant_Count := 0;
      Constants : Text_Constants := [others => (others => <>)];
      Constant_Text : String (1 .. MAX_CONSTANT_BYTES) := [others => ' '];
   end record;

   type Validation_Error is
     (Valid,
      Empty_Program,
      Unreachable_Instruction,
      Missing_Halt,
      Invalid_Jump_Target,
      Backward_Jump,
      Stack_Underflow,
      Stack_Overflow,
      Type_Mismatch,
      Inconsistent_Stack,
      Invalid_Import,
      Invalid_Data_Type,
      Invalid_Match,
      Invalid_Ownership,
      Invalid_Function,
      --  A text constant outside the pool, or a pool entry outside its text.
      Invalid_Constant,
      --  A Text_Builtin or List_Builtin naming no operation, or a
      --  List_Builtin on a list it does not apply to.
      Invalid_Builtin);

   type Validated_Program is private;

   procedure Verify
     (Candidate : Program;
      Result    : out Validated_Program;
      Error     : out Validation_Error)
   with
      Post =>
        (if Error = Valid then Is_Valid (Result));

   function Is_Valid (Item : Validated_Program) return Boolean;

   type Execution_Status is
     (Completed,
      Paused,
      Stopped,
      Fuel_Exhausted,
      Arithmetic_Overflow,
      Division_By_Zero,
      --  The run's value arena is full.
      Object_Storage_Exhausted,
      --  The run's text region is full.
      Text_Storage_Exhausted,
      --  parse-int on text that is not a decimal integer.
      Invalid_Number,
      --  at outside 1 .. the text's or list's length.
      Index_Out_Of_Range,
      --  The run's list region is full.
      List_Storage_Exhausted,
      --  A value outside the range type of the position it enters.
      Range_Error,
      --  Calls through function values nested deeper than the frame table.
      Call_Depth_Exhausted,
      --  A stream view the host could not answer (CCL.Streams.View_Status),
      --  a window outside 1 .. Maximum_Window, or elements not of the type
      --  the program names.
      Stream_Unavailable,
      Stream_Empty,
      Stream_Window_Out_Of_Range,
      Stream_Element_Mismatch,
      Invalid_Bytecode,
      Waiting_For_Host,
      Host_Call_Failed,
      No_Result);

   type Execution_Result is record
      Status         : Execution_Status := No_Result;
      Has_Value      : Boolean := False;
      Result_Value   : Value := (others => <>);
      Fuel_Remaining : Unsigned_32 := 0;
      Steps          : Unsigned_32 := 0;
      Requested_Import : Import_Index := 0;
      Request_Argument : Value := (others => <>);
      Request_Receiver : CCL.Resources.Reference := CCL.Resources.No_Reference;
      Request_Owned : Boolean := False;
      Requested_Authority : Authority_Class := No_Authority;
      Requested_Binding   : Unsigned_32 := 0;
      --  A text result's characters (Result_Value.Kind = Text_Value), when
      --  they fit; a longer text still completes, and a host exports it.
      Has_Result_Text : Boolean := False;
      Result_Text_Value : Result_Text := (others => <>);
      --  A list result (Result_Value.Kind = List_Value): List_Length of its
      --  List_Total elements are carried out.
      List_Length : List_Result_Count := 0;
      List_Total : Natural range 0 .. MAX_LIST_ELEMENTS := 0;
      List_Values : List_Result_Values := [others => (others => <>)];
      List_Text : Result_Text := (others => <>);
      List_Text_Ends : List_Result_Ends := [others => 0];
      --  A record or payload variant, or a list of them: its canonical CCL
      --  literal, as the interpreter prints it, when it has one that fits.
      Has_Literal : Boolean := False;
      Literal : Result_Text := (others => <>);
      --  Its rows' record type and fields, for a table (Count = 0: none).
      Literal_Shape : CCL.Types.Shapes.Row_Shape := (others => <>);
      --  Waiting_For_Host on a stream view, not an import: the host answers
      --  Stream_Request through Native_Objects.Complete_Stream_View.
      Stream_Requested : Boolean := False;
      Stream_Request : CCL.Streams.View_Request := (others => <>);
   end record;

   type Machine_State is private;

   function Is_Well_Formed
     (Item : Validated_Program; State : Machine_State) return Boolean;

   function Fuel_Limit (State : Machine_State) return Unsigned_32;

   procedure Initialize
     (Item  : Validated_Program;
      Fuel  : Natural;
      State : out Machine_State)
   with
     Pre => Is_Valid (Item),
     Post => Is_Well_Formed (Item, State) and then
       Fuel_Limit (State) = Unsigned_32 (Fuel);

   procedure Initialize_With_Locals
     (Item     : Validated_Program;
      Fuel     : Natural;
      Values   : Local_Value_Array;
      Count    : Local_Count;
      State    : out Machine_State;
      Accepted : out Boolean)
   with
     Pre => Is_Valid (Item),
     Post =>
       (if Accepted then
          Is_Well_Formed (Item, State) and then
          Fuel_Limit (State) = Unsigned_32 (Fuel));

   procedure Continue_Execution
     (Item   : Validated_Program;
      State  : in out Machine_State;
      Result : out Execution_Result)
   with
     Pre => Is_Valid (Item) and then Is_Well_Formed (Item, State),
     Post => Is_Well_Formed (Item, State) and then
       Fuel_Limit (State) = Fuel_Limit (State'Old) and then
       Result.Steps <= Fuel_Limit (State);

   procedure Continue_Execution_For
     (Item         : Validated_Program;
      State        : in out Machine_State;
      Instructions : Natural;
      Result       : out Execution_Result)
   with
     Pre => Is_Valid (Item) and then Is_Well_Formed (Item, State),
     Post => Is_Well_Formed (Item, State) and then
       Fuel_Limit (State) = Fuel_Limit (State'Old) and then
       Result.Steps <= Fuel_Limit (State);

   type Machine_Snapshot is record
      Instruction : Instruction_Index := 0;
      Fuel_Remaining : Unsigned_32 := 0;
      Steps : Unsigned_32 := 0;
      Waiting : Boolean := False;
      Terminal : Boolean := False;
      Status : Execution_Status := No_Result;
   end record;

   function Snapshot (State : Machine_State) return Machine_Snapshot;

   type Stack_Snapshot_Array is array (Stack_Index) of Value;

   type Local_Inspection is record
      Value           : CCL.VM.Value := (others => <>);
      Kind            : Value_Kind := Integer_Value;
      Type_Tag        : CCL.Ownership.Type_Id := 0;
      Mode            : CCL.Ownership.Ownership_Mode :=
        CCL.Ownership.Unrestricted;
      Ownership_State : CCL.Ownership.Binding_State :=
        CCL.Ownership.Not_Declared;
      Read_Borrows    : CCL.Ownership.Borrow_Count := 0;
      Write_Borrow    : Boolean := False;
   end record;

   type Local_Inspection_Array is
     array (CCL.Ownership.Binding_Id) of Local_Inspection;

   type Inspection_Snapshot is record
      Machine       : Machine_Snapshot;
      Stack_Length  : Stack_Depth := 0;
      --  Position zero is the current operand-stack top.
      Stack         : Stack_Snapshot_Array := [others => (others => <>)];
      Locals_Length : Local_Count := 0;
      Locals        : Local_Inspection_Array := [others => (others => <>)];
      Waiting_Import : Import_Index := 0;
      Waiting_Result_Kind : Value_Kind := Integer_Value;
      Waiting_Argument : Value := (others => <>);
      Waiting_Receiver : CCL.Resources.Reference := CCL.Resources.No_Reference;
      Waiting_Owned : Boolean := False;
      Import_Phase : CCL.Imports.Import_Phase := CCL.Imports.Import_Idle;
   end record;

   procedure Inspect
     (Item   : Validated_Program;
      State  : Machine_State;
      Result : out Inspection_Snapshot)
   with
      Pre => Is_Valid (Item) and then Is_Well_Formed (Item, State);

   procedure Stop (State : in out Machine_State);

   procedure Complete_Host_Call
     (Item     : Validated_Program;
      State    : in out Machine_State;
      Response : Value;
      Accepted : Boolean)
   with
     Pre => Is_Valid (Item) and then Is_Well_Formed (Item, State),
     Post => Is_Well_Formed (Item, State);

   --  Must be called after the host attempts to enqueue an owned import.
   --  Rejection preserves ownership; acceptance activates move/borrow state.
   procedure Acknowledge_Host_Submission
     (Item     : Validated_Program;
      State    : in out Machine_State;
      Accepted : Boolean)
   with
     Pre => Is_Valid (Item) and then Is_Well_Formed (Item, State),
     Post => Is_Well_Formed (Item, State);

   procedure Execute
     (Item   : Validated_Program;
      Fuel   : Natural;
      Result : out Execution_Result)
   with
      Pre => Is_Valid (Item),
      Post => Result.Steps <= Unsigned_32 (Fuel);

private
   --  The value arena. A node's components are Count slots from First; a
   --  slot holds one component as a cell, read back by the component's
   --  static type. Every node a slot refers to is older than the slot's own
   --  node (Allocate_Node checks it), so the arena is acyclic.
   type Slot is record
      Element : List_Element;
      Items : List_Regions.Array_Value;
   end record;
   type Arena_Node is record
      Data_Type : CCL.Types.Type_Reference := CCL.Types.Invalid_Type;
      Alternative : CCL.Types.Component_Count := 0;
      First : Positive range 1 .. MAX_VALUE_SLOTS + 1 := 1;
      Count : CCL.Types.Component_Count := 0;
   end record;
   type Node_Array is array (Node_Index) of Arena_Node;
   type Slot_Array is array (Slot_Index) of Slot;
   type Value_Arena is record
      Nodes : Node_Array := [others => (others => <>)];
      Slots : Slot_Array := [others => (others => <>)];
      Nodes_Used : Node_Count := 0;
      Slots_Used : Slot_Count := 0;
   end record;
   type Component_Values is array (CCL.Types.Component_Index) of Value;

   --  A List_Apply in progress: at most one per live frame level (the main
   --  body and each call), so the table never needs more entries.
   MAX_ITERATIONS : constant := MAX_FUNCTIONS + 1;
   type Iteration is record
      Active : Boolean := False;
      --  Waiting for the function's result for element Position.
      Awaiting : Boolean := False;
      At_PC : Instruction_Index := 0;
      Frame_Level : Natural range 0 .. MAX_FUNCTIONS := 0;
      Operation : CCL.List_Operations.Apply_Operation := CCL.List_Operations.Each_Items;
      List_Type : CCL.Types.Type_Reference := CCL.Types.Invalid_Type;
      Subject : Value := (others => <>);
      Callee : Value := (others => <>);
      --  fold's accumulator; count's count; any/all's answer so far.
      Accumulator : Value := (others => <>);
      Position : Natural range 0 .. MAX_LIST_ELEMENTS := 0;
      --  each/where/sort-by build into Built (Kept elements); sort-by's keys.
      Built : Value := (others => <>);
      Kept : Natural range 0 .. MAX_LIST_ELEMENTS := 0;
      Keys : List_Regions.Array_Value;
   end record;
   subtype Iteration_Count is Natural range 0 .. MAX_ITERATIONS;
   type Iteration_Array is array (1 .. MAX_ITERATIONS) of Iteration;

   --  A value of type Ref (Kind_For_Type) as a cell, and back. From_Slot
   --  checks what a cell cannot carry by itself: a character's range, a
   --  member within its type, and a node of the right type in the arena.
   function To_Slot (Item : Value) return Slot;
   procedure From_Slot
     (Arena : Value_Arena; Types : CCL.Types.Registry; Ref : CCL.Types.Type_Reference;
      Item : Slot; Result : out Value; Good : out Boolean);
   --  A node of a record (Alternative 0) or payload-variant member from
   --  Count components; Good is False when the arena is full or a component
   --  does not fit its part. A unit member needs no node.
   procedure Allocate_Node
     (Arena : in out Value_Arena; Types : CCL.Types.Registry;
      Data_Type : CCL.Types.Type_Reference; Alternative : CCL.Types.Component_Count;
      Components : Component_Values; Count : CCL.Types.Component_Count;
      Result : out Value; Good : out Boolean);
   --  A list element and the value it stands for in a list of type
   --  List_Type (From_Element checks it as From_Slot does).
   function To_Element (Item : Value) return List_Element;
   procedure From_Element
     (Arena : Value_Arena; Types : CCL.Types.Registry; List_Type : CCL.Types.Type_Reference;
      Element : List_Element; Item : out Value; Good : out Boolean);
   --  Component P of a record (a field) or payload variant (P = 1, payload).
   procedure Component
     (Arena : Value_Arena; Types : CCL.Types.Registry; Owner : Value;
      P : CCL.Types.Component_Index; Result : out Value; Good : out Boolean);
   -- Only the native-object child admits object references, after copying and
   -- validating their owned storage. Public scalar completion cannot mint one.
   --  Answer a stream view: push Response (already of Stream_Result_Type)
   --  and continue, or stop the run with Failure.
   procedure Complete_Stream_Call
     (Item : Validated_Program; State : in out Machine_State;
      Response : Value; Failure : Execution_Status)
     with Pre => Is_Valid (Item) and then Is_Well_Formed (Item, State),
       Post => Is_Well_Formed (Item, State);
   procedure Complete_Checked_Host_Call
     (Item : Validated_Program; State : in out Machine_State;
      Host_Response : Value; Accepted : Boolean; Native_Response : Boolean;
      Resource_Response : Boolean := False)
     with Pre => Is_Valid (Item) and then Is_Well_Formed (Item, State),
       Post => Is_Well_Formed (Item, State);
   type Runtime_Stack_Index is mod MAX_STACK_DEPTH;
   package Runtime_Stacks is new CCL.Bounded_Stacks
     (Index_Type    => Runtime_Stack_Index,
      Element_Type  => Value,
      Default_Value => (others => <>));

   type Validated_Program is record
      Checked : Boolean := False;
      Content : Program;
   end record;

   --  A live call: where to continue, and whose frame it is.
   type Call_Frame is record
      Return_PC : Instruction_Index := 0;
      Callee    : Function_Index := 0;
   end record;
   type Call_Frames is array (Function_Index) of Call_Frame;

   type Machine_State is record
      Stack               : Runtime_Stacks.Stack;
      PC                  : Instruction_Index := 0;
      Execution_Budget    : CCL.Execution_Budgets.Budget;
      Waiting             : Boolean := False;
      Waiting_Import      : Import_Index := 0;
      Waiting_Result_Kind : Value_Kind := Integer_Value;
      Waiting_Argument    : Value := (others => <>);
      Waiting_Receiver    : CCL.Resources.Reference := CCL.Resources.No_Reference;
      Waiting_Owned       : Boolean := False;
      Import_Lifecycle    : CCL.Imports.Lifecycle;
      Terminal            : Boolean := False;
      Terminal_Status     : Execution_Status := No_Result;
      Has_Value           : Boolean := False;
      Result_Value        : Value := (others => <>);
      Ownership           : CCL.Ownership.Environment;
      Locals              : Local_Value_Array := [others => (others => <>)];
      Frames              : Call_Frames := [others => (others => <>)];
      Frame_Count         : Function_Count := 0;
      Text                : Text_Regions.Stack;
      Lists               : List_Regions.Stack;
      Arena               : Value_Arena;
      Iterations          : Iteration_Array := [others => (others => <>)];
      Iteration_Depth     : Iteration_Count := 0;
      --  Suspended on a stream view (separate from import waits): the
      --  request, and the type its answer must have (T, List<T>, Integer).
      Waiting_Stream      : Boolean := False;
      Stream_Request      : CCL.Streams.View_Request := (others => <>);
      Stream_Result_Type  : CCL.Types.Type_Reference := CCL.Types.Invalid_Type;
   end record;

   function Is_Valid (Item : Validated_Program) return Boolean is
     (Item.Checked and then Item.Content.Length > 0 and then
      Item.Content.Dynamic_Locals_Length <= Item.Content.Locals_Length);

   function Fuel_Limit (State : Machine_State) return Unsigned_32 is
     (CCL.Execution_Budgets.Limit (State.Execution_Budget));

   function Is_Well_Formed
     (Item : Validated_Program; State : Machine_State) return Boolean is
     ((not State.Waiting or else
       Natural (State.Waiting_Import) < Item.Content.Imports_Length) and then
      (not State.Waiting_Owned or else
       (State.Waiting and then
        CCL.Imports.Phase (State.Import_Lifecycle) in
          CCL.Imports.Import_Offered | CCL.Imports.Import_Accepted)) and then
      (State.Waiting_Owned or else
       CCL.Imports.Phase (State.Import_Lifecycle) not in
         CCL.Imports.Import_Offered | CCL.Imports.Import_Accepted));
end CCL.VM;

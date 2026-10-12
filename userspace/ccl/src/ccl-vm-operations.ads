with CCL.Text_Operations;

--  Instruction operands that the verifier and the executor decode the same
--  way: the built-in or view an Immediate names, and whether a list
--  operation applies to a list type (as the type checker decides).
private package CCL.VM.Operations with SPARK_Mode => On is
   package T renames CCL.Text_Operations;
   package L renames CCL.List_Operations;
   use type T.Operand_Kind;
   use type L.Operation;
   use type L.Apply_Operation;
   use type Text_Regions.Operation_Result;

   procedure Find_Operation (Immediate : Integer_64; Item : out T.Operation; Found : out Boolean)
     with Global => null;
   procedure Find_List_Operation (Immediate : Integer_64; Item : out L.Operation; Found : out Boolean)
     with Global => null;
   procedure Find_View (Immediate : Integer_64; Item : out CCL.Streams.View_Kind; Found : out Boolean)
     with Global => null;
   procedure Find_Apply_Operation (Immediate : Integer_64; Item : out L.Apply_Operation; Found : out Boolean)
     with Global => null;

   --  The registry's List<Element>, as the type checker specialized it (each
   --  builds a list of its function's result type, a range type's values as
   --  Integers, as the interpreter does), or Invalid_Type.
   function List_Of
     (Types : CCL.Types.Registry; Result : CCL.Types.Type_Reference) return CCL.Types.Type_Reference
     with Global => null;

   --  Whether List_Apply's Operation applies a function of type Fn to the
   --  list type List_Type: its parameters (fold: accumulator then element)
   --  and result, as the type checker decides.
   function Apply_Fits
     (Types : CCL.Types.Registry; Operation : L.Apply_Operation;
      List_Type, Fn : CCL.Types.Type_Reference) return Boolean is
     (Supported_List (Types, List_Type) and then CCL.Types.Is_Function (Types, Fn) and then
      CCL.Types.Describe (Types, Fn).Count = (if Operation = L.Fold_Items then 3 else 2) and then
      (CCL.Types.Describe (Types, Fn).Parts
         ((if Operation = L.Fold_Items then 2 else 1)).Payload = CCL.Types.Element_Of (Types, List_Type) or else
       (CCL.Types.Element_Of (Types, List_Type) = CCL.Types.Integer_Type and then
        CCL.Types.Is_Range (Types, CCL.Types.Describe (Types, Fn).Parts
          ((if Operation = L.Fold_Items then 2 else 1)).Payload))) and then
      (case Operation is
          when L.Each_Items =>
             Supported_List (Types, List_Of (Types, CCL.Types.Describe (Types, Fn).Parts (2).Payload)),
          when L.Where_Items | L.Any_Items | L.All_Items | L.Count_Items =>
             CCL.Types.Describe (Types, Fn).Parts (2).Payload = CCL.Types.Boolean_Type,
          when L.Fold_Items =>
             CCL.Types.Describe (Types, Fn).Parts (3).Payload = CCL.Types.Describe (Types, Fn).Parts (1).Payload,
          when L.Sort_By_Items =>
             CCL.Types.Describe (Types, Fn).Parts (2).Payload in CCL.Types.Integer_Type | CCL.Types.String_Type));

   --  Whether a list built-in applies to List_Type (the subject's list type,
   --  or the result's for range and split), as the type checker decides.
   function List_Builtin_Applies
     (Types : CCL.Types.Registry; Item : L.Operation; List_Type : CCL.Types.Type_Reference)
      return Boolean is
     (Supported_List (Types, List_Type) and then
      (case Item is
          when L.Sort_Items | L.Contains_Item =>
             CCL.Types.Element_Of (Types, List_Type) in CCL.Types.Integer_Type | CCL.Types.String_Type |
               CCL.Types.Character_Type or else
             (Item = L.Contains_Item and then CCL.Types.Element_Of (Types, List_Type) = CCL.Types.Boolean_Type) or else
             CCL.Types.Is_Enumeration (Types, CCL.Types.Element_Of (Types, List_Type)),
          when L.Sum_Items | L.Min_Items | L.Max_Items | L.Range_Items =>
             Element_Kind (Types, List_Type) = Integer_Value,
          when L.Join_Items | L.Split_Text => Element_Kind (Types, List_Type) = Text_Value,
          when others => True));

   --  The operand kind of a comparison opcode.
   function Comparison_Kind (Op : Op_Code) return Value_Kind is
     (case Op is
         when Equal_Boolean => Boolean_Value,
         when Equal_Character => Character_Value,
         when others => Integer_Value);

   function Kind_Of (Kind : T.Operand_Kind) return Value_Kind is
     (case Kind is
         when T.Text_Operand => Text_Value,
         when T.Integer_Operand => Integer_Value,
         when T.Boolean_Operand => Boolean_Value);

   --  An instruction's Immediate as an index into the table it names, once
   --  the caller has checked it is in range: each operand conversion is here.
   function Function_Operand (Immediate : Integer_64) return Function_Index is
     (Function_Index (Immediate))
     with Pre => Immediate in 0 .. MAX_FUNCTIONS - 1;
   function Constant_Operand (Immediate : Integer_64) return Constant_Index is
     (Constant_Index (Immediate))
     with Pre => Immediate in 0 .. MAX_CONSTANTS - 1;
   function Match_Operand (Immediate : Integer_64) return Match_Index is
     (Match_Index (Immediate))
     with Pre => Immediate in 0 .. Maximum_Matches - 1;
   function Component_Operand (Immediate : Integer_64) return CCL.Types.Component_Index is
     (CCL.Types.Component_Index (Immediate))
     with Pre => Immediate in 1 .. CCL.Types.Maximum_Components;

   --  A text region failure as a run's status: a full region is exhaustion,
   --  anything else an invalid descriptor.
   function Text_Failure (Status : Text_Regions.Operation_Result) return Execution_Status is
     (if Status in Text_Regions.Storage_Full | Text_Regions.Value_Table_Full
      then Text_Storage_Exhausted else Invalid_Bytecode);
end CCL.VM.Operations;

with CCL.Bounded_Stacks;
with CCL.Ownership.Bytecode;

--  The verifier's abstract machine: at every instruction, the kinds and
--  types of the values on the operand stack (never their contents), merged
--  over all paths that reach it. The verifier (CCL.VM.Verify) sweeps the
--  code once, forward; each opcode family's transfer function is a
--  separate procedure (CCL.VM.Verifier_Values, Verifier_Texts,
--  Verifier_Flow) with its own contract, so no proof context holds the
--  whole sweep (CODE-002).
private package CCL.VM.Verifier_State with SPARK_Mode => On is
   type Stack_Type is record
      Kind : Value_Kind := Integer_Value;
      Data_Type : CCL.Types.Type_Reference := CCL.Types.Invalid_Type;
      Copyable : Boolean := True;
      Type_Tag : CCL.Ownership.Type_Id := 0;
   end record;

   type Abstract_Stack_Index is mod MAX_STACK_DEPTH;
   package Abstract_Stacks is new CCL.Bounded_Stacks
     (Index_Type    => Abstract_Stack_Index,
      Element_Type  => Stack_Type,
      Default_Value => (others => <>));
   use type Abstract_Stacks.Stack;

   type Abstract_State is record
      Seen  : Boolean := False;
      Values : Abstract_Stacks.Stack;
   end record;

   type State_Table is array (Instruction_Index) of Abstract_State;
   --  Each Switch_Variant's targets, for the ownership pass.
   type Switch_Table is array (Instruction_Index) of CCL.Ownership.Bytecode.Instruction;

   --  Functions: the region of a PC is the function whose code holds it,
   --  or the main body (No_Region), which comes first.
   No_Region : constant := MAX_FUNCTIONS;
   subtype Region_Index is Function_Count range 0 .. No_Region;

   --  Stack demand: a depth, or one past the machine's stack (overflow).
   type Demand is range 0 .. MAX_STACK_DEPTH + 1;
   type Region_Demands is array (Region_Index) of Demand;
   type Call_Demands is array (Region_Index, Function_Index) of Demand;
   type Call_Presence is array (Region_Index, Function_Index) of Boolean;

   --  The parameters a call takes, as a stack demand.
   function Demand_Of (Count : Parameter_Count) return Demand is (Demand (Count));

   function Depth_Of (Item : Abstract_State) return Demand is
     (Demand (Unsigned_32'Min (Abstract_Stacks.Depth (Item.Values), MAX_STACK_DEPTH)));

   --  The steps below do nothing once Error is set: the sweep stops at the
   --  first error, which they never overwrite.
   procedure Merge_State
     (States : in out State_Table;
      Target : Instruction_Index;
      Source : Abstract_State;
      Error  : in out Validation_Error)
     with Global => null,
          Post => (if Error'Old /= Valid then Error = Error'Old);

   procedure Push_Kind
     (State : in out Abstract_State;
      Kind  : Value_Kind;
      Error : in out Validation_Error;
      Data_Type : CCL.Types.Type_Reference := CCL.Types.Invalid_Type;
      Copyable : Boolean := True;
      Type_Tag : CCL.Ownership.Type_Id := 0)
     with Global => null,
          Post => (if Error'Old /= Valid then Error = Error'Old);

   procedure Pop_Kind
     (State    : in out Abstract_State;
      Expected : Value_Kind;
      Error    : in out Validation_Error;
      Data_Type : CCL.Types.Type_Reference := CCL.Types.Invalid_Type)
     with Global => null,
          Post => (if Error'Old /= Valid then Error = Error'Old);

   --  The function whose code holds PC, or No_Region.
   function Region_Of (Candidate : Program; PC : Instruction_Index) return Region_Index
     with Global => null,
          Post => Region_Of'Result = No_Region or else Region_Of'Result < Candidate.Functions_Length;

   --  Where region R's code ends.
   function Region_End (Candidate : Program; R : Region_Index) return Program_Length is
     (if Candidate.Functions_Length = 0 then Candidate.Length
      elsif R = No_Region then Program_Length (Candidate.Functions (0).Entry_PC)
      elsif R + 1 < Candidate.Functions_Length then
         Program_Length (Candidate.Functions (R + 1).Entry_PC)
      else Candidate.Length);

   --  A parameter or result: any value a run holds (not a resource).
   function Function_Data
     (Types : CCL.Types.Registry; Kind : Value_Kind; Ref : CCL.Types.Type_Reference) return Boolean is
     (case Kind is
         when Integer_Value =>
            Ref = CCL.Types.Invalid_Type or else CCL.Types.Is_Handle (Types, Ref),
         when Boolean_Value | Text_Value | Character_Value => Ref = CCL.Types.Invalid_Type,
         when Variant_Value => CCL.Types.Is_Scalar_Sum (Types, Ref),
         when List_Value => Supported_List (Types, Ref),
         when Object_Value => Node_Type (Types, Ref),
         when Function_Value => CCL.Types.Is_Function (Types, Ref),
         when Resource_Value => False);

   --  Whether function F is a value of the function type T: its parameters
   --  after the captures, and its result, are T's.
   function Matches_Signature
     (Candidate : Program; F : Function_Index; T : CCL.Types.Type_Reference) return Boolean
     with Global => null;
end CCL.VM.Verifier_State;

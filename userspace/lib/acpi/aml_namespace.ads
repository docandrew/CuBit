pragma Ada_2022;
with AML_Clock;
with AML_Names;
with AML_Decode;
with AML_Execute;
with AML_Table_Backing;
with AML_Objects;
with AML_Field_Data;
--  Stable namespace IDs; temporary method declarations are reclaimed at last exit. IDs are scoped to one State lifetime;
--  they are not capabilities or externally reusable service handles.
generic
   Capacity : Positive;
   with procedure Read_Microseconds
     (Value : out AML_Decode.Integer_Value; Available : out Boolean) is AML_Clock.No_Sample;
package AML_Namespace with SPARK_Mode is
   use type AML_Decode.Integer_Value;
   use type AML_Names.Parse_Status;
   use type AML_Objects.Object_Kind;
   use type AML_Objects.Usage;
   subtype Node_ID is Natural range 0 .. Capacity;
   Root : constant Node_ID := 0;
   type State is private;
   -- High-water slot count. During execution, removed temporary slots below a
   -- live slot remain reserved; Present/Child distinguish these from live nodes.
   function Count (Tree : State) return Node_ID;
   function Present (Tree : State; Node : Node_ID) return Boolean
     with Pre => Node <= Count (Tree);
   function Value_Usage (Tree : State) return AML_Objects.Usage;
   function Method_Usage (Tree : State) return AML_Execute.Method_Length;
   function Parent (Tree : State; Node : Node_ID) return Node_ID
     with Pre => Node <= Count (Tree),
          Post => (if Node = Root then Parent'Result = Root
                   else Parent'Result < Node);
   function Name (Tree : State; Node : Node_ID) return AML_Names.Segment
     with Pre => Node > Root and then Node <= Count (Tree);
   function Empty return State with Post => Count (Empty'Result) = 0
     and then Method_Usage (Empty'Result) = 0
     and then Value_Usage (Empty'Result) = AML_Objects.Usage'(others => 0);

   function Child
     (Tree : State; Scope : Node_ID; Part : AML_Names.Segment) return Node_ID
     with Pre => Scope <= Count (Tree),
          Post => Child'Result <= Count (Tree) and then
            (if Child'Result /= Root then
               Present (Tree, Child'Result) and then
               Parent (Tree, Child'Result) = Scope and then
               Name (Tree, Child'Result) = Part
             else (for all I in 1 .. Count (Tree) =>
                     not Present (Tree, I) or else Parent (Tree, I) /= Scope or else Name (Tree, I) /= Part));

   type Lookup_Status is (Found, Not_Found, Above_Root, Invalid_Path);
   type Lookup_Result (Status : Lookup_Status := Not_Found) is record
      case Status is
         when Found => Node : Node_ID;
         when others => null;
      end case;
   end record;
   --  Existing-object lookup, not declaration placement. Only an unprefixed
   --  single segment uses ancestor search. Null paths identify the base scope;
   --  opcode-specific NullName/target semantics remain the caller's job.
   function Resolve
     (Tree : State; Scope : Node_ID; Path : AML_Names.Name_Result)
      return Lookup_Result
     with Pre => Scope <= Count (Tree),
          Post => (if Resolve'Result.Status = Found then
                     Resolve'Result.Node <= Count (Tree) and then
                     Path.Kind = AML_Names.Accepted and then
                     (if Path.Count > 0 then
                        Resolve'Result.Node /= Root and then
                        Name (Tree, Resolve'Result.Node) = Path.Parts (Path.Count)));

   -- Insertion preserves existing records and both payload stores exactly.
   function Insert_Frame (Tree, Prior : State) return Boolean with Ghost;
   type Insert_Status is (Inserted, Duplicate, Full, Invalid_Name);
   procedure Insert
     (Tree : in out State; Scope : Node_ID; Part : AML_Names.Segment;
      Node : out Node_ID; Result : out Insert_Status)
     with Pre => Scope <= Count (Tree),
          Post => Insert_Frame (Tree, Tree'Old)
            and then Method_Usage (Tree) = Method_Usage (Tree'Old)
            and then (if Result = Inserted then
                Count (Tree) = Count (Tree'Old) + 1 and then
                Node = Count (Tree) and then Present (Tree, Node) and then
                Parent (Tree, Node) = Scope and then Name (Tree, Node) = Part
                and then (for all I in 1 .. Count (Tree'Old) =>
                  Parent (Tree, I) = Parent (Tree'Old, I) and then
                  Name (Tree, I) = Name (Tree'Old, I))
             else Tree = Tree'Old and then Node = Root);
   type Object_Kind is (Scope_Object, Device_Object, Integer_Object, String_Object, Buffer_Object, Package_Object, Method_Object,
                       Table_Region_Object, Table_Field_Object, Uninitialized_Region_Object);
   function Kind (Tree : State; Node : Node_ID) return Object_Kind
     with Pre => Node <= Count (Tree),
          Post => (if Node = Root then Kind'Result = Scope_Object);
   -- Table identities are local to the containing service lifetime, not
   -- physical addresses. Only its admitted immutable table catalog may bind
   -- these records. A field keeps the resolved region value, not a recyclable
   -- namespace node ID. Dynamic declaration callers supply their method Owner.
   type Table_Region is record
      Table : Positive := 1;
      Extent : Positive := 1;
   end record;
   type Table_Field is record
      Region : Table_Region;
      Offset, Bits : Natural := 0;
   end record;
   function Region_Data (Tree : State; Node : Node_ID) return Table_Region
     with Pre => Node > Root and then Node <= Count (Tree) and then
       Present (Tree, Node) and then Kind (Tree, Node) = Table_Region_Object;
   function Field_Data (Tree : State; Node : Node_ID) return Table_Field
     with Pre => Node > Root and then Node <= Count (Tree) and then
       Present (Tree, Node) and then Kind (Tree, Node) = Table_Field_Object,
       Post => AML_Field_Data.Fits (Field_Data'Result.Region.Extent,
                                    Field_Data'Result.Offset, Field_Data'Result.Bits);
   type Bind_Status is (Bound, Binding_Duplicate, Binding_Full, Binding_Invalid);
   procedure Bind_Table_Region
     (Tree : in out State; Scope : Node_ID; Part : AML_Names.Segment;
      Region : Table_Region; Node : out Node_ID; Result : out Bind_Status;
      Owner : Node_ID := Root)
     with Pre => Scope <= Count (Tree) and then Owner <= Count (Tree),
       Post => Insert_Frame (Tree, Tree'Old) and then
         (if Result /= Bound then Tree = Tree'Old and then Node = Root
          else Count (Tree) = Count (Tree'Old) + 1 and then Node = Count (Tree)
            and then Present (Tree, Node) and then Kind (Tree, Node) = Table_Region_Object
            and then Parent (Tree, Node) = Scope and then Name (Tree, Node) = Part
            and then Region_Data (Tree, Node) = Region);
   procedure Bind_Table_Field
     (Tree : in out State; Scope : Node_ID; Part : AML_Names.Segment;
      Field : Table_Field; Node : out Node_ID; Result : out Bind_Status;
      Owner : Node_ID := Root)
     with Pre => Scope <= Count (Tree) and then Owner <= Count (Tree),
       Post => Insert_Frame (Tree, Tree'Old) and then
         (if Result /= Bound then Tree = Tree'Old and then Node = Root
          else Count (Tree) = Count (Tree'Old) + 1 and then Node = Count (Tree)
            and then Present (Tree, Node) and then Kind (Tree, Node) = Table_Field_Object
            and then Parent (Tree, Node) = Scope and then Name (Tree, Node) = Part
            and then Field_Data (Tree, Node) = Field);
   function Has_Integer (Tree : State; Node : Node_ID) return Boolean
     with Pre => Node <= Count (Tree);
   function Integer_Data (Tree : State; Node : Node_ID)
      return AML_Decode.Integer_Value
     with Pre => Node <= Count (Tree) and then Has_Integer (Tree, Node);
   function String_Data (Tree : State; Node : Node_ID) return String
     with Pre => Node <= Count (Tree) and then Kind (Tree, Node) = String_Object;
   function Buffer_Data (Tree : State; Node : Node_ID) return AML_Decode.Bytes
     with Pre => Node <= Count (Tree) and then Kind (Tree, Node) = Buffer_Object;
   function Method_Data (Tree : State; Node : Node_ID) return AML_Decode.Bytes
     with Pre => Node <= Count (Tree) and then Kind (Tree, Node) = Method_Object,
          Post => Method_Data'Result'Length <= AML_Execute.Max_Method_Bytes;
   function Value_Store (Tree : State) return AML_Objects.State;
   function Data_Object (Tree : State; Node : Node_ID) return AML_Objects.Object_ID
     with Pre => Node > Root and then Node <= Count (Tree)
       and then Kind (Tree, Node) in Integer_Object | String_Object | Buffer_Object | Package_Object,
          Post => Data_Object'Result > 0 and then
            Data_Object'Result <= AML_Objects.Count (Value_Store (Tree));
   -- Storage operation for an already resolved integer destination. AML Store
   -- conversion and reference semantics belong to the evaluator, not here.
   function Integer_Updated
     (Tree, Prior : State; Node : Node_ID; Value : AML_Decode.Integer_Value)
      return Boolean with Ghost,
      Pre => Node > Root and then Node <= Count (Prior)
        and then Has_Integer (Prior, Node);
   procedure Set_Integer
     (Tree : in out State; Node : Node_ID; Value : AML_Decode.Integer_Value)
     with Pre => Node > Root and then Node <= Count (Tree)
       and then Has_Integer (Tree, Node),
       Post => Count (Tree) = Count (Tree'Old)
         and then Value_Usage (Tree) = Value_Usage (Tree'Old)
         and then Integer_Updated (Tree, Tree'Old, Node, Value)
         and then Has_Integer (Tree, Node)
         and then Integer_Data (Tree, Node) = Value;
   function Invoke
     (Tree : State; Node : Node_ID; Args : AML_Execute.Arguments;
      Argument_Count : Natural; Budget : Natural) return AML_Execute.Execution_Result
     with Pre => Node <= Count (Tree) and then Argument_Count <= 7,
          Post => Invoke'Result.Charged <= Budget;
   -- Mutations completed before an execution error remain visible, as in AML.
   procedure Invoke_Mutable
     (Tree : in out State; Node : Node_ID; Args : AML_Execute.Arguments;
      Argument_Count : Natural; Budget : Natural; Result : out AML_Execute.Execution_Result)
     with Pre => Node <= Count (Tree) and then Argument_Count <= 7
       and then not Result'Constrained,
       Post => Result.Charged <= Budget;
   -- Table bytes remain external and immutable; buffer-valued field reads may
   -- allocate objects in Tree.Values. Metadata inspection never materializes.
   procedure Invoke_With_Tables
     (Tree : in out State; Input : aliased AML_Table_Backing.State; Node : Node_ID;
      Args : AML_Execute.Arguments; Argument_Count : Natural; Budget : Natural;
      Result : out AML_Execute.Execution_Result)
     with Pre => Node <= Count (Tree) and then Argument_Count <= 7
         and then not Result'Constrained,
       Post => Result.Charged <= Budget;

   type Load_Status is
     (Loaded, Bad_Name, Bad_Integer, Unsupported_Opcode, Missing_Scope,
      Duplicate_Name, Storage_Full, Bad_Package, Nesting_Limit, Bad_String, Value_Limit, Bad_Buffer, Bad_Method);
   --  Term-list subset: integer Name, Scope and Device. Input is the AML
   --  payload of an already admitted immutable table, not its SDT header.
   --  Unknown opcodes fail the whole transaction. No table activation effects.
   procedure Load_Names
     (Tree : in out State; Data : AML_Decode.Bytes;
      Width : AML_Decode.Integer_Width; Result : out Load_Status)
     with Post => (if Result /= Loaded then Tree = Tree'Old);
private
   type Entry_Record is record
      Alive : Boolean := True;
      Owner : Node_ID := Root;
      Active_Calls : Natural := 0;
      Up : Node_ID := Root;
      Part : AML_Names.Segment := "____";
      Object_Type : Object_Kind := Scope_Object;
      Object_Ref : AML_Objects.Object_ID := 0;
      Table_Binding : Table_Field;
      Method_Offset : AML_Execute.Method_Length := 0;
      Method_Size : AML_Execute.Method_Length := 0;
      Method_Flags : AML_Decode.Byte := 0;
      Method_Width : AML_Decode.Integer_Width := AML_Decode.Bits_64;
   end record;
   type Entries is array (Positive range 1 .. Capacity) of Entry_Record;
   type State is record
      Timer_State : AML_Clock.State := AML_Clock.Fresh;
      Used : Node_ID := 0;
      Items : Entries;
      Values : AML_Objects.State := AML_Objects.Empty;
      Code : AML_Decode.Bytes (1 .. AML_Execute.Max_Method_Bytes) := [others => 0];
      Code_Used : AML_Execute.Method_Length := 0;
   end record
     with Type_Invariant =>
       AML_Objects.Valid (State.Values) and then
       (for all I in 1 .. State.Used => State.Items (I).Up < I and then State.Items (I).Owner < I and then
          (if State.Items (I).Object_Type = Table_Field_Object then
             AML_Field_Data.Fits (State.Items (I).Table_Binding.Region.Extent,
               State.Items (I).Table_Binding.Offset, State.Items (I).Table_Binding.Bits)) and then
          State.Items (I).Method_Offset <= State.Code_Used and then
          State.Items (I).Method_Size <= State.Code_Used - State.Items (I).Method_Offset and then
          (if State.Items (I).Object_Type in Integer_Object | String_Object | Buffer_Object | Package_Object then
              State.Items (I).Object_Ref > 0 and then
              State.Items (I).Object_Ref <= AML_Objects.Count (State.Values) and then
              AML_Objects.Kind (State.Values, State.Items (I).Object_Ref) =
                (case State.Items (I).Object_Type is
                   when Integer_Object => AML_Objects.Integer_Object,
                   when String_Object => AML_Objects.String_Object,
                   when Buffer_Object => AML_Objects.Buffer_Object,
                   when others => AML_Objects.Package_Object)));
end AML_Namespace;

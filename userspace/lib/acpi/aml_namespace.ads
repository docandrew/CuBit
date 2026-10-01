pragma Ada_2022;
with AML_Names;
with AML_Decode;
--  Append-only namespace structure. IDs are scoped to one State lifetime;
--  they are not capabilities or externally reusable service handles.
generic
   Capacity : Positive;
package AML_Namespace with SPARK_Mode is
   use type AML_Names.Parse_Status;
   subtype Node_ID is Natural range 0 .. Capacity;
   Root : constant Node_ID := 0;
   type State is private;
   function Count (Tree : State) return Node_ID;
   function Parent (Tree : State; Node : Node_ID) return Node_ID
     with Pre => Node <= Count (Tree),
          Post => (if Node = Root then Parent'Result = Root
                   else Parent'Result < Node);
   function Name (Tree : State; Node : Node_ID) return AML_Names.Segment
     with Pre => Node > Root and then Node <= Count (Tree);
   function Empty return State with Post => Count (Empty'Result) = 0;

   function Child
     (Tree : State; Scope : Node_ID; Part : AML_Names.Segment) return Node_ID
     with Pre => Scope <= Count (Tree),
          Post => Child'Result <= Count (Tree) and then
            (if Child'Result /= Root then
               Parent (Tree, Child'Result) = Scope and then
               Name (Tree, Child'Result) = Part
             else (for all I in 1 .. Count (Tree) =>
                     Parent (Tree, I) /= Scope or else Name (Tree, I) /= Part));

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

   type Insert_Status is (Inserted, Duplicate, Full, Invalid_Name);
   procedure Insert
     (Tree : in out State; Scope : Node_ID; Part : AML_Names.Segment;
      Node : out Node_ID; Result : out Insert_Status)
     with Pre => Scope <= Count (Tree),
          Post =>
            (if Result = Inserted then
                Count (Tree) = Count (Tree'Old) + 1 and then
                Node = Count (Tree) and then
                Parent (Tree, Node) = Scope and then Name (Tree, Node) = Part
                and then (for all I in 1 .. Count (Tree'Old) =>
                  Parent (Tree, I) = Parent (Tree'Old, I) and then
                  Name (Tree, I) = Name (Tree'Old, I))
             else Tree = Tree'Old and then Node = Root);
   type Object_Kind is (Scope_Object, Device_Object, Integer_Object, String_Object, Buffer_Object);
   function Kind (Tree : State; Node : Node_ID) return Object_Kind
     with Pre => Node <= Count (Tree),
          Post => (if Node = Root then Kind'Result = Scope_Object);
   function Has_Integer (Tree : State; Node : Node_ID) return Boolean
     with Pre => Node <= Count (Tree);
   function Integer_Data (Tree : State; Node : Node_ID)
      return AML_Decode.Integer_Value
     with Pre => Node <= Count (Tree) and then Has_Integer (Tree, Node);
   function String_Data (Tree : State; Node : Node_ID) return String
     with Pre => Node <= Count (Tree) and then Kind (Tree, Node) = String_Object;
   function Buffer_Data (Tree : State; Node : Node_ID) return AML_Decode.Bytes
     with Pre => Node <= Count (Tree) and then Kind (Tree, Node) = Buffer_Object;
   type Load_Status is
     (Loaded, Bad_Name, Bad_Integer, Unsupported_Opcode, Missing_Scope,
      Duplicate_Name, Storage_Full, Bad_Package, Nesting_Limit, Bad_String, Value_Limit, Bad_Buffer);
   --  Term-list subset: integer Name, Scope and Device. Input is the AML
   --  payload of an already admitted immutable table, not its SDT header.
   --  Unknown opcodes fail the whole transaction. No table activation effects.
   procedure Load_Names
     (Tree : in out State; Data : AML_Decode.Bytes;
      Width : AML_Decode.Integer_Width; Result : out Load_Status)
     with Post => (if Result /= Loaded then Tree = Tree'Old);
private
   type Entry_Record is record
      Up : Node_ID := Root;
      Part : AML_Names.Segment := "____";
      Object_Type : Object_Kind := Scope_Object;
      Value : AML_Decode.Integer_Value := 0;
      Text : AML_Decode.String_Storage := [others => Character'Val (0)];
      Text_Length : Natural range 0 .. AML_Decode.Max_String_Length := 0;
      Buffer_Value : AML_Decode.Buffer_Storage := [others => 0];
      Buffer_Length : Natural range 0 .. AML_Decode.Max_Buffer_Length := 0;
   end record;
   type Entries is array (Positive range 1 .. Capacity) of Entry_Record;
   type State is record
      Used : Node_ID := 0;
      Items : Entries;
   end record
     with Type_Invariant =>
       (for all I in 1 .. State.Used => State.Items (I).Up < I);
end AML_Namespace;

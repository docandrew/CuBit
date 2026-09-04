generic
   Capacity   : Positive;
   Max_Values : Positive;
package CCL.Secondary_Stacks with
   SPARK_Mode => On
is
   --  A bounded region for variable-sized immutable CCL values.  Values carry
   --  their actual bounds; the region owns their bytes.  This mirrors Ada's
   --  constrained-by-initial-value semantics without exposing native pointers
   --  or depending on the GNAT secondary stack.

   subtype Storage_Count is Natural range 0 .. Capacity;
   subtype Value_Count is Natural range 0 .. Max_Values;
   --  Reserving Capacity positions in the index subtype makes Last_Index
   --  representable for every value the region can construct.  The type, not
   --  a defensive run-time guard, carries that arithmetic invariant.
   subtype String_Index is
     Positive range 1 .. Positive'Last - Capacity + 1;

   type Stack is private;
   type Stack_Mark is private;
   type String_Value is private;

   type Operation_Result is
     (Operation_Ok,
      Storage_Full,
      Value_Table_Full,
      Invalid_Bounds,
      Invalid_Mark,
      Invalid_Value,
      Length_Mismatch,
      Generation_Exhausted);

   procedure Initialize (Item : out Stack)
   with
      Post => Used_Bytes (Item) = 0 and then Live_Values (Item) = 0;

   function Mark (Item : Stack) return Stack_Mark;

   procedure Allocate_String
     (Item      : in out Stack;
      Text      : String;
      Value     : out String_Value;
      Result    : out Operation_Result;
      First     : String_Index := 1;
      Sensitive : Boolean := False)
   with
      Post =>
        (if Result = Operation_Ok then
           Is_Valid (Item, Value) and then
           Length (Value) = Text'Length and then
           First_Index (Value) = First);

   --  Releases every value created after Boundary.  Descriptors for released
   --  values become invalid even if their slots and storage are later reused.
   --  Bytes belonging to Sensitive values are zeroed before release.
   procedure Release
     (Item     : in out Stack;
      Boundary : Stack_Mark;
      Result   : out Operation_Result);

   --  Execution teardown scrubs the complete used region, including values
   --  not individually marked Sensitive, and invalidates every descriptor.
   procedure Clear (Item : in out Stack)
   with
      Post => Used_Bytes (Item) = 0 and then Live_Values (Item) = 0;

   function Is_Valid
     (Item : Stack; Value : String_Value) return Boolean;

   function First_Index (Value : String_Value) return String_Index;
   function Last_Index (Value : String_Value) return Natural;
   function Length (Value : String_Value) return Storage_Count;

   procedure Read
     (Item   : Stack;
      Value  : String_Value;
      Index  : String_Index;
      Element : out Character;
      Result : out Operation_Result);

   --  Ada-like array assignment: source and target bounds may differ, but the
   --  lengths must match.  Elements are slid into Target's bounds.
   procedure Copy_To
     (Item   : Stack;
      Value  : String_Value;
      Target : out String;
      Result : out Operation_Result);

   function Used_Bytes (Item : Stack) return Storage_Count;
   function Live_Values (Item : Stack) return Value_Count;

private
   subtype Storage_Offset is Natural range 0 .. Capacity - 1;
   subtype Value_Slot is Natural range 0 .. Max_Values - 1;
   subtype Generation is Natural range 0 .. Natural'Last;

   type String_Value is record
      Slot       : Value_Slot := 0;
      Generation_Number : Generation := 0;
      First      : String_Index := 1;
      Count      : Storage_Count := 0;
   end record;

   type Stack_Mark is record
      Bytes       : Storage_Count := 0;
      Values      : Value_Count := 0;
      Boundary_Generation : Generation := 0;
   end record;

   type Allocation is record
      Offset      : Storage_Offset := 0;
      Count       : Storage_Count := 0;
      Generation_Number : Generation := 0;
      Sensitive   : Boolean := False;
      Active      : Boolean := False;
   end record;

   type Allocation_Table is array (Value_Slot) of Allocation;
   type Storage_Array is array (Storage_Offset) of Character;

   type Stack is record
      Data            : Storage_Array := [others => Character'Val (0)];
      Allocations     : Allocation_Table := [others => (others => <>)];
      Used            : Storage_Count := 0;
      Count           : Value_Count := 0;
      Next_Generation : Generation := 1;
   end record;

   function Used_Bytes (Item : Stack) return Storage_Count is (Item.Used);
   function Live_Values (Item : Stack) return Value_Count is (Item.Count);

   function First_Index (Value : String_Value) return String_Index is
     (Value.First);

   function Last_Index (Value : String_Value) return Natural is
     (if Value.Count = 0 then Value.First - 1
      else Value.First + (Value.Count - 1));

   function Length (Value : String_Value) return Storage_Count is
     (Value.Count);

   function Is_Valid
     (Item : Stack; Value : String_Value) return Boolean is
     (Natural (Value.Slot) < Item.Count and then
      Item.Allocations (Value.Slot).Active and then
      Item.Allocations (Value.Slot).Generation_Number =
        Value.Generation_Number and then
      Item.Allocations (Value.Slot).Count = Value.Count and then
      (Value.Count = 0 or else
       Item.Allocations (Value.Slot).Offset <= Item.Used - Value.Count));
end CCL.Secondary_Stacks;

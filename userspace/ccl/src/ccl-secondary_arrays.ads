generic
   type Element_Type is private;
   Null_Element : Element_Type;
   type Element_Array is array (Positive range <>) of Element_Type;
   Capacity   : Positive;
   Max_Values : Positive;
   Max_Array_Length : Positive := Capacity;
package CCL.Secondary_Arrays with
   SPARK_Mode => On
is
   --  The array counterpart of CCL.Secondary_Stacks (strings): a bounded
   --  region for variable-sized immutable CCL arrays such as List<T>
   --  (docs/ccl-repl.md, "Lists"). Values carry their actual bounds; the
   --  region owns their elements. This mirrors Ada's constrained-by-initial-
   --  value semantics without exposing native pointers or depending on the
   --  GNAT secondary stack. Released elements are reset to Null_Element.

   subtype Storage_Count is Natural range 0 .. Capacity;
   --  Region capacity and per-value capacity are separate budgets.
   subtype Array_Length is
     Natural range 0 .. Natural'Min (Capacity, Max_Array_Length);
   subtype Value_Count is Natural range 0 .. Max_Values;
   --  Reserving Capacity positions in the index subtype makes Last_Index
   --  representable for every value the region can construct.  The type, not
   --  a defensive run-time guard, carries that arithmetic invariant.
   subtype Array_Index is
     Positive range 1 .. Positive'Last - Capacity + 1;

   type Stack is private;
   type Stack_Mark is private;
   type Array_Value is private;

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

   procedure Allocate
     (Item      : in out Stack;
      Items     : Element_Array;
      Value     : out Array_Value;
      Result    : out Operation_Result;
      First     : Array_Index := 1;
      Sensitive : Boolean := False)
   with
      Post =>
        (if Result = Operation_Ok then
           Is_Valid (Item, Value) and then
           Length (Value) = Items'Length and then
           First_Index (Value) = First);

   --  Building a value in place, for results whose elements are computed one
   --  at a time (the list builtins): Reserve Length null elements, Write
   --  them, then Shrink to the length actually produced.  No caller needs a
   --  stack buffer sized by the data.
   procedure Reserve
     (Item   : in out Stack;
      Count  : Natural;
      Value  : out Array_Value;
      Result : out Operation_Result)
   with
      Post =>
        (if Result = Operation_Ok then
           Is_Valid (Item, Value) and then Length (Value) = Count and then
           First_Index (Value) = 1);

   procedure Write
     (Item    : in out Stack;
      Value   : Array_Value;
      Index   : Array_Index;
      Element : Element_Type;
      Result  : out Operation_Result)
   with
      Post => Used_Bytes (Item) = Used_Bytes (Item'Old) and then
              Live_Values (Item) = Live_Values (Item'Old) and then
              (if Is_Valid (Item'Old, Value) then Is_Valid (Item, Value));

   --  Keeps the first Count elements.  The newest value shrinks in place;
   --  an older one (something was allocated after it) is copied into a new
   --  value, and the old storage stays used until release.
   procedure Shrink
     (Item   : in out Stack;
      Value  : in out Array_Value;
      Count  : Natural;
      Result : out Operation_Result)
   with
      Post =>
        (if Result = Operation_Ok then
           Is_Valid (Item, Value) and then Length (Value) = Count and then
           First_Index (Value) = First_Index (Value'Old));

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
     (Item : Stack; Value : Array_Value) return Boolean;

   function First_Index (Value : Array_Value) return Array_Index;
   function Last_Index (Value : Array_Value) return Natural;
   function Length (Value : Array_Value) return Array_Length;

   procedure Read
     (Item   : Stack;
      Value  : Array_Value;
      Index  : Array_Index;
      Element : out Element_Type;
      Result : out Operation_Result);

   --  Ada-like array assignment: source and target bounds may differ, but the
   --  lengths must match.  Elements are slid into Target's bounds.
   procedure Copy_To
     (Item   : Stack;
      Value  : Array_Value;
      Target : out Element_Array;
      Result : out Operation_Result);

   function Used_Bytes (Item : Stack) return Storage_Count;
   function Live_Values (Item : Stack) return Value_Count;

private
   subtype Storage_Offset is Natural range 0 .. Capacity - 1;
   subtype Value_Slot is Natural range 0 .. Max_Values - 1;
   subtype Generation is Natural range 0 .. Natural'Last;

   type Array_Value is record
      Slot       : Value_Slot := 0;
      Generation_Number : Generation := 0;
      First      : Array_Index := 1;
      Count      : Array_Length := 0;
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
   type Storage_Array is array (Storage_Offset) of Element_Type;

   type Stack is record
      Data            : Storage_Array := [others => Null_Element];
      Allocations     : Allocation_Table := [others => (others => <>)];
      Used            : Storage_Count := 0;
      Count           : Value_Count := 0;
      Next_Generation : Generation := 1;
   end record;

   function Used_Bytes (Item : Stack) return Storage_Count is (Item.Used);
   function Live_Values (Item : Stack) return Value_Count is (Item.Count);

   function First_Index (Value : Array_Value) return Array_Index is
     (Value.First);

   function Last_Index (Value : Array_Value) return Natural is
     (if Value.Count = 0 then Value.First - 1
      else Value.First + (Value.Count - 1));

   function Length (Value : Array_Value) return Array_Length is
     (Value.Count);

   function Is_Valid
     (Item : Stack; Value : Array_Value) return Boolean is
     (Natural (Value.Slot) < Item.Count and then
      Item.Allocations (Value.Slot).Active and then
      Item.Allocations (Value.Slot).Generation_Number =
        Value.Generation_Number and then
      Item.Allocations (Value.Slot).Count = Value.Count and then
      (Value.Count = 0 or else
       Item.Allocations (Value.Slot).Offset <= Item.Used - Value.Count));
end CCL.Secondary_Arrays;

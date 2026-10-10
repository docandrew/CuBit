with AML_Frame_Handles;
with AML_Root_Slots;
with AML_Objects.Root_Snapshots;
generic
   type Value_Type is private;
   Empty_Value : Value_Type;
   Capacity : Positive;
package AML_Frame_Roots with SPARK_Mode, Pure is
   use type AML_Frame_Handles.Frame_Handle;
   subtype Root_Count is Natural range 0 .. Capacity;
   subtype Root_Index is Positive range 1 .. Capacity;
   type State is private;
   type Result_Status is (Ready, Root_Limit, Invalid_Frame, Duplicate_Frame, Invalid_Value);
   type Read_Result is record
      Reserved : Boolean := False;
      Initialized : Boolean := False;
      Value : Value_Type := Empty_Value;
   end record;
   function Valid (Store : State) return Boolean;
   function Count (Store : State) return Root_Count;
   function Empty return State with Post => Valid (Empty'Result) and then Count (Empty'Result) = 0;
   -- Trusted owner admits actual live frames. Full handles, including domain,
   -- are keys; this ledger neither mints nor independently authenticates them.
   function Reserved_Frame (After, Before : State; Frame : AML_Frame_Handles.Frame_Handle)
      return Boolean with Ghost;
   function Updated_Cell (After, Before : State; Frame : AML_Frame_Handles.Frame_Handle;
      Cell : AML_Frame_Handles.Cell_ID; Initialized : Boolean; Value : Value_Type)
      return Boolean with Ghost;
   function Released_Frame (After, Before : State; Frame : AML_Frame_Handles.Frame_Handle)
      return Boolean with Ghost;
   procedure Reserve (Store : in out State; Frame : AML_Frame_Handles.Frame_Handle;
      Status : out Result_Status) with Pre => Valid (Store),
      Post => Valid (Store) and then (if Status = Ready then Reserved_Frame (Store, Store'Old, Frame)
        else Store = Store'Old);
   procedure Update (Store : in out State; Frame : AML_Frame_Handles.Frame_Handle;
      Cell : AML_Frame_Handles.Cell_ID; Initialized : Boolean; Value : Value_Type;
      Status : out Result_Status) with Pre => Valid (Store),
      Post => Valid (Store) and then (if Status = Ready then
        Updated_Cell (Store, Store'Old, Frame, Cell, Initialized, Value) else Store = Store'Old);
   procedure Release (Store : in out State; Frame : AML_Frame_Handles.Frame_Handle;
      Status : out Result_Status) with Pre => Valid (Store),
      Post => Valid (Store) and then (if Status = Ready then Released_Frame (Store, Store'Old, Frame)
        else Store = Store'Old);
   function Updated_Held (After, Before : State; Frame : AML_Frame_Handles.Frame_Handle;
      Root : AML_Root_Slots.Held_Root; Initialized : Boolean; Value : Value_Type)
      return Boolean with Ghost;
   procedure Update_Held (Store : in out State; Frame : AML_Frame_Handles.Frame_Handle;
      Root : AML_Root_Slots.Held_Root; Initialized : Boolean; Value : Value_Type;
      Status : out Result_Status) with Pre => Valid (Store),
      Post => Valid (Store) and then (if Status = Ready then
        Updated_Held (Store, Store'Old, Frame, Root, Initialized, Value) else Store = Store'Old);
   function Held_Read (Store : State; Index : Root_Index; Root : AML_Root_Slots.Held_Root;
      Result : Read_Result) return Boolean with Ghost;
   function Read_Held (Store : State; Index : Root_Index; Root : AML_Root_Slots.Held_Root)
      return Read_Result with Pre => Valid (Store),
      Post => Held_Read (Store, Index, Root, Read_Held'Result);
   function Cell_Read (Store : State; Index : Root_Index; Cell : AML_Frame_Handles.Cell_ID;
      Result : Read_Result) return Boolean with Ghost;
   function Read_Cell (Store : State; Index : Root_Index; Cell : AML_Frame_Handles.Cell_ID)
      return Read_Result with Pre => Valid (Store),
      Post => Cell_Read (Store, Index, Cell, Read_Cell'Result);
   -- Snapshots are store-local metadata. The owner authenticates addresses;
   -- this ledger preserves all phases, including poisoned/incomplete snapshots,
   -- so a root gatherer cannot mistake a failed publication for an empty set.
   function Updated_Snapshot (After, Before : State; Frame : AML_Frame_Handles.Frame_Handle;
      Roots : AML_Objects.Root_Snapshots.Snapshot) return Boolean with Ghost;
   procedure Update_Snapshot (Store : in out State; Frame : AML_Frame_Handles.Frame_Handle;
      Roots : AML_Objects.Root_Snapshots.Snapshot; Status : out Result_Status)
      with Pre => Valid (Store), Post => Valid (Store) and then
        (if Status = Ready then Updated_Snapshot (Store, Store'Old, Frame, Roots)
         else Store = Store'Old);
   function Snapshot_Read (Store : State; Index : Root_Index;
      Roots : AML_Objects.Root_Snapshots.Snapshot) return Boolean with Ghost;
   function Read_Snapshot (Store : State; Index : Root_Index)
      return AML_Objects.Root_Snapshots.Snapshot with Pre => Valid (Store),
      Post => Snapshot_Read (Store, Index, Read_Snapshot'Result);
private
   type Cell_Values is array (AML_Frame_Handles.Cell_ID) of Value_Type;
   type Cell_Flags is array (AML_Frame_Handles.Cell_ID) of Boolean;
   type Held_Values is array (AML_Root_Slots.Held_Root) of Value_Type;
   type Held_Flags is array (AML_Root_Slots.Held_Root) of Boolean;
   type Entry_Data is record
      Snapshot : AML_Objects.Root_Snapshots.Snapshot := AML_Objects.Root_Snapshots.Empty;
      Held_Initialized : Held_Flags := [others => False];
      Held : Held_Values := [others => Empty_Value];
      Frame : AML_Frame_Handles.Frame_Handle := AML_Frame_Handles.No_Frame;
      Initialized : Cell_Flags := [others => False];
      Values : Cell_Values := [others => Empty_Value];
   end record;
   type Entry_Array is array (Root_Index) of Entry_Data;
   type State is record
      Used : Root_Count := 0;
      Entries : Entry_Array := [others => <>];
   end record;
   function Count (Store : State) return Root_Count is (Store.Used);
end AML_Frame_Roots;

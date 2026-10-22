pragma Ada_2022;
with AML_Mutexes;
with AML_Decode;
with AML_Field_Data;
with AML_References;
with AML_Metadata_Kinds;
with AML_Object_Identifiers;
package AML_Objects with SPARK_Mode, Pure is
   use type AML_Decode.Byte;
   use type AML_Mutexes.Sync_Level;
   use type AML_Decode.Integer_Value;
   use type AML_Decode.Integer_Origin;
   use type AML_Decode.Bytes;
   use type AML_References.Reference;
   use type AML_Metadata_Kinds.Snapshot_Description;
   use type AML_Object_Identifiers.Slot_Incarnation;
   Max_Objects : constant := AML_Object_Identifiers.Max_Objects;
   Max_Bytes : constant := 65_536;
   Max_Elements : constant := 8192;
   subtype Object_ID is AML_Object_Identifiers.Object_ID;
   No_Object : constant Object_ID := 0;
   type Object_Kind is (Integer_Object, String_Object, Buffer_Object, Package_Object, Reference_Object, Metadata_Object, Metadata_Snapshot_Object, Event_Object, Mutex_Object);
   subtype Byte_Kind is Object_Kind range String_Object .. Buffer_Object;
   type State is private;
   type Usage is record
      Objects : Object_ID;
      Bytes : Natural range 0 .. Max_Bytes;
      Elements : Natural range 0 .. Max_Elements;
   end record;
   function Usage_Of (Store : State) return Usage;
   function Valid (Store : State) return Boolean;
   --  Every previously allocated object and its backing storage is unchanged.
   --  This includes package links, even when they form cycles.
   function Extends (Store, Prior : State) return Boolean with Ghost;
   function Slot_Bound (Store : State) return Object_ID;
   function Live_Count (Store : State) return Object_ID;
   function Is_Live (Store : State; ID : Object_ID) return Boolean;
   function Last_Incarnation (Store : State; ID : Object_ID)
      return AML_Object_Identifiers.Slot_Incarnation with Pre => ID /= No_Object;
   function Incarnation_Limit (Store : State) return AML_Object_Identifiers.Incarnation_Budget;
   function Address_Of (Store : State; ID : Object_ID) return AML_Object_Identifiers.Object_Address;
   function Matches_Address (Store : State; Address : AML_Object_Identifiers.Object_Address) return Boolean;
   function Byte_Count (Store : State) return Natural;
   function Element_Count (Store : State) return Natural;
   function Empty (Max_Incarnation : AML_Object_Identifiers.Incarnation_Budget :=
      AML_Object_Identifiers.Slot_Incarnation'Last) return State with Post => Valid (Empty'Result) and then Slot_Bound (Empty'Result) = 0
     and then Usage_Of (Empty'Result) = Usage'(others => 0)
     and then Incarnation_Limit (Empty'Result) = Max_Incarnation;
   function Kind (Store : State; ID : Object_ID) return Object_Kind
     with Pre => Is_Live (Store, ID);
   function Length (Store : State; ID : Object_ID) return Natural
     with Pre => Is_Live (Store, ID);
   function Origin_Of (Store : State; ID : Object_ID) return AML_Decode.Integer_Origin
     with Pre => Is_Live (Store, ID)
       and then Kind (Store, ID) = Integer_Object;
   Max_Event_Units : constant := 2 ** 32 - 1;
   subtype Event_Unit_Count is AML_Decode.Integer_Value range 0 .. Max_Event_Units;
   function Event_Units (Store : State; ID : Object_ID) return Event_Unit_Count
     with Pre => Is_Live (Store, ID) and then Kind (Store, ID) = Event_Object;
   function Integer_Data (Store : State; ID : Object_ID) return AML_Decode.Integer_Value
     with Pre => Is_Live (Store, ID) and then Kind (Store, ID) = Integer_Object;
   -- Typed payload only: structural validity is not live referent authority.
   function Reference_Data (Store : State; ID : Object_ID) return AML_References.Reference
     with Pre => Is_Live (Store, ID) and then Kind (Store, ID) = Reference_Object;
   --  Representation only: named/root metadata remains owner-authenticated by
   --  the arena. It is not a dereferenceable Reference_Object or data address.
   function Metadata_Identity (Value : AML_References.Reference) return Boolean;
   function Metadata_Data (Store : State; ID : Object_ID) return AML_References.Reference
     with Pre => Is_Live (Store, ID) and then Kind (Store, ID) = Metadata_Object;
   function Metadata_Snapshot_Data (Store : State; ID : Object_ID)
      return AML_Metadata_Kinds.Snapshot_Description
     with Pre => Valid (Store) and then Is_Live (Store, ID)
       and then Kind (Store, ID) = Metadata_Snapshot_Object,
       Post => AML_Metadata_Kinds.Supported (Metadata_Snapshot_Data'Result);
   function Byte_Data (Store : State; ID : Object_ID) return AML_Decode.Bytes
     with Pre => Valid (Store) and then Is_Live (Store, ID)
       and then Kind (Store, ID) in Byte_Kind;
   function Element (Store : State; ID : Object_ID; Index : Natural) return Object_ID
     with Pre => Valid (Store) and then Is_Live (Store, ID)
       and then Kind (Store, ID) = Package_Object and then Index < Length (Store, ID),
          Post => Element'Result = No_Object or else Is_Live (Store, Element'Result);
   function Stored_Byte (Store : State; ID : Object_ID; Index : Natural)
     return AML_Decode.Byte
   with Pre => Valid (Store) and then Is_Live (Store, ID)
     and then Kind (Store, ID) in Byte_Kind and then Index < Length (Store, ID);
   function Stored_Byte_Updated
     (Store, Prior : State; ID : Object_ID; Index : Natural;
      Value : AML_Decode.Byte) return Boolean with Ghost,
     Pre => Valid (Prior) and then Is_Live (Prior, ID)
       and then Kind (Prior, ID) in Byte_Kind and then Index < Length (Prior, ID);
   procedure Set_Stored_Byte
     (Store : in out State; ID : Object_ID; Index : Natural; Value : AML_Decode.Byte)
   with Pre => Valid (Store) and then Is_Live (Store, ID)
       and then Kind (Store, ID) in Byte_Kind and then Index < Length (Store, ID),
     Post => Valid (Store) and then Usage_Of (Store) = Usage_Of (Store'Old)
       and then Slot_Bound (Store) = Slot_Bound (Store'Old)
       and then Stored_Byte_Updated (Store, Store'Old, ID, Index, Value)
       and then Kind (Store, ID) in Byte_Kind
       and then Length (Store, ID) = Length (Store'Old, ID)
       and then Stored_Byte (Store, ID, Index) = Value
       and then (for all J in 1 .. Slot_Bound (Store) => (if Is_Live (Store, J) then
         Kind (Store, J) = Kind (Store'Old, J)
         and then Length (Store, J) = Length (Store'Old, J)));
   --  Internal arena-local identity, not an authority-bearing handle.
   type String_Update_Status is
     (String_Updated, Invalid_String_ID, Not_A_String, String_Byte_Limit);
   function String_Replaced
     (Store, Prior : State; ID : Object_ID; Data : AML_Decode.Bytes)
      return Boolean with Ghost,
      Pre => Valid (Prior) and then Is_Live (Prior, ID)
        and then Kind (Prior, ID) = String_Object;
   procedure Replace_String
     (Store : in out State; ID : Object_ID; Data : AML_Decode.Bytes;
      Status : out String_Update_Status)
     with Pre => Valid (Store),
       Post => Valid (Store) and then
         (if Status = String_Updated then
            Is_Live (Store, ID)
            and then Kind (Store, ID) = String_Object
            and then Byte_Data (Store, ID) = Data
            and then String_Replaced (Store, Store'Old, ID, Data)
          else Store = Store'Old);
   type Buffer_Update_Status is
     (Buffer_Updated, Invalid_Buffer_ID, Not_A_Buffer, Buffer_Byte_Limit);
   function Buffer_Stored
     (Store, Prior : State; ID : Object_ID; Data : AML_Decode.Bytes)
      return Boolean with Ghost,
      Pre => Valid (Prior) and then Is_Live (Prior, ID)
        and then Kind (Prior, ID) = Buffer_Object;
   procedure Store_Buffer
     (Store : in out State; ID : Object_ID; Data : AML_Decode.Bytes;
      Status : out Buffer_Update_Status)
     with Pre => Valid (Store),
       Post => Valid (Store) and then
         (if Status = Buffer_Updated then
            Is_Live (Store, ID)
            and then Kind (Store, ID) = Buffer_Object
            and then Buffer_Stored (Store, Store'Old, ID, Data)
          else Store = Store'Old);
   -- Exact frame condition: only this existing integer's payload changes.
   -- Object identity, package links and every allocation counter are retained.
   function Integer_Updated
     (Store, Prior : State; ID : Object_ID; Value : AML_Decode.Integer_Value)
      return Boolean with Ghost,
      Pre => Is_Live (Prior, ID);
   procedure Set_Integer
     (Store : in out State; ID : Object_ID; Value : AML_Decode.Integer_Value)
     with Pre => Valid (Store) and then Is_Live (Store, ID)
       and then Kind (Store, ID) = Integer_Object,
       Post => Valid (Store) and then Usage_Of (Store) = Usage_Of (Store'Old)
         and then Slot_Bound (Store) = Slot_Bound (Store'Old)
         and then Integer_Updated (Store, Store'Old, ID, Value)
         and then Origin_Of (Store, ID) = Origin_Of (Store'Old, ID)
         and then Integer_Data (Store, ID) = Value
         and then (for all J in 1 .. Slot_Bound (Store) => (if Is_Live (Store, J) then
           Kind (Store, J) = Kind (Store'Old, J)
           and then Length (Store, J) = Length (Store'Old, J)));
   --  Reused slots receive a nonwrapping incarnation; exhausted slots retire.
   --  Zero denotes an uninitialized package element, not an Integer zero.
   type Allocation_Status is (Allocated, Object_Limit, Generation_Limit, Byte_Limit, Element_Limit, Invalid_Reference, Invalid_Metadata, Invalid_Mutex_Flags, Unsupported_Copy);
   function Fresh_Allocation (Store, Prior : State; ID : Object_ID) return Boolean
     with Ghost;
   function Reference_Allocated
     (Store, Prior : State; ID : Object_ID; Value : AML_References.Reference)
      return Boolean with Ghost;
   -- Invalid shape takes precedence over exhausted object quota. Well-formed
   -- stale/foreign payloads remain data; only the owner can validate live use.
   procedure New_Reference (Store : in out State; Value : AML_References.Reference;
                            ID : out Object_ID; Status : out Allocation_Status)
     with Pre => Valid (Store),
       Post => Valid (Store) and then Extends (Store, Store'Old)
         and then Status in Allocated | Object_Limit | Generation_Limit | Invalid_Reference
         and then (if Status = Allocated then
           Fresh_Allocation (Store, Store'Old, ID)
           and then Reference_Allocated (Store, Store'Old, ID, Value)
           and then Is_Live (Store, ID) and then Live_Count (Store) = Live_Count (Store'Old) + 1
           and then Kind (Store, ID) = Reference_Object and then Length (Store, ID) = 0
           and then Reference_Data (Store, ID) = Value
           and then Byte_Count (Store) = Byte_Count (Store'Old)
           and then Element_Count (Store) = Element_Count (Store'Old)
          else Store = Store'Old and then ID = No_Object)
         and then (if not AML_References.Well_Formed (Value) then Status = Invalid_Reference);
   function Metadata_Allocated
     (Store, Prior : State; ID : Object_ID; Value : AML_References.Reference)
      return Boolean with Ghost;
   -- Invalid shape/kind takes precedence over exhausted object quota.
   -- A well-formed named/root payload still needs arena authentication.
   procedure New_Metadata (Store : in out State; Value : AML_References.Reference;
                            ID : out Object_ID; Status : out Allocation_Status)
     with Pre => Valid (Store),
       Post => Valid (Store) and then Extends (Store, Store'Old)
         and then Status in Allocated | Object_Limit | Generation_Limit | Invalid_Reference
         and then (if Status = Allocated then
           Fresh_Allocation (Store, Store'Old, ID)
           and then Metadata_Allocated (Store, Store'Old, ID, Value)
           and then Is_Live (Store, ID) and then Live_Count (Store) = Live_Count (Store'Old) + 1
           and then Kind (Store, ID) = Metadata_Object and then Length (Store, ID) = 0
           and then Metadata_Data (Store, ID) = Value
           and then Byte_Count (Store) = Byte_Count (Store'Old)
           and then Element_Count (Store) = Element_Count (Store'Old)
          else Store = Store'Old and then ID = No_Object)
         and then (if not Metadata_Identity (Value) then Status = Invalid_Reference);
   function Metadata_Snapshot_Allocated
     (Store, Prior : State; ID : Object_ID;
      Value : AML_Metadata_Kinds.Snapshot_Description) return Boolean with Ghost;
   --  Unsupported descriptor class precedes quota failure. This is structural
   --  validation only; the owning arena authenticates capture and handles.
   procedure New_Metadata_Snapshot
     (Store : in out State; Value : AML_Metadata_Kinds.Snapshot_Description;
      ID : out Object_ID; Status : out Allocation_Status)
     with Pre => Valid (Store),
       Post => Valid (Store) and then Extends (Store, Store'Old)
         and then Status in Allocated | Object_Limit | Generation_Limit | Invalid_Metadata
         and then (if Status = Allocated then
           Fresh_Allocation (Store, Store'Old, ID)
           and then Metadata_Snapshot_Allocated (Store, Store'Old, ID, Value)
           and then Is_Live (Store, ID)
           and then Live_Count (Store) = Live_Count (Store'Old) + 1
           and then Kind (Store, ID) = Metadata_Snapshot_Object
           and then Length (Store, ID) = 0
           and then Metadata_Snapshot_Data (Store, ID) = Value
           and then Byte_Count (Store) = Byte_Count (Store'Old)
           and then Element_Count (Store) = Element_Count (Store'Old)
          else Store = Store'Old and then ID = No_Object)
         and then (if not AML_Metadata_Kinds.Supported (Value) then Status = Invalid_Metadata);
   -- Runtime Mutex flags are canonical: reserved high bits are rejected.
   function Mutex_Flags (Store : State; ID : Object_ID) return AML_Decode.Byte
     with Pre => Valid (Store) and then Is_Live (Store, ID) and then Kind (Store, ID) = Mutex_Object;
   function Mutex_Level (Store : State; ID : Object_ID) return AML_Mutexes.Sync_Level
     with Pre => Valid (Store) and then Is_Live (Store, ID) and then Kind (Store, ID) = Mutex_Object;
   procedure New_Mutex (Store : in out State; Flags : AML_Decode.Byte;
     ID : out Object_ID; Status : out Allocation_Status)
     with Pre => Valid (Store),
       Post => Valid (Store) and then Extends (Store, Store'Old)
         and then Status in Allocated | Object_Limit | Generation_Limit | Invalid_Mutex_Flags
         and then (if not AML_Mutexes.Canonical_Flags (Flags) then Status = Invalid_Mutex_Flags)
         and then (if Status = Allocated then Fresh_Allocation (Store, Store'Old, ID)
           and then Kind (Store, ID) = Mutex_Object and then Mutex_Flags (Store, ID) = Flags
           and then Mutex_Level (Store, ID) = AML_Mutexes.Sync_Level (Flags) and then Length (Store, ID) = 0
           else Store = Store'Old and then ID = No_Object);
   procedure New_Event (Store : in out State; ID : out Object_ID;
     Status : out Allocation_Status)
     with Pre => Valid (Store),
       Post => Valid (Store) and then Extends (Store, Store'Old)
         and then Status in Allocated | Object_Limit | Generation_Limit
         and then (if Status = Allocated then Fresh_Allocation (Store, Store'Old, ID)
           and then Kind (Store, ID) = Event_Object and then Event_Units (Store, ID) = 0
           and then Length (Store, ID) = 0
           else Store = Store'Old and then ID = No_Object);
   procedure Set_Event_Units (Store : in out State; ID : Object_ID; Units : Event_Unit_Count)
     with Pre => Valid (Store) and then Is_Live (Store, ID) and then Kind (Store, ID) = Event_Object,
       Post => Valid (Store) and then Integer_Updated (Store, Store'Old, ID, Units)
         and then Event_Units (Store, ID) = Units;
   procedure New_Integer (Store : in out State; Value : AML_Decode.Integer_Value;
                          ID : out Object_ID; Status : out Allocation_Status;
                          Origin : AML_Decode.Integer_Origin := AML_Decode.Ordinary_Integer)
     with Pre => Valid (Store),
          Post => Valid (Store) and then Extends (Store, Store'Old) and then
            (if Status = Allocated then
               Fresh_Allocation (Store, Store'Old, ID)
               and then Live_Count (Store) = Live_Count (Store'Old) + 1 and then Is_Live (Store, ID)
               and then Kind (Store, ID) = Integer_Object
               and then Integer_Data (Store, ID) = Value
               and then Origin_Of (Store, ID) = Origin
               and then (for all J in 1 .. Slot_Bound (Store'Old) => (if Is_Live (Store'Old, J) then Kind (Store, J) = Kind (Store'Old, J)
                 and then Length (Store, J) = Length (Store'Old, J)))
             else Store = Store'Old and ID = 0);
   procedure New_Bytes (Store : in out State; Tag : Byte_Kind; Data : AML_Decode.Bytes;
                        ID : out Object_ID; Status : out Allocation_Status)
     with Pre => Valid (Store),
          Post => Valid (Store) and then Extends (Store, Store'Old) and then
            (if Status = Allocated then
               Fresh_Allocation (Store, Store'Old, ID)
               and then Live_Count (Store) = Live_Count (Store'Old) + 1 and then Is_Live (Store, ID)
               and then Kind (Store, ID) = Tag
               and then Byte_Data (Store, ID) = Data
               and then Length (Store, ID) = Data'Length
               and then (for all J in 1 .. Slot_Bound (Store'Old) => (if Is_Live (Store'Old, J) then Kind (Store, J) = Kind (Store'Old, J)
                 and then Length (Store, J) = Length (Store'Old, J)))
             else Store = Store'Old and ID = 0);
   -- Requested_Length is already a size, not an AML integer operand: no
   -- implicit UINT32 truncation. Initializer may extend the requested length.
   procedure New_Buffer
     (Store : in out State; Requested_Length : AML_Decode.Integer_Value;
      Initializer : AML_Decode.Bytes; ID : out Object_ID;
      Status : out Allocation_Status)
     with Pre => Valid (Store),
       Post => Valid (Store) and then Extends (Store, Store'Old)
         and then Status in Allocated | Object_Limit | Generation_Limit | Byte_Limit
         and then (if Status = Allocated then
           Fresh_Allocation (Store, Store'Old, ID) and then Is_Live (Store, ID)
           and then Kind (Store, ID) = Buffer_Object
           and then Live_Count (Store) = Live_Count (Store'Old) + 1
           and then AML_Decode.Integer_Value (Length (Store, ID)) =
             AML_Decode.Integer_Value'Max (Requested_Length, AML_Decode.Integer_Value (Initializer'Length))
           and then Byte_Count (Store) = Byte_Count (Store'Old) + Length (Store, ID)
           and then Element_Count (Store) = Element_Count (Store'Old)
           and then (for all I in 1 .. Length (Store, ID) =>
             Stored_Byte (Store, ID, I - 1) =
               (if I <= Initializer'Length then Initializer (Initializer'First + (I - 1)) else 0))
          else Store = Store'Old and then ID = No_Object);
   -- An immutable byte view, not a table descriptor or physical authority.
   -- The explicitly aliased source record is passed by reference. Readers
   -- receive only that immutable source; no callback state or pointer escapes.
   generic
      type Source_Type (<>) is private;
      with function Source_Length (Data : aliased Source_Type) return Natural;
      with function Source_Byte (Data : aliased Source_Type; Index : Natural)
         return AML_Decode.Byte;
   package Bit_Buffer_Initializers is
      function Fits_Source
        (Data : aliased Source_Type; First_Byte, Byte_Length, Offset, Bits : Natural)
         return Boolean is
        (First_Byte <= Source_Length (Data) and then Byte_Length <= Source_Length (Data) - First_Byte
         and then AML_Field_Data.Fits (Byte_Length, Offset, Bits));
      function Window
        (Data : aliased Source_Type; First_Byte, Byte_Length, Offset : Natural;
         Bits : Positive) return AML_Decode.Byte
        with Pre => Bits <= 8 and then Fits_Source (Data, First_Byte, Byte_Length, Offset, Bits),
          Post => Natural (Window'Result) =
            (Natural (Source_Byte (Data, First_Byte + Offset / 8)) / AML_Field_Data.Scale (Offset mod 8)
             + (if Bits + Offset mod 8 > 8 then
                  Natural (Source_Byte (Data, First_Byte + Offset / 8 + 1)) * AML_Field_Data.Scale (8 - Offset mod 8)
                else 0)) mod AML_Field_Data.Scale (Bits);
      procedure Allocate
        (Store : in out State; Data : aliased Source_Type;
         First_Byte, Byte_Length, Offset, Bits : Natural;
         ID : out Object_ID; Status : out Allocation_Status)
        with Pre => Valid (Store) and then Fits_Source (Data, First_Byte, Byte_Length, Offset, Bits),
          Post => Valid (Store) and then Extends (Store, Store'Old)
            and then Status in Allocated | Object_Limit | Generation_Limit | Byte_Limit
            and then (if Status = Allocated then
              Fresh_Allocation (Store, Store'Old, ID) and then Is_Live (Store, ID)
              and then Kind (Store, ID) = Buffer_Object
              and then Live_Count (Store) = Live_Count (Store'Old) + 1
              and then Length (Store, ID) = Bits / 8 + (if Bits mod 8 = 0 then 0 else 1)
              and then Byte_Count (Store) = Byte_Count (Store'Old) + Length (Store, ID)
              and then Element_Count (Store) = Element_Count (Store'Old)
              and then (for all I in 1 .. Length (Store, ID) =>
                Stored_Byte (Store, ID, I - 1) = Window
                  (Data, First_Byte, Byte_Length, Offset + (I - 1) * 8,
                   Positive'Min (8, Bits - (I - 1) * 8)))
             else Store = Store'Old and then ID = No_Object);
   end Bit_Buffer_Initializers;
   -- Immutable reader source; no reference escapes the allocation. This
   -- stores general string bytes, including NUL/non-ASCII conversion results.
   -- Literal syntax validation belongs to AML_Decode.Read_String_Span.
   generic
      type Source_Type (<>) is limited private;
      with function Source_Length (Data : aliased Source_Type) return Natural;
      with function Source_Byte (Data : aliased Source_Type; Index : Natural)
         return AML_Decode.Byte;
   package String_Initializers is
      function Fits_Source
        (Data : aliased Source_Type; First_Byte, Byte_Length : Natural)
         return Boolean is
        (First_Byte <= Source_Length (Data) and then
         Byte_Length <= Source_Length (Data) - First_Byte);
      procedure Allocate
        (Store : in out State; Data : aliased Source_Type;
         First_Byte, Byte_Length : Natural; ID : out Object_ID;
         Status : out Allocation_Status)
        with Pre => Valid (Store) and then Fits_Source (Data, First_Byte, Byte_Length),
          Post => Valid (Store) and then Extends (Store, Store'Old)
            and then Status in Allocated | Object_Limit | Generation_Limit | Byte_Limit
            and then (if Status = Allocated then
              Fresh_Allocation (Store, Store'Old, ID) and then Is_Live (Store, ID)
              and then Kind (Store, ID) = String_Object
              and then Live_Count (Store) = Live_Count (Store'Old) + 1
              and then Length (Store, ID) = Byte_Length
              and then Byte_Count (Store) = Byte_Count (Store'Old) + Byte_Length
              and then Element_Count (Store) = Element_Count (Store'Old)
              and then (for all I in 1 .. Byte_Length =>
                Stored_Byte (Store, ID, I - 1) = Source_Byte (Data, First_Byte + (I - 1)))
             else Store = Store'Old and then ID = No_Object);
   end String_Initializers;
   procedure New_Package (Store : in out State; Size : Natural;
                          ID : out Object_ID; Status : out Allocation_Status)
     with Pre => Valid (Store),
          Post => Valid (Store) and then Extends (Store, Store'Old) and then
            (if Status = Allocated then
               Fresh_Allocation (Store, Store'Old, ID)
               and then Live_Count (Store) = Live_Count (Store'Old) + 1 and then Is_Live (Store, ID)
               and then Kind (Store, ID) = Package_Object and then Length (Store, ID) = Size
               and then (for all I in 1 .. Size => Element (Store, ID, I - 1) = 0)
               and then (for all J in 1 .. Slot_Bound (Store'Old) => (if Is_Live (Store'Old, J) then Kind (Store, J) = Kind (Store'Old, J)
                 and then Length (Store, J) = Length (Store'Old, J)))
             else Store = Store'Old and ID = 0);
   function Element_Updated
     (Store, Prior : State; ID : Object_ID; Index : Natural; Value : Object_ID)
     return Boolean with Ghost,
     Pre => Valid (Prior) and then Is_Live (Prior, ID)
       and then Kind (Prior, ID) = Package_Object and then Index < Length (Prior, ID);
   procedure Set_Element (Store : in out State; ID : Object_ID; Index : Natural; Value : Object_ID)
     with Pre => Valid (Store) and then Is_Live (Store, ID)
       and then Kind (Store, ID) = Package_Object and then Index < Length (Store, ID)
       and then (Value = No_Object or else Is_Live (Store, Value)),
          Post => Valid (Store) and then Slot_Bound (Store) = Slot_Bound (Store'Old)
            and then Usage_Of (Store) = Usage_Of (Store'Old)
            and then Element_Updated (Store, Store'Old, ID, Index, Value)
            and then Kind (Store, ID) = Package_Object
            and then Length (Store, ID) = Length (Store'Old, ID)
            and then Element (Store, ID, Index) = Value
            and then (for all J in 1 .. Slot_Bound (Store'Old) => (if Is_Live (Store'Old, J) then
              Kind (Store, J) = Kind (Store'Old, J)
              and then Length (Store, J) = Length (Store'Old, J)
              and then (if Kind (Store'Old, J) = Integer_Object then
                Integer_Data (Store, J) = Integer_Data (Store'Old, J))));
private
   type Object_Record is record
      Occupied : Boolean := False;
      Stamp : AML_Object_Identifiers.Slot_Incarnation := AML_Object_Identifiers.No_Incarnation;
      Tag : Object_Kind := Integer_Object;
      Value : AML_Decode.Integer_Value := 0;
      Origin : AML_Decode.Integer_Origin := AML_Decode.Ordinary_Integer;
      Reference : AML_References.Reference := AML_References.No_Reference;
      Snapshot : AML_Metadata_Kinds.Snapshot_Description := (Object_Type => AML_Metadata_Kinds.Device_Metadata);
      First : Natural := 0;
      Size : Natural := 0;
   end record;
   type Object_Array is array (Positive range 1 .. Max_Objects) of Object_Record;
   type Element_Array is array (Positive range 1 .. Max_Elements) of Object_ID;
   type State is record
      Used : Object_ID := 0;
      Live_Used : Object_ID := 0;
      Generation_Limit : AML_Object_Identifiers.Incarnation_Budget := AML_Object_Identifiers.Slot_Incarnation'Last;
      Bytes_Used : Natural range 0 .. Max_Bytes := 0;
      Elements_Used : Natural range 0 .. Max_Elements := 0;
      Objects : Object_Array;
      Bytes : AML_Decode.Bytes (1 .. Max_Bytes) := [others => 0];
      Elements : Element_Array := [others => 0];
   end record;
end AML_Objects;

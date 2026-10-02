pragma Ada_2022;
with AML_Decode;
package AML_Objects with SPARK_Mode, Pure is
   use type AML_Decode.Integer_Value;
   use type AML_Decode.Bytes;
   Max_Objects : constant := 2048;
   Max_Bytes : constant := 65_536;
   Max_Elements : constant := 8192;
   subtype Object_ID is Natural range 0 .. Max_Objects;
   No_Object : constant Object_ID := 0;
   type Object_Kind is (Integer_Object, String_Object, Buffer_Object, Package_Object);
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
   function Count (Store : State) return Object_ID;
   function Byte_Count (Store : State) return Natural;
   function Element_Count (Store : State) return Natural;
   function Empty return State with Post => Valid (Empty'Result) and then Count (Empty'Result) = 0
     and then Usage_Of (Empty'Result) = Usage'(others => 0);
   function Kind (Store : State; ID : Object_ID) return Object_Kind
     with Pre => ID > 0 and ID <= Count (Store);
   function Length (Store : State; ID : Object_ID) return Natural
     with Pre => ID > 0 and ID <= Count (Store);
   function Integer_Data (Store : State; ID : Object_ID) return AML_Decode.Integer_Value
     with Pre => ID > 0 and then ID <= Count (Store) and then Kind (Store, ID) = Integer_Object;
   function Byte_Data (Store : State; ID : Object_ID) return AML_Decode.Bytes
     with Pre => Valid (Store) and then ID > 0 and then ID <= Count (Store)
       and then Kind (Store, ID) in Byte_Kind;
   function Element (Store : State; ID : Object_ID; Index : Natural) return Object_ID
     with Pre => Valid (Store) and then ID > 0 and then ID <= Count (Store)
       and then Kind (Store, ID) = Package_Object and then Index < Length (Store, ID),
          Post => Element'Result <= Count (Store);
   -- Exact frame condition: only this existing integer's payload changes.
   -- Object identity, package links and every allocation counter are retained.
   function Integer_Updated
     (Store, Prior : State; ID : Object_ID; Value : AML_Decode.Integer_Value)
      return Boolean with Ghost,
      Pre => ID > 0 and then ID <= Count (Prior);
   procedure Set_Integer
     (Store : in out State; ID : Object_ID; Value : AML_Decode.Integer_Value)
     with Pre => Valid (Store) and then ID > 0 and then ID <= Count (Store)
       and then Kind (Store, ID) = Integer_Object,
       Post => Valid (Store) and then Usage_Of (Store) = Usage_Of (Store'Old)
         and then Count (Store) = Count (Store'Old)
         and then Integer_Updated (Store, Store'Old, ID, Value)
         and then Integer_Data (Store, ID) = Value
         and then (for all J in 1 .. Count (Store) =>
           Kind (Store, J) = Kind (Store'Old, J)
           and then Length (Store, J) = Length (Store'Old, J));
   --  Handles are arena-local, never reused by this append-only allocator.
   --  Zero denotes an uninitialized package element, not an Integer zero.
   type Allocation_Status is (Allocated, Object_Limit, Byte_Limit, Element_Limit);
   procedure New_Integer (Store : in out State; Value : AML_Decode.Integer_Value;
                          ID : out Object_ID; Status : out Allocation_Status)
     with Pre => Valid (Store),
          Post => Valid (Store) and then Extends (Store, Store'Old) and then
            (if Status = Allocated then
               Count (Store) = Count (Store'Old) + 1 and then ID = Count (Store)
               and then Kind (Store, ID) = Integer_Object
               and then Integer_Data (Store, ID) = Value
               and then (for all J in 1 .. Count (Store'Old) => Kind (Store, J) = Kind (Store'Old, J)
                 and then Length (Store, J) = Length (Store'Old, J))
             else Store = Store'Old and ID = 0);
   procedure New_Bytes (Store : in out State; Tag : Byte_Kind; Data : AML_Decode.Bytes;
                        ID : out Object_ID; Status : out Allocation_Status)
     with Pre => Valid (Store),
          Post => Valid (Store) and then Extends (Store, Store'Old) and then
            (if Status = Allocated then
               Count (Store) = Count (Store'Old) + 1 and then ID = Count (Store)
               and then Kind (Store, ID) = Tag
               and then Byte_Data (Store, ID) = Data
               and then (for all J in 1 .. Count (Store'Old) => Kind (Store, J) = Kind (Store'Old, J)
                 and then Length (Store, J) = Length (Store'Old, J))
             else Store = Store'Old and ID = 0);
   procedure New_Package (Store : in out State; Size : Natural;
                          ID : out Object_ID; Status : out Allocation_Status)
     with Pre => Valid (Store),
          Post => Valid (Store) and then Extends (Store, Store'Old) and then
            (if Status = Allocated then
               Count (Store) = Count (Store'Old) + 1 and then ID = Count (Store)
               and then Kind (Store, ID) = Package_Object and then Length (Store, ID) = Size
               and then (for all I in 1 .. Size => Element (Store, ID, I - 1) = 0)
               and then (for all J in 1 .. Count (Store'Old) => Kind (Store, J) = Kind (Store'Old, J)
                 and then Length (Store, J) = Length (Store'Old, J))
             else Store = Store'Old and ID = 0);
   procedure Set_Element (Store : in out State; ID : Object_ID; Index : Natural; Value : Object_ID)
     with Pre => Valid (Store) and then ID > 0 and then ID <= Count (Store)
       and then Kind (Store, ID) = Package_Object and then Index < Length (Store, ID)
       and then Value <= Count (Store),
          Post => Valid (Store) and then Count (Store) = Count (Store'Old)
            and then Kind (Store, ID) = Package_Object
            and then Length (Store, ID) = Length (Store'Old, ID)
            and then Element (Store, ID, Index) = Value
            and then (for all J in 1 .. Count (Store'Old) =>
              Kind (Store, J) = Kind (Store'Old, J)
              and then Length (Store, J) = Length (Store'Old, J));
private
   type Object_Record is record
      Tag : Object_Kind := Integer_Object;
      Value : AML_Decode.Integer_Value := 0;
      First : Natural := 0;
      Size : Natural := 0;
   end record;
   type Object_Array is array (Positive range 1 .. Max_Objects) of Object_Record;
   type Element_Array is array (Positive range 1 .. Max_Elements) of Object_ID;
   type State is record
      Used : Object_ID := 0;
      Bytes_Used : Natural range 0 .. Max_Bytes := 0;
      Elements_Used : Natural range 0 .. Max_Elements := 0;
      Objects : Object_Array;
      Bytes : AML_Decode.Bytes (1 .. Max_Bytes) := [others => 0];
      Elements : Element_Array := [others => 0];
   end record;
end AML_Objects;

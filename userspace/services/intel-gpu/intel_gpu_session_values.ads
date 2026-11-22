with Interfaces;
with Intel_GPU_Slot_Storage;
-- Intel_GPU_Session_Store for plain values with an explicit initial value.
generic
   with package Storage is new Intel_GPU_Slot_Storage (<>);
   type Slot is range <>;
   type Element is private;
   Default : Element;
package Intel_GPU_Session_Values is
   type Element_Array is array (Slot) of Element;
   Region : Storage.Region :=
     Storage.Open (Interfaces.Unsigned_64 (Element_Array'Max_Size_In_Storage_Elements));
   Items : Element_Array with Import, Address => Storage.Address (Region);
   function Grown return Slot'Base;
   procedure Grow (To : Slot'Base; OK : out Boolean)
     with Pre => To in Slot'First - 1 .. Slot'Last;
   procedure Reset (Index : Slot)
     with Pre => Index <= Grown;
private
   Count : Slot'Base := Slot'First - 1;
   function Grown return Slot'Base is (Count);
end Intel_GPU_Session_Values;

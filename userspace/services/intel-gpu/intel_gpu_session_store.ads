with Interfaces;
with Intel_GPU_Slot_Storage;
-- One per-session table of the driver (Element per session slot), stored in
-- its own Intel_GPU_Slot_Storage region: address space for every slot the
-- hardware can name, pages committed as the table grows. Items is an
-- ordinary array object at a fixed address; only Items (Slot'First ..
-- Grown) is backed and may be touched.
generic
   with package Storage is new Intel_GPU_Slot_Storage (<>);
   type Slot is range <>;
   type Element is limited private;
package Intel_GPU_Session_Store is
   type Element_Array is array (Slot) of Element;
   Region : Storage.Region :=
     Storage.Open (Interfaces.Unsigned_64 (Element_Array'Max_Size_In_Storage_Elements));
   Items : Element_Array with Import, Address => Storage.Address (Region);
   function Grown return Slot'Base;
   -- Back Items up to To and default-initialize the new elements (Element's
   -- own default values; memory is zeroed first). OK = False leaves Grown
   -- unchanged.
   procedure Grow (To : Slot'Base; OK : out Boolean)
     with Pre => To in Slot'First - 1 .. Slot'Last;
   -- Return one backed element to its default values, as Grow made it.
   procedure Reset (Index : Slot)
     with Pre => Index <= Grown;
private
   Count : Slot'Base := Slot'First - 1;
   function Grown return Slot'Base is (Count);
end Intel_GPU_Session_Store;

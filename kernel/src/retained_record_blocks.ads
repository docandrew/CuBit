pragma Ada_2022;
with System;
with System.Address_To_Access_Conversions;
with System.Storage_Elements;

-- The caller serializes every operation and supplies fresh, disjoint, writable
-- backing. Published blocks are retained for the store's entire lifetime.
-- This address adapter is Ada, not a SPARK-proved memory-safety boundary.
generic
   type Element is private;
   Maximum_Index : Natural;
   Records_Per_Block : Positive;
   with function Allocate
     (Bytes, Alignment : System.Storage_Elements.Storage_Count)
      return System.Address;
package Retained_Record_Blocks is
   subtype Slot is Natural range 0 .. Maximum_Index;
   Maximum_Blocks : constant Positive := Maximum_Index / Records_Per_Block + 1;
   type Element_Access is access all Element;
   type Store is limited private;
   type Allocation_Result is
     (Existing, Added, At_Quota, Out_Of_Memory, Invalid_Backing);

   function Find (Storage : Store; Index : Slot) return Element_Access;
   function Allocated_Blocks (Storage : Store) return Natural;
   procedure Ensure
     (Storage : in out Store; Index : Slot; Initial : Element;
      Block_Limit : Natural; Value : out Element_Access;
      Result : out Allocation_Result);
   -- Iterate committed blocks, skipping absent blocks rather than each slot.
   procedure Next_Present
     (Storage : Store; From : Slot; Index : out Slot; Found : out Boolean);
private
   type Block is array (Natural range 0 .. Records_Per_Block - 1)
     of aliased Element;
   package Pointers is new System.Address_To_Access_Conversions (Block);
   type Directory is array (Natural range 0 .. Maximum_Blocks - 1)
     of Pointers.Object_Pointer;
   type Store is limited record
      Blocks : Directory := [others => null];
      Count : Natural := 0;
   end record;
end Retained_Record_Blocks;

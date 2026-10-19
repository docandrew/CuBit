with System;
with Desktop_Heap_Objects;
package body Desktop_Icon_Store.Atlas_Memory with SPARK_Mode => Off,
  Refined_State => (Block => Pixels)
is
   use type System.Address;
   subtype Atlas_Buffer is Q.Pixel_Buffer (Atlas_Index);
   package Heap is new Desktop_Heap_Objects (Atlas_Buffer);
   use type Heap.Reference;

   Pixels : Heap.Reference := null;

   function Reserved return Boolean is (Pixels /= null);

   procedure Reserve (Done : out Boolean) is
      Where : System.Address;
   begin
      if Pixels = null then
         Where := Heap.Allocate (Atlas_Buffer'Max_Size_In_Storage_Elements);
         if Where /= System.Null_Address then
            Pixels := Heap.To_Reference (Where);
            --  Transparent until decoded: a short atlas never shows garbage.
            --  (A loop, not an aggregate: no stack temporary of the block.)
            for Index in Atlas_Index loop
               Pixels (Index) := 0;
            end loop;
         end if;
      end if;
      Done := Pixels /= null;
   end Reserve;

   procedure Feed (D : in out Q.Decoder; Input : Q.Byte_Array) is
   begin
      Q.Feed (D, Input, Pixels.all);
   end Feed;

   function Read (Index : Atlas_Index) return Interfaces.Unsigned_32 is
     (if Pixels = null then 0 else Pixels (Index));
end Desktop_Icon_Store.Atlas_Memory;

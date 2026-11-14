pragma Ada_2022;
with Interfaces; use Interfaces;
with System.Storage_Elements; use System.Storage_Elements;

package body Heap_Arrays with SPARK_Mode => Off is
   use type System.Address;

   --  CuBit's process heap (as System.Memory uses it): zero when it refuses.
   function CuAlloc_Allocate (Bytes, Alignment : Unsigned_64) return Unsigned_64
   with Import, Convention => C, External_Name => "cualloc_allocate";
   procedure CuAlloc_Free (Block : Unsigned_64)
   with Import, Convention => C, External_Name => "cualloc_free";

   type Element_Array is array (Natural range <>) of Element;
   Element_Bytes : constant Unsigned_64 := Element_Array'Component_Size / System.Storage_Unit;
   Element_Alignment : constant Unsigned_64 := Element'Alignment;
   No_Block : constant Unsigned_64 := 0;

   function Capacity (B : Block) return Natural is (B.Size);
   function Empty return Block is ((Base => System.Null_Address, Size => 0));

   function Get (B : Block; Index : Natural) return Element is
      Data : constant Element_Array (0 .. B.Size - 1) with Import, Address => B.Base;
   begin
      return Data (Index);
   end Get;

   procedure Set (B : in out Block; Index : Natural; Item : Element) is
      Data : Element_Array (0 .. B.Size - 1) with Import, Address => B.Base;
   begin
      Data (Index) := Item;
   end Set;

   procedure Allocate (B : out Block; Size : Positive) is
      Raw : Unsigned_64;
   begin
      B := Empty;
      if Element_Bytes = 0 or else Unsigned_64 (Size) > Unsigned_64'Last / Element_Bytes then
         return;
      end if;
      Raw := CuAlloc_Allocate (Unsigned_64 (Size) * Element_Bytes, Element_Alignment);
      if Raw /= No_Block then
         B := (Base => To_Address (Integer_Address (Raw)), Size => Size);
      end if;
   end Allocate;

   procedure Release (B : in out Block) is
   begin
      if B.Base /= System.Null_Address then
         CuAlloc_Free (Unsigned_64 (To_Integer (B.Base)));
      end if;
      B := Empty;
   end Release;
end Heap_Arrays;

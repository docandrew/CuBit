with Interfaces; use Interfaces;
-- Dedicated user mapping aperture: above the heap and the image, below the
-- received grants (MEM-007, K4: 48 TiB, room for Ladybird's 4 TiB cage and
-- its GC regions many times over; it was 1 TiB). The heap (sbrk) stops at
-- First. No legacy mapper may write into it.
package Owned_Memory_Layout with SPARK_Mode => On is
   First : constant Unsigned_64 := 16#0000_1000_0000_0000#;
   Limit : constant Unsigned_64 := 16#0000_4000_0000_0000#;
   -- Half-open byte interval; malformed/empty/wrapping queries fail closed.
   function Conflicts (Base, Bytes : Unsigned_64) return Boolean is
     (Bytes = 0 or else Bytes > Unsigned_64'Last - Base or else
      (Base < Limit and then
       (Base >= First or else Bytes > First - Base)))
   with Post => Conflicts'Result =
     (if Bytes = 0 or else Bytes > Unsigned_64'Last - Base then True
      else Base < Limit and Base + Bytes > First);
end Owned_Memory_Layout;

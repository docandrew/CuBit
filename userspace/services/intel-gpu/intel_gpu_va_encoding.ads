with Interfaces; use Interfaces;
package Intel_GPU_VA_Encoding with SPARK_Mode is
   -- GPU address representation only; never CPU/DMA translation or authority.
   -- Allocators use raw48-bit offsets. Command consumers may require bit47
   -- sign extension, as in Mesa intel_gem.h:intel_canonical_address.
   subtype Raw_Address is Unsigned_64 range 0 .. 16#0000_FFFF_FFFF_FFFF#;
   function Canonical (Raw : Raw_Address) return Unsigned_64 is
     (if Raw < 2 ** 47 then Raw else Raw or 16#FFFF_0000_0000_0000#)
   with Post => (Canonical'Result and 16#0000_FFFF_FFFF_FFFF#) = Raw and then
     (if Raw < 2 ** 47 then Shift_Right (Canonical'Result, 48) = 0
      else Shift_Right (Canonical'Result, 48) = 65535);
   function Is_Canonical (Value : Unsigned_64) return Boolean is
     (Value = Canonical (Value and 16#0000_FFFF_FFFF_FFFF#));
   type Decoded_Address is record
      Valid : Boolean := False;
      Raw : Raw_Address := 0;
   end record;
   -- Reject malformed upper bits before truncation. In particular2**48 is a
   -- possible exclusive allocator limit, not an address that aliases zero.
   function Decode (Value : Unsigned_64) return Decoded_Address is
     (if Is_Canonical (Value) then
        (Valid => True, Raw => Value and 16#0000_FFFF_FFFF_FFFF#)
      else (Valid => False, Raw => 0))
   with Post => Decode'Result.Valid = Is_Canonical (Value) and then
     (if Decode'Result.Valid then Canonical (Decode'Result.Raw) = Value
      else Decode'Result.Raw = 0);
end Intel_GPU_VA_Encoding;

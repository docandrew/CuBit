with Interfaces; use Interfaces;
package Intel_GPU_Image_Layout with Pure, SPARK_Mode is
   -- Internal decoded metadata, not a wire record or an Intel register.
   -- This baseline describes linear packed bytes only. A valid layout does
   -- not prove GPU import/scanout support, allocation ownership or completion.
   type Pixel_Format is (Unsupported_Format, BGRA8_UNorm);
   type Storage_Layout is (Unsupported_Layout, Linear);
   type Descriptor is record
      Format : Pixel_Format := Unsupported_Format;
      Layout : Storage_Layout := Unsupported_Layout;
      Width, Height : Unsigned_32 := 0;
      Pitch, Offset : Unsigned_64 := 0;
   end record;
   function Valid (Image : Descriptor; Backing_Bytes : Unsigned_64)
      return Boolean;
   -- Span includes inter-row padding but not unused final-row padding.
   -- Zero is never a valid image span. All arithmetic is checked before use.
   function Span (Image : Descriptor; Backing_Bytes : Unsigned_64)
      return Unsigned_64 with
     Post => (if Valid (Image, Backing_Bytes) then
                Span'Result > 0 and then
                Span'Result <= Backing_Bytes - Image.Offset
              else Span'Result = 0);
end Intel_GPU_Image_Layout;

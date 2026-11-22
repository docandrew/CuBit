package body Intel_GPU_Image_Layout with SPARK_Mode is
   function Valid (Image : Descriptor; Backing_Bytes : Unsigned_64)
      return Boolean is
     (Image.Format = BGRA8_UNorm and then Image.Layout = Linear and then
      Image.Width > 0 and then Image.Height > 0 and then
      Image.Pitch >= Unsigned_64 (Image.Width) * 4 and then
      Image.Pitch mod 4 = 0 and then Image.Offset mod 4 = 0 and then
      Image.Offset <= Backing_Bytes and then
      Unsigned_64 (Image.Width) * 4 <= Backing_Bytes - Image.Offset and then
      -- Division bounds the product without ever constructing a wrapped span.
      Unsigned_64 (Image.Height - 1) <=
        (Backing_Bytes - Image.Offset - Unsigned_64 (Image.Width) * 4) / Image.Pitch and then
      -- Retain an explicit checked-product bound for downstream modular
      -- arithmetic proofs; the preceding division rejects wraparound first.
      Unsigned_64 (Image.Height - 1) * Image.Pitch <=
        Backing_Bytes - Image.Offset - Unsigned_64 (Image.Width) * 4 and then
      Unsigned_64 (Image.Height - 1) * Image.Pitch + Unsigned_64 (Image.Width) * 4 <=
        Backing_Bytes - Image.Offset);
   function Span (Image : Descriptor; Backing_Bytes : Unsigned_64)
      return Unsigned_64 is
   begin
      if not Valid (Image, Backing_Bytes) then return 0; end if;
      return Unsigned_64 (Image.Height - 1) * Image.Pitch +
        Unsigned_64 (Image.Width) * 4;
   end Span;
end Intel_GPU_Image_Layout;

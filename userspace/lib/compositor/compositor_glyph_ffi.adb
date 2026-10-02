package body Compositor_Glyph_FFI with SPARK_Mode => Off is
   subtype Word is Interfaces.Unsigned_32;
   use type Word, Interfaces.Unsigned_64, System.Address;
   type Request is record
      Font, Code, Em_Numerator, Em_Denominator, Width, Height, Pitch, Capacity : Word;
   end record with Convention => C;
   type Metrics is record
      Advance, Height : Word;
   end record with Convention => C;
   function Native (Value : access constant Request; Pixels : System.Address;
                    Result : access Metrics) return Word
     with Import, Convention => C, External_Name => "cubit_font_raster_mask";
   procedure Rasterize
     (Font, Code : Interfaces.Unsigned_32;
      Layout : Compositor_Glyph_Layout.Layout;
      Pixels : System.Address; Capacity : Interfaces.Unsigned_64;
      Advance : out Natural; Completed : out Boolean) is
      Value : aliased Request :=
        (Font, Code, Word (Layout.Em_Numerator), Word (Layout.Em_Denominator),
         Word (Layout.Width), Word (Layout.Height), Word (Layout.Pitch), Word (Layout.Bytes));
      Result : aliased Metrics := (0, 0);
      Status : Word;
   begin
      Advance := 0; Completed := False;
      if not Compositor_Glyph_Layout.Valid (Layout) or else
        Pixels = System.Null_Address or else Capacity < Interfaces.Unsigned_64 (Layout.Bytes)
      then return; end if;
      pragma Assert (Request'Size = 32 * 8 and Metrics'Size = 8 * 8);
      Status := Native (Value'Access, Pixels, Result'Access);
      if Status /= 0 or else Result.Advance not in 1 .. Word (Layout.Width) or else
        Result.Height /= Word (Layout.Height)
      then return; end if;
      Advance := Natural (Result.Advance); Completed := True;
   end Rasterize;
end Compositor_Glyph_FFI;

with Intel_GPU_GGTT;
package body Intel_GPU_Scanout_Range with SPARK_Mode is
   function Linear
     (Table_Bytes, Surface, Pitch, Width, Height, X, Y,
      Pixel_Bytes : Unsigned_64) return Extent
   is
      Limit, Rows, Span, Rounded : Unsigned_64;
   begin
      if Table_Bytes = 0 or else
        Table_Bytes > Intel_GPU_GGTT.Maximum_Table_Bytes or else
        Table_Bytes mod 4096 /= 0 or else Surface mod 4096 /= 0 or else
        Pitch = 0 or else Width = 0 or else Height = 0 or else
        Pixel_Bytes not in 1 | 2 | 4 | 8
      then return (others => <>); end if;
      Limit := Table_Bytes / 8 * 4096;
      if Surface >= Limit then return (others => <>); end if;
      -- Divide before multiplying: malformed dimensions must not wrap into
      -- an apparently small valid footprint.
      if X > Pitch / Pixel_Bytes or else
        Width > Pitch / Pixel_Bytes - X or else
        Y > Unsigned_64'Last - Height
      then return (others => <>); end if;
      Rows := Y + Height;
      if Rows > (Limit - Surface) / Pitch then
         return (others => <>);
      end if;
      Span := Rows * Pitch;
      if Span = 0 then return (others => <>); end if;
      Rounded := ((Span - 1) / 4096 + 1) * 4096;
      if Rounded = 0 or else Rounded < Span or else Rounded > Limit - Surface
      then return (others => <>); end if;
      return (True, Surface, Rounded);
   end Linear;
end Intel_GPU_Scanout_Range;

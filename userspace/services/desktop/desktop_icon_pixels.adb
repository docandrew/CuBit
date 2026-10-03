package body Desktop_Icon_Pixels with SPARK_Mode is
   function Pixel (Item : Asset; X, Y : Natural) return Interfaces.Unsigned_32 is
   begin
      return (if Item.Kind = Application then Desktop_Icons.Pixels (Item.Icon) (Y * Size (Item) + X)
      else Desktop_Window_Icons.Pixels (Item.Control) (Y * Size (Item) + X));
   end Pixel;
   procedure Copy_Chunk (Item : Asset; Target : in out Pixels;
      Plan : U.Plan; Complete : out Boolean) is
      use type U.Pixel_Format;
      A : constant U.Rectangle := U.Area (Plan);
      Stride : constant Natural := (if U.Row_Length (Plan) = 0 then A.Width else U.Row_Length (Plan));
   begin
      Complete := False;
      if not U.Valid (Plan) or else U.Format (Plan) /= U.BGRA8 or else
        U.Image_Width (Plan) /= Size (Item) or else U.Image_Height (Plan) /= Size (Item) or else
        U.Capacity (Plan) > Target'Length * 4 then return; end if;
      -- Check the complete destination span before the first store. Wide
      -- arithmetic also covers malformed row lengths without machine overflow.
      if Long_Long_Integer (U.Buffer_Offset (Plan)) / 4 +
        Long_Long_Integer (A.Height - 1) * Long_Long_Integer (Stride) +
        Long_Long_Integer (A.Width) > Long_Long_Integer (Target'Length)
      then return; end if;
      for Y in 0 .. A.Height - 1 loop
         for X in 0 .. A.Width - 1 loop
            Target (Natural (Long_Long_Integer (U.Buffer_Offset (Plan)) / 4 +
              Long_Long_Integer (Y) * Long_Long_Integer (Stride) + Long_Long_Integer (X))) :=
              Pixel (Item, A.X + X, A.Y + Y);
         end loop;
      end loop;
      Complete := True;
   end Copy_Chunk;
end Desktop_Icon_Pixels;

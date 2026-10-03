package body Desktop_Icon_Pixels.Atlases with SPARK_Mode is
   function Region (Item : Asset) return R.Rectangle is
      Row : constant Natural := (if Item.Kind = Application then Desktop_Icons.Icon_ID'Pos (Item.Icon)
        else Desktop_Window_Icons.Icon_ID'Pos (Item.Control));
      Edge : constant Positive := Size (Item);
   begin
      return (0, R.Word (Row * Edge), R.Word (Edge), R.Word (Edge),
        R.Word (Width (Item.Kind)), R.Word (Height (Item.Kind)));
   end Region;
   function Cursor_Region (Item : Desktop_Cursors.Cursor_ID) return R.Rectangle is
      M : constant Desktop_Cursors.Cursor_Metadata := Desktop_Cursors.Metadata (Item);
   begin
      return (0, R.Word (Cursor_Row (Item)), R.Word (M.Width), R.Word (M.Height),
        R.Word (Width (Window_Control)), R.Word (Height (Window_Control)));
   end Cursor_Region;
   function Cursor_Value (Item : Desktop_Cursors.Cursor_ID; X, Y : Natural)
     return Interfaces.Unsigned_32 is
      M : constant Desktop_Cursors.Cursor_Metadata := Desktop_Cursors.Metadata (Item);
   begin
      return Desktop_Cursors.Pixels (M.Offset + Y * M.Width + X);
   end Cursor_Value;
   function Pixel (Kind : Family; X, Y : Natural) return Interfaces.Unsigned_32 is
   begin
      if Kind = Application then
         return Desktop_Icon_Pixels.Pixel
           ((Application, Desktop_Icons.Icon_ID'Val (Y / 24)), X, Y mod 24);
      elsif Y < 45 then
         if X >= 9 then return 0; end if;
         return Desktop_Icon_Pixels.Pixel
           ((Window_Control, Desktop_Window_Icons.Icon_ID'Val (Y / 9)), X, Y mod 9);
      else
         for Item in Desktop_Cursors.Cursor_ID loop
            declare
               M : constant Desktop_Cursors.Cursor_Metadata := Desktop_Cursors.Metadata (Item);
               Row : constant Natural := Cursor_Row (Item);
            begin
               if Y >= Row and then Y - Row < M.Height and then X < M.Width then
                  return Cursor_Value (Item, X, Y - Row);
               end if;
            end;
         end loop;
         return 0;
      end if;
   end Pixel;
   procedure Copy_Chunk (Kind : Family; Target : in out Pixels;
      Plan : U.Plan; Complete : out Boolean) is
      use type U.Pixel_Format;
      A : constant U.Rectangle := U.Area (Plan);
      Stride : constant Natural := (if U.Row_Length (Plan) = 0 then A.Width else U.Row_Length (Plan));
   begin
      Complete := False;
      if not U.Valid (Plan) or else U.Format (Plan) /= U.BGRA8 or else
        U.Image_Width (Plan) /= Width (Kind) or else U.Image_Height (Plan) /= Height (Kind) or else
        U.Capacity (Plan) > Target'Length * 4 then return; end if;
      if Long_Long_Integer (U.Buffer_Offset (Plan)) / 4 +
        Long_Long_Integer (A.Height - 1) * Long_Long_Integer (Stride) +
        Long_Long_Integer (A.Width) > Long_Long_Integer (Target'Length)
      then return; end if;
      for Y in 0 .. A.Height - 1 loop
         for X in 0 .. A.Width - 1 loop
            Target (Natural (Long_Long_Integer (U.Buffer_Offset (Plan)) / 4 +
              Long_Long_Integer (Y) * Long_Long_Integer (Stride) + Long_Long_Integer (X))) :=
              Pixel (Kind, A.X + X, A.Y + Y);
         end loop;
      end loop;
      Complete := True;
   end Copy_Chunk;
end Desktop_Icon_Pixels.Atlases;

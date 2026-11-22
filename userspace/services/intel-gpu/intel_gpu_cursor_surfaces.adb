package body Intel_GPU_Cursor_Surfaces with SPARK_Mode is
   function Pick (Items : Bases; Programmed, Live : Unsigned_32)
      return Surface_Index is
   begin
      if Writable (Items, 1, Programmed, Live) then
         return 1;
      elsif Writable (Items, 2, Programmed, Live) then
         return 2;
      end if;
      --  Surfaces 1 and 2 are distinct and each is armed or live, so they
      --  are those two values; the distinct third one is neither.
      pragma Assert (Writable (Items, 3, Programmed, Live));
      return 3;
   end Pick;

   function Fit (Item : Extent) return CP.Image is
      Need : constant Positive := Integer'Max (Item.Width, Item.Height);
      Width : CP.Cursor_Width;
   begin
      if Need <= CP.Pixels (CP.Width_64) then
         Width := CP.Width_64;
      elsif Need <= CP.Pixels (CP.Width_128) then
         Width := CP.Width_128;
      else
         Width := CP.Width_256;
      end if;
      return (Width => Width,
              Height => Integer'Max (CP.Minimum_Height, Item.Height),
              Base => CP.Base_Alignment);
   end Fit;
end Intel_GPU_Cursor_Surfaces;

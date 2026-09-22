package body CuBit.Graphics_Metrics with SPARK_Mode is
   procedure Add (Item : in out Counter; Bytes : Unsigned_64) is
   begin
      if Item.Overflowed or else Bytes = 0 then
         return;
      elsif Bytes > Unsigned_64'Last - Item.Bytes or else
            Item.Regions = Unsigned_64'Last
      then
         Item.Overflowed := True;
      else
         Item.Bytes := Item.Bytes + Bytes;
         Item.Regions := Item.Regions + 1;
      end if;
   end Add;
end CuBit.Graphics_Metrics;

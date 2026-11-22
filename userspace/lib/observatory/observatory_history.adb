package body Observatory_History with SPARK_Mode is
   function Sample_At (Item : History; Position : Index) return Sample is
      Slot : constant Index :=
        (if Item.Used < Capacity then Position else (Item.Next + Position) mod Capacity);
   begin return Item.Values (Slot); end Sample_At;
   procedure Clear (Item : out History) is
   begin Item := (others => <>); end Clear;
   procedure Break_Continuity (Item : in out History) is
   begin Item.Continuous := False; end Break_Continuity;
   procedure Observe (Item : in out History; Series : Identity;
      Total, Upper : Unsigned_64; Lossy : Boolean) is
      Value : Sample;
   begin
      if Item.Used > 0 and then (Series /= Item.Series or else Total < Item.Last_Total) then
         Clear (Item);
      end if;
      Value := (Upper => Upper, Added => 0, Has_Latency => Total > 0,
                Has_Delta => Item.Continuous, Lossy => Lossy);
      if Item.Continuous then
         pragma Assert (Total >= Item.Last_Total);
         Value.Added := Total - Item.Last_Total;
      end if;
      Item.Values (Item.Next) := Value;
      Item.Next := (if Item.Next = Index'Last then 0 else Item.Next + 1);
      if Item.Used < Capacity then Item.Used := Item.Used + 1; end if;
      Item.Series := Series; Item.Last_Total := Total; Item.Continuous := True;
   end Observe;
   function Scale (Value, Maximum : Unsigned_64; Height : Positive_Height)
     return Pixel_Height is
      Remainder : Unsigned_64 := 0;
      Pixels : Pixel_Height := 0;
   begin
      if Maximum = 0 then return 0; end if;
      if Value = Maximum then return Height; end if;
      for Step in 1 .. Height loop
         if Remainder >= Maximum - Value then
            Remainder := Remainder - (Maximum - Value);
            Pixels := Pixels + 1;
         else
            pragma Assert (Remainder <= Unsigned_64'Last - Value);
            Remainder := Remainder + Value;
         end if;
         pragma Loop_Invariant (Remainder < Maximum);
         pragma Loop_Invariant (Pixels <= Step);
      end loop;
      return Pixels;
   end Scale;
end Observatory_History;

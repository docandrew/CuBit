package body Intel_GPU_ADS_Regset with SPARK_Mode is
   procedure Add (List : in out Register_List; Item : Register_Entry;
                  MMIO_Bytes : Unsigned_32; Status : out Add_Result) is
      Position : Positive range 1 .. Capacity + 1 := List.Count + 1;
   begin
      if not Valid (Item, MMIO_Bytes) then
         Status := Invalid_Entry;
         return;
      end if;
      for I in 1 .. List.Count loop
         if List.Entries (I).Offset = Item.Offset then
            Status := (if List.Entries (I) = Item then Already_Present
                       else Conflicting_Entry);
            return;
         elsif List.Entries (I).Offset > Item.Offset then
            Position := I;
            exit;
         end if;
      end loop;
      if List.Count = Capacity then
         Status := No_Space;
         return;
      end if;
      for I in reverse Position .. List.Count loop
         List.Entries (I + 1) := List.Entries (I);
      end loop;
      List.Entries (Position) := Item;
      List.Count := List.Count + 1;
      Status := Added;
   end Add;

   function Encode (Item : Register_Entry) return Wire_Entry is
      Result : Wire_Entry := [others => 0];
      Flags : Unsigned_32 := (if Item.Masked then 1 else 0);
   begin
      if Item.Steered then
         Flags := Flags or 2 or Shift_Left (Unsigned_32 (Item.Group_ID), 12)
           or Shift_Left (Unsigned_32 (Item.Instance_ID), 20);
      end if;
      for I in 0 .. 3 loop
         Result (I) := Unsigned_8 (Shift_Right (Item.Offset, I * 8) and 255);
         Result (8 + I) := Unsigned_8 (Shift_Right (Flags, I * 8) and 255);
      end loop;
      return Result;
   end Encode;
end Intel_GPU_ADS_Regset;

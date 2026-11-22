with Ada.Text_IO;
with Interfaces; use Interfaces;
with Intel_GPU_ADS_Regset; use Intel_GPU_ADS_Regset;
procedure ADS_Regset_Tests is
   List, Before : Register_List;
   Status : Add_Result;
   Item : Register_Entry;
   Wire : Wire_Entry;
   function Word (B : Wire_Entry; N : Natural) return Unsigned_32 is
     (Unsigned_32 (B (N)) + 256 * Unsigned_32 (B (N + 1)) +
      65536 * Unsigned_32 (B (N + 2)) + 16777216 * Unsigned_32 (B (N + 3)));
begin
   for M in Boolean loop
      for S in Boolean loop
         for G in Steering_ID loop
            for I in Steering_ID loop
               Item := (16#12345678#, M, S, G, I);
               Wire := Encode (Item);
               pragma Assert (Word (Wire, 0) = Item.Offset);
               pragma Assert (Word (Wire, 4) = 0 and Word (Wire, 12) = 0);
               pragma Assert (Word (Wire, 8) =
                 (if M then 1 else 0) +
                 (if S then 2 + Unsigned_32 (G) * 4096 +
                    Unsigned_32 (I) * 1048576 else 0));
            end loop;
         end loop;
      end loop;
   end loop;
   for I in reverse 1 .. Capacity loop
      Item := (Unsigned_32 (I * 4), False, False, 0, 0);
      Add (List, Item, 16#2000#, Status);
      pragma Assert (Status = Added);
   end loop;
   for I in 1 .. Capacity loop
      pragma Assert (List.Entries (I).Offset = Unsigned_32 (I * 4));
   end loop;
   Before := List;
   Add (List, Item, 16#2000#, Status);
   pragma Assert (Status = Already_Present and List = Before);
   Item.Masked := True;
   Add (List, Item, 16#2000#, Status);
   pragma Assert (Status = Conflicting_Entry and List = Before);
   Item.Offset := 16#1800#;
   Add (List, Item, 16#2000#, Status);
   pragma Assert (Status = No_Space and List = Before);
   for Offset in Unsigned_32 range 16#1FFD# .. 16#2004# loop
      Item.Offset := Offset;
      Add (List, Item, 16#2000#, Status);
      pragma Assert (Status = Invalid_Entry and List = Before);
   end loop;
   Item.Offset := 0;
   for Size in Unsigned_32 range 0 .. 3 loop
      Add (List, Item, Size, Status);
      pragma Assert (Status = Invalid_Entry and List = Before);
   end loop;
   Item.Group_ID := 1;
   Add (List, Item, 16#2000#, Status);
   pragma Assert (Status = Invalid_Entry and List = Before);
   -- Odd strides permute all256 positions, exercising append and middle
   -- insertion as well as the descending/head insertion above.
   for Stride in 0 .. 15 loop
      List := (others => <>);
      for I in 0 .. Capacity - 1 loop
         Item := (Unsigned_32 (((I * (2 * Stride + 1)) mod Capacity) * 4),
                  False, False, 0, 0);
         Add (List, Item, 16#2000#, Status);
         pragma Assert (Status = Added and List.Count = I + 1);
         for J in 2 .. List.Count loop
            pragma Assert (List.Entries (J - 1).Offset < List.Entries (J).Offset);
         end loop;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("ADS regset: 1024 encodings, 16 insertion permutations and atomic rejection PASS");
end ADS_Regset_Tests;

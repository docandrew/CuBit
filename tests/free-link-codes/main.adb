--  Hosted test of Free_Link_Codes (KERN-014 slice 1): every list code and
--  frame codes at the edges and across the range decode to what they
--  encoded and never coincide; the link pair is 8 bytes, next first.
pragma Ada_2022;
with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with Free_Link_Codes; use Free_Link_Codes;

procedure Main is
   Checks : Natural := 0;
   type Frame_List is array (Positive range <>) of Frame_Number;
   Edges : constant Frame_List :=
     [Frame_Number'Last, Frame_Number'Last - 1, 2 ** 20, 2 ** 31, 2 ** 32 - List_Count - 1];
   procedure Check_Frame (F : Frame_Number) is
   begin
      pragma Assert (not Is_List (Of_Frame (F)));
      pragma Assert (Frame_Of (Of_Frame (F)) = F);
      for L in List_Index loop
         pragma Assert (Of_Frame (F) /= Of_List (L));
      end loop;
      Checks := Checks + 1;
   end Check_Frame;
   Pair : Links := (Next => 16#1122_3344#, Previous => 16#5566_7788#);
   Raw : Unsigned_64 with Import, Address => Pair'Address;
begin
   for L in List_Index loop
      pragma Assert (Is_List (Of_List (L)) and then List_Of (Of_List (L)) = L);
   end loop;
   for F in Frame_Number range 0 .. 4095 loop
      Check_Frame (F);
   end loop;
   for F of Edges loop
      Check_Frame (F);
   end loop;
   declare
      Seed : Unsigned_64 := 16#2545_F491_4F6C_DD1D#;
   begin
      for I in 1 .. 1_000_000 loop
         Seed := Seed xor Shift_Left (Seed, 13);
         Seed := Seed xor Shift_Right (Seed, 7);
         Seed := Seed xor Shift_Left (Seed, 17);
         Check_Frame (Seed mod (Frame_Number'Last + 1));
      end loop;
   end;
   pragma Assert (Links'Size = 64 and then Link_Bytes = 8);
   pragma Assert (Raw = 16#5566_7788_1122_3344#);
   Pair.Next := 0;
   Put_Line ("free-link-codes: frame checks" & Checks'Image);
   Put_Line ("free-link-codes: PASS");
end Main;

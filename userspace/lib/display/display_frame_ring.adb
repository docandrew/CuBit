------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body Display_Frame_Ring is

   function Message_Of (Head : Unsigned_64; W0, W1, W2, W3 : Unsigned_64) return DP.Wire_Message is
     ((Label => Unsigned_32 (Head and 16#FFFF_FFFF#),
       Length => Unsigned_8 (Shift_Right (Head, 32) and 16#FF#),
       Flags => DP.Protocol_Flags (Shift_Right (Head, 40) and 16#F#),
       Reserved => Unsigned_16 (Shift_Right (Head, 48) and 16#FFFF#),
       Words => [W0, W1, W2, W3]));

   function Encode (Item : Present) return Present_Words is
      Words : Present_Words := [others => 0];
   begin
      Words (Frame_Header_Word) := Header (Item.Frame);
      for I in 0 .. 3 loop
         Words (Frame_First_Word + I) := Item.Frame.Words (I);
      end loop;
      for I in FD.Word_Index loop
         Words (Damage_First_Word + I) := Item.Damage (I);
      end loop;
      Words (Submitted_Word) := Item.Submitted_Us;
      return Words;
   end Encode;

   function Decode (Words : Present_Words) return Present_Decoding is
      Item : Present;
   begin
      if not Valid_Header (Words (Frame_Header_Word)) then
         return (Valid => False);
      end if;
      Item.Frame := Message_Of
        (Words (Frame_Header_Word), Words (Frame_First_Word), Words (Frame_First_Word + 1),
         Words (Frame_First_Word + 2), Words (Frame_First_Word + 3));
      for I in FD.Word_Index loop
         Item.Damage (I) := Words (Damage_First_Word + I);
      end loop;
      Item.Submitted_Us := Words (Submitted_Word);
      return (True, Item);
   end Decode;

   function Encode (Item : Answer) return Answer_Words is
     ([Answer_Header_Word => Header (Item.Result),
       1 => Item.Result.Words (0), 2 => Item.Result.Words (1),
       3 => Item.Result.Words (2), 4 => Item.Result.Words (3),
       Taken_Word => Item.Taken_Us, Answered_Word => Item.Answered_Us]);

   function Decode (Words : Answer_Words) return Answer_Decoding is
   begin
      if not Valid_Header (Words (Answer_Header_Word)) then
         return (Valid => False);
      end if;
      return (True, (Result => Message_Of
                       (Words (Answer_Header_Word), Words (Answer_First_Word),
                        Words (Answer_First_Word + 1), Words (Answer_First_Word + 2),
                        Words (Answer_First_Word + 3)),
                     Taken_Us => Words (Taken_Word), Answered_Us => Words (Answered_Word)));
   end Decode;

end Display_Frame_Ring;

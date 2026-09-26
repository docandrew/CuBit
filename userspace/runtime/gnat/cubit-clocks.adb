with CuBit.Messages; use CuBit.Messages;
package body CuBit.Clocks is
   procedure Read (Value : out Snapshot; Success : out Boolean) is
      Msg : Message := NULL_MESSAGE;
      Y, M, D, H, N, S : Unsigned_64;
   begin
      Value := (others => <>);
      Msg.tag.label := Snapshot_Operation;
      Msg.tag := capCall (Endpoint_Slot, Msg);
      Y := Shift_Right (Msg.words (1), 40);
      M := Shift_Right (Msg.words (1), 32) and 255;
      D := Shift_Right (Msg.words (1), 24) and 255;
      H := Shift_Right (Msg.words (1), 16) and 255;
      N := Shift_Right (Msg.words (1), 8) and 255;
      S := Msg.words (1) and 255;
      Success := Msg.tag.label = 16#F000# and then Msg.tag.length = 4
        and then Msg.tag.flags = 0 and then Msg.tag.reserved = 0
        and then Msg.words (2) <= 172_800
        and then Msg.words (3) <= Time_Quality'Enum_Rep (Time_Quality'Last)
        and then Y in 1970 .. 2399 and then M in 1 .. 12
        and then D in 1 .. 31 and then H <= 23 and then N <= 59
        and then S <= 59;
      if Success then
         Value :=
           (Msg.words (0), Natural (Y), Natural (M), Natural (D),
            Natural (H), Natural (N), Natural (S),
            Integer (Msg.words (2)) - 86_400,
            Time_Quality'Enum_Val (Msg.words (3)));
      end if;
   end Read;
end CuBit.Clocks;

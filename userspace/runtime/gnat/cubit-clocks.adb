with CuBit.Messages; use CuBit.Messages;
package body CuBit.Clocks is
   --  The snapshot by a call ending at Deadline (an absolute time).
   procedure Read_By
     (Service : CuBit.Handles.Endpoint; Deadline : Unsigned_64; Value : out Snapshot; Success : out Boolean);

   procedure Read (Service : CuBit.Handles.Endpoint; Value : out Snapshot; Success : out Boolean) is
   begin
      Read_By (Service, CuBit.Messages.Wait_Forever, Value, Success);
   end Read;

   procedure Read
     (Service : CuBit.Handles.Endpoint; Within_Ms : Unsigned_64; Value : out Snapshot; Success : out Boolean) is
   begin
      Read_By (Service, Deadline_After (Within_Ms), Value, Success);
   end Read;

   procedure Read_By
     (Service : CuBit.Handles.Endpoint; Deadline : Unsigned_64; Value : out Snapshot; Success : out Boolean)
   is
      Msg : Message := NULL_MESSAGE;
      Y, M, D, H, N, S : Unsigned_64;
   begin
      Value := (others => <>);
      Msg.tag.label := Snapshot_Operation;
      Msg.tag := capCall (Service, Msg, Deadline);
      Y := Shift_Right (Msg.words (1), 40);
      M := Shift_Right (Msg.words (1), 32) and 255;
      D := Shift_Right (Msg.words (1), 24) and 255;
      H := Shift_Right (Msg.words (1), 16) and 255;
      N := Shift_Right (Msg.words (1), 8) and 255;
      S := Msg.words (1) and 255;
      Success := Msg.tag.label = 16#F000# and then Msg.tag.length = 4
        and then Msg.tag.protocol = 0 and then Msg.tag.reserved = 0
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
   end Read_By;
end CuBit.Clocks;

pragma Ada_2022;
with Ada.Text_IO;
with Interfaces;
with I2C_Configure_Replies;
procedure Reply_Tests is
   package S renames I2C_Configure_Replies;
   use I2C_Configure_Replies;
   use type Interfaces.Unsigned_64;
   use type Interfaces.Unsigned_32;
   Checks : Natural := 0;
   procedure Check (Condition : Boolean; Context : String) is
   begin
      if not Condition then raise Program_Error with Context; end if;
      Checks := Checks + 1;
   end Check;
   procedure Test (Value : Reply) is
      Words : constant Reply_Words := Encode (Value);
      Decoded_Reply : constant Decoding := Decode (Words);
   begin
      Check (Decoded_Reply.Status = Decoded, "valid typed reply decodes");
      Check (Decoded_Reply.Value = Value, "all reply status/stage/value bits round-trip");
      Check (Encode (Decoded_Reply.Value) = Words, "canonical raw round-trip");
   end Test;
begin
   Test ((Status => Ready));
   Check (Encode ((Status => Ready)) = Reply_Words'[1, 0, 0, 0], "ready golden frame");
   for Status in Reply_Status range Refused .. Failed_Effects_Possible loop
      for Stage in Startup_Stage loop
         Test ((Status, Stage, 0)); Test ((Status, Stage, 1));
         Test ((Status, Stage, Register_Value'Last));
         for Bit in 0 .. 31 loop
            Test ((Status, Stage, Interfaces.Shift_Left (Register_Value'(1), Bit)));
            Test ((Status, Stage, Register_Value'Last - Interfaces.Shift_Left (Register_Value'(1), Bit)));
         end loop;
      end loop;
   end loop;
   for Length in 0 .. 8 loop
      declare Words : S.Message_Words (17 .. 16 + Length) := [others => 0];
      begin
         if Length > 0 then Words (17) := 1; end if;
         Check (Decode (Words).Status = (if Length = Reply_Word_Count then Decoded else Wrong_Length),
                "strict framing with nonzero origin");
      end;
   end loop;
   declare
      Extreme : constant S.Message_Words (Natural'Last - 3 .. Natural'Last) :=
        [1, 3, Word (Startup_Stage'Pos (Startup_Stage'Last)), Word (Register_Value'Last)];
   begin
      Check (Decode (Extreme).Value = Reply'(Failed_Effects_Possible, Status_Clear, Register_Value'Last),
             "full high-bound indexed frame retains every field");
   end;
   for Bit in 0 .. 63 loop
      declare
         Mask : constant Word := Interfaces.Shift_Left (Word'(1), Bit);
      begin
         Check (Decode (S.Message_Words'[1 xor Mask, 0, 0, 0]).Status = Wrong_Version,
                "all version bits checked");
         Check (Decode (S.Message_Words'[1, 0, Mask, 0]).Status = Noncanonical_Ready,
                "ready cannot conceal failure stage bits");
         Check (Decode (S.Message_Words'[1, 0, 0, Mask]).Status = Noncanonical_Ready,
                "ready cannot conceal register value bits");
         if Mask > 3 then
            Check (Decode (S.Message_Words'[1, Mask, 0, 0]).Status = Invalid_Status,
                   "unknown status not accepted or narrowed");
         end if;
         if Mask > Word (Startup_Stage'Pos (Startup_Stage'Last)) then
            Check (Decode (S.Message_Words'[1, 3, Mask, 0]).Status = Invalid_Stage,
                   "unknown stage not accepted or narrowed");
         end if;
         if Mask > Word (Register_Value'Last) then
            Check (Decode (S.Message_Words'[1, 3, 0, Mask]).Status = Invalid_Register_Value,
                   "wide register diagnostic never truncated");
         end if;
      end;
   end loop;
   Check (Decode (S.Message_Words'[1, 4, 0, 0]).Status = Invalid_Status,
          "first unknown status refused");
   Check (Decode (S.Message_Words'[1, 3, 14, 0]).Status = Invalid_Stage,
          "first unknown stage refused");
   Check (Decode (S.Message_Words'[1, 3, 0, 2 ** 32]).Status = Invalid_Register_Value,
          "first out of range register value refused");
   Ada.Text_IO.Put_Line ("I2C configure reply checks" & Checks'Image);
end Reply_Tests;

pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with GPIO_Bootstrap_Payload;
procedure Codec_Tests is
   package P renames GPIO_Bootstrap_Payload;
   use type P.Request, P.Decode_Status;
   type Counts is array (Positive range <>) of Unsigned_32;
   type Digests is array (Positive range <>) of Unsigned_64;
   Count_Values : constant Counts := [0, 1, 3, 16#8000_0000#, Unsigned_32'Last];
   Digest_Values : constant Digests := [P.Empty_Initializer_Digest, 0, Unsigned_64'Last, 16#0123_4567_89AB_CDEF#];
   pragma Compile_Time_Error (P.Format_Version /= 2 or P.Header_Bytes /= 102
      or P.Initializer_Count_Offset /= 90 or P.Initializer_Digest_Offset /= 94
      or P.Phase'Enum_Rep (P.Before_GPIO_Consumer_Initialization) /= 2, "coordinator agreed wire layout changed");
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Why : String) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Why; end if; end;
   R, Decoded : P.Request;
   Result : P.Decode_Result;
   function Byte_At (Word : Unsigned_64; I : Natural) return Unsigned_8 is
     (Unsigned_8 (Shift_Right (Word, I * 8) and 16#FF#));
begin
   R.HID := "INTC1057"; R.Segments := 2; R.Path (1) := "_SB_"; R.Path (2) := "GPI0";
   R.STA_From := P.Default_STA; R.STA := P.Default_STA_Value;
   R.Table := "DSDT"; R.Table_ID := "TESTV002"; R.Epoch := Unsigned_64'Last;
   for Spans in P.Span_Index loop
      R.Spans := Spans;
      R.Items (Spans) := (Base => Unsigned_64 (Spans) * 4096, Length => 4096,
         From => P.Memory32_Fixed, Writable => True);
      for Count of Count_Values loop for Digest of Digest_Values loop
         R.Initializer_Invocations := Count; R.Initializer_Ordered_Digest := Digest;
         Check (P.Valid (R), "request fixture valid");
         declare Bytes : P.Byte_Array (0 .. P.Encoded_Length (R.Spans) - 1); begin
            P.Encode (R, Bytes);
            Check (Bytes (P.Version_Offset) = 2 and Bytes (P.Phase_Offset) = 2, "only v2 phase encoded");
            for I in 0 .. P.Initializer_Count_Bytes - 1 loop
               Check (Bytes (P.Initializer_Count_Offset + I) = Byte_At (Unsigned_64 (Count), I), "count little endian and full width");
            end loop;
            for I in 0 .. P.Initializer_Digest_Bytes - 1 loop
               Check (Bytes (P.Initializer_Digest_Offset + I) = Byte_At (Digest, I), "digest little endian and full width");
            end loop;
            P.Decode (Bytes, Decoded, Result);
            Check (Result.Status = P.Decoded and Decoded = R, "round trip preserves every existing and new field");
            if Count = Count_Values (Count_Values'First) and Digest = Digest_Values (Digest_Values'First) then
               for Version in Unsigned_8 loop if Version /= 2 then
                  Bytes (P.Version_Offset) := Version; P.Decode (Bytes, Decoded, Result);
                  Check (Result.Status = P.Malformed and Result.Offset = P.Version_Offset, "v1 and every other version refused");
               end if; end loop;
               Bytes (P.Version_Offset) := 2;
               for Phase in Unsigned_8 loop if Phase /= 2 then
                  Bytes (P.Phase_Offset) := Phase; P.Decode (Bytes, Decoded, Result);
                  Check (Result.Status = P.Malformed and Result.Offset = P.Phase_Offset, "old and unknown phase refused");
               end if; end loop;
               Bytes (P.Phase_Offset) := 2;
               for Length in 0 .. Bytes'Length - 1 loop
                  P.Decode (Bytes (0 .. Length - 1), Decoded, Result);
                  Check (Result.Status = P.Malformed, "every truncated prefix refused");
               end loop;
            end if;
         end;
      end loop; end loop;
   end loop;
   Ada.Text_IO.Put_Line ("GPIO v2 codec checks" & Checks'Image);
end Codec_Tests;

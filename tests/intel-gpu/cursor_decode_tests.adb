with Ada.Text_IO;
with Intel_GPU_Cursor_Control;
with Interfaces; use Interfaces;
with Intel_GPU_Cursor_Decode; use Intel_GPU_Cursor_Decode;
procedure Cursor_Decode_Tests is
   S, T : Sample;
   R : Decoded;
begin
   -- Every individual bit must land in its documented numeric field.
   for Bit in 0 .. 31 loop
      declare
         Word : constant Unsigned_32 := Shift_Left (1, Bit);
         Fields : constant Intel_GPU_Cursor_Control.Control :=
           Intel_GPU_Cursor_Control.From_Word (Word);
      begin
         pragma Assert (Intel_GPU_Cursor_Control.To_Word (Fields) = Word);
         pragma Assert (Unsigned_32 (Fields.Mode_Select) =
           (if Bit in 0 .. 5 then Shift_Right (Word, 0) else 0));
         pragma Assert (Unsigned_32 (Fields.Reserved_6) =
           (if Bit in 6 .. 7 then Shift_Right (Word, 6) else 0));
         pragma Assert (Unsigned_32 (Fields.Force_Alpha_Value) =
           (if Bit in 8 .. 9 then Shift_Right (Word, 8) else 0));
         pragma Assert (Unsigned_32 (Fields.Force_Alpha_Plane_Select) =
           (if Bit in 10 .. 11 then Shift_Right (Word, 10) else 0));
         pragma Assert (Unsigned_32 (Fields.Reserved_12) =
           (if Bit in 12 .. 14 then Shift_Right (Word, 12) else 0));
         pragma Assert (Unsigned_32 (Fields.Rotate_180) =
           (if Bit in 15 .. 15 then Shift_Right (Word, 15) else 0));
         pragma Assert (Unsigned_32 (Fields.CSC_Enable) =
           (if Bit in 16 .. 16 then Shift_Right (Word, 16) else 0));
         pragma Assert (Unsigned_32 (Fields.Reserved_17) =
           (if Bit in 17 .. 17 then Shift_Right (Word, 17) else 0));
         pragma Assert (Unsigned_32 (Fields.Pre_CSC_Gamma_Enable) =
           (if Bit in 18 .. 18 then Shift_Right (Word, 18) else 0));
         pragma Assert (Unsigned_32 (Fields.Reserved_19) =
           (if Bit in 19 .. 22 then Shift_Right (Word, 19) else 0));
         pragma Assert (Unsigned_32 (Fields.Allow_Update_Disable) =
           (if Bit in 23 .. 23 then Shift_Right (Word, 23) else 0));
         pragma Assert (Unsigned_32 (Fields.Pipe_CSC_Enable) =
           (if Bit in 24 .. 24 then Shift_Right (Word, 24) else 0));
         pragma Assert (Unsigned_32 (Fields.Reserved_25) =
           (if Bit in 25 .. 25 then Shift_Right (Word, 25) else 0));
         pragma Assert (Unsigned_32 (Fields.Gamma_Enable) =
           (if Bit in 26 .. 26 then Shift_Right (Word, 26) else 0));
         pragma Assert (Unsigned_32 (Fields.Reserved_27) =
           (if Bit in 27 .. 27 then Shift_Right (Word, 27) else 0));
         pragma Assert (Unsigned_32 (Fields.Arbitration_Slots) =
           (if Bit in 28 .. 30 then Shift_Right (Word, 28) else 0));
         pragma Assert (Unsigned_32 (Fields.Reserved_31) =
           (if Bit in 31 .. 31 then Shift_Right (Word, 31) else 0));
      end;
   end loop;
   for Control in Unsigned_32 range 0 .. 65_535 loop
      S := (Control, 4096, 4096, 0);
      R := Decode (S, S, 8_388_608);
      if Control in 16#22# | 16#23# | 16#27# then
         pragma Assert (R.State = Ready and R.Memory.First = 4096);
         pragma Assert (R.Memory.Bytes = (case Control is
           when 16#22# => 65_536, when 16#23# => 262_144, when others => 16_384));
      elsif Control mod 64 = 0 then
         pragma Assert (R.State = Disabled and not R.Memory.Valid);
      else
         pragma Assert (R.State = Unsupported and not R.Memory.Valid);
      end if;
   end loop;
   for Fault in 1 .. 9 loop
      S := (16#27#, 4096, 4096, 0);
      case Fault is
         when 1 => S.Control := Unsigned_32'Last;
         when 2 => S.Base := Unsigned_32'Last;
         when 3 => S.Live_Base := Unsigned_32'Last;
         when 4 => S.FBC_Control := Unsigned_32'Last;
         when 5 => S.Base := 4097; S.Live_Base := 4097;
         when 6 => S.Live_Base := 8192;
         when 7 => S.FBC_Control := 16#8000003F#;
         when 8 => S.Control := 16#80000027#;
         when 9 => S.Base := 16#FFFFF000#; S.Live_Base := S.Base;
      end case;
      R := Decode (S, S, 8_388_608);
      pragma Assert (not R.Memory.Valid and R.State /= Ready);
   end loop;
   S := (16#27#, 4096, 4096, 0); T := S; T.Base := 8192;
   R := Decode (S, T, 8_388_608);
   pragma Assert (R.State = Changing and not R.Memory.Valid);
   S := (16#23#, 16#FFFC0000#, 16#FFFC0000#, 0);
   R := Decode (S, S, 8_388_608);
   pragma Assert (R.State = Ready and R.Memory.Bytes = 262_144);
   R := Decode (S, S, 4_194_304);
   pragma Assert (R.State = Invalid_Geometry and not R.Memory.Valid);
   Ada.Text_IO.Put_Line ("cursor decode PASS: 65536 controls, invalid samples and aperture boundaries");
end Cursor_Decode_Tests;

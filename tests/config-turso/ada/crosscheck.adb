with Ada.Command_Line;
with Ada.Text_IO;
with CBOR; use CBOR;
with CBOR.Decoding;
with CBOR.Encoding;
with Interfaces;

--  Linux-hosted fixture. Compare independent Rust/Ada implementations against
--  the same checked-in bytes; this is not a native Config decoder or a proof.
procedure Crosscheck is
   package E renames CBOR.Encoding;
   package D renames CBOR.Decoding;
   use type CBOR.Byte_Array;
   use type CBOR.UInt64;
   use type CBOR.SE_Offset;
   use type CBOR.Byte;
   use type Interfaces.Integer_64;
   use type Interfaces.Unsigned_8;

   function Nibble (C : Character) return Natural is
   begin
      case C is
         when '0' .. '9' => return Character'Pos (C) - Character'Pos ('0');
         when 'a' .. 'f' => return Character'Pos (C) - Character'Pos ('a') + 10;
         when others => raise Constraint_Error with "invalid fixture hex";
      end case;
   end Nibble;

   function From_Hex (Hex : String) return Byte_Array is
      Result : Byte_Array (1 .. SE_Offset (Hex'Length / 2));
      Pos : Natural := Hex'First;
   begin
      pragma Assert (Hex'Length mod 2 = 0);
      for B of Result loop
         B := Byte (16 * Nibble (Hex (Pos)) + Nibble (Hex (Pos + 1)));
         Pos := Pos + 2;
      end loop;
      return Result;
   end From_Hex;

   Digest : constant Byte_Array := From_Hex
     ("d843aa92b3ef7ace9d53153212321e39c958240cf3807603f1e8986cbd786cb5");
   Expected : constant Byte_Array :=
     E.Encode_Array (3) & E.Encode_Unsigned (1) &
     E.Encode_Byte_String (Digest) & E.Encode_Map (3) &
     E.Encode_Text_String ("b") & E.Encode_Integer (-25) &
     E.Encode_Text_String ("z") & E.Encode_Bool (True) &
     E.Encode_Text_String ("aa") & E.Encode_Text_String ("Cubie");
   Input : Ada.Text_IO.File_Type;
begin
   Ada.Text_IO.Open (Input, Ada.Text_IO.In_File, Ada.Command_Line.Argument (1));
   declare
      Data : constant Byte_Array := From_Hex (Ada.Text_IO.Get_Line (Input));
      R : constant Decode_All_Result := D.Decode_All_Strict
        (Data, Max_Depth => 2, Max_String_Len => 128);
   begin
      pragma Assert (Data = Expected);
      pragma Assert (R.Status = OK and then R.Count = 10);
      pragma Assert (R.Items (1).Kind = MT_Array and then R.Items (1).Arr_Count = 3);
      pragma Assert (R.Items (2).Kind = MT_Unsigned_Integer
                     and then R.Items (2).UInt_Value = 1);
      pragma Assert (R.Items (3).Kind = MT_Byte_String
                     and then D.Get_String (Data, R.Items (3).BS_Ref) = Digest);
      pragma Assert (R.Items (4).Kind = MT_Map and then R.Items (4).Map_Count = 3);
      pragma Assert (R.Items (6).Kind = MT_Negative_Integer
                     and then R.Items (6).NInt_Arg = 24);
      pragma Assert (R.Items (8).Kind = MT_Simple_Value
                     and then R.Items (8).SV_Value = 21);
      pragma Assert (R.Items (10).Kind = MT_Text_String
                     and then D.Get_String (Data, R.Items (10).TS_Ref) =
                       Byte_Array'[67, 117, 98, 105, 101]);
      pragma Assert (Ada.Text_IO.End_Of_File (Input));
   end;
   Ada.Text_IO.Close (Input);
   Ada.Text_IO.Put_Line
     ("PASS: shared scalar-profile bytes encode/decode with pinned Ada CBOR");
end Crosscheck;

with CBOR.Decoding;
with CBOR.Encoding;

package body CCL.Objects.Persistence with SPARK_Mode is
   use type CBOR.SE_Offset;
   use type CBOR.Decode_Status;
   use type CBOR.Major_Type;
   use type CBOR.Byte;
   package E renames CBOR.Encoding;
   package D renames CBOR.Decoding;

   procedure Encode
     (Object : Image; Contract : Binding; Data : out Packet; Result : out Outcome)
   is
      Schema : CBOR.Byte_Array (1 .. 32);
      Text : CBOR.Byte_Array (1 .. Maximum_Text_Bytes);
      Fits : Boolean := True;
      --  All encoder outputs and our slices are one-based. State this at the
      --  helper boundary: a general Storage_Array can span negative through
      --  positive indices with a length larger than its index type can hold.
      procedure Put (Bytes : CBOR.Byte_Array) with Pre => Bytes'First = 1;
      procedure Put (Bytes : CBOR.Byte_Array) is
      begin
         if not Fits then return; end if;
         if Bytes'Length > CBOR.SE_Offset (Maximum_Encoded_Bytes - Data.Length) then
            Fits := False;
         else
            Data.Data (CBOR.SE_Offset (Data.Length) + 1 ..
                       CBOR.SE_Offset (Data.Length) + Bytes'Length) := Bytes;
            Data.Length := Data.Length + Natural (Bytes'Length);
         end if;
      end Put;
      procedure Put_Bytes (Bytes : CBOR.Byte_Array) with Pre => Bytes'First = 1;
      procedure Put_Bytes (Bytes : CBOR.Byte_Array) is
         --  Emit the head and payload separately. Avoid an unconstrained
         --  temporary containing a second copy of the entire text arena.
         Head : CBOR.Byte_Array := E.Encode_Unsigned (Unsigned_64 (Bytes'Length));
      begin
         Head (1) := Head (1) or 16#40#;
         Put (Head);
         Put (Bytes);
      end Put_Bytes;
   begin
      Data := (others => <>);
      Result := Invalid_Object;
      if not Validate (Object, Contract) then return; end if;
      for I in Schema'Range loop
         Schema (I) := CBOR.Byte (Shift_Right
           (Object.Schema (Natural ((I - 1) / 8)), Natural (7 - (I - 1) mod 8) * 8) and 255);
      end loop;
      for I in Text'Range loop
         Text (I) := CBOR.Byte (Character'Pos (Object.Text (Integer (I))));
      end loop;
      Put (E.Encode_Array (4));
      Put (E.Encode_Unsigned (1));
      Put_Bytes (Schema);
      Put (E.Encode_Array (Unsigned_64 (Object.Used_Cells)));
      --  Explicit bounded loop also makes Encode total independently of
      --  whether the prover inlines the semantic validator.
      for I in Object.Cells'Range loop
         exit when Unsigned_32 (I) > Object.Used_Cells;
         Put (E.Encode_Array (2));
         Put (E.Encode_Unsigned (Object.Cells (I).First));
         Put (E.Encode_Unsigned (Object.Cells (I).Second));
      end loop;
      if Object.Used_Bytes <= Maximum_Text_Bytes then
         Put_Bytes (Text (1 .. CBOR.SE_Offset (Object.Used_Bytes)));
      else
         Fits := False;
      end if;
      if Fits then Result := Success;
      else Data := (others => <>);
      end if;
   end Encode;

   procedure Decode
     (Data : CBOR.Byte_Array; Contract : Binding; Object : out Image; Result : out Outcome)
   is
      Candidate : Image := Empty (Contract);
      Position : CBOR.SE_Offset := Data'First;
      Item : CBOR.Decode_Result;
      Count : Natural;
      function Take return CBOR.Decode_Result with Side_Effects,
        Post => (if Take'Result.Status = CBOR.OK then
          Data'First >= 0 and then Data'Last <= D.Max_Data_Length and then
          D.Valid_Item_Refs (Data, Take'Result.Item));
      function Take return CBOR.Decode_Result is
         R : CBOR.Decode_Result;
      begin
         if Data'First < 0 or else Data'Last > D.Max_Data_Length or else
           Position not in Data'Range
         then return (Status => CBOR.Err_Truncated, others => <>); end if;
         R := D.Decode (Data, Position);
         --  Decode is a single-head parser. An indefinite byte string has
         --  not consumed its children; it is never an empty definite string.
         if R.Status = CBOR.OK and then Data (Position) mod 32 = 31 then
            R.Status := CBOR.Err_Not_Well_Formed;
         end if;
         if R.Status = CBOR.OK then Position := R.Next; end if;
         return R;
      end Take;
   begin
      Object := Empty (Contract);
      Result := Invalid_Encoding;
      if Data'Length not in 1 .. Maximum_Encoded_Bytes or else
        Data'First < 0 or else Data'Last > D.Max_Data_Length
      then return; end if;
      Item := Take;
      if Item.Status /= CBOR.OK or else Item.Item.Kind /= CBOR.MT_Array or else
        Item.Item.Arr_Count /= 4 then return; end if;
      Item := Take;
      if Item.Status /= CBOR.OK or else Item.Item.Kind /= CBOR.MT_Unsigned_Integer or else
        Item.Item.UInt_Value /= 1 then return; end if;
      Item := Take;
      if Item.Status /= CBOR.OK or else Item.Item.Kind /= CBOR.MT_Byte_String or else
        Item.Item.BS_Ref.Length /= 32 then return; end if;
      declare
         Bytes : constant CBOR.Byte_Array := D.Get_String (Data, Item.Item.BS_Ref);
      begin
         for Word in Candidate.Schema'Range loop
            Candidate.Schema (Word) := 0;
            for B in 0 .. 7 loop
               Candidate.Schema (Word) := Shift_Left (Candidate.Schema (Word), 8) or
                 Unsigned_64 (Bytes (CBOR.SE_Offset (Word * 8 + B + 1)));
            end loop;
         end loop;
      end;
      if Candidate.Schema /= Identity (Contract) then Result := Invalid_Object; return; end if;
      Item := Take;
      if Item.Status /= CBOR.OK or else Item.Item.Kind /= CBOR.MT_Array or else
        Item.Item.Arr_Count not in 1 .. Maximum_Cells then return; end if;
      Count := Natural (Item.Item.Arr_Count);
      Candidate.Used_Cells := Unsigned_32 (Count);
      for I in 1 .. Count loop
         Item := Take;
         if Item.Status /= CBOR.OK or else Item.Item.Kind /= CBOR.MT_Array or else
           Item.Item.Arr_Count /= 2 then return; end if;
         Item := Take;
         if Item.Status /= CBOR.OK or else Item.Item.Kind /= CBOR.MT_Unsigned_Integer then return; end if;
         Candidate.Cells (I).First := Item.Item.UInt_Value;
         Item := Take;
         if Item.Status /= CBOR.OK or else Item.Item.Kind /= CBOR.MT_Unsigned_Integer then return; end if;
         Candidate.Cells (I).Second := Item.Item.UInt_Value;
      end loop;
      Item := Take;
      if Item.Status /= CBOR.OK or else Item.Item.Kind /= CBOR.MT_Byte_String or else
        Item.Item.BS_Ref.Length > Maximum_Text_Bytes or else Position /= Data'Last + 1
      then return; end if;
      declare
         Bytes : constant CBOR.Byte_Array := D.Get_String (Data, Item.Item.BS_Ref);
      begin
         Candidate.Used_Bytes := Unsigned_32 (Bytes'Length);
         for I in Bytes'Range loop
            Candidate.Text (Integer (I)) := Character'Val (Bytes (I));
         end loop;
      end;
      Result := Invalid_Object;
      if Validate (Candidate, Contract) then
         Object := Candidate;
         Result := Success;
      end if;
   end Decode;
end CCL.Objects.Persistence;

with CBOR.Decoding;
with CBOR.Encoding;

package body CCL.Objects.Schemas.Persistence with SPARK_Mode is
   package E renames CBOR.Encoding;
   package D renames CBOR.Decoding;
   use type CBOR.SE_Offset;
   use type CBOR.Decode_Status;
   use type CBOR.Major_Type;
   use type CBOR.Byte;
   use type CBOR.Byte_Array;
   use type Types.Type_Reference;
   use type Types.Shape;

   procedure Encode (Contract : Binding; Data : out Packet; Result : out Outcome) is
      Fits : Boolean := True;
      Selected : constant Binding := Root_Closure (Contract);
      Key : CBOR.Byte_Array (1 .. 32);
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
         Head : CBOR.Byte_Array := E.Encode_Unsigned (Unsigned_64 (Bytes'Length));
      begin
         Head (1) := Head (1) or 16#40#;
         Put (Head); Put (Bytes);
      end Put_Bytes;
      procedure Put_Name (Name : Types.Name) is
         Bytes : CBOR.Byte_Array (1 .. Types.Maximum_Name_Length);
      begin
         for I in Bytes'Range loop
            Bytes (I) := CBOR.Byte (Character'Pos (Name.Data (Integer (I))));
         end loop;
         Put_Bytes (Bytes (1 .. CBOR.SE_Offset (Name.Length)));
      end Put_Name;
   begin
      Data := (others => <>); Result := Invalid_Binding;
      if not Is_Bound (Selected) then return; end if;
      for I in Key'Range loop
         Key (I) := CBOR.Byte (Shift_Right
           (Selected.Key (Natural ((I - 1) / 8)), Natural (7 - (I - 1) mod 8) * 8) and 255);
      end loop;
      Put (E.Encode_Array (4)); Put (E.Encode_Unsigned (1)); Put_Bytes (Key);
      Put (E.Encode_Unsigned (Unsigned_64 (Wire_ID (Selected.Root))));
      Put (E.Encode_Array (Unsigned_64 (Types.Last (Selected.Types) - Types.Unit_Type)));
      for Ref in Types.Declared_Type'First .. Types.Last (Selected.Types) loop
         declare
            Item : constant Types.Description := Types.Describe (Selected.Types, Ref);
         begin
            Put (E.Encode_Array (3)); Put_Name (Item.Identifier);
            Put (E.Encode_Unsigned (Unsigned_64
              (if Item.Form = Types.Product then Product_Form else Sum_Form)));
            Put (E.Encode_Array (Unsigned_64 (Item.Count)));
            for Part in 1 .. Item.Count loop
               Put (E.Encode_Array (2)); Put_Name (Item.Parts (Part).Identifier);
               Put (E.Encode_Unsigned (Unsigned_64 (Wire_ID (Item.Parts (Part).Payload))));
            end loop;
         end;
      end loop;
      if Fits then Result := Success; else Data := (others => <>); end if;
   end Encode;

   procedure Decode (Data : CBOR.Byte_Array; Contract : out Binding; Result : out Outcome) is
      Metadata : Image;
      Candidate : Binding;
      Canonical : Packet;
      Encoded : Outcome;
      Accepted : Boolean;
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
         if Data'First < 0 or else Data'Last > D.Max_Data_Length or else Position not in Data'Range then
            return (Status => CBOR.Err_Truncated, others => <>);
         end if;
         R := D.Decode (Data, Position);
         if R.Status = CBOR.OK and then Data (Position) mod 32 = 31 then
            R.Status := CBOR.Err_Not_Well_Formed;
         end if;
         if R.Status = CBOR.OK then Position := R.Next; end if;
         return R;
      end Take;
      procedure Take_Name (Name : out Native_Name; Good : out Boolean) is
         Part : CBOR.Decode_Result;
      begin
         Name := (others => <>); Good := False;
         Part := Take;
         if Part.Status /= CBOR.OK or else Part.Item.Kind /= CBOR.MT_Byte_String
           or else Part.Item.BS_Ref.Length not in 1 .. Types.Maximum_Name_Length
         then return; end if;
         declare
            Bytes : constant CBOR.Byte_Array := D.Get_String (Data, Part.Item.BS_Ref);
         begin
            Name.Length := Unsigned_32 (Bytes'Length);
            for I in Bytes'Range loop Name.Text (Integer (I)) := Character'Val (Bytes (I)); end loop;
         end;
         Good := True;
      end Take_Name;
   begin
      Contract := (others => <>); Result := Invalid_Encoding;
      if Data'Length not in 1 .. Maximum_Encoded_Bytes or else
        Data'First < 0 or else Data'Last > D.Max_Data_Length then return; end if;
      Item := Take;
      if Item.Status /= CBOR.OK or else Item.Item.Kind /= CBOR.MT_Array or else Item.Item.Arr_Count /= 4 then return; end if;
      Item := Take;
      if Item.Status /= CBOR.OK or else Item.Item.Kind /= CBOR.MT_Unsigned_Integer or else Item.Item.UInt_Value /= 1 then return; end if;
      Item := Take;
      if Item.Status /= CBOR.OK or else Item.Item.Kind /= CBOR.MT_Byte_String or else Item.Item.BS_Ref.Length /= 32 then return; end if;
      declare
         Bytes : constant CBOR.Byte_Array := D.Get_String (Data, Item.Item.BS_Ref);
      begin
         for Word in Metadata.Key'Range loop
            for B in 0 .. 7 loop
               Metadata.Key (Word) := Shift_Left (Metadata.Key (Word), 8) or
                 Unsigned_64 (Bytes (CBOR.SE_Offset (Word * 8 + B + 1)));
            end loop;
         end loop;
      end;
      Item := Take;
      if Item.Status /= CBOR.OK or else Item.Item.Kind /= CBOR.MT_Unsigned_Integer
        or else Item.Item.UInt_Value > Unsigned_64 (Unsigned_32'Last) then return; end if;
      Metadata.Root := Unsigned_32 (Item.Item.UInt_Value);
      Item := Take;
      if Item.Status /= CBOR.OK or else Item.Item.Kind /= CBOR.MT_Array
        or else Item.Item.Arr_Count > Types.Maximum_Declarations then return; end if;
      Count := Natural (Item.Item.Arr_Count);
      Metadata.Count := Unsigned_32 (Count);
      for Index in 1 .. Count loop
         Item := Take;
         if Item.Status /= CBOR.OK or else Item.Item.Kind /= CBOR.MT_Array or else Item.Item.Arr_Count /= 3 then return; end if;
         Take_Name (Metadata.Definitions (Index).Identifier, Accepted);
         if not Accepted then return; end if;
         Item := Take;
         if Item.Status /= CBOR.OK or else Item.Item.Kind /= CBOR.MT_Unsigned_Integer
           or else Item.Item.UInt_Value > Unsigned_64 (Unsigned_32'Last) then return; end if;
         Metadata.Definitions (Index).Form := Unsigned_32 (Item.Item.UInt_Value);
         Item := Take;
         if Item.Status /= CBOR.OK or else Item.Item.Kind /= CBOR.MT_Array
           or else Item.Item.Arr_Count > Types.Maximum_Components then return; end if;
         Metadata.Definitions (Index).Count := Unsigned_32 (Item.Item.Arr_Count);
         for Part in 1 .. Natural (Item.Item.Arr_Count) loop
            Item := Take;
            if Item.Status /= CBOR.OK or else Item.Item.Kind /= CBOR.MT_Array or else Item.Item.Arr_Count /= 2 then return; end if;
            Take_Name (Metadata.Definitions (Index).Parts (Part).Identifier, Accepted);
            if not Accepted then return; end if;
            Item := Take;
            if Item.Status /= CBOR.OK or else Item.Item.Kind /= CBOR.MT_Unsigned_Integer
              or else Item.Item.UInt_Value > Unsigned_64 (Unsigned_32'Last) then return; end if;
            Metadata.Definitions (Index).Parts (Part).Payload := Unsigned_32 (Item.Item.UInt_Value);
         end loop;
      end loop;
      if Position /= Data'Last + 1 then return; end if;
      Schemas.Read (Metadata, Candidate, Accepted);
      if not Accepted then return; end if;
      Encode (Candidate, Canonical, Encoded);
      if Encoded /= Success or else Canonical.Length /= Data'Length or else
        Canonical.Data (1 .. CBOR.SE_Offset (Canonical.Length)) /= Data then return; end if;
      Contract := Candidate; Result := Success;
   end Decode;
end CCL.Objects.Schemas.Persistence;

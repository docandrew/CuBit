pragma Ada_2022;
package body CuBit.Log_Streams with SPARK_Mode is
   WORD_BYTES : constant := 8;
   BYTE_BITS : constant := 8;
   KIND_AT : constant := 0;
   SOURCE_AT : constant := 8;
   NODE_HIGH_AT : constant := 16;
   NODE_LOW_AT : constant := 24;
   MS_AT : constant := 32;
   TAG_AT : constant := 40;
   LENGTH_AT : constant := 48;

   procedure Put_Word (Into : in out Entry_Buffer; At_Byte : Natural; Value : Unsigned_64)
     with Pre => At_Byte <= HEADER_BYTES - WORD_BYTES;
   function Word (From : Entry_Buffer; At_Byte : Natural) return Unsigned_64
     with Pre => At_Byte <= HEADER_BYTES - WORD_BYTES;

   procedure Put_Word (Into : in out Entry_Buffer; At_Byte : Natural; Value : Unsigned_64) is
   begin
      for I in 0 .. WORD_BYTES - 1 loop
         Into (At_Byte + I) := Unsigned_8 (Shift_Right (Value, I * BYTE_BITS) and 16#FF#);
      end loop;
   end Put_Word;

   function Word (From : Entry_Buffer; At_Byte : Natural) return Unsigned_64 is
      Value : Unsigned_64 := 0;
   begin
      for I in reverse 0 .. WORD_BYTES - 1 loop
         Value := Shift_Left (Value, BYTE_BITS) or Unsigned_64 (From (At_Byte + I));
      end loop;
      return Value;
   end Word;

   procedure Encode_Event
     (Value : CuBit.Log_Protocol.Event; Into : out Entry_Buffer; Length : out Entry_Length)
   is
      Bytes : Logs.Wire_Buffer;
      Used : Logs.Wire_Count;
   begin
      Into := [others => 0];
      Logs.Encode (Value.Data, Bytes, Used);
      Put_Word (Into, KIND_AT, Entry_Kind'Pos (Event_Entry));
      Put_Word (Into, SOURCE_AT, Value.Source);
      Put_Word (Into, NODE_HIGH_AT, Value.Node.High);
      Put_Word (Into, NODE_LOW_AT, Value.Node.Low);
      Put_Word (Into, MS_AT, Value.Monotonic_Ms);
      Put_Word (Into, TAG_AT, Value.Publication_Tag);
      Put_Word (Into, LENGTH_AT, Unsigned_64 (Used));
      for I in 1 .. Natural (Used) loop
         Into (HEADER_BYTES + I - 1) := Bytes (I);
      end loop;
      Length := HEADER_BYTES + Natural (Used);
   end Encode_Event;

   procedure Encode_Gap (Lost : Unsigned_64; Into : out Entry_Buffer; Length : out Entry_Length) is
   begin
      Into := [others => 0];
      Put_Word (Into, KIND_AT, Entry_Kind'Pos (Gap_Entry));
      Put_Word (Into, MS_AT, Lost);
      Length := HEADER_BYTES;
   end Encode_Gap;

   procedure Decode
     (From : Entry_Buffer; Length : Natural; Kind : out Entry_Kind;
      Value : out CuBit.Log_Protocol.Event; Lost : out Unsigned_64; Valid : out Boolean)
   is
      Kind_Word, Encoded : Unsigned_64;
      Bytes : Logs.Wire_Buffer := [others => 0];
      Decoded : Logs.Decoded;
   begin
      Kind := Event_Entry;
      Value := (others => <>);
      Lost := 0;
      Valid := False;
      if Length < HEADER_BYTES or else Length > Maximum_Entry_Bytes then
         return;
      end if;
      Kind_Word := Word (From, KIND_AT);
      Encoded := Word (From, LENGTH_AT);
      if Kind_Word = Entry_Kind'Pos (Gap_Entry) then
         Kind := Gap_Entry;
         Lost := Word (From, MS_AT);
         Valid := Length = HEADER_BYTES and then Encoded = 0 and then Lost > 0;
         if not Valid then
            Lost := 0;
         end if;
         return;
      elsif Kind_Word /= Entry_Kind'Pos (Event_Entry) or else
        Encoded < Unsigned_64 (Logs.Header_Bytes) or else
        Encoded /= Unsigned_64 (Length - HEADER_BYTES)
      then
         return;
      end if;
      for I in 1 .. Natural (Encoded) loop
         Bytes (I) := From (HEADER_BYTES + I - 1);
      end loop;
      Decoded := Logs.Decode (Bytes, Logs.Wire_Count (Encoded));
      if not Decoded.Success then
         return;
      end if;
      Value :=
        (Source => Word (From, SOURCE_AT),
         Node => (High => Word (From, NODE_HIGH_AT), Low => Word (From, NODE_LOW_AT)),
         Monotonic_Ms => Word (From, MS_AT),
         Publication_Tag => Word (From, TAG_AT),
         Data => Decoded.Value);
      Valid := True;
   end Decode;
end CuBit.Log_Streams;

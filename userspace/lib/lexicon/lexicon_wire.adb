package body Lexicon_Wire with SPARK_Mode is

   procedure Next
     (Answer : Answer_Bytes; Position : in out Natural; Found, Damaged : out Boolean; Got : out Item)
   is
      Kind : Unsigned_8;
      Length : Natural;
   begin
      Found := False;
      Damaged := False;
      Got := (Kind => Query, First => 0, Length => 0);
      if Position >= Answer'Length then
         return;
      end if;
      if Answer'Length - Position < ITEM_HEADER_BYTES then
         Damaged := True;
         return;
      end if;
      Kind := Answer (Position);
      Length := Natural (Answer (Position + 1)) + 256 * Natural (Answer (Position + 2));
      if Kind > Item_Kind'Pos (Item_Kind'Last)
        or else Answer'Length - Position - ITEM_HEADER_BYTES < Length
      then
         Damaged := True;
         return;
      end if;
      Got := (Kind => Item_Kind'Val (Kind), First => Position + ITEM_HEADER_BYTES, Length => Length);
      Position := Position + ITEM_HEADER_BYTES + Length;
      Found := True;
   end Next;

   function Number (Answer : Answer_Bytes; Got : Item) return Unsigned_32 is
   begin
      if Got.Length < NUMBER_BYTES then
         return 0;
      end if;
      return Unsigned_32 (Answer (Got.First)) or Shift_Left (Unsigned_32 (Answer (Got.First + 1)), 8)
        or Shift_Left (Unsigned_32 (Answer (Got.First + 2)), 16)
        or Shift_Left (Unsigned_32 (Answer (Got.First + 3)), 24);
   end Number;

   procedure Span (Answer : Answer_Bytes; Got : Item; First : out Unsigned_32; Length : out Unsigned_16) is
   begin
      if Got.Length < SPAN_BYTES then
         First := 0;
         Length := 0;
         return;
      end if;
      First := Number (Answer, Got);
      Length := Unsigned_16 (Answer (Got.First + 4)) or Shift_Left (Unsigned_16 (Answer (Got.First + 5)), 8);
   end Span;

   procedure Put_Byte (Into : in out Answer_Bytes; Used : in out Natural; Value : Unsigned_8) is
   begin
      Into (Used) := Value;
      Used := Used + 1;
   end Put_Byte;

   procedure Put_Header (Into : in out Answer_Bytes; Used : in out Natural; Kind : Item_Kind;
                         Payload : Payload_Length) is
   begin
      Put_Byte (Into, Used, Item_Kind'Pos (Kind));
      Put_Byte (Into, Used, Unsigned_8 (Payload mod 256));
      Put_Byte (Into, Used, Unsigned_8 (Payload / 256));
   end Put_Header;

   procedure Put_Number (Into : in out Answer_Bytes; Used : in out Natural; Kind : Item_Kind;
                         Value : Unsigned_32) is
   begin
      Put_Header (Into, Used, Kind, NUMBER_BYTES);
      for Shift in 0 .. NUMBER_BYTES - 1 loop
         pragma Loop_Invariant (Used = Used'Loop_Entry + Shift);
         Put_Byte (Into, Used, Unsigned_8 (Shift_Right (Value, 8 * Shift) and 16#FF#));
      end loop;
   end Put_Number;

   procedure Put_Span (Into : in out Answer_Bytes; Used : in out Natural; First : Unsigned_32;
                       Length : Unsigned_16) is
   begin
      Put_Header (Into, Used, Misspelling, SPAN_BYTES);
      for Shift in 0 .. NUMBER_BYTES - 1 loop
         pragma Loop_Invariant (Used = Used'Loop_Entry + Shift);
         Put_Byte (Into, Used, Unsigned_8 (Shift_Right (First, 8 * Shift) and 16#FF#));
      end loop;
      Put_Byte (Into, Used, Unsigned_8 (Length and 16#FF#));
      Put_Byte (Into, Used, Unsigned_8 (Shift_Right (Length, 8)));
   end Put_Span;

end Lexicon_Wire;

with Interfaces; use Interfaces;
with Lexicon_Images;

--  The lexicon's answers on the wire (docs/lexicon.md, "Answers"): a
--  sequence of items, each a kind byte, a little-endian 16-bit payload
--  length and the payload. Text payloads are UTF-8; numeric ones are
--  little-endian. An answer that did not fit its page ends in a Continue_At
--  item: ask again from that item number for the rest.
--
--  One format for every face of the lexicon: lexicon.svc writes it into
--  channel answers, the hosted CCL adapter and the Dictionary app read it.
--
--  Proved: no run-time errors; Next never reads outside the answer and
--  always advances.
package Lexicon_Wire with SPARK_Mode is

   type Item_Kind is
     (Query,          --  the normalized word or text asked about
      Headword,       --  a word with senses; its senses follow
      Sense_Total,    --  payload: the headword's number of senses (4)
      Sense_Start,    --  payload: the part of speech (1 byte)
      Definition,     --  the sense's gloss
      Example,        --  a usage example
      Synonym,        --  another word in the same sense
      Antonym,
      Similar,        --  related: WordNet similar-to and see-also
      Broader,        --  a more general sense's words (hypernym)
      Narrower,       --  a more specific sense's words (hyponym)
      Suggestion,     --  a near spelling (not-found answers, misspellings)
      Misspelling,    --  payload: first byte (4) and length (2) in the text
      Checked_Upto,   --  payload: the text byte checking stops before (4)
      Continue_At);   --  payload: the item number to ask from (4)

   ITEM_HEADER_BYTES : constant := 3;
   MAXIMUM_PAYLOAD : constant := 2 ** 16 - 1;
   subtype Payload_Length is Natural range 0 .. MAXIMUM_PAYLOAD;
   NUMBER_BYTES : constant := 4;
   SPAN_BYTES : constant := 6;

   --  The largest text item: a gloss or an example (Lexicon_Images), or a
   --  word key or display spelling.
   MAXIMUM_TEXT_ITEM : constant := Lexicon_Images.MAXIMUM_TEXT_ITEM_BYTES;

   --  A page must hold the largest item and a Continue_At after it, so
   --  every page makes progress.
   MINIMUM_PAGE_BYTES : constant :=
     ITEM_HEADER_BYTES + MAXIMUM_TEXT_ITEM + ITEM_HEADER_BYTES + NUMBER_BYTES;

   subtype Answer_Index is Natural range 0 .. Natural'Last - 1;
   type Answer_Bytes is array (Answer_Index range <>) of Unsigned_8;

   --  Items are numbered from 0 in the order an answer produces them.
   type Item_Number is range 0 .. 2 ** 31 - 1;

   ---------------------------------------------------------------------------
   --  Reading.
   ---------------------------------------------------------------------------
   type Item is record
      Kind   : Item_Kind := Query;
      First  : Answer_Index := 0;   --  payload's first byte
      Length : Payload_Length := 0;
   end record;

   --  The item at Position (bytes from Answer'First), and the position after
   --  it. Found False: the end, or a malformed item (Damaged then True).
   procedure Next
     (Answer : Answer_Bytes; Position : in out Natural; Found, Damaged : out Boolean; Got : out Item)
     with Pre  => Answer'First = 0 and then Answer'Length < Natural'Last,
          Post => (if Found then Position > Position'Old and then Position <= Answer'Length
                   and then Got.Length <= Answer'Length and then Got.First <= Answer'Length - Got.Length
                   and then Got.First >= Position'Old);

   --  A numeric payload's value (4 bytes, little-endian); 0 if shorter.
   function Number (Answer : Answer_Bytes; Got : Item) return Unsigned_32
     with Pre => Answer'First = 0 and then Got.Length <= Answer'Length
                 and then Got.First <= Answer'Length - Got.Length;

   --  A Misspelling's span (first byte, length); zeros if malformed.
   procedure Span (Answer : Answer_Bytes; Got : Item; First : out Unsigned_32; Length : out Unsigned_16)
     with Pre => Answer'First = 0 and then Got.Length <= Answer'Length
                 and then Got.First <= Answer'Length - Got.Length;

   ---------------------------------------------------------------------------
   --  Writing: items appended at Used; Fits says whether one more fits
   --  while leaving room for the closing Continue_At.
   ---------------------------------------------------------------------------
   CONTINUE_BYTES : constant := ITEM_HEADER_BYTES + NUMBER_BYTES;

   function Fits (Capacity, Used : Natural; Payload : Payload_Length) return Boolean is
     (Used <= Capacity and then Capacity - Used >= CONTINUE_BYTES
      and then Capacity - Used - CONTINUE_BYTES >= ITEM_HEADER_BYTES + Payload);

   procedure Put_Header (Into : in out Answer_Bytes; Used : in out Natural; Kind : Item_Kind;
                         Payload : Payload_Length)
     with Pre  => Into'First = 0 and then Used <= Into'Length
                  and then Into'Length - Used >= ITEM_HEADER_BYTES + Payload,
          Post => Used = Used'Old + ITEM_HEADER_BYTES;

   procedure Put_Number (Into : in out Answer_Bytes; Used : in out Natural; Kind : Item_Kind;
                         Value : Unsigned_32)
     with Pre  => Into'First = 0 and then Used <= Into'Length
                  and then Into'Length - Used >= ITEM_HEADER_BYTES + NUMBER_BYTES,
          Post => Used = Used'Old + ITEM_HEADER_BYTES + NUMBER_BYTES;

   procedure Put_Span (Into : in out Answer_Bytes; Used : in out Natural; First : Unsigned_32;
                       Length : Unsigned_16)
     with Pre  => Into'First = 0 and then Used <= Into'Length
                  and then Into'Length - Used >= ITEM_HEADER_BYTES + SPAN_BYTES,
          Post => Used = Used'Old + ITEM_HEADER_BYTES + SPAN_BYTES;

   procedure Put_Byte (Into : in out Answer_Bytes; Used : in out Natural; Value : Unsigned_8)
     with Pre  => Into'First = 0 and then Used < Into'Length,
          Post => Used = Used'Old + 1;

end Lexicon_Wire;

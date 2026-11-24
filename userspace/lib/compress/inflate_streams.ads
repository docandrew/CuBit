------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  An inflater for untrusted streams (SELF-003 step 2): RFC 1951 deflate
--  decoding, inside an RFC 1950 zlib wrapper or raw, streaming in and out
--  with bounded state.
--
--  @description
--  The caller hands in input and output buffers of any size; Inflate takes
--  what it can and says why it stopped: it needs more input, the output
--  buffer is full, the stream finished, or it was refused. It does no
--  input or output itself. git's loose objects and pack entries are zlib
--  streams; zip members are raw deflate.
--
--  The bit reader pulls one input byte at a time and only when the field
--  being read needs it, so it never reads ahead: when the stream
--  finishes, In_Next is just past its last byte, and whatever
--  follows (the next pack entry) is untouched. Huffman codes are decoded
--  one bit at a time against canonical tables (RFC 1951 3.2.2), so a
--  stream may stop and resume at any bit.
--
--  What it guarantees (proved at level 2, tests/inflate):
--  * no run-time error on any input: every table index, window position
--    and buffer position is in range;
--  * output goes only to Output (Out_Next'Old .. Out_Next - 1); the rest
--    of Output is untouched, and the totals count exactly what moved;
--  * the output never passes the caller's ceiling;
--  * a back-reference never reaches before the start of the output nor
--    past the window the zlib header declared;
--  * the bit reader holds only bits of the field it is reading, and at
--    the end no more than the last byte's padding (no byte read ahead);
--  * a refusal is final, and so is the end.
--  Malformed streams are refused with a reason: a bad zlib header, a
--  preset dictionary, the reserved block type, a stored length whose
--  complement does not match, too many codes, over-subscribed or
--  incomplete codes, a repeat with nothing to repeat or past the end, a
--  missing end-of-block code, a code or symbol that does not exist, a
--  distance too far back, an Adler-32 mismatch, or a truncated stream.
--  That the bytes produced are the stream's decoding is tested against
--  zlib (tests/inflate), not proved.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces;

package Inflate_Streams with SPARK_Mode, Pure is
   subtype Byte is Interfaces.Unsigned_8;
   use type Byte;
   type Byte_Array is array (Positive range <>) of Byte;

   --  Byte counts: ceilings and totals.
   type Byte_Count is range 0 .. 2 ** 62 - 1;

   --  RFC 1951 3.2.5: distances reach back at most 32768 bytes.
   WINDOW_BYTES : constant := 32_768;

   type Stream_Format is
     (Zlib_Stream,    --  RFC 1950: a two-byte header, deflate data, Adler-32
      Raw_Deflate);   --  RFC 1951 alone (zip members)

   --  Why a stream was refused.
   type Refusal is
     (No_Refusal,
      Bad_Method,            --  zlib CM is not 8 (deflate)
      Bad_Window,            --  zlib CINFO above 7 (a window over 32 KiB)
      Bad_Header_Check,      --  zlib (CMF * 256 + FLG) is not a multiple of 31
      Preset_Dictionary,     --  zlib FDICT: a dictionary this reader was not given
      Reserved_Block,        --  BTYPE 11
      Bad_Stored_Length,     --  a stored block's NLEN is not LEN's complement
      Too_Many_Codes,        --  HLIT above 286 or HDIST above 30
      Bad_Code_Lengths,      --  the code-length code is over-subscribed or incomplete
      Bad_Repeat,            --  code 16 with no previous length, or a repeat past the end
      Oversubscribed_Code,   --  more codes of some length than the length allows
      Incomplete_Code,       --  a literal/length or distance code with unused codes
      Missing_End_Of_Block,  --  no code for symbol 256
      Bad_Symbol,            --  a code with no symbol, literal/length 286-287, distance 30-31
      Distance_Too_Far,      --  before the start of the output, or past the window
      Bad_Checksum,          --  the Adler-32 of the output does not match
      Too_Large,             --  past the caller's ceiling
      Truncated);            --  the input ended inside the stream

   --  The reason's name (native builds discard enumeration images).
   function Name (Why : Refusal) return String is
     (case Why is
         when No_Refusal => "No_Refusal",
         when Bad_Method => "Bad_Method",
         when Bad_Window => "Bad_Window",
         when Bad_Header_Check => "Bad_Header_Check",
         when Preset_Dictionary => "Preset_Dictionary",
         when Reserved_Block => "Reserved_Block",
         when Bad_Stored_Length => "Bad_Stored_Length",
         when Too_Many_Codes => "Too_Many_Codes",
         when Bad_Code_Lengths => "Bad_Code_Lengths",
         when Bad_Repeat => "Bad_Repeat",
         when Oversubscribed_Code => "Oversubscribed_Code",
         when Incomplete_Code => "Incomplete_Code",
         when Missing_End_Of_Block => "Missing_End_Of_Block",
         when Bad_Symbol => "Bad_Symbol",
         when Distance_Too_Far => "Distance_Too_Far",
         when Bad_Checksum => "Bad_Checksum",
         when Too_Large => "Too_Large",
         when Truncated => "Truncated");

   type Progress is
     (Need_Input,     --  every input byte was taken; give more (or call Finish)
      Output_Full,    --  the output buffer is full; give more room
      Finished,       --  the stream ended well; In_Next is just past it
      Refused);       --  Failure says why; final

   type Inflater is private;

   function Valid (D : Inflater) return Boolean;
   function Failed (D : Inflater) return Boolean;
   function Done (D : Inflater) return Boolean;
   function Failure (D : Inflater) return Refusal;
   function Format (D : Inflater) return Stream_Format;
   function Ceiling (D : Inflater) return Byte_Count;
   --  Bytes taken from the input and written to the output so far.
   function Total_In (D : Inflater) return Byte_Count;
   function Total_Out (D : Inflater) return Byte_Count;
   --  Input bits taken but not yet used: under 16 (part of one field),
   --  and at the end under 8 (the last byte's padding).
   function Held_Bits (D : Inflater) return Natural;

   procedure Start (D : out Inflater; Kind : Stream_Format; Maximum_Output : Byte_Count)
   with Post => Valid (D) and then not Failed (D) and then not Done (D) and then Format (D) = Kind
                  and then Ceiling (D) = Maximum_Output and then Total_In (D) = 0
                  and then Total_Out (D) = 0 and then Held_Bits (D) = 0;

   --  Decode from Input (In_Next ..) into Output (Out_Next ..) until more
   --  input or room is needed, the stream ends, or it is refused.
   procedure Inflate
     (D : in out Inflater;
      Input : Byte_Array;
      In_Next : in out Positive;
      Output : in out Byte_Array;
      Out_Next : in out Positive;
      Result : out Progress)
   with
     Pre =>
       Valid (D)
       and then Input'Last < Positive'Last and then Output'Last < Positive'Last
       and then In_Next in Input'First .. Input'Last + 1
       and then Out_Next in Output'First .. Output'Last + 1
       and then Total_In (D) <= Byte_Count'Last - Byte_Count (Positive'Last),
     Post =>
       Valid (D)
       and then Format (D) = Format (D'Old) and then Ceiling (D) = Ceiling (D'Old)
       and then In_Next in In_Next'Old .. Input'Last + 1
       and then Out_Next in Out_Next'Old .. Output'Last + 1
       and then Total_In (D) = Total_In (D'Old) + Byte_Count (In_Next - In_Next'Old)
       and then Total_Out (D) = Total_Out (D'Old) + Byte_Count (Out_Next - Out_Next'Old)
       and then Total_Out (D) <= Ceiling (D)
       and then Held_Bits (D) <= 15 and then (if Done (D) then Held_Bits (D) <= 7)
       and then (for all I in Output'Range =>
                   (if I < Out_Next'Old or else I >= Out_Next then Output (I) = Output'Old (I)))
       and then (if Failed (D'Old) or else Done (D'Old) then
                   In_Next = In_Next'Old and then Out_Next = Out_Next'Old)
       and then (if Failed (D'Old) then Result = Refused)
       and then (if Done (D'Old) then Result = Finished)
       and then (Result = Refused) = Failed (D)
       and then (Result = Finished) = Done (D)
       and then (if Result = Refused then Failure (D) /= No_Refusal)
       and then (if Result = Need_Input then In_Next = Input'Last + 1)
       and then (if Result = Output_Full then Out_Next = Output'Last + 1);

   --  The input ended: a stream not yet finished is Truncated.
   procedure Finish (D : in out Inflater; Result : out Progress)
   with Pre => Valid (D),
        Post => Valid (D) and then Result in Finished | Refused
          and then (Result = Finished) = Done (D'Old)
          and then (Result = Refused) = Failed (D)
          and then (if Result = Refused then Failure (D) /= No_Refusal)
          and then Total_In (D) = Total_In (D'Old) and then Total_Out (D) = Total_Out (D'Old);

private
   use type Interfaces.Unsigned_32;

   --  RFC 1951 3.2.7: codes are at most 15 bits; 288 literal/length
   --  symbols (286 and 287 never occur), 32 distance symbols (30 and 31
   --  never occur), 19 code-length symbols.
   MAXIMUM_CODE_BITS : constant := 15;
   LITERAL_SYMBOLS : constant := 288;
   DISTANCE_SYMBOLS : constant := 32;
   CODE_LENGTH_SYMBOLS : constant := 19;
   --  The first length symbol: HLIT + 257 literal/length codes.
   FIRST_LENGTH_SYMBOL : constant := 257;
   --  HLIT + 257 and HDIST + 1 may be at most these.
   MAXIMUM_LITERAL_CODES : constant := 286;
   MAXIMUM_DISTANCE_CODES : constant := 30;
   MAXIMUM_LENGTHS : constant := MAXIMUM_LITERAL_CODES + MAXIMUM_DISTANCE_CODES;

   subtype Code_Length is Natural range 0 .. MAXIMUM_CODE_BITS;
   subtype Code_Bits is Code_Length range 1 .. MAXIMUM_CODE_BITS;
   subtype Symbol is Natural range 0 .. LITERAL_SYMBOLS - 1;
   subtype Symbol_Count is Natural range 0 .. LITERAL_SYMBOLS;

   type Length_Counts is array (Code_Bits) of Symbol_Count;
   type Offset_Table is array (1 .. MAXIMUM_CODE_BITS + 1) of Symbol_Count;
   type Symbol_Table is array (Symbol) of Symbol;

   --  A canonical Huffman code (RFC 1951 3.2.2): Count (L) codes of L
   --  bits; the symbols of the L-bit codes, in code order, are
   --  Symbols (Offset (L) .. Offset (L + 1) - 1).
   type Huffman is record
      Count : Length_Counts := [others => 0];
      Offset : Offset_Table := [others => 0];
      Symbols : Symbol_Table := [others => 0];
   end record;

   function Well_Formed (H : Huffman) return Boolean is
     (H.Offset (1) = 0
      and then (for all L in Code_Bits => H.Offset (L) + H.Count (L) = H.Offset (L + 1)));

   type Length_List is array (Natural range <>) of Code_Length;
   subtype Lengths_Buffer is Length_List (0 .. MAXIMUM_LENGTHS - 1);
   subtype Code_Length_Lengths is Length_List (0 .. CODE_LENGTH_SYMBOLS - 1);

   subtype Window_Index is Natural range 0 .. WINDOW_BYTES - 1;
   type Window_Buffer is array (Window_Index) of Byte;
   subtype Window_Size is Natural range 1 .. WINDOW_BYTES;

   --  RFC 1951 3.2.5: a match is 3 to 258 bytes.
   MAXIMUM_MATCH : constant := 258;
   subtype Match_Length is Natural range 0 .. MAXIMUM_MATCH;

   --  Adler-32 (RFC 1950 8.2): two sums modulo 65521.
   ADLER_MODULUS : constant := 65_521;
   subtype Adler_Sum is Natural range 0 .. ADLER_MODULUS - 1;

   type Phase_Kind is
     (Zlib_Header,         --  CMF and FLG
      Block_Header,        --  BFINAL and BTYPE
      Stored_Lengths,      --  LEN and NLEN, byte-aligned
      Stored_Data,
      Dynamic_Counts,      --  HLIT, HDIST, HCLEN
      Code_Length_Codes,   --  HCLEN + 4 three-bit lengths
      Code_Lengths,        --  HLIT + HDIST lengths, in the code-length code
      Repeat_Extra,        --  the extra bits of code 16, 17 or 18
      Literal,             --  a literal/length symbol
      Length_Extra,
      Distance,            --  a distance symbol
      Distance_Extra,
      Copy,                --  a match being copied out of the window
      Checksum,            --  the Adler-32, byte-aligned, most significant byte first
      Done_Phase,
      Failed_Phase);

   type Code_Kind is (Code_Length_Code, Literal_Code, Distance_Code);

   --  RFC 1951 3.2.5: the extra bits of each distance symbol.
   LAST_DISTANCE_SYMBOL : constant := 29;
   subtype Extra_Bits is Natural range 0 .. 13;
   type Distance_Extras is array (0 .. LAST_DISTANCE_SYMBOL) of Extra_Bits;
   DISTANCE_EXTRA_BITS : constant Distance_Extras :=
     [0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13];
   --  The zlib header and a dynamic block's three counts are read whole.
   ZLIB_HEADER_BITS : constant := 16;
   DYNAMIC_COUNT_BITS : constant := 14;

   type Inflater is record
      Phase : Phase_Kind := Block_Header;
      Why : Refusal := No_Refusal;
      Kind : Stream_Format := Raw_Deflate;
      Ceiling : Byte_Count := 0;
      Total_In : Byte_Count := 0;
      Total_Out : Byte_Count := 0;
      --  The bit reader: Bit_Count bits of the last input bytes, least
      --  significant first (RFC 1951 3.1.1).
      Bits : Interfaces.Unsigned_32 := 0;
      Bit_Count : Natural range 0 .. 31 := 0;
      Final_Block : Boolean := False;
      --  The window: the last WINDOW_BYTES bytes of output, circular.
      Window : Window_Buffer := [others => 0];
      Window_Next : Window_Index := 0;
      Window_Limit : Window_Size := WINDOW_BYTES;
      Adler_A : Adler_Sum := 1;
      Adler_B : Adler_Sum := 0;
      Expected : Interfaces.Unsigned_32 := 0;
      --  Bytes of LEN/NLEN or the Adler-32 read so far, into Expected.
      Aligned_Bytes : Natural range 0 .. 3 := 0;
      --  The current block's codes, and the code-length code.
      Literals : Huffman;
      Distances : Huffman;
      Lengths_Code : Huffman;
      --  A Huffman code being read bit by bit: Code_Read bits so far make
      --  Code; First is the first code of that length.
      Code_Read : Natural range 0 .. MAXIMUM_CODE_BITS - 1 := 0;
      Code : Natural range 0 .. 2 ** MAXIMUM_CODE_BITS := 0;
      First : Natural range 0 .. 2 ** MAXIMUM_CODE_BITS := 0;
      --  A dynamic block's header.
      Literal_Codes : Natural range 0 .. MAXIMUM_LITERAL_CODES := 0;
      Distance_Codes : Natural range 0 .. MAXIMUM_DISTANCE_CODES := 0;
      Length_Codes : Natural range 0 .. CODE_LENGTH_SYMBOLS := 0;
      Code_Length_Read : Natural range 0 .. CODE_LENGTH_SYMBOLS := 0;
      Code_Length_Lens : Code_Length_Lengths := [others => 0];
      Lengths : Lengths_Buffer := [others => 0];
      Lengths_Read : Natural range 0 .. MAXIMUM_LENGTHS := 0;
      Repeat_Symbol : Natural range 16 .. 18 := 16;
      --  A stored block's bytes left, or a match's length and distance.
      Stored_Remaining : Natural range 0 .. 65_535 := 0;
      Match_Symbol : Natural range 0 .. 28 := 0;
      Copy_Remaining : Match_Length := 0;
      Copy_Distance : Natural range 0 .. WINDOW_BYTES := 0;
      Distance_Symbol : Natural range 0 .. LAST_DISTANCE_SYMBOL := 0;
   end record;

   --  Bytes of history a back-reference may use.
   function Window_Have (D : Inflater) return Natural is
     (if D.Total_Out >= WINDOW_BYTES then WINDOW_BYTES else Natural (D.Total_Out));

   --  The width of a field wider than a byte that the phase reads: its
   --  bits may be held, part read, while the input runs dry.
   function Field_Width (D : Inflater) return Natural is
     (case D.Phase is
         when Zlib_Header => ZLIB_HEADER_BITS,
         when Dynamic_Counts => DYNAMIC_COUNT_BITS,
         when Distance_Extra => DISTANCE_EXTRA_BITS (D.Distance_Symbol),
         when others => 0);

   --  What holds in every phase.
   function Valid_Common (D : Inflater) return Boolean is
     ((D.Phase = Failed_Phase) = (D.Why /= No_Refusal)
      and then D.Bit_Count <= 15
      and then D.Total_Out <= D.Ceiling
      and then Well_Formed (D.Literals) and then Well_Formed (D.Distances)
      and then Well_Formed (D.Lengths_Code)
      and then D.Lengths_Read <= D.Literal_Codes + D.Distance_Codes
      and then D.Code_Length_Read <= D.Length_Codes);

   function Valid (D : Inflater) return Boolean is
     (Valid_Common (D)
      and then (D.Phase = Failed_Phase or else D.Bit_Count <= 7 or else D.Bit_Count < Field_Width (D))
      and then (if D.Phase = Zlib_Header then D.Kind = Zlib_Stream)
      and then (if D.Phase = Checksum then D.Kind = Zlib_Stream and then D.Bit_Count = 0)
      and then (if D.Phase in Stored_Lengths | Stored_Data then D.Bit_Count = 0)
      and then (if D.Phase = Stored_Data then D.Stored_Remaining >= 1)
      and then (if D.Phase = Code_Length_Codes then
                  D.Code_Length_Read < D.Length_Codes and then D.Lengths_Read = 0
                  and then D.Literal_Codes >= FIRST_LENGTH_SYMBOL and then D.Distance_Codes >= 1)
      and then (if D.Phase in Code_Lengths | Repeat_Extra then
                  D.Literal_Codes >= FIRST_LENGTH_SYMBOL and then D.Distance_Codes >= 1
                  and then D.Lengths_Read < D.Literal_Codes + D.Distance_Codes)
      and then (if D.Phase in Distance | Distance_Extra then D.Copy_Remaining >= 1)
      and then (if D.Phase = Copy then
                  D.Copy_Remaining >= 1 and then D.Copy_Distance in 1 .. Window_Have (D)
                  and then D.Copy_Distance <= D.Window_Limit));

   function Failed (D : Inflater) return Boolean is (D.Phase = Failed_Phase);
   function Done (D : Inflater) return Boolean is (D.Phase = Done_Phase);
   function Failure (D : Inflater) return Refusal is (D.Why);
   function Format (D : Inflater) return Stream_Format is (D.Kind);
   function Ceiling (D : Inflater) return Byte_Count is (D.Ceiling);
   function Total_In (D : Inflater) return Byte_Count is (D.Total_In);
   function Total_Out (D : Inflater) return Byte_Count is (D.Total_Out);
   function Held_Bits (D : Inflater) return Natural is (D.Bit_Count);
end Inflate_Streams;

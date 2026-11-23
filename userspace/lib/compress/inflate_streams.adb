------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  The inflater (see the spec). Section numbers are RFC 1951's (deflate)
--  and RFC 1950's (zlib). Which incomplete codes are accepted follows
--  zlib's inflate as its documentation describes it, so that every stream
--  zlib accepts is accepted here: a literal/length or distance code may be
--  incomplete only when it is a single one-bit code, or (distances) has no
--  codes at all; the code-length code must be complete.
------------------------------------------------------------------------------
pragma Ada_2022;

package body Inflate_Streams with SPARK_Mode is
   subtype U32 is Interfaces.Unsigned_32;

   --  RFC 1950 2.2: CMF's method and window, FLG's dictionary flag, and the
   --  header check.
   DEFLATE_METHOD : constant := 8;
   MAXIMUM_WINDOW_INFO : constant := 7;
   HEADER_CHECK_DIVISOR : constant := 31;
   DICTIONARY_FLAG : constant := 16#20#;
   type Window_Sizes is array (0 .. MAXIMUM_WINDOW_INFO) of Window_Size;
   --  2 ** (CINFO + 8).
   WINDOW_OF_INFO : constant Window_Sizes := [256, 512, 1_024, 2_048, 4_096, 8_192, 16_384, 32_768];

   --  RFC 1951 3.2.3: BTYPE.
   STORED_BLOCK : constant := 0;
   FIXED_BLOCK : constant := 1;
   DYNAMIC_BLOCK : constant := 2;

   --  RFC 1951 3.2.5: the end-of-block symbol, the length symbols, and
   --  the base and extra bits of each length and distance symbol.
   END_OF_BLOCK : constant := 256;
   LAST_LENGTH_SYMBOL : constant := 285;
   type Length_Bases is array (0 .. LAST_LENGTH_SYMBOL - FIRST_LENGTH_SYMBOL) of Natural range 3 .. MAXIMUM_MATCH;
   type Length_Extras is array (0 .. LAST_LENGTH_SYMBOL - FIRST_LENGTH_SYMBOL) of Extra_Bits;
   LENGTH_BASE : constant Length_Bases :=
     [3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31, 35, 43, 51, 59, 67, 83, 99, 115, 131, 163,
      195, 227, 258];
   LENGTH_EXTRA_BITS : constant Length_Extras :=
     [0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0];
   type Distance_Bases is array (0 .. LAST_DISTANCE_SYMBOL) of Window_Size;
   DISTANCE_BASE : constant Distance_Bases :=
     [1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193, 257, 385, 513, 769, 1_025, 1_537,
      2_049, 3_073, 4_097, 6_145, 8_193, 12_289, 16_385, 24_577];

   --  RFC 1951 3.2.7: HLIT, HDIST and HCLEN's widths and biases, the order
   --  of the code-length code's lengths, and the repeat codes 16, 17, 18.
   HLIT_BITS : constant := 5;
   HDIST_BITS : constant := 5;
   MINIMUM_DISTANCE_CODES : constant := 1;
   MINIMUM_LENGTH_CODES : constant := 4;
   CODE_LENGTH_BITS : constant := 3;
   REPEAT_PREVIOUS : constant := 16;
   REPEAT_ZERO : constant := 17;
   REPEAT_ZERO_LONG : constant := 18;
   type Code_Length_Order is array (0 .. CODE_LENGTH_SYMBOLS - 1) of Natural range 0 .. CODE_LENGTH_SYMBOLS - 1;
   ORDER : constant Code_Length_Order := [16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15];

   --  RFC 1951 3.2.6: the fixed codes' lengths.
   FIXED_LITERALS : constant Length_List (0 .. LITERAL_SYMBOLS - 1) :=
     [0 .. 143 => 8, 144 .. 255 => 9, 256 .. 279 => 7, 280 .. 287 => 8];
   FIXED_DISTANCES : constant Length_List (0 .. DISTANCE_SYMBOLS - 1) := [others => 5];

   --  The largest field read at once (LEN), and masks of N low bits.
   MAXIMUM_FIELD_BITS : constant := 16;
   subtype Field_Bits is Natural range 0 .. MAXIMUM_FIELD_BITS;
   type Mask_Table is array (Field_Bits) of Natural;
   MASKS : constant Mask_Table :=
     [0, 1, 3, 7, 15, 31, 63, 127, 255, 511, 1_023, 2_047, 4_095, 8_191, 16_383, 32_767, 65_535];

   BYTE_BITS : constant := 8;

   ---------------------------------------------------------------------------
   --  The bit reader (RFC 1951 3.1.1: least significant bit first).

   --  Pull input bytes until N bits are held; Have says whether they are.
   procedure Pull
     (D : in out Inflater; Input : Byte_Array; In_Next : in out Positive; N : Field_Bits;
      Have : out Boolean)
   with
     Pre => D.Bit_Count <= 15 and then Input'Last < Positive'Last
       and then In_Next in Input'First .. Input'Last + 1,
     Post => In_Next in In_Next'Old .. Input'Last + 1
       and then D = (D'Old with delta Bits => D.Bits, Bit_Count => D.Bit_Count)
       and then (if D.Bit_Count'Old >= N then D.Bit_Count = D.Bit_Count'Old else D.Bit_Count <= N + 7)
       and then (if Have then D.Bit_Count >= N else D.Bit_Count < N and then In_Next = Input'Last + 1)
   is
   begin
      while D.Bit_Count < N loop
         pragma Loop_Invariant (In_Next in In_Next'Loop_Entry .. Input'Last + 1);
         pragma Loop_Invariant (D = (D'Loop_Entry with delta Bits => D.Bits, Bit_Count => D.Bit_Count));
         pragma Loop_Invariant (D.Bit_Count < N);
         pragma Loop_Invariant (D.Bit_Count'Loop_Entry < N);
         pragma Loop_Invariant (D.Bit_Count <= N + 7);
         if In_Next > Input'Last then
            Have := False;
            return;
         end if;
         D.Bits := D.Bits or Interfaces.Shift_Left (U32 (Input (In_Next)), D.Bit_Count);
         D.Bit_Count := D.Bit_Count + BYTE_BITS;
         In_Next := In_Next + 1;
      end loop;
      Have := True;
   end Pull;

   --  Take N held bits.
   procedure Take (D : in out Inflater; N : Field_Bits; Value : out Natural)
   with
     Pre => D.Bit_Count >= N,
     Post => Value <= MASKS (N) and then D.Bit_Count = D.Bit_Count'Old - N
       and then D = (D'Old with delta Bits => D.Bits, Bit_Count => D.Bit_Count)
   is
   begin
      Value := Natural (D.Bits and U32 (MASKS (N)));
      D.Bits := Interfaces.Shift_Right (D.Bits, N);
      D.Bit_Count := D.Bit_Count - N;
   end Take;

   ---------------------------------------------------------------------------
   --  Canonical Huffman codes (RFC 1951 3.2.2).

   --  The code whose symbol S (from 0) has length Lengths (Lengths'First +
   --  S). On a refusal H is the empty code.
   procedure Build (H : out Huffman; Lengths : Length_List; Kind : Code_Kind; Why : out Refusal)
   with
     Pre => Lengths'Length in 1 .. LITERAL_SYMBOLS and then Lengths'Last < Natural'Last,
     Post => Well_Formed (H)
   is
      Count : Length_Counts := [others => 0];
      Used : Symbol_Count := 0;
      Longest : Code_Length := 0;
      --  Codes of the current length not yet assigned (no more than 2 ** L).
      Left : Natural := 1;
      Next : Offset_Table;
      MAXIMUM_LEFT : constant := 2 ** MAXIMUM_CODE_BITS;
      Bad_Code : constant Refusal :=
        (if Kind = Code_Length_Code then Bad_Code_Lengths else Oversubscribed_Code);
   begin
      H := (others => <>);
      Why := No_Refusal;
      for S in Lengths'Range loop
         if Lengths (S) /= 0 then
            Count (Lengths (S)) := Count (Lengths (S)) + 1;
            Used := Used + 1;
            if Lengths (S) > Longest then
               Longest := Lengths (S);
            end if;
         end if;
         pragma Loop_Invariant (for all L in Code_Bits => Count (L) <= S - Lengths'First + 1);
         pragma Loop_Invariant (Used <= S - Lengths'First + 1);
      end loop;

      for L in Code_Bits loop
         pragma Loop_Invariant (Left <= MAXIMUM_LEFT);
         if Left * 2 < Count (L) then
            Why := Bad_Code;
            return;
         end if;
         Left := Left * 2 - Count (L);
         if Left > MAXIMUM_LEFT then
            --  2 ** L at most; this bounds Left for the prover.
            Why := Bad_Code;
            return;
         end if;
      end loop;
      if Left > 0 then
         if Kind = Code_Length_Code then
            Why := Bad_Code_Lengths;
            return;
         elsif Longest > 1 then
            Why := Incomplete_Code;
            return;
         end if;
      end if;
      if Kind = Code_Length_Code and then Used = 0 then
         Why := Bad_Code_Lengths;
         return;
      end if;

      for L in Code_Bits loop
         pragma Loop_Invariant (H.Offset (1) = 0);
         pragma Loop_Invariant (for all K in 1 .. L - 1 => H.Offset (K) + Count (K) = H.Offset (K + 1));
         if H.Offset (L) + Count (L) > LITERAL_SYMBOLS then
            --  No more codes than symbols; this bounds Offset for the prover.
            H := (others => <>);
            Why := Bad_Code;
            return;
         end if;
         H.Offset (L + 1) := H.Offset (L) + Count (L);
      end loop;
      H.Count := Count;
      pragma Assert (Well_Formed (H));

      Next := H.Offset;
      for S in Lengths'Range loop
         pragma Loop_Invariant (Well_Formed (H));
         pragma Loop_Invariant (for all L in Code_Bits => Next (L) <= H.Offset (L + 1));
         if Lengths (S) /= 0 then
            declare
               L : constant Code_Bits := Lengths (S);
            begin
               if Next (L) >= H.Offset (L + 1) then
                  --  Count (L) symbols have length L; this bounds Next.
                  H := (others => <>);
                  Why := Bad_Code;
                  return;
               end if;
               H.Symbols (Next (L)) := S - Lengths'First;
               Next (L) := Next (L) + 1;
            end;
         end if;
      end loop;
   end Build;

   type Decode_Outcome is (Decoded, Need_More, No_Such_Code);

   --  One more bit of a code: Code_Read bits read so far make Code, and
   --  First is the first code of that length.
   procedure Decode_Bit
     (H : Huffman; Bit : Natural; Code_Read : in out Natural; Code : in out Natural;
      First : in out Natural; Outcome : out Decode_Outcome; Value : out Symbol)
   with
     Pre => Well_Formed (H) and then Bit <= 1 and then Code_Read < MAXIMUM_CODE_BITS
       and then Code <= 2 ** MAXIMUM_CODE_BITS and then First <= 2 ** MAXIMUM_CODE_BITS,
     Post => Code_Read < MAXIMUM_CODE_BITS and then Code <= 2 ** MAXIMUM_CODE_BITS
       and then First <= 2 ** MAXIMUM_CODE_BITS
       and then (if Outcome /= Need_More then Code_Read = 0 and then Code = 0 and then First = 0)
   is
      Length : constant Code_Bits := Code_Read + 1;
      Count : constant Symbol_Count := H.Count (Length);
      Extended : constant Natural := Code * 2 + Bit;
   begin
      Value := 0;
      if Extended >= First and then Extended - First < Count then
         Value := H.Symbols (H.Offset (Length) + (Extended - First));
         Outcome := Decoded;
      elsif Length = MAXIMUM_CODE_BITS
        or else Extended > 2 ** MAXIMUM_CODE_BITS
        or else (First + Count) * 2 > 2 ** MAXIMUM_CODE_BITS
      then
         --  No code of 15 bits or fewer starts this way.
         Outcome := No_Such_Code;
      else
         Code := Extended;
         First := (First + Count) * 2;
         Code_Read := Length;
         Outcome := Need_More;
         return;
      end if;
      Code_Read := 0;
      Code := 0;
      First := 0;
   end Decode_Bit;

   ---------------------------------------------------------------------------
   --  Output, refusals and block ends.

   procedure Emit (D : in out Inflater; Output : in out Byte_Array; Out_Next : in out Positive; B : Byte)
   with
     Pre => Output'Last < Positive'Last and then Out_Next in Output'Range and then D.Total_Out < D.Ceiling,
     Post => Out_Next = Out_Next'Old + 1 and then D.Total_Out = D.Total_Out'Old + 1
       and then Output = (Output'Old with delta Out_Next'Old => B)
       and then D = (D'Old with delta Window => D.Window, Window_Next => D.Window_Next,
                     Total_Out => D.Total_Out, Adler_A => D.Adler_A, Adler_B => D.Adler_B)
   is
   begin
      Output (Out_Next) := B;
      Out_Next := Out_Next + 1;
      D.Window (D.Window_Next) := B;
      D.Window_Next := (if D.Window_Next = Window_Index'Last then 0 else D.Window_Next + 1);
      D.Total_Out := D.Total_Out + 1;
      if D.Kind = Zlib_Stream then
         D.Adler_A := (D.Adler_A + Natural (B)) mod ADLER_MODULUS;
         D.Adler_B := (D.Adler_B + D.Adler_A) mod ADLER_MODULUS;
      end if;
   end Emit;

   procedure Refuse (D : in out Inflater; Why : Refusal)
   with
     Pre => Why /= No_Refusal,
     Post => D = (D'Old with delta Phase => Failed_Phase, Why => Why)
   is
   begin
      D.Phase := Failed_Phase;
      D.Why := Why;
   end Refuse;

   --  After a block's last byte: the next block, the zlib trailer (on a
   --  byte boundary: the last byte's other bits are padding), or the end.
   procedure End_Block (D : in out Inflater)
   with
     Pre => D.Bit_Count <= 7,
     Post => D.Phase in Block_Header | Checksum | Done_Phase and then D.Bit_Count <= 7
       and then (if D.Phase = Checksum then D.Kind = Zlib_Stream and then D.Bit_Count = 0)
       and then D = (D'Old with delta Phase => D.Phase, Bits => D.Bits, Bit_Count => D.Bit_Count,
                     Expected => D.Expected, Aligned_Bytes => D.Aligned_Bytes)
   is
   begin
      if not D.Final_Block then
         D.Phase := Block_Header;
      elsif D.Kind = Zlib_Stream then
         D.Bits := 0;
         D.Bit_Count := 0;
         D.Expected := 0;
         D.Aligned_Bytes := 0;
         D.Phase := Checksum;
      else
         D.Phase := Done_Phase;
      end if;
   end End_Block;

   --  The decoded code lengths are all read: build the block's codes.
   procedure Lengths_Done (D : in out Inflater)
   with
     Pre => Valid_Common (D) and then D.Bit_Count <= 7 and then D.Phase in Code_Lengths | Repeat_Extra
       and then D.Literal_Codes >= FIRST_LENGTH_SYMBOL and then D.Distance_Codes >= MINIMUM_DISTANCE_CODES,
     Post => Valid (D) and then D.Phase in Literal | Failed_Phase
       and then D.Kind = D'Old.Kind and then D.Ceiling = D'Old.Ceiling
       and then D.Total_In = D'Old.Total_In and then D.Total_Out = D'Old.Total_Out
   is
      Why : Refusal;
      Literal_Last : constant Natural := D.Literal_Codes - 1;
   begin
      if D.Lengths (END_OF_BLOCK) = 0 then
         Refuse (D, Missing_End_Of_Block);
         return;
      end if;
      Build (D.Literals, D.Lengths (0 .. Literal_Last), Literal_Code, Why);
      if Why /= No_Refusal then
         Refuse (D, Why);
         return;
      end if;
      Build (D.Distances, D.Lengths (D.Literal_Codes .. Literal_Last + D.Distance_Codes), Distance_Code, Why);
      if Why /= No_Refusal then
         Refuse (D, Why);
         return;
      end if;
      D.Code_Read := 0;
      D.Code := 0;
      D.First := 0;
      D.Phase := Literal;
   end Lengths_Done;

   --  A match's distance is known: copy, unless it reaches too far back.
   procedure Start_Copy (D : in out Inflater; Distance_Value : Natural)
   with
     Pre => Valid (D) and then D.Bit_Count <= 7 and then D.Phase in Distance | Distance_Extra
       and then D.Copy_Remaining >= 1,
     Post => Valid (D) and then D.Phase in Copy | Failed_Phase
       and then D.Kind = D'Old.Kind and then D.Ceiling = D'Old.Ceiling
       and then D.Total_In = D'Old.Total_In and then D.Total_Out = D'Old.Total_Out
   is
   begin
      if Distance_Value = 0 or else Distance_Value > Window_Have (D) or else Distance_Value > D.Window_Limit then
         Refuse (D, Distance_Too_Far);
         return;
      end if;
      D.Copy_Distance := Distance_Value;
      D.Phase := Copy;
   end Start_Copy;

   ---------------------------------------------------------------------------
   --  One step: one field, one symbol's bit, or a run of bytes. A step
   --  either stops (Result says why) or leaves the inflater ready for the
   --  next step; Input_Stepped and Output_Stepped are that contract.

   function Input_Stepped
     (Before, After : Inflater; In_Before, In_After : Positive; Input_Last : Natural;
      Result : Progress; Stop : Boolean) return Boolean
   is (Valid (After)
       and then After.Kind = Before.Kind and then After.Ceiling = Before.Ceiling
       and then After.Total_In = Before.Total_In
       and then In_After in In_Before .. Input_Last + 1
       and then (if not Stop then not Failed (After) and then not Done (After))
       and then (if Stop then
                   (Result = Refused) = Failed (After) and then (Result = Finished) = Done (After)
                   and then (if Result = Refused then Failure (After) /= No_Refusal)
                   and then (if Result = Need_Input then In_After = Input_Last + 1)))
   with Ghost, Pre => Input_Last < Positive'Last;

   function Output_Stepped
     (Before, After : Inflater; Old_Output, Output : Byte_Array; Out_Before, Out_After : Positive;
      Result : Progress; Stop : Boolean) return Boolean
   is (Old_Output'First = Output'First and then Old_Output'Last = Output'Last
       and then Out_After in Out_Before .. Output'Last + 1
       and then After.Total_Out = Before.Total_Out + Byte_Count (Out_After - Out_Before)
       and then (for all I in Output'Range =>
                   (if I < Out_Before or else I >= Out_After then Output (I) = Old_Output (I)))
       and then (if Stop and then Result = Output_Full then Out_After = Output'Last + 1))
   with Ghost, Pre => Output'Last < Positive'Last and then Out_Before in Output'First .. Output'Last + 1;

   procedure Stop_Need_Input (Result : out Progress; Stop : out Boolean)
   with Post => Stop and then Result = Need_Input
   is
   begin
      Stop := True;
      Result := Need_Input;
   end Stop_Need_Input;

   procedure Stop_Refused (D : in out Inflater; Reason : Refusal; Result : out Progress; Stop : out Boolean)
   with Pre => Reason /= No_Refusal,
        Post => Stop and then Result = Refused and then D = (D'Old with delta Phase => Failed_Phase, Why => Reason)
   is
   begin
      Refuse (D, Reason);
      Stop := True;
      Result := Refused;
   end Stop_Refused;


   procedure Step_Headers
     (D : in out Inflater;
      Input : Byte_Array;
      In_Next : in out Positive;
      Result : out Progress;
      Stop : out Boolean)
   with
     Pre =>
       Valid (D) and then not Failed (D) and then not Done (D) and then D.Phase in Zlib_Header | Block_Header | Stored_Lengths | Checksum
       and then Input'Last < Positive'Last and then In_Next in Input'First .. Input'Last + 1,
     Post =>
       Input_Stepped (D'Old, D, In_Next'Old, In_Next, Input'Last, Result, Stop)
       and then D.Total_Out = D'Old.Total_Out and then (if Stop then Result /= Output_Full)
   is
      Have : Boolean;
      Value : Natural;
      Why : Refusal;
   begin
      Stop := False;
      Result := Need_Input;
      case D.Phase is
         when Zlib_Header =>
            Pull (D, Input, In_Next, ZLIB_HEADER_BITS, Have);
            if not Have then
               Stop_Need_Input (Result, Stop);
               return;
            end if;
            Take (D, ZLIB_HEADER_BITS, Value);
            declare
               CMF : constant Natural := Value mod 256;
               FLG : constant Natural := Value / 256;
            begin
               if CMF mod 16 /= DEFLATE_METHOD then
                  Stop_Refused (D, Bad_Method, Result, Stop);
               elsif CMF / 16 > MAXIMUM_WINDOW_INFO then
                  Stop_Refused (D, Bad_Window, Result, Stop);
               elsif (CMF * 256 + FLG) mod HEADER_CHECK_DIVISOR /= 0 then
                  Stop_Refused (D, Bad_Header_Check, Result, Stop);
               elsif (FLG / DICTIONARY_FLAG) mod 2 = 1 then
                  Stop_Refused (D, Preset_Dictionary, Result, Stop);
               else
                  D.Window_Limit := WINDOW_OF_INFO (CMF / 16);
                  D.Phase := Block_Header;
               end if;
            end;

         when Block_Header =>
            Pull (D, Input, In_Next, 3, Have);
            if not Have then
               Stop_Need_Input (Result, Stop);
               return;
            end if;
            Take (D, 3, Value);
            D.Final_Block := Value mod 2 = 1;
            case Value / 2 is
               when STORED_BLOCK =>
                  --  3.2.4: the rest of the current byte is skipped.
                  D.Bits := 0;
                  D.Bit_Count := 0;
                  D.Expected := 0;
                  D.Aligned_Bytes := 0;
                  D.Phase := Stored_Lengths;
               when FIXED_BLOCK =>
                  Build (D.Literals, FIXED_LITERALS, Literal_Code, Why);
                  if Why /= No_Refusal then
                     Stop_Refused (D, Why, Result, Stop);
                     return;
                  end if;
                  Build (D.Distances, FIXED_DISTANCES, Distance_Code, Why);
                  if Why /= No_Refusal then
                     Stop_Refused (D, Why, Result, Stop);
                     return;
                  end if;
                  D.Code_Read := 0;
                  D.Code := 0;
                  D.First := 0;
                  D.Phase := Literal;
               when DYNAMIC_BLOCK =>
                  D.Phase := Dynamic_Counts;
               when others =>
                  Stop_Refused (D, Reserved_Block, Result, Stop);
            end case;

         when Stored_Lengths | Checksum =>
            --  Byte-aligned: LEN and NLEN (least significant byte first),
            --  or the Adler-32 (most significant byte first).
            if In_Next > Input'Last then
               Stop_Need_Input (Result, Stop);
               return;
            end if;
            if D.Phase = Stored_Lengths then
               D.Expected := D.Expected or Interfaces.Shift_Left (U32 (Input (In_Next)), BYTE_BITS * D.Aligned_Bytes);
            else
               D.Expected := Interfaces.Shift_Left (D.Expected, BYTE_BITS) or U32 (Input (In_Next));
            end if;
            In_Next := In_Next + 1;
            if D.Aligned_Bytes < 3 then
               D.Aligned_Bytes := D.Aligned_Bytes + 1;
            elsif D.Phase = Checksum then
               if D.Expected /= U32 (D.Adler_B) * 2 ** 16 + U32 (D.Adler_A) then
                  Stop_Refused (D, Bad_Checksum, Result, Stop);
               else
                  D.Phase := Done_Phase;
                  Stop := True;
                  Result := Finished;
               end if;
            else
               declare
                  Length : constant U32 := D.Expected and 16#FFFF#;
                  Complement : constant U32 := Interfaces.Shift_Right (D.Expected, 16);
               begin
                  if Complement /= (Length xor 16#FFFF#) then
                     Stop_Refused (D, Bad_Stored_Length, Result, Stop);
                  elsif Length = 0 then
                     End_Block (D);
                     if D.Phase = Done_Phase then
                        Stop := True;
                        Result := Finished;
                     end if;
                  else
                     D.Stored_Remaining := Natural (Length);
                     D.Phase := Stored_Data;
                  end if;
               end;
            end if;

         when others =>
            --  Excluded by the precondition.
            Stop_Refused (D, Bad_Symbol, Result, Stop);
      end case;
   end Step_Headers;

   procedure Step_Code_Lengths
     (D : in out Inflater;
      Input : Byte_Array;
      In_Next : in out Positive;
      Result : out Progress;
      Stop : out Boolean)
   with
     Pre =>
       Valid (D) and then not Failed (D) and then not Done (D) and then D.Phase in Dynamic_Counts | Code_Length_Codes | Code_Lengths | Repeat_Extra
       and then Input'Last < Positive'Last and then In_Next in Input'First .. Input'Last + 1,
     Post =>
       Input_Stepped (D'Old, D, In_Next'Old, In_Next, Input'Last, Result, Stop)
       and then D.Total_Out = D'Old.Total_Out and then (if Stop then Result /= Output_Full)
   is
      Have : Boolean;
      Value : Natural;
      Outcome : Decode_Outcome;
      Sym : Symbol;
      Why : Refusal;
   begin
      Stop := False;
      Result := Need_Input;
      case D.Phase is
         when Dynamic_Counts =>
            Pull (D, Input, In_Next, DYNAMIC_COUNT_BITS, Have);
            if not Have then
               Stop_Need_Input (Result, Stop);
               return;
            end if;
            Take (D, DYNAMIC_COUNT_BITS, Value);
            declare
               Literals : constant Natural := Value mod 2 ** HLIT_BITS + FIRST_LENGTH_SYMBOL;
               Distances : constant Natural := (Value / 2 ** HLIT_BITS) mod 2 ** HDIST_BITS + MINIMUM_DISTANCE_CODES;
               Length_Codes : constant Natural := Value / 2 ** (HLIT_BITS + HDIST_BITS) + MINIMUM_LENGTH_CODES;
            begin
               if Literals > MAXIMUM_LITERAL_CODES or else Distances > MAXIMUM_DISTANCE_CODES then
                  Stop_Refused (D, Too_Many_Codes, Result, Stop);
               else
                  D.Literal_Codes := Literals;
                  D.Distance_Codes := Distances;
                  D.Length_Codes := Length_Codes;
                  D.Code_Length_Read := 0;
                  D.Code_Length_Lens := [others => 0];
                  D.Lengths_Read := 0;
                  D.Phase := Code_Length_Codes;
               end if;
            end;

         when Code_Length_Codes =>
            Pull (D, Input, In_Next, CODE_LENGTH_BITS, Have);
            if not Have then
               Stop_Need_Input (Result, Stop);
               return;
            end if;
            Take (D, CODE_LENGTH_BITS, Value);
            D.Code_Length_Lens (ORDER (D.Code_Length_Read)) := Value;
            D.Code_Length_Read := D.Code_Length_Read + 1;
            if D.Code_Length_Read = D.Length_Codes then
               Build (D.Lengths_Code, D.Code_Length_Lens, Code_Length_Code, Why);
               if Why /= No_Refusal then
                  Stop_Refused (D, Bad_Code_Lengths, Result, Stop);
                  return;
               end if;
               D.Code_Read := 0;
               D.Code := 0;
               D.First := 0;
               D.Phase := Code_Lengths;
            end if;

         when Code_Lengths =>
            Pull (D, Input, In_Next, 1, Have);
            if not Have then
               Stop_Need_Input (Result, Stop);
               return;
            end if;
            Take (D, 1, Value);
            Decode_Bit (D.Lengths_Code, Value, D.Code_Read, D.Code, D.First, Outcome, Sym);
            case Outcome is
               when Need_More =>
                  null;
               when No_Such_Code =>
                  Stop_Refused (D, Bad_Code_Lengths, Result, Stop);
               when Decoded =>
                  if Sym > REPEAT_ZERO_LONG then
                     --  Not reachable: the code has 19 symbols.
                     Stop_Refused (D, Bad_Code_Lengths, Result, Stop);
                  elsif Sym < REPEAT_PREVIOUS then
                     D.Lengths (D.Lengths_Read) := Sym;
                     D.Lengths_Read := D.Lengths_Read + 1;
                     if D.Lengths_Read = D.Literal_Codes + D.Distance_Codes then
                        Lengths_Done (D);
                        if D.Phase = Failed_Phase then
                           Stop := True;
                           Result := Refused;
                        end if;
                     end if;
                  else
                     D.Repeat_Symbol := Sym;
                     D.Phase := Repeat_Extra;
                  end if;
            end case;

         when Repeat_Extra =>
            declare
               Bits_Wanted : constant Field_Bits :=
                 (case D.Repeat_Symbol is when REPEAT_PREVIOUS => 2, when REPEAT_ZERO => 3, when others => 7);
               Base : constant Natural :=
                 (if D.Repeat_Symbol = REPEAT_ZERO_LONG then 11 else 3);
               Total : constant Natural := D.Literal_Codes + D.Distance_Codes;
               Fill : Code_Length := 0;
            begin
               Pull (D, Input, In_Next, Bits_Wanted, Have);
               if not Have then
                  Stop_Need_Input (Result, Stop);
                  return;
               end if;
               Take (D, Bits_Wanted, Value);
               if D.Lengths_Read + Base + Value > Total then
                  Stop_Refused (D, Bad_Repeat, Result, Stop);
                  return;
               end if;
               if D.Repeat_Symbol = REPEAT_PREVIOUS then
                  if D.Lengths_Read = 0 then
                     Stop_Refused (D, Bad_Repeat, Result, Stop);
                     return;
                  end if;
                  Fill := D.Lengths (D.Lengths_Read - 1);
               end if;
               for K in 1 .. Base + Value loop
                  pragma Loop_Invariant (D.Lengths_Read = D.Lengths_Read'Loop_Entry + K - 1);
                  pragma Loop_Invariant
                    (D = (D'Loop_Entry with delta Lengths => D.Lengths, Lengths_Read => D.Lengths_Read));
                  D.Lengths (D.Lengths_Read) := Fill;
                  D.Lengths_Read := D.Lengths_Read + 1;
               end loop;
               if D.Lengths_Read = Total then
                  Lengths_Done (D);
                  if D.Phase = Failed_Phase then
                     Stop := True;
                     Result := Refused;
                  end if;
               else
                  D.Phase := Code_Lengths;
               end if;
            end;

         when others =>
            --  Excluded by the precondition.
            Stop_Refused (D, Bad_Symbol, Result, Stop);
      end case;
   end Step_Code_Lengths;

   procedure Step_Match
     (D : in out Inflater;
      Input : Byte_Array;
      In_Next : in out Positive;
      Result : out Progress;
      Stop : out Boolean)
   with
     Pre =>
       Valid (D) and then not Failed (D) and then not Done (D) and then D.Phase in Length_Extra | Distance | Distance_Extra
       and then Input'Last < Positive'Last and then In_Next in Input'First .. Input'Last + 1,
     Post =>
       Input_Stepped (D'Old, D, In_Next'Old, In_Next, Input'Last, Result, Stop)
       and then D.Total_Out = D'Old.Total_Out and then (if Stop then Result /= Output_Full)
   is
      Have : Boolean;
      Value : Natural;
      Outcome : Decode_Outcome;
      Sym : Symbol;
   begin
      Stop := False;
      Result := Need_Input;
      case D.Phase is
         when Length_Extra =>
            Pull (D, Input, In_Next, LENGTH_EXTRA_BITS (D.Match_Symbol), Have);
            if not Have then
               Stop_Need_Input (Result, Stop);
               return;
            end if;
            Take (D, LENGTH_EXTRA_BITS (D.Match_Symbol), Value);
            if LENGTH_BASE (D.Match_Symbol) + Value > MAXIMUM_MATCH then
               --  227 + 31 at most (RFC 1951 3.2.5).
               Stop_Refused (D, Bad_Symbol, Result, Stop);
               return;
            end if;
            D.Copy_Remaining := LENGTH_BASE (D.Match_Symbol) + Value;
            D.Code_Read := 0;
            D.Code := 0;
            D.First := 0;
            D.Phase := Distance;

         when Distance =>
            Pull (D, Input, In_Next, 1, Have);
            if not Have then
               Stop_Need_Input (Result, Stop);
               return;
            end if;
            Take (D, 1, Value);
            Decode_Bit (D.Distances, Value, D.Code_Read, D.Code, D.First, Outcome, Sym);
            case Outcome is
               when Need_More =>
                  null;
               when No_Such_Code =>
                  Stop_Refused (D, Bad_Symbol, Result, Stop);
               when Decoded =>
                  if Sym > LAST_DISTANCE_SYMBOL then
                     Stop_Refused (D, Bad_Symbol, Result, Stop);
                  else
                     D.Distance_Symbol := Sym;
                     if DISTANCE_EXTRA_BITS (Sym) = 0 then
                        Start_Copy (D, DISTANCE_BASE (Sym));
                        if D.Phase = Failed_Phase then
                           Stop := True;
                           Result := Refused;
                        end if;
                     else
                        D.Phase := Distance_Extra;
                     end if;
                  end if;
            end case;

         when Distance_Extra =>
            Pull (D, Input, In_Next, DISTANCE_EXTRA_BITS (D.Distance_Symbol), Have);
            if not Have then
               Stop_Need_Input (Result, Stop);
               return;
            end if;
            Take (D, DISTANCE_EXTRA_BITS (D.Distance_Symbol), Value);
            Start_Copy (D, DISTANCE_BASE (D.Distance_Symbol) + Value);
            if D.Phase = Failed_Phase then
               Stop := True;
               Result := Refused;
            end if;

         when others =>
            --  Excluded by the precondition.
            Stop_Refused (D, Bad_Symbol, Result, Stop);
      end case;
   end Step_Match;

   procedure Step_Literal
     (D : in out Inflater;
      Input : Byte_Array;
      In_Next : in out Positive;
      Output : in out Byte_Array;
      Out_Next : in out Positive;
      Result : out Progress;
      Stop : out Boolean)
   with
     Pre =>
       Valid (D) and then not Failed (D) and then not Done (D) and then D.Phase = Literal
       and then Input'Last < Positive'Last and then In_Next in Input'First .. Input'Last + 1
       and then Output'Last < Positive'Last and then Out_Next in Output'First .. Output'Last + 1,
     Post =>
       Input_Stepped (D'Old, D, In_Next'Old, In_Next, Input'Last, Result, Stop)
       and then Output_Stepped (D'Old, D, Output'Old, Output, Out_Next'Old, Out_Next, Result, Stop)
   is
      Have : Boolean;
      Value : Natural;
      Outcome : Decode_Outcome;
      Sym : Symbol;
   begin
      Stop := False;
      Result := Need_Input;
      if Out_Next > Output'Last then
         --  Room for a literal first, so a decoded one is never held.
         Stop := True;
         Result := Output_Full;
         return;
      end if;
      Pull (D, Input, In_Next, 1, Have);
      if not Have then
         Stop_Need_Input (Result, Stop);
         return;
      end if;
      Take (D, 1, Value);
      Decode_Bit (D.Literals, Value, D.Code_Read, D.Code, D.First, Outcome, Sym);
      case Outcome is
         when Need_More =>
            null;
         when No_Such_Code =>
            Stop_Refused (D, Bad_Symbol, Result, Stop);
         when Decoded =>
            if Sym < END_OF_BLOCK then
               if D.Total_Out >= D.Ceiling then
                  Stop_Refused (D, Too_Large, Result, Stop);
               else
                  Emit (D, Output, Out_Next, Byte (Sym));
               end if;
            elsif Sym = END_OF_BLOCK then
               End_Block (D);
               if D.Phase = Done_Phase then
                  Stop := True;
                  Result := Finished;
               end if;
            elsif Sym > LAST_LENGTH_SYMBOL then
               Stop_Refused (D, Bad_Symbol, Result, Stop);
            else
               D.Match_Symbol := Sym - FIRST_LENGTH_SYMBOL;
               D.Copy_Remaining := LENGTH_BASE (D.Match_Symbol);
               D.Phase := (if LENGTH_EXTRA_BITS (D.Match_Symbol) = 0 then Distance else Length_Extra);
            end if;
      end case;
   end Step_Literal;

   procedure Step_Stored
     (D : in out Inflater;
      Input : Byte_Array;
      In_Next : in out Positive;
      Output : in out Byte_Array;
      Out_Next : in out Positive;
      Result : out Progress;
      Stop : out Boolean)
   with
     Pre =>
       Valid (D) and then not Failed (D) and then not Done (D) and then D.Phase = Stored_Data
       and then Input'Last < Positive'Last and then In_Next in Input'First .. Input'Last + 1
       and then Output'Last < Positive'Last and then Out_Next in Output'First .. Output'Last + 1,
     Post =>
       Input_Stepped (D'Old, D, In_Next'Old, In_Next, Input'Last, Result, Stop)
       and then Output_Stepped (D'Old, D, Output'Old, Output, Out_Next'Old, Out_Next, Result, Stop)
   is
   begin
      Stop := False;
      Result := Need_Input;
      while D.Stored_Remaining > 0 and then In_Next <= Input'Last and then Out_Next <= Output'Last
        and then D.Total_Out < D.Ceiling
      loop
         pragma Loop_Invariant (In_Next in In_Next'Loop_Entry .. Input'Last);
         pragma Loop_Invariant (Out_Next in Out_Next'Loop_Entry .. Output'Last);
         pragma Loop_Invariant (D.Total_Out = D.Total_Out'Loop_Entry + Byte_Count (Out_Next - Out_Next'Loop_Entry));
         pragma Loop_Invariant
           (D = (D'Loop_Entry with delta Window => D.Window, Window_Next => D.Window_Next,
                 Total_Out => D.Total_Out, Adler_A => D.Adler_A, Adler_B => D.Adler_B,
                 Stored_Remaining => D.Stored_Remaining));
         pragma Loop_Invariant
           (for all I in Output'Range =>
              (if I < Out_Next'Loop_Entry or else I >= Out_Next then Output (I) = Output'Loop_Entry (I)));
         Emit (D, Output, Out_Next, Input (In_Next));
         In_Next := In_Next + 1;
         D.Stored_Remaining := D.Stored_Remaining - 1;
      end loop;
      if D.Stored_Remaining = 0 then
         End_Block (D);
         if D.Phase = Done_Phase then
            Stop := True;
            Result := Finished;
         end if;
      elsif D.Total_Out = D.Ceiling then
         Stop_Refused (D, Too_Large, Result, Stop);
      elsif Out_Next > Output'Last then
         Stop := True;
         Result := Output_Full;
      else
         Stop_Need_Input (Result, Stop);
      end if;
   end Step_Stored;

   procedure Step_Copy
     (D : in out Inflater;
      Output : in out Byte_Array;
      Out_Next : in out Positive;
      Result : out Progress;
      Stop : out Boolean)
   with
     Pre =>
       Valid (D) and then not Failed (D) and then not Done (D) and then D.Phase = Copy
       and then Output'Last < Positive'Last and then Out_Next in Output'First .. Output'Last + 1,
     Post =>
       Input_Stepped (D'Old, D, 1, 1, 0, Result, Stop) and then (if Stop then Result /= Need_Input)
       and then Output_Stepped (D'Old, D, Output'Old, Output, Out_Next'Old, Out_Next, Result, Stop)
   is
   begin
      Stop := False;
      Result := Need_Input;
      while D.Copy_Remaining > 0 and then Out_Next <= Output'Last and then D.Total_Out < D.Ceiling loop
         pragma Loop_Invariant (Out_Next in Out_Next'Loop_Entry .. Output'Last);
         pragma Loop_Invariant (D.Total_Out = D.Total_Out'Loop_Entry + Byte_Count (Out_Next - Out_Next'Loop_Entry));
         pragma Loop_Invariant
           (D = (D'Loop_Entry with delta Window => D.Window, Window_Next => D.Window_Next,
                 Total_Out => D.Total_Out, Adler_A => D.Adler_A, Adler_B => D.Adler_B,
                 Copy_Remaining => D.Copy_Remaining));
         pragma Loop_Invariant (D.Total_Out >= D.Total_Out'Loop_Entry);
         pragma Loop_Invariant
           (for all I in Output'Range =>
              (if I < Out_Next'Loop_Entry or else I >= Out_Next then Output (I) = Output'Loop_Entry (I)));
         Emit (D, Output, Out_Next,
               D.Window ((D.Window_Next + WINDOW_BYTES - D.Copy_Distance) mod WINDOW_BYTES));
         D.Copy_Remaining := D.Copy_Remaining - 1;
      end loop;
      if D.Copy_Remaining = 0 then
         D.Code_Read := 0;
         D.Code := 0;
         D.First := 0;
         D.Phase := Literal;
      elsif D.Total_Out = D.Ceiling then
         Stop_Refused (D, Too_Large, Result, Stop);
      else
         Stop := True;
         Result := Output_Full;
      end if;
   end Step_Copy;

   procedure Step
     (D : in out Inflater;
      Input : Byte_Array;
      In_Next : in out Positive;
      Output : in out Byte_Array;
      Out_Next : in out Positive;
      Result : out Progress;
      Stop : out Boolean)
   with
     Pre =>
       Valid (D) and then not Failed (D) and then not Done (D)
       and then Input'Last < Positive'Last and then Output'Last < Positive'Last
       and then In_Next in Input'First .. Input'Last + 1
       and then Out_Next in Output'First .. Output'Last + 1,
     Post =>
       Input_Stepped (D'Old, D, In_Next'Old, In_Next, Input'Last, Result, Stop)
       and then Output_Stepped (D'Old, D, Output'Old, Output, Out_Next'Old, Out_Next, Result, Stop)
   is
   begin
      case D.Phase is
         when Zlib_Header | Block_Header | Stored_Lengths | Checksum =>
            Step_Headers (D, Input, In_Next, Result, Stop);
         when Dynamic_Counts | Code_Length_Codes | Code_Lengths | Repeat_Extra =>
            Step_Code_Lengths (D, Input, In_Next, Result, Stop);
         when Length_Extra | Distance | Distance_Extra =>
            Step_Match (D, Input, In_Next, Result, Stop);
         when Literal =>
            Step_Literal (D, Input, In_Next, Output, Out_Next, Result, Stop);
         when Stored_Data =>
            Step_Stored (D, Input, In_Next, Output, Out_Next, Result, Stop);
         when Copy =>
            Step_Copy (D, Output, Out_Next, Result, Stop);
         when Done_Phase | Failed_Phase =>
            --  Excluded by the precondition.
            Stop := True;
            Result := (if D.Phase = Done_Phase then Finished else Refused);
      end case;
   end Step;


   ---------------------------------------------------------------------------

   procedure Start (D : out Inflater; Kind : Stream_Format; Maximum_Output : Byte_Count) is
   begin
      D := (Kind => Kind, Ceiling => Maximum_Output,
            Phase => (if Kind = Zlib_Stream then Zlib_Header else Block_Header),
            others => <>);
   end Start;

   procedure Inflate
     (D : in out Inflater;
      Input : Byte_Array;
      In_Next : in out Positive;
      Output : in out Byte_Array;
      Out_Next : in out Positive;
      Result : out Progress)
   is
      In_Start : constant Positive := In_Next;
      Stop : Boolean;
   begin
      if D.Phase = Done_Phase then
         Result := Finished;
         return;
      elsif D.Phase = Failed_Phase then
         Result := Refused;
         return;
      end if;
      loop
         Step (D, Input, In_Next, Output, Out_Next, Result, Stop);
         exit when Stop;
         pragma Loop_Invariant (Valid (D) and then not Failed (D) and then not Done (D));
         pragma Loop_Invariant
           (D.Kind = D.Kind'Loop_Entry and then D.Ceiling = D.Ceiling'Loop_Entry
            and then D.Total_In = D.Total_In'Loop_Entry);
         pragma Loop_Invariant (In_Next in In_Start .. Input'Last + 1);
         pragma Loop_Invariant (Out_Next in Out_Next'Loop_Entry .. Output'Last + 1);
         pragma Loop_Invariant
           (D.Total_Out = D.Total_Out'Loop_Entry + Byte_Count (Out_Next - Out_Next'Loop_Entry));
         pragma Loop_Invariant
           (for all I in Output'Range =>
              (if I < Out_Next'Loop_Entry or else I >= Out_Next then Output (I) = Output'Loop_Entry (I)));
      end loop;
      D.Total_In := D.Total_In + Byte_Count (In_Next - In_Start);
   end Inflate;

   procedure Finish (D : in out Inflater; Result : out Progress) is
   begin
      if D.Phase = Done_Phase then
         Result := Finished;
      elsif D.Phase = Failed_Phase then
         Result := Refused;
      else
         Refuse (D, Truncated);
         Result := Refused;
      end if;
   end Finish;
end Inflate_Streams;

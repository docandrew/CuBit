------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Unchecked_Deallocation;

package body Remote_PNG is
   --  The match finder's table: on the heap (CuAlloc) while encoding.
   Hash_Bits : constant := 15;
   type Head_Table is array (0 .. 2 ** Hash_Bits - 1) of Integer_64;
   type Head_Access is access Head_Table;
   procedure Free is new Ada.Unchecked_Deallocation (Head_Table, Head_Access);

   type CRC_Table is array (Unsigned_8) of Unsigned_32;
   function Make_Table return CRC_Table is
      T : CRC_Table;
      C : Unsigned_32;
      Polynomial : constant Unsigned_32 := 16#EDB8_8320#;
   begin
      for N in Unsigned_8 loop
         C := Unsigned_32 (N);
         for K in 1 .. 8 loop
            C := (if (C and 1) /= 0 then Polynomial xor Shift_Right (C, 1) else Shift_Right (C, 1));
         end loop;
         T (N) := C;
      end loop;
      return T;
   end Make_Table;
   Table : constant CRC_Table := Make_Table;

   function CRC32 (Data : Byte_Array; Seed : Unsigned_32 := 0) return Unsigned_32 is
      C : Unsigned_32 := not Seed;
   begin
      for B of Data loop
         C := Table (Unsigned_8 (C and 16#FF#) xor B) xor Shift_Right (C, 8);
      end loop;
      return not C;
   end CRC32;

   procedure Encode (Width, Height : Side) is
      --  The filtered image stream: per row a filter byte (0, None) then
      --  R, G, B per pixel.
      Row_Length : constant Natural := 1 + 3 * Width;
      Total : constant Integer_64 := Integer_64 (Row_Length) * Integer_64 (Height);
      subtype Position is Integer_64 range 0 .. Total;

      function Byte_At (P : Position) return Unsigned_8 is
         Row : constant Natural := Natural (P / Integer_64 (Row_Length));
         Column : constant Natural := Natural (P mod Integer_64 (Row_Length));
      begin
         if Column = 0 then
            return 0;
         end if;
         declare
            Pixel : constant Natural := (Column - 1) / 3;
            Channel : constant Natural := (Column - 1) mod 3;
         begin
            --  BGRA in memory: R is byte 2, G byte 1, B byte 0.
            return Source ((Row * Width + Pixel) * 4 + (2 - Channel));
         end;
      end Byte_At;

      ------------------------------------------------------------------
      --  Chunks
      ------------------------------------------------------------------
      procedure Emit_U32 (Value : Unsigned_32) is
      begin
         Emit ([Unsigned_8 (Shift_Right (Value, 24) and 16#FF#), Unsigned_8 (Shift_Right (Value, 16) and 16#FF#),
                Unsigned_8 (Shift_Right (Value, 8) and 16#FF#), Unsigned_8 (Value and 16#FF#)]);
      end Emit_U32;

      function Kind_Bytes (Kind : String) return Byte_Array is
        [for I in 1 .. 4 => Character'Pos (Kind (Kind'First + I - 1))];

      procedure Chunk (Kind : String; Data : Byte_Array) is
         K : constant Byte_Array := Kind_Bytes (Kind);
      begin
         Emit_U32 (Data'Length);
         Emit (K);
         if Data'Length > 0 then
            Emit (Data);
         end if;
         Emit_U32 (CRC32 (Data, CRC32 (K)));
      end Chunk;

      IDAT : Byte_Array (1 .. Chunk_Bytes);
      IDAT_Used : Natural := 0;

      procedure Flush_IDAT is
      begin
         if IDAT_Used > 0 then
            Chunk ("IDAT", IDAT (1 .. IDAT_Used));
            IDAT_Used := 0;
         end if;
      end Flush_IDAT;

      procedure Put_Byte (B : Unsigned_8) is
      begin
         if IDAT_Used = IDAT'Last then
            Flush_IDAT;
         end if;
         IDAT_Used := IDAT_Used + 1;
         IDAT (IDAT_Used) := B;
      end Put_Byte;

      ------------------------------------------------------------------
      --  Bits, least significant first (deflate's order)
      ------------------------------------------------------------------
      Bits : Unsigned_64 := 0;
      Bit_Count : Natural := 0;

      procedure Put_Bits (Value : Unsigned_32; Count : Natural) is
      begin
         Bits := Bits or Shift_Left (Unsigned_64 (Value), Bit_Count);
         Bit_Count := Bit_Count + Count;
         while Bit_Count >= 8 loop
            Put_Byte (Unsigned_8 (Bits and 16#FF#));
            Bits := Shift_Right (Bits, 8);
            Bit_Count := Bit_Count - 8;
         end loop;
      end Put_Bits;

      --  A Huffman code: its most significant bit goes first.
      procedure Put_Code (Code : Unsigned_32; Length : Natural) is
         Reversed : Unsigned_32 := 0;
      begin
         for I in 0 .. Length - 1 loop
            if (Shift_Right (Code, I) and 1) /= 0 then
               Reversed := Reversed or Shift_Left (1, Length - 1 - I);
            end if;
         end loop;
         Put_Bits (Reversed, Length);
      end Put_Code;

      --  The fixed literal/length code (RFC 1951 3.2.6).
      procedure Put_Symbol (Symbol : Natural) is
      begin
         if Symbol <= 143 then
            Put_Code (16#30# + Unsigned_32 (Symbol), 8);
         elsif Symbol <= 255 then
            Put_Code (16#190# + Unsigned_32 (Symbol - 144), 9);
         elsif Symbol <= 279 then
            Put_Code (Unsigned_32 (Symbol - 256), 7);
         else
            Put_Code (16#C0# + Unsigned_32 (Symbol - 280), 8);
         end if;
      end Put_Symbol;

      type Length_Table is array (257 .. 285) of Natural;
      Length_Base : constant Length_Table :=
        [3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31, 35, 43, 51, 59, 67, 83, 99, 115,
         131, 163, 195, 227, 258];
      Length_Extra : constant Length_Table :=
        [0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0];
      type Distance_Table is array (0 .. 29) of Natural;
      Distance_Base : constant Distance_Table :=
        [1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193, 257, 385, 513, 769, 1025, 1537,
         2049, 3073, 4097, 6145, 8193, 12289, 16385, 24577];
      Distance_Extra : constant Distance_Table :=
        [0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13];

      procedure Put_Match (Length, Distance : Positive) is
         L : Natural := 285;
         D : Natural := 29;
      begin
         while Length_Base (L) > Length loop
            L := L - 1;
         end loop;
         Put_Symbol (L);
         if Length_Extra (L) > 0 then
            Put_Bits (Unsigned_32 (Length - Length_Base (L)), Length_Extra (L));
         end if;
         while Distance_Base (D) > Distance loop
            D := D - 1;
         end loop;
         Put_Code (Unsigned_32 (D), 5);
         if Distance_Extra (D) > 0 then
            Put_Bits (Unsigned_32 (Distance - Distance_Base (D)), Distance_Extra (D));
         end if;
      end Put_Match;

      ------------------------------------------------------------------
      --  LZ77
      ------------------------------------------------------------------
      Window : constant := 32_768;
      Minimum_Match : constant := 3;
      Maximum_Match : constant := 258;
      --  The latest position whose next three bytes hashed here, plus 1 (0: none).
      Heads : Head_Access := new Head_Table'(others => 0);

      function Hash (P : Position) return Natural is
        (Natural ((Unsigned_32 (Byte_At (P)) * 2_654_435_761 xor
                   Unsigned_32 (Byte_At (P + 1)) * 40_503 xor Unsigned_32 (Byte_At (P + 2)))
                  and (2 ** Hash_Bits - 1)));

      --  Adler-32 of the uncompressed stream.
      Adler_Modulus : constant := 65_521;
      Adler_A : Unsigned_32 := 1;
      Adler_B : Unsigned_32 := 0;
      Since_Reduce : Natural := 0;
      procedure Take (B : Unsigned_8) is
      begin
         Adler_A := Adler_A + Unsigned_32 (B);
         Adler_B := Adler_B + Adler_A;
         Since_Reduce := Since_Reduce + 1;
         if Since_Reduce = 5_552 then
            Adler_A := Adler_A mod Adler_Modulus;
            Adler_B := Adler_B mod Adler_Modulus;
            Since_Reduce := 0;
         end if;
      end Take;

      procedure Insert (P : Position) is
      begin
         if Total - P >= Minimum_Match then
            Heads (Hash (P)) := P + 1;
         end if;
      end Insert;

      P : Position := 0;
      Best_Length, Best_Distance : Natural;

      procedure Try (Candidate : Integer_64) is
         Distance : Integer_64;
         Limit : Natural;
         N : Natural := 0;
      begin
         if Candidate < 0 or else Candidate >= P then
            return;
         end if;
         Distance := P - Candidate;
         if Distance > Window then
            return;
         end if;
         Limit := Natural (Integer_64'Min (Maximum_Match, Total - P));
         while N < Limit and then Byte_At (Candidate + Integer_64 (N)) = Byte_At (P + Integer_64 (N)) loop
            N := N + 1;
         end loop;
         if N > Best_Length then
            Best_Length := N;
            Best_Distance := Natural (Distance);
         end if;
      end Try;
   begin
      --  The signature and the header.
      Emit ([137, 80, 78, 71, 13, 10, 26, 10]);
      declare
         Header : Byte_Array (1 .. 13) := [others => 0];
         procedure Put32 (At_Index : Positive; Value : Unsigned_32) is
         begin
            Header (At_Index) := Unsigned_8 (Shift_Right (Value, 24) and 16#FF#);
            Header (At_Index + 1) := Unsigned_8 (Shift_Right (Value, 16) and 16#FF#);
            Header (At_Index + 2) := Unsigned_8 (Shift_Right (Value, 8) and 16#FF#);
            Header (At_Index + 3) := Unsigned_8 (Value and 16#FF#);
         end Put32;
      begin
         Put32 (1, Unsigned_32 (Width));
         Put32 (5, Unsigned_32 (Height));
         Header (9) := 8;    --  bits per channel
         Header (10) := 2;   --  RGB
         Chunk ("IHDR", Header);
      end;
      --  zlib: deflate, 32 KiB window, no dictionary (CMF 78, FLG 01).
      Put_Byte (16#78#);
      Put_Byte (16#01#);
      --  One final block of fixed Huffman codes.
      Put_Bits (1, 1);
      Put_Bits (1, 2);
      while P < Total loop
         Best_Length := 0;
         Best_Distance := 0;
         if Total - P >= Minimum_Match then
            declare
               H : constant Natural := Hash (P);
               Previous : constant Integer_64 := Heads (H) - 1;
            begin
               Try (Previous);
               Try (P - 3);
               Try (P - Integer_64 (Row_Length));
               Heads (H) := P + 1;
            end;
         end if;
         if Best_Length >= Minimum_Match then
            Put_Match (Best_Length, Best_Distance);
            Take (Byte_At (P));
            for K in 1 .. Integer_64 (Best_Length) - 1 loop
               Take (Byte_At (P + K));
               Insert (P + K);
            end loop;
            P := P + Integer_64 (Best_Length);
         else
            Put_Symbol (Natural (Byte_At (P)));
            Take (Byte_At (P));
            P := P + 1;
         end if;
      end loop;
      Put_Symbol (256);
      if Bit_Count > 0 then
         Put_Bits (0, 8 - Bit_Count);
      end if;
      Adler_A := Adler_A mod Adler_Modulus;
      Adler_B := Adler_B mod Adler_Modulus;
      declare
         Adler : constant Unsigned_32 := Shift_Left (Adler_B, 16) or Adler_A;
      begin
         Put_Byte (Unsigned_8 (Shift_Right (Adler, 24) and 16#FF#));
         Put_Byte (Unsigned_8 (Shift_Right (Adler, 16) and 16#FF#));
         Put_Byte (Unsigned_8 (Shift_Right (Adler, 8) and 16#FF#));
         Put_Byte (Unsigned_8 (Adler and 16#FF#));
      end;
      Flush_IDAT;
      Chunk ("IEND", [1 .. 0 => 0]);
      Free (Heads);
   end Encode;
end Remote_PNG;

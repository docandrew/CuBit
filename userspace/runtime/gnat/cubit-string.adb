 ------------------------------------------------------------------------------
 -- CuBit
 -- Copyright (C) 2021 Jon Andrew
 --
 -- @summary
 -- Replacement for C string.h
 ------------------------------------------------------------------------------
with Ada.Unchecked_Conversion;
with System.Machine_Code; use System.Machine_Code;

package body CuBit.String is

   function memcpy (dest : System.Address;
                    src  : System.Address;
                    len  : System.Storage_Elements.Storage_Count)
      return System.Address
   is
         use System.Storage_Elements;

         dstl : System.Address := dest;
         srcl : System.Address := src;
         lenl : Storage_Count  := len;
   begin
         Asm ("rep movsb",
             Outputs => (
                System.Address'Asm_Output ("=D", dstl),
                System.Address'Asm_Output ("=S", srcl),
                Storage_Count'Asm_Output ("=c", lenl)
             ),
             Inputs => (
                System.Address'Asm_Input ("0", dstl),
                System.Address'Asm_Input ("1", srcl),
                Storage_Count'Asm_Input ("2", lenl)
             ),
             Clobber  => "memory",
             Volatile => True);

      return dest;
   end memcpy;

   function memmove (dest : System.Address;
                     src  : System.Address;
                     len  : System.Storage_Elements.Storage_Count)
      return System.Address
   is
      use System.Storage_Elements;

      type Byte is mod 256;
      type Byte_Ptr is access all Byte;
      pragma No_Strict_Aliasing (Byte_Ptr);

      function To_Ptr is new Ada.Unchecked_Conversion
        (System.Address, Byte_Ptr);
   begin
      if len = 0 then
         return dest;
      end if;

      if To_Integer (dest) <= To_Integer (src) then
         for I in 0 .. len - 1 loop
            To_Ptr (dest + I).all := To_Ptr (src + I).all;
         end loop;
      else
         for I in reverse 0 .. len - 1 loop
            To_Ptr (dest + I).all := To_Ptr (src + I).all;
         end loop;
      end if;

      return dest;
   end memmove;

   function memcmp (left  : System.Address;
                    right : System.Address;
                    len   : System.Storage_Elements.Storage_Count)
      return Integer
   is
      use System.Storage_Elements;

      type Byte is mod 2 ** 8 with Size => 8, Alignment => 1;
      type Word is mod 2 ** 64 with Size => 64, Alignment => 1;
      type Byte_Ptr is access all Byte;
      type Word_Ptr is access all Word;
      pragma No_Strict_Aliasing (Byte_Ptr);
      pragma No_Strict_Aliasing (Word_Ptr);

      function To_Byte is new Ada.Unchecked_Conversion
        (System.Address, Byte_Ptr);
      function To_Word is new Ada.Unchecked_Conversion
        (System.Address, Word_Ptr);

      BYTES_PER_WORD  : constant Storage_Count := 8;
      WORDS_PER_BATCH : constant Storage_Count := 8;
      BYTES_PER_BATCH : constant Storage_Count :=
        BYTES_PER_WORD * WORDS_PER_BATCH;
      Left_Cursor  : System.Address := left;
      Right_Cursor : System.Address := right;
      Remaining    : Storage_Count := len;
      Left_Byte, Right_Byte : Byte;

      function Word_Difference (Offset : Storage_Count) return Word is
        (To_Word (Left_Cursor + Storage_Offset (Offset)).all xor
         To_Word (Right_Cursor + Storage_Offset (Offset)).all);
      pragma Inline_Always (Word_Difference);

      function Differing_Word_Result
        (Offset : Storage_Count;
         Difference : Word) return Integer
      is
         Bit_Position : Word;
         Byte_Offset  : Storage_Offset;
      begin
         --  Difference is nonzero at every call site, as required by BSF.
         Asm ("bsfq %1, %0",
              Outputs => Word'Asm_Output ("=r", Bit_Position),
              Inputs  => Word'Asm_Input ("r", Difference),
              Clobber => "cc");
         Byte_Offset := Storage_Offset
           (Offset + Storage_Count (Bit_Position / 8));
         Left_Byte := To_Byte (Left_Cursor + Byte_Offset).all;
         Right_Byte := To_Byte (Right_Cursor + Byte_Offset).all;
         return Integer (Left_Byte) - Integer (Right_Byte);
      end Differing_Word_Result;
      pragma Inline_Always (Differing_Word_Result);

      Difference : Word;
   begin
      --  Eight independent qword comparisons amortize the loop branch and
      --  give an out-of-order x86-64 core enough loads to overlap.
      while Remaining >= BYTES_PER_BATCH loop
         for Index in Storage_Count range 0 .. WORDS_PER_BATCH - 1 loop
            pragma Loop_Optimize (Unroll);
            Difference := Word_Difference (Index * BYTES_PER_WORD);
            if Difference /= 0 then
               return Differing_Word_Result
                 (Index * BYTES_PER_WORD, Difference);
            end if;
         end loop;
         Left_Cursor := Left_Cursor + Storage_Offset (BYTES_PER_BATCH);
         Right_Cursor := Right_Cursor + Storage_Offset (BYTES_PER_BATCH);
         Remaining := Remaining - BYTES_PER_BATCH;
      end loop;

      while Remaining >= BYTES_PER_WORD loop
         Difference := Word_Difference (0);
         if Difference /= 0 then
            return Differing_Word_Result (0, Difference);
         end if;
         Left_Cursor := Left_Cursor + Storage_Offset (BYTES_PER_WORD);
         Right_Cursor := Right_Cursor + Storage_Offset (BYTES_PER_WORD);
         Remaining := Remaining - BYTES_PER_WORD;
      end loop;

      while Remaining > 0 loop
         Left_Byte := To_Byte (Left_Cursor).all;
         Right_Byte := To_Byte (Right_Cursor).all;
         if Left_Byte /= Right_Byte then
            return Integer (Left_Byte) - Integer (Right_Byte);
         end if;
         Left_Cursor := Left_Cursor + 1;
         Right_Cursor := Right_Cursor + 1;
         Remaining := Remaining - 1;
      end loop;
      return 0;
   end memcmp;

   function memset (dest : System.Address;
                    val  : Integer;
                    len  : System.Storage_Elements.Storage_Count)
      return System.Address
   is
      use System.Storage_Elements;
      dstl : System.Address := dest;
      lenl : Storage_Count := len;
      byte : constant Integer := val mod 256;
   begin
      Asm ("rep stosb",
          Outputs => (
             System.Address'Asm_Output ("=D", dstl),
             Storage_Count'Asm_Output ("=c", lenl)
          ),
          Inputs => (
             System.Address'Asm_Input ("0", dstl),
             Storage_Count'Asm_Input ("1", lenl),
             Integer'Asm_Input ("a", byte)
          ),
          Clobber  => "memory",
          Volatile => True);

      return dest;
   end memset;

end CuBit.String;

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

   function memset (dest : System.Address;
                    val  : Integer;
                    len  : System.Storage_Elements.Storage_Count)
      return System.Address
   is
      use System.Storage_Elements;
      type Byte is mod 256;
      b : constant Byte := Byte (val mod 256);
   begin
      for i in 0 .. len - 1 loop
         declare
            dst : Byte with Import, Address => dest + i;
         begin
            dst := b;
         end;
      end loop;
      return dest;
   end memset;

end CuBit.String;

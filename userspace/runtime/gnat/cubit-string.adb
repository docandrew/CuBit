 ------------------------------------------------------------------------------
 -- CuBit
 -- Copyright (C) 2021 Jon Andrew
 --
 -- @summary
 -- Replacement for C string.h
 ------------------------------------------------------------------------------
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

end CuBit.String;

 ------------------------------------------------------------------------------
 -- CuBit
 -- Copyright (C) 2021 Jon Andrew
 --
 -- @summary
 -- Replacement for C string.h
 ------------------------------------------------------------------------------
with System;
with System.Storage_Elements;

package CuBit.String is

   function memcpy (dest : System.Address;
                    src  : System.Address;
                    len  : System.Storage_Elements.Storage_Count)
      return System.Address with
      Export => True,
      Convention => C,
      External_Name => "memcpy";

   function memmove (dest : System.Address;
                     src  : System.Address;
                     len  : System.Storage_Elements.Storage_Count)
      return System.Address with
      Export => True,
      Convention => C,
      External_Name => "memmove";

   --  Ordinary lexicographic memory comparison.  This routine is optimized
   --  for general use and is not constant-time; secrets require a dedicated
   --  constant-time comparison primitive.
   function memcmp (left  : System.Address;
                    right : System.Address;
                    len   : System.Storage_Elements.Storage_Count)
      return Integer with
      Export => True,
      Convention => C,
      External_Name => "memcmp";

   function memset (dest : System.Address;
                    val  : Integer;
                    len  : System.Storage_Elements.Storage_Count)
      return System.Address with
      Export => True,
      Convention => C,
      External_Name => "memset";

end CuBit.String;

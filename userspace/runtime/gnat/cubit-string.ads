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

end CuBit.String;

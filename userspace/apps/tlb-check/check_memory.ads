------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The pages tlb-check grants each revoke life of tlb-probe.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package Check_Memory is
   Page_Bytes : constant := 4096;
   Words_Per_Page : constant := Page_Bytes / 8;
   Granted_Pages : constant := 16;

   type Word_Array is array (Natural range <>) of Unsigned_64;

   Region : Word_Array (0 .. Granted_Pages * Words_Per_Page - 1)
     with Alignment => Page_Bytes, Volatile;
end Check_Memory;

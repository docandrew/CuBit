------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;

package body CuBit.Secondary_Stack_Layout with SPARK_Mode is

   function TLS_Block_Bytes (Bytes : TLS_Bytes; Alignment : TLS_Alignment) return Region_Bytes is
      --  In Natural: the provers reason about integer mod better than
      --  about 64-bit modular remainders.
      B : constant Natural := Natural (Bytes);
      A : constant Positive := Positive (Alignment);
      Pad : constant Natural := (A - B mod A) mod A;
   begin
      return Bytes + Unsigned_64 (Pad);
   end TLS_Block_Bytes;

end CuBit.Secondary_Stack_Layout;

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

   function Next_Commit (Committed, Needed : Region_Bytes) return Commit_Bytes is
      Left : constant Region_Bytes := Usable_Bytes - Committed;
      Want : Region_Bytes := Page_Round (Needed - Committed);
   begin
      Want := Unsigned_64'Max (Want, Minimum_Growth_Bytes);
      Want := Unsigned_64'Max (Want, Committed);
      return Unsigned_64'Min (Unsigned_64'Min (Want, Maximum_Commit_Bytes), Left);
   end Next_Commit;

end CuBit.Secondary_Stack_Layout;

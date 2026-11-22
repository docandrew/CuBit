-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
--
-- @summary Address-space tags (x86 PCIDs): CR3 encoding
-------------------------------------------------------------------------------
package body Address_Space_Tags with SPARK_Mode => On is
    function CR3_Value (Root : Unsigned_64; T : Tag; Keep_Translations : Boolean)
      return Unsigned_64
    is
        Value : constant Unsigned_64 := Root or Unsigned_64 (T);
    begin
        pragma Assert (Unsigned_64 (T) <= Tag_Mask);
        if Keep_Translations then
            return Value or Keep_Bit;
        end if;
        return Value;
    end CR3_Value;
end Address_Space_Tags;

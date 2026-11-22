with Interfaces; use Interfaces;
with CuBit.Grant_References; use CuBit.Grant_References;
package body Codec_Proof with SPARK_Mode is
   procedure Round_Trip (Item : Reference) is
   begin
      pragma Assert (Decode (Encode (Item)) = Item);
   end Round_Trip;
   procedure Canonical (Word : Unsigned_64) is
   begin
      pragma Assert (Encode (Decode (Word)) = Word);
   end Canonical;
end Codec_Proof;

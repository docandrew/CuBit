with Interfaces;
with CuBit.Grant_References;
package Codec_Proof with SPARK_Mode is
   procedure Round_Trip (Item : CuBit.Grant_References.Reference)
     with Ghost, Global => null;
   procedure Canonical (Word : Interfaces.Unsigned_64)
     with Ghost, Global => null,
          Pre => CuBit.Grant_References.Valid_Wire (Word);
end Codec_Proof;

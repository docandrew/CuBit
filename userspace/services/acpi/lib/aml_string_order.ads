pragma Ada_2022;
with AML_Decode;
package AML_String_Order with SPARK_Mode, Pure is
   use type AML_Decode.Bytes;
   type Ordering is (Less, Equal, Greater);
   -- Lexical unsigned-byte order; shorter equal prefixes precede longer ones.
   function Compare (Left, Right : AML_Decode.Bytes) return Ordering
     with Post => (Compare'Result = Equal) = (Left = Right);
end AML_String_Order;

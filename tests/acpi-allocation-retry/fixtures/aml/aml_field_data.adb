pragma Ada_2022;
package body AML_Field_Data with SPARK_Mode is
   function Window
     (Data : AML_Decode.Bytes; Offset : Natural; Count : Positive)
      return AML_Decode.Byte
   is
      Value : Natural := Natural (Data (Data'First + Offset / 8)) / Scale (Offset mod 8);
   begin
      if Count + Offset mod 8 > 8 then
         Value := Value + Natural (Data (Data'First + Offset / 8 + 1)) * Scale (8 - Offset mod 8);
      end if;
      return AML_Decode.Byte (Value mod Scale (Count));
   end Window;
   function Read_Bits
     (Data : AML_Decode.Bytes; Offset, Count : Natural) return Read_Result
   is
      Content : AML_Decode.Buffer_Storage := [others => 0];
      Length : Natural;
   begin
      if Count > Max_Bits then return (Status => AML_Decode.Limit_Exceeded); end if;
      if not Fits (Data'Length, Offset, Count) then return (Status => AML_Decode.Truncated); end if;
      Length := Count / 8 + (if Count mod 8 = 0 then 0 else 1);
      for I in 1 .. Length loop
         Content (I) := Window
           (Data, Offset + (I - 1) * 8, Positive'Min (8, Count - (I - 1) * 8));
         pragma Loop_Invariant
           (for all J in 1 .. I => Content (J) = Window
              (Data, Offset + (J - 1) * 8, Positive'Min (8, Count - (J - 1) * 8)));
         pragma Loop_Invariant
           (for all J in I + 1 .. AML_Decode.Max_Buffer_Length => Content (J) = 0);
      end loop;
      return (Status => AML_Decode.Accepted, Length => Length, Content => Content);
   end Read_Bits;
end AML_Field_Data;

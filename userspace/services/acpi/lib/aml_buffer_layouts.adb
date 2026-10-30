pragma Ada_2022;
package body AML_Buffer_Layouts with SPARK_Mode is
   use AML_Decode;
   use type Byte;
   Buffer_Op : constant Byte := 16#11#;
   subtype Failure_Status is Status range Truncated .. Limit_Exceeded;
   function Failure (Kind : Failure_Status) return Layout_Result is ((Kind => Kind));
   function With_Count
     (Data : Bytes; Count_Value : Integer_Value; Count_Consumed : Natural)
      return Layout_Result
   is
      Span : constant Buffer_Count_Layout := Check_Buffer_Count_Span (Data, Count_Consumed);
      Requested : Buffer_Length;
   begin
      if Span.Kind /= Accepted then return Failure (Span.Kind); end if;
      -- Validated PkgLength bounds the initializer to 28 bits; its Natural
      -- length also fits Buffer_Length. Descriptor subtype checks stay enabled.
      Requested := Count_Value and Buffer_Length_Mask;
      return (Kind => Accepted, Consumed => Span.Consumed,
        Initializer_Offset => Span.Raw_Offset, Initializer_Length => Span.Raw_Length,
        Requested_Length => Requested,
        Effective_Length => Integer_Value'Max (Requested, Integer_Value (Span.Raw_Length)));
   end With_Count;
   function Literal_Layout_Matches
     (Data : Bytes; Width : Integer_Width; Layout : Layout_Result) return Boolean is
     (Layout.Kind = Accepted and then Data'Length >= 2
      and then Data (Data'First) = Buffer_Op
      and then
        (declare
           P : constant Package_Result := Read_Package (Data (Data'First + 1 .. Data'Last));
         begin
           P.Kind = Accepted and then P.Encoding_Bytes /= P.Extent
           and then
             (declare
                Count_Offset : constant Natural := 1 + P.Encoding_Bytes;
                Count : constant Integer_Result := Read_Integer
                  (Data (Data'First + Count_Offset .. Data'First + P.Extent), Width);
              begin
                Count.Kind = Accepted
                and then Count.Consumed <= P.Extent - P.Encoding_Bytes
                and then Layout.Consumed = P.Extent + 1
                and then Layout.Initializer_Offset = Count_Offset + Count.Consumed
                and then Layout.Initializer_Length = P.Extent - P.Encoding_Bytes - Count.Consumed
                and then Layout.Requested_Length = (Count.Value and Buffer_Length_Mask)
                and then Layout.Effective_Length = Integer_Value'Max
                  (Count.Value and Buffer_Length_Mask,
                   Integer_Value (P.Extent - P.Encoding_Bytes - Count.Consumed)))));
   function Read (Data : Bytes; Width : Integer_Width) return Layout_Result is
      P : Package_Result;
      Count : Integer_Result;
      Offset : Natural;
   begin
      if Data'Length = 0 then return (Kind => Truncated); end if;
      if Data (Data'First) /= Buffer_Op then return (Kind => Unsupported); end if;
      if Data'Length = 1 then return (Kind => Truncated); end if;
      P := Read_Package (Data (Data'First + 1 .. Data'Last));
      if P.Kind /= Accepted then return Failure (P.Kind); end if;
      if P.Encoding_Bytes = P.Extent then return (Kind => Malformed); end if;
      Offset := 1 + P.Encoding_Bytes;
      Count := Read_Integer (Data (Data'First + Offset .. Data'First + P.Extent), Width);
      if Count.Kind /= Accepted then return Failure (Count.Kind); end if;
      return With_Count (Data, Count.Value, Count.Consumed);
   end Read;
end AML_Buffer_Layouts;

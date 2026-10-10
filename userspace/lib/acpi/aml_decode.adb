pragma Ada_2022;
package body AML_Decode with SPARK_Mode is
   use type Interfaces.Unsigned_8;
   use type Interfaces.Unsigned_64;

   function Read_Integer
     (Data : Bytes; Width : Integer_Width) return Integer_Result
   is
      Payload : Natural range 0 .. 8 := 0;
      Value : Integer_Value := 0;
   begin
      if Data'Length = 0 then
         return (Kind => Truncated);
      end if;
      case Data (Data'First) is
         when 16#00# => null;
         when 16#01# => Value := 1;
         when 16#FF# => Value := Integer_Value'Last;
         when 16#0A# => Payload := 1;
         when 16#0B# => Payload := 2;
         when 16#0C# => Payload := 4;
         when 16#0E# => Payload := 8;
         when Extended_Op =>
            if Data'Length < Revision_Bytes then return (Kind => Truncated); end if;
            if Data (Data'First + 1) /= Revision_Extension then return (Kind => Unsupported); end if;
            return (Kind => Accepted, Value => Interpreter_Revision, Consumed => Revision_Bytes);
         when others => return (Kind => Unsupported);
      end case;
      if Data'Length < Payload + 1 then
         return (Kind => Truncated);
      end if;
      for I in 1 .. Payload loop
         Value := Value or Interfaces.Shift_Left
           (Integer_Value (Data (Data'First + I)), 8 * (I - 1));
      end loop;
      if Width = Bits_32 then
         Value := Value and 16#FFFF_FFFF#;
      end if;
      return (Kind => Accepted, Value => Value, Consumed => Payload + 1);
   end Read_Integer;

   function Read_String (Data : Bytes) return String_Result is
      Text : String_Storage := [others => Character'Val (0)];
      C : Byte;
   begin
      if Data'Length = 0 then return (Kind => Truncated); end if;
      if Data (Data'First) /= 16#0D# then return (Kind => Unsupported); end if;
      for I in 0 .. Max_String_Length loop
         pragma Loop_Invariant (I < Data'Length);
         pragma Loop_Invariant
           (for all J in 1 .. I =>
              Text (J) = Character'Val (Data (Data'First + J)) and then
              Character'Pos (Text (J)) in 1 .. 127);
         if Data'Length - 1 <= I then return (Kind => Truncated); end if;
         C := Data (Data'First + (I + 1));
         if C = 0 then
            return (Kind => Accepted, Text => Text, Length => I, Consumed => I + 2);
         elsif C > 127 then
            return (Kind => Malformed);
         elsif I = Max_String_Length then
            return (Kind => Limit_Exceeded);
         end if;
         Text (I + 1) := Character'Val (C);
      end loop;
      return (Kind => Limit_Exceeded);
   end Read_String;

   function Read_Field_Length (Data : Bytes) return Field_Length_Result is
      Following : Natural range 0 .. 3;
      Count : Positive range 1 .. 4;
      Length : Natural range 0 .. 16#0FFF_FFFF#;
   begin
      if Data'Length = 0 then
         return (Kind => Truncated);
      end if;
      Following := Natural (Data (Data'First) / 64);
      Count := Following + 1;
      if Data'Length < Count then
         return (Kind => Truncated);
      end if;
      if Following = 0 then
         Length := Natural (Data (Data'First) and 16#3F#);
      else
         if (Data (Data'First) and 16#30#) /= 0 then
            return (Kind => Malformed);
         end if;
         Length := Natural (Data (Data'First)) mod 16
           + Natural (Data (Data'First + 1)) * 16;
         if Following >= 2 then
            Length := Length + Natural (Data (Data'First + 2)) * 4096;
         end if;
         if Following = 3 then
            Length := Length + Natural (Data (Data'First + 3)) * 1048576;
         end if;
      end if;
      return (Kind => Accepted, Encoding_Bytes => Count, Bits => Length);
   end Read_Field_Length;
   function Read_Package (Data : Bytes) return Package_Result is
      Length : constant Field_Length_Result := Read_Field_Length (Data);
   begin
      case Length.Kind is
         when Accepted => null;
         when Truncated => return (Kind => Truncated);
         when others => return (Kind => Malformed);
      end case;
      if Length.Bits < Length.Encoding_Bytes then
         return (Kind => Malformed);
      elsif Length.Bits > Data'Length then
         return (Kind => Truncated);
      end if;
      return (Kind => Accepted, Encoding_Bytes => Length.Encoding_Bytes, Extent => Length.Bits);
   end Read_Package;
   function Check_Buffer_Count_Span
     (Data : Bytes; Count_Consumed : Natural) return Buffer_Count_Layout
   is
      subtype Failure_Status is Status range Truncated .. Limit_Exceeded;
      function Failure (Kind : Failure_Status) return Buffer_Count_Layout is ((Kind => Kind));
      P : Package_Result;
      Offset : Natural;
   begin
      if Data'Length < 2 then return (Kind => Truncated); end if;
      if Data (Data'First) /= 16#11# then return (Kind => Unsupported); end if;
      P := Read_Package (Data (Data'First + 1 .. Data'Last));
      if P.Kind /= Accepted then return Failure (P.Kind); end if;
      if Count_Consumed = 0 or else Count_Consumed > P.Extent - P.Encoding_Bytes then
         return (Kind => Malformed);
      end if;
      Offset := 1 + P.Encoding_Bytes + Count_Consumed;
      return (Kind => Accepted, Raw_Offset => Offset,
        Raw_Length => P.Extent + 1 - Offset, Consumed => P.Extent + 1);
   end Check_Buffer_Count_Span;

   function Read_Buffer_With_Count
     (Data : Bytes; Width : Integer_Width; Count_Value : Integer_Value;
      Count_Consumed : Natural) return Buffer_Result
   is
      pragma Unreferenced (Width); -- Both AML widths feed a UINT32 Buffer length.
      subtype Failure_Status is Status range Truncated .. Limit_Exceeded;
      function Failure (Kind : Failure_Status) return Buffer_Result is ((Kind => Kind));
      Buffer_Size_Mask : constant Integer_Value := 16#FFFF_FFFF#;
      Size : constant Integer_Value := Count_Value and Buffer_Size_Mask;
      Layout : constant Buffer_Count_Layout := Check_Buffer_Count_Span (Data, Count_Consumed);
      Content : Buffer_Storage := [others => 0];
   begin
      if Layout.Kind /= Accepted then return Failure (Layout.Kind); end if;
      if Size > Max_Buffer_Length or else Layout.Raw_Length > Max_Buffer_Length then
         return (Kind => Limit_Exceeded);
      end if;
      for I in 1 .. Layout.Raw_Length loop
         Content (I) := Data (Data'First + (Layout.Raw_Offset + I - 1));
      end loop;
      return (Kind => Accepted, Content => Content,
        Length => Natural'Max (Natural (Size), Layout.Raw_Length), Consumed => Layout.Consumed);
   end Read_Buffer_With_Count;

   function Read_Buffer (Data : Bytes; Width : Integer_Width) return Buffer_Result is
      subtype Failure_Status is Status range Truncated .. Limit_Exceeded;
      function Failure (Kind : Failure_Status) return Buffer_Result is
        ((Kind => Kind));
      P : Package_Result;
      Size : Integer_Result;
      Offset : Natural;
   begin
      if Data'Length = 0 then return (Kind => Truncated); end if;
      if Data (Data'First) /= 16#11# then return (Kind => Unsupported); end if;
      if Data'Length = 1 then return (Kind => Truncated); end if;
      P := Read_Package (Data (Data'First + 1 .. Data'Last));
      if P.Kind /= Accepted then return Failure (P.Kind); end if;
      if P.Encoding_Bytes = P.Extent then return (Kind => Malformed); end if;
      Offset := 1 + P.Encoding_Bytes;
      Size := Read_Integer
        (Data (Data'First + Offset .. Data'First + P.Extent), Width);
      if Size.Kind /= Accepted then return Failure (Size.Kind); end if;
      return Read_Buffer_With_Count (Data, Width, Size.Value, Size.Consumed);
   end Read_Buffer;
end AML_Decode;

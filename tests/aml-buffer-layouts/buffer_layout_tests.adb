with Ada.Text_IO;
with AML_Decode; use AML_Decode;
with AML_Buffer_Layouts;
procedure Buffer_Layout_Tests is
   use type AML_Decode.Bytes;
   package L renames AML_Buffer_Layouts;
   use type Integer_Value;
   use type Byte;
   use type L.Layout_Result;
   Checks : Natural := 0;
   procedure Check (Good : Boolean; Message : String) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Message; end if;
   end Check;
   function Literal (Count : Integer_Value; Initializer : Bytes) return Bytes is
      Count_Code : Bytes (1 .. 9) := [others => 0];
      Payload_Length : constant Natural := Count_Code'Length + Initializer'Length;
      Encoding : Positive := 1;
      Extent : Natural;
      Remaining : Natural;
      Header : Bytes (1 .. 4) := [others => 0];
   begin
      Count_Code (1) := 16#0E#;
      for I in 0 .. 7 loop
         Count_Code (I + 2) := Byte ((Count / 2 ** (I * 8)) mod 256);
      end loop;
      if Payload_Length + 1 > 63 then Encoding := 2; end if;
      if Payload_Length + 2 > 4095 then Encoding := 3; end if;
      if Payload_Length + 3 > 1_048_575 then Encoding := 4; end if;
      Extent := Payload_Length + Encoding;
      if Encoding = 1 then Header (1) := Byte (Extent);
      else
         Header (1) := Byte ((Encoding - 1) * 64 + Extent mod 16);
         Remaining := Extent / 16;
         for I in 2 .. Encoding loop
            Header (I) := Byte (Remaining mod 256); Remaining := Remaining / 256;
         end loop;
      end if;
      return Bytes'[16#11#] & Header (1 .. Encoding) & Count_Code & Initializer;
   end Literal;
   procedure Check_Layout (Count : Integer_Value; Initializer : Bytes; Width : Integer_Width) is
      Data : constant Bytes := Literal (Count, Initializer);
      Padded : constant Bytes := Data & [16#CC#, 16#DD#];
      Shifted : constant Bytes (7 .. Data'Length + 6) := Data;
      R : L.Layout_Result;
      Expected : constant Integer_Value := Count and L.Buffer_Length_Mask;
   begin
      R := L.Read (Data, Width);
      Check (R.Kind = Accepted and then R.Consumed = Data'Length
        and then R.Initializer_Length = Initializer'Length
        and then R.Initializer_Offset + R.Initializer_Length = R.Consumed
        and then R.Requested_Length = Expected
        and then R.Effective_Length = Integer_Value'Max (Expected, Integer_Value (Initializer'Length)),
        "complete compact layout");
      Check (L.Read (Padded, Width) = R, "trailing bytes excluded from initializer");
      Check (L.Read (Shifted, Width) = R, "initializer offset independent of slice lower bound");
      Check (L.With_Count (Data, Count, 9) = R, "evaluated count shares normalized layout");
   end Check_Layout;
   Lengths : constant array (Positive range <>) of Integer_Value :=
     [0, 1, 1024, 1025, 8192, 65_536, 65_537, 16#FFFF_FFFF#, 16#1_0000_0001#, Integer_Value'Last];
   Large : constant Bytes (1 .. 1025) := [others => 16#A5#];
   procedure Package_Encoding_Boundary (Length : Positive; Encoding : Positive) is
      Initializer : Bytes (5 .. Length + 4) := [others => 16#A7#];
   begin
      Initializer (Initializer'First) := 16#D3#;
      Initializer (Initializer'Last) := 16#7E#;
      declare
         Data : constant Bytes := Literal (0, Initializer);
         Package_Info : constant Package_Result := Read_Package (Data (2 .. Data'Last));
         Layout : constant L.Layout_Result := L.Read (Data, Bits_64);
      begin
         Check (Package_Info.Kind = Accepted and then Package_Info.Encoding_Bytes = Encoding,
           "actual multi-byte package length encoding");
         Check (Layout.Kind = Accepted and then Layout.Consumed = Data'Length
           and then Layout.Initializer_Length = Length and then Layout.Effective_Length = Integer_Value (Length)
           and then Layout.Initializer_Offset = 1 + Encoding + 9
           and then Data (Data'First + Layout.Initializer_Offset) = 16#D3#
           and then Data (Data'First + Layout.Initializer_Offset + Layout.Initializer_Length - 1) = 16#7E#,
           "large initializer spans precisely match encoded payload");
      end;
   end Package_Encoding_Boundary;
   R : L.Layout_Result;
begin
   Package_Encoding_Boundary (4096, 3);
   Package_Encoding_Boundary (1_048_576, 4);
   for Width in Integer_Width loop
      for Count of Lengths loop
         Check_Layout (Count, Bytes'(1 .. 0 => 0), Width);
         Check_Layout (Count, [16#A5#, 16#5A#], Width);
      end loop;
      Check_Layout (0, Large, Width); Check_Layout (1025, Large, Width); Check_Layout (8192, Large, Width);
      R := L.Read ([16#11#, 1], Width);
      Check (R.Kind = Malformed, "missing literal count malformed");
      R := L.Read ([16#11#, 3, 16#0B#, 1], Width);
      Check (R.Kind = Truncated, "truncated integer count");
   end loop;
   R := L.With_Count ([16#11#, 5, 0], Integer_Value'Last, 1);
   Check (R.Kind = Truncated, "package truncation precedes huge count sizing");
   R := L.With_Count ([16#11#, 2, 0], Integer_Value'Last, 0);
   Check (R.Kind = Malformed, "zero consumed count rejected");
   R := L.With_Count ([16#11#, 2, 0], Integer_Value'Last, 2);
   Check (R.Kind = Malformed, "count span cannot exceed package");
   R := L.Read ([16#12#, 2, 0], Bits_64);
   Check (R.Kind = Unsupported, "wrong opcode");
   R := L.Read (Bytes'(1 .. 0 => 0), Bits_64);
   Check (R.Kind = Truncated, "empty input");
   Ada.Text_IO.Put_Line ("Buffer layout checks" & Checks'Image);
end Buffer_Layout_Tests;

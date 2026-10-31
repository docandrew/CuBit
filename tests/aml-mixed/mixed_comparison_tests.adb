pragma Ada_2022;
with Ada.Text_IO;
with AML_Coercions.Strings;
with AML_Decode; use AML_Decode;
with AML_Mixed_Comparison; use AML_Mixed_Comparison;
procedure Mixed_Comparison_Tests is
   Count : Natural := 0;
   procedure Check (Actual : Result; Status : Comparison_Status;
                    Value : Comparison_Value) is
   begin
      Count := Count + 1;
      if Actual.Status /= Status or else Actual.Value /= Value then
         raise Program_Error with "comparison check" & Count'Image;
      end if;
   end Check;
begin
   -- Cached ACPICA II, revision 1
   Check (Compare (Bits_32, Integer_Input, 18, Bytes'(1 .. 0 => 0),
     Integer_Input, 18, Bytes'(1 .. 0 => 0)), Compared, 0);
   -- Cached ACPICA IS, revision 1
   Check (Compare (Bits_32, Integer_Input, 18, Bytes'(1 .. 0 => 0),
     String_Input, 0, Bytes'(1 => 49, 2 => 50)), Compared, 0);
   -- Cached ACPICA IB, revision 1
   Check (Compare (Bits_32, Integer_Input, 18, Bytes'(1 .. 0 => 0),
     Buffer_Input, 0, Bytes'(1 => 18)), Compared, 0);
   -- Cached ACPICA SI, revision 1
   Check (Compare (Bits_32, String_Input, 0, Bytes'(1 => 48, 2 => 48, 3 => 48, 4 => 48, 5 => 48, 6 => 48, 7 => 49, 8 => 50),
     Integer_Input, 18, Bytes'(1 .. 0 => 0)), Compared, 0);
   -- Cached ACPICA SS, revision 1
   Check (Compare (Bits_32, String_Input, 0, Bytes'(1 => 49, 2 => 50),
     String_Input, 0, Bytes'(1 => 49, 2 => 56)), Compared, -1);
   -- Cached ACPICA SB, revision 1
   Check (Compare (Bits_32, String_Input, 0, Bytes'(1 => 48, 2 => 120, 3 => 49, 4 => 50, 5 => 32, 6 => 48, 7 => 120, 8 => 65, 9 => 66),
     Buffer_Input, 0, Bytes'(1 => 18, 2 => 171)), Compared, 0);
   -- Cached ACPICA BI, revision 1
   Check (Compare (Bits_32, Buffer_Input, 0, Bytes'(1 => 18, 2 => 0, 3 => 0, 4 => 0),
     Integer_Input, 18, Bytes'(1 .. 0 => 0)), Compared, 0);
   -- Cached ACPICA BS, revision 1
   Check (Compare (Bits_32, Buffer_Input, 0, Bytes'(1 => 49, 2 => 50, 3 => 0),
     String_Input, 0, Bytes'(1 => 49, 2 => 50)), Compared, 0);
   -- Cached ACPICA BB, revision 1
   Check (Compare (Bits_32, Buffer_Input, 0, Bytes'(1 => 0, 2 => 255),
     Buffer_Input, 0, Bytes'(1 => 1, 2 => 0)), Compared, -1);
   -- Cached ACPICA shortSI, revision 1
   Check (Compare (Bits_32, String_Input, 0, Bytes'(1 => 49, 2 => 50),
     Integer_Input, 18, Bytes'(1 .. 0 => 0)), Compared, 1);
   -- Cached ACPICA shortBI, revision 1
   Check (Compare (Bits_32, Buffer_Input, 0, Bytes'(1 => 18),
     Integer_Input, 18, Bytes'(1 .. 0 => 0)), Compared, -1);
   -- Cached ACPICA emptyIB, revision 1
   Check (Compare (Bits_32, Integer_Input, 0, Bytes'(1 .. 0 => 0),
     Buffer_Input, 0, Bytes'(1 .. 0 => 0)), Empty_Buffer, 0);
   -- Cached ACPICA emptySB, revision 1
   Check (Compare (Bits_32, String_Input, 0, Bytes'(1 .. 0 => 0),
     Buffer_Input, 0, Bytes'(1 .. 0 => 0)), Compared, 0);
   -- Cached ACPICA emptyBS, revision 1
   Check (Compare (Bits_32, Buffer_Input, 0, Bytes'(1 .. 0 => 0),
     String_Input, 0, Bytes'(1 .. 0 => 0)), Compared, -1);
   -- Cached ACPICA emptyIS, revision 1
   Check (Compare (Bits_32, Integer_Input, 0, Bytes'(1 .. 0 => 0),
     String_Input, 0, Bytes'(1 .. 0 => 0)), Compared, 0);
   -- Cached ACPICA badIS, revision 1
   Check (Compare (Bits_32, Integer_Input, 0, Bytes'(1 .. 0 => 0),
     String_Input, 0, Bytes'(1 => 90, 2 => 51)), Compared, 0);
   -- Cached ACPICA signIS, revision 1
   Check (Compare (Bits_32, Integer_Input, 0, Bytes'(1 .. 0 => 0),
     String_Input, 0, Bytes'(1 => 45, 2 => 49)), Compared, 0);
   -- Cached ACPICA prefixIS, revision 1
   Check (Compare (Bits_32, Integer_Input, 18, Bytes'(1 .. 0 => 0),
     String_Input, 0, Bytes'(1 => 48, 2 => 120, 3 => 49, 4 => 50)), Compared, 0);
   -- Cached ACPICA suffixIS, revision 1
   Check (Compare (Bits_32, Integer_Input, 3, Bytes'(1 .. 0 => 0),
     String_Input, 0, Bytes'(1 => 51, 2 => 90)), Compared, 0);
   -- Cached ACPICA widthIS, revision 1
   Check (Compare (Bits_32, Integer_Input, 268435456, Bytes'(1 .. 0 => 0),
     String_Input, 0, Bytes'(1 => 49, 2 => 48, 3 => 48, 4 => 48, 5 => 48, 6 => 48, 7 => 48, 8 => 48, 9 => 51)), Compared, 0);
   -- Cached ACPICA widthIB, revision 1
   Check (Compare (Bits_32, Integer_Input, 3, Bytes'(1 .. 0 => 0),
     Buffer_Input, 0, Bytes'(1 => 3, 2 => 0, 3 => 0, 4 => 0, 5 => 1, 6 => 0, 7 => 0, 8 => 0, 9 => 255)), Compared, 0);
   -- Cached ACPICA zeroSB, revision 1
   Check (Compare (Bits_32, String_Input, 0, Bytes'(1 => 48, 2 => 120, 3 => 48, 4 => 48, 5 => 32, 6 => 48, 7 => 120, 8 => 70, 9 => 70),
     Buffer_Input, 0, Bytes'(1 => 0, 2 => 255)), Compared, 0);
   -- Cached ACPICA termBS, revision 1
   Check (Compare (Bits_32, Buffer_Input, 0, Bytes'(1 => 65),
     String_Input, 0, Bytes'(1 => 65)), Compared, -1);
   -- Cached ACPICA highBB, revision 1
   Check (Compare (Bits_32, Buffer_Input, 0, Bytes'(1 => 255),
     Buffer_Input, 0, Bytes'(1 => 127)), Compared, 1);
   -- Cached ACPICA prefixSS, revision 1
   Check (Compare (Bits_32, String_Input, 0, Bytes'(1 => 65),
     String_Input, 0, Bytes'(1 => 65, 2 => 65)), Compared, -1);
   -- Cached ACPICA emptyBI, revision 1
   Check (Compare (Bits_32, Buffer_Input, 0, Bytes'(1 .. 0 => 0),
     Integer_Input, 0, Bytes'(1 .. 0 => 0)), Compared, -1);
   -- Cached ACPICA II, revision 2
   Check (Compare (Bits_64, Integer_Input, 18, Bytes'(1 .. 0 => 0),
     Integer_Input, 18, Bytes'(1 .. 0 => 0)), Compared, 0);
   -- Cached ACPICA IS, revision 2
   Check (Compare (Bits_64, Integer_Input, 18, Bytes'(1 .. 0 => 0),
     String_Input, 0, Bytes'(1 => 49, 2 => 50)), Compared, 0);
   -- Cached ACPICA IB, revision 2
   Check (Compare (Bits_64, Integer_Input, 18, Bytes'(1 .. 0 => 0),
     Buffer_Input, 0, Bytes'(1 => 18)), Compared, 0);
   -- Cached ACPICA SI, revision 2
   Check (Compare (Bits_64, String_Input, 0, Bytes'(1 => 48, 2 => 48, 3 => 48, 4 => 48, 5 => 48, 6 => 48, 7 => 48, 8 => 48, 9 => 48, 10 => 48, 11 => 48, 12 => 48, 13 => 48, 14 => 48, 15 => 49, 16 => 50),
     Integer_Input, 18, Bytes'(1 .. 0 => 0)), Compared, 0);
   -- Cached ACPICA SS, revision 2
   Check (Compare (Bits_64, String_Input, 0, Bytes'(1 => 49, 2 => 50),
     String_Input, 0, Bytes'(1 => 49, 2 => 56)), Compared, -1);
   -- Cached ACPICA SB, revision 2
   Check (Compare (Bits_64, String_Input, 0, Bytes'(1 => 48, 2 => 120, 3 => 49, 4 => 50, 5 => 32, 6 => 48, 7 => 120, 8 => 65, 9 => 66),
     Buffer_Input, 0, Bytes'(1 => 18, 2 => 171)), Compared, 0);
   -- Cached ACPICA BI, revision 2
   Check (Compare (Bits_64, Buffer_Input, 0, Bytes'(1 => 18, 2 => 0, 3 => 0, 4 => 0, 5 => 0, 6 => 0, 7 => 0, 8 => 0),
     Integer_Input, 18, Bytes'(1 .. 0 => 0)), Compared, 0);
   -- Cached ACPICA BS, revision 2
   Check (Compare (Bits_64, Buffer_Input, 0, Bytes'(1 => 49, 2 => 50, 3 => 0),
     String_Input, 0, Bytes'(1 => 49, 2 => 50)), Compared, 0);
   -- Cached ACPICA BB, revision 2
   Check (Compare (Bits_64, Buffer_Input, 0, Bytes'(1 => 0, 2 => 255),
     Buffer_Input, 0, Bytes'(1 => 1, 2 => 0)), Compared, -1);
   -- Cached ACPICA shortSI, revision 2
   Check (Compare (Bits_64, String_Input, 0, Bytes'(1 => 49, 2 => 50),
     Integer_Input, 18, Bytes'(1 .. 0 => 0)), Compared, 1);
   -- Cached ACPICA shortBI, revision 2
   Check (Compare (Bits_64, Buffer_Input, 0, Bytes'(1 => 18),
     Integer_Input, 18, Bytes'(1 .. 0 => 0)), Compared, -1);
   -- Cached ACPICA emptyIB, revision 2
   Check (Compare (Bits_64, Integer_Input, 0, Bytes'(1 .. 0 => 0),
     Buffer_Input, 0, Bytes'(1 .. 0 => 0)), Empty_Buffer, 0);
   -- Cached ACPICA emptySB, revision 2
   Check (Compare (Bits_64, String_Input, 0, Bytes'(1 .. 0 => 0),
     Buffer_Input, 0, Bytes'(1 .. 0 => 0)), Compared, 0);
   -- Cached ACPICA emptyBS, revision 2
   Check (Compare (Bits_64, Buffer_Input, 0, Bytes'(1 .. 0 => 0),
     String_Input, 0, Bytes'(1 .. 0 => 0)), Compared, -1);
   -- Cached ACPICA emptyIS, revision 2
   Check (Compare (Bits_64, Integer_Input, 0, Bytes'(1 .. 0 => 0),
     String_Input, 0, Bytes'(1 .. 0 => 0)), Compared, 0);
   -- Cached ACPICA badIS, revision 2
   Check (Compare (Bits_64, Integer_Input, 0, Bytes'(1 .. 0 => 0),
     String_Input, 0, Bytes'(1 => 90, 2 => 51)), Compared, 0);
   -- Cached ACPICA signIS, revision 2
   Check (Compare (Bits_64, Integer_Input, 0, Bytes'(1 .. 0 => 0),
     String_Input, 0, Bytes'(1 => 45, 2 => 49)), Compared, 0);
   -- Cached ACPICA prefixIS, revision 2
   Check (Compare (Bits_64, Integer_Input, 18, Bytes'(1 .. 0 => 0),
     String_Input, 0, Bytes'(1 => 48, 2 => 120, 3 => 49, 4 => 50)), Compared, 0);
   -- Cached ACPICA suffixIS, revision 2
   Check (Compare (Bits_64, Integer_Input, 3, Bytes'(1 .. 0 => 0),
     String_Input, 0, Bytes'(1 => 51, 2 => 90)), Compared, 0);
   -- Cached ACPICA widthIS, revision 2
   Check (Compare (Bits_64, Integer_Input, 4294967299, Bytes'(1 .. 0 => 0),
     String_Input, 0, Bytes'(1 => 49, 2 => 48, 3 => 48, 4 => 48, 5 => 48, 6 => 48, 7 => 48, 8 => 48, 9 => 51)), Compared, 0);
   -- Cached ACPICA widthIB, revision 2
   Check (Compare (Bits_64, Integer_Input, 4294967299, Bytes'(1 .. 0 => 0),
     Buffer_Input, 0, Bytes'(1 => 3, 2 => 0, 3 => 0, 4 => 0, 5 => 1, 6 => 0, 7 => 0, 8 => 0, 9 => 255)), Compared, 0);
   -- Cached ACPICA zeroSB, revision 2
   Check (Compare (Bits_64, String_Input, 0, Bytes'(1 => 48, 2 => 120, 3 => 48, 4 => 48, 5 => 32, 6 => 48, 7 => 120, 8 => 70, 9 => 70),
     Buffer_Input, 0, Bytes'(1 => 0, 2 => 255)), Compared, 0);
   -- Cached ACPICA termBS, revision 2
   Check (Compare (Bits_64, Buffer_Input, 0, Bytes'(1 => 65),
     String_Input, 0, Bytes'(1 => 65)), Compared, -1);
   -- Cached ACPICA highBB, revision 2
   Check (Compare (Bits_64, Buffer_Input, 0, Bytes'(1 => 255),
     Buffer_Input, 0, Bytes'(1 => 127)), Compared, 1);
   -- Cached ACPICA prefixSS, revision 2
   Check (Compare (Bits_64, String_Input, 0, Bytes'(1 => 65),
     String_Input, 0, Bytes'(1 => 65, 2 => 65)), Compared, -1);
   -- Cached ACPICA emptyBI, revision 2
   Check (Compare (Bits_64, Buffer_Input, 0, Bytes'(1 .. 0 => 0),
     Integer_Input, 0, Bytes'(1 .. 0 => 0)), Compared, -1);
   for Width in Integer_Width loop
      declare
         Raw : constant Bytes (Positive'Last - 1023 .. Positive'Last) := [others => 255];
         Rendered : constant Bytes := AML_Coercions.Strings.From_Buffer (Raw);
         Prefix : constant Bytes (1 .. 1025) := [others => Character'Pos ('0')];
         Full : constant Bytes (1 .. Max_Input_Length) := [others => 255];
         High : constant Bytes (Positive'Last - 2 .. Positive'Last) := [65, 0, 66];
         Low : constant Bytes := [65, 0, 67];
      begin
         Check (Compare (Width, String_Input, 0, Rendered,
           Buffer_Input, 0, Raw), Compared, 0);
         Check (Compare (Width, String_Input, 0, Rendered (1 .. Rendered'Last - 1),
           Buffer_Input, 0, Raw), Compared, -1);
         -- Conversion expands beyond 1024 without materializing an object.
         Check (Compare (Width, String_Input, 0, Prefix,
           Buffer_Input, 0, Raw), Compared, -1);
         Check (Compare (Width, String_Input, 0, Prefix,
           Buffer_Input, 0, Full), Compared, -1);
         -- Full String plus terminator exceeds the input bound as a view.
         Check (Compare (Width, Buffer_Input, 0, Full,
           String_Input, 0, Full), Compared, -1);
         Check (Compare (Width, String_Input, 0, High,
           String_Input, 0, Low), Compared, -1);
         Check (Compare (Width, Buffer_Input, 0, High,
           Buffer_Input, 0, High), Compared, 0);
      end;
   end loop;
   Ada.Text_IO.Put_Line ("PASS" & Count'Image);
end Mixed_Comparison_Tests;

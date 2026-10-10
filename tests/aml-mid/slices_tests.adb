with Ada.Text_IO;
with AML_Decode;
with AML_Slices; use AML_Slices;
procedure Slices_Tests is
   use type AML_Decode.Integer_Value;
   Checks : Natural := 0;
   procedure Check (Good : Boolean) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Checks'Image; end if;
   end Check;
   procedure Case_Check (Size : Extent; Start, Count : AML_Decode.Integer_Value;
                         Expected_Offset, Expected_Length : Extent) is
      R : constant Slice_Range := Select_Range (Size, Start, Count);
   begin
      Check (R.Offset = Expected_Offset and then R.Length = Expected_Length);
      Check (R.Offset <= Size and then R.Length <= Size - R.Offset);
   end Case_Check;
   type Values is array (Positive range <>) of AML_Decode.Integer_Value;
   Extremes : constant Values := [0, 1, 16#FFFF_FFFF#, 16#1_0000_0000#, AML_Decode.Integer_Value'Last];
   type Sizes is array (Positive range <>) of Extent;
   Boundaries : constant Sizes := [0, 1, 2, Extent'Last - 1, Extent'Last];
begin
   for Width in AML_Decode.Integer_Width loop
      declare
         Maximum : constant AML_Decode.Integer_Value :=
           (case Width is when AML_Decode.Bits_32 => 16#FFFF_FFFF#,
                          when AML_Decode.Bits_64 => AML_Decode.Integer_Value'Last);
      begin
         for Size of Boundaries loop
            Case_Check (Size, 0, 0, 0, 0);
            Case_Check (Size, 0, Maximum, 0, Size);
            Case_Check (Size, AML_Decode.Integer_Value (Size), Maximum, Size, 0);
            Case_Check (Size, AML_Decode.Integer_Value (Size) + 1, Maximum, Size, 0);
            Case_Check (Size, Maximum, Maximum, Size, 0);
            if Size > 0 then
               Case_Check (Size, AML_Decode.Integer_Value (Size - 1), Maximum, Size - 1, 1);
               Case_Check (Size, 0, AML_Decode.Integer_Value (Size - 1), 0, Size - 1);
            end if;
         end loop;
      end;
   end loop;
   -- Small-domain reference counts selected positions, rather than mirroring
   -- the helper's clamp/subtract implementation. No Start+Count is formed.
   for Size in Extent range 0 .. 16 loop
      for Start in AML_Decode.Integer_Value range 0 .. 20 loop
         for Count in AML_Decode.Integer_Value range 0 .. 20 loop
            declare
               Offset : Extent := 0;
               Length : Extent := 0;
            begin
               for I in 0 .. Size - 1 loop
                  if AML_Decode.Integer_Value (I) < Start then Offset := Offset + 1;
                  elsif AML_Decode.Integer_Value (I) - Start < Count then Length := Length + 1;
                  end if;
               end loop;
               Case_Check (Size, Start, Count, Offset, Length);
            end;
         end loop;
      end loop;
   end loop;
   -- Raw helper safety is wider than evaluator-normalized inputs.
   for Start of Extremes loop
      for Count of Extremes loop
         declare R : constant Slice_Range := Select_Range (Extent'Last, Start, Count); begin
            Check (R.Length <= Extent'Last - R.Offset);
         end;
      end loop;
   end loop;
   Ada.Text_IO.Put_Line ("Mid slice-range checks" & Checks'Image);
end Slices_Tests;

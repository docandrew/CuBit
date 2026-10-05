pragma Ada_2022;

package body CuBit.Doom_Lumps with SPARK_Mode is

   function Parse (Head : Header; Length : Lump_Length) return Sound is
      Tag      : constant Unsigned_32 :=
        Unsigned_32 (Head (0)) or Shift_Left (Unsigned_32 (Head (1)), 8);
      Rate     : constant Unsigned_32 :=
        Unsigned_32 (Head (2)) or Shift_Left (Unsigned_32 (Head (3)), 8);
      Declared : constant Unsigned_32 :=
        Unsigned_32 (Head (4)) or Shift_Left (Unsigned_32 (Head (5)), 8) or
        Shift_Left (Unsigned_32 (Head (6)), 16) or
        Shift_Left (Unsigned_32 (Head (7)), 24);
      Room     : constant Lump_Length := Length - Header_Bytes;
      --  A count past the lump's end is cut to the lump, as DMX does.
      Count    : constant Lump_Length :=
        (if Declared > Room then Room else Declared);
   begin
      if Tag /= Format_Tag or else Rate = 0 or else Rate > Sample_Rate'Last
        or else Count < Minimum_Samples
      then
         return (Valid => False);
      elsif Count > 2 * Padding_Samples then
         return (Valid => True, Rate => Rate,
                 First => Header_Bytes + Padding_Samples,
                 Count => Count - 2 * Padding_Samples);
      else
         return (Valid => True, Rate => Rate, First => Header_Bytes,
                 Count => Count);
      end if;
   end Parse;

end CuBit.Doom_Lumps;

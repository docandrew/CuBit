with Ada.Text_IO; use Ada.Text_IO;
with Interfaces; use Interfaces;
with CuBit.UI; use CuBit.UI;
with CuBit.UI.Theme_CCL; use CuBit.UI.Theme_CCL;
with CuBit.UI.Theme_Data; use CuBit.UI.Theme_Data;

procedure Main is
   R : Result;
   Count : Natural := 0;
   procedure Reject (Source : String) is
   begin
      Load (Source, CuBit_Alloy_Dark, R);
      pragma Assert (not R.Success and R.Error /= No_Error);
      pragma Assert (R.Value = CuBit_Alloy_Dark);
      Count := Count + 1;
   end Reject;
   function Number (N : Natural) return String is
      S : constant String := N'Image;
   begin return S (2 .. S'Last); end Number;
begin
   Load ("(theme v1 (base alloy-light))", CuBit_Alloy_Dark, R);
   pragma Assert (R.Success and R.Value = CuBit_Alloy);
   Load ("# comment" & ASCII.LF & "(theme v1 (base alloy-dark) (text (rgb 1 2 255)))", CuBit_Alloy, R);
   pragma Assert (R.Success and R.Value.text = 16#0102FF#);
   for Item in CuBit.UI.Theme_Data.Field loop
      for C in 0 .. 255 loop
         Load ("(theme v1 (base alloy-light) (" & Name (Item) & " (rgb " &
           Number (C) & " 0 255)))", CuBit_Alloy, R);
         pragma Assert (R.Success);
         pragma Assert (Colors (R.Value) (Item) = Unsigned_32 (C) * 65536 + 255);
         Count := Count + 1;
      end loop;
   end loop;
   Reject (""); Reject ("(theme v2 (base alloy-light))");
   Reject ("(theme v1 (base unknown))");
   Reject ("(theme v1 (base alloy-dark) (typo (rgb 0 0 0)))");
   Reject ("(theme v1 (base alloy-dark) (text (rgb 1 2 3)) (text (rgb 4 5 6)))");
   Reject ("(theme v1 (base alloy-light) (text (rgb 256 0 0)))");
   Reject ("(theme v1 (base alloy-light) (text (rgb -1 0 0)))");
   Reject ("(theme v1 (base alloy-light) (text (rgb (+ 1 2) 0 0)))");
   Reject ("(theme v1 (base alloy-light)) (extra)");
   Reject ("(theme v1 (base alloy-light) (base alloy-dark))");
   Reject ([1 .. Maximum_Source + 1 => ' ']);
   declare
      Original : constant Palette_Colors := Colors (CuBit_Alloy_Dark);
      Copy : Palette_Colors := Colors (CuBit_Alloy);
      Valid : Boolean;
      Bad : Color_Chunk;
   begin
      for Index in Chunk_Index loop
         Merge (Copy, Index, Chunk (Original, Index), Valid);
         pragma Assert (Valid);
      end loop;
      pragma Assert (Copy = Original);
      for Index in Chunk_Index loop
         for Word in Color_Chunk'Range loop
            for Bit in 24 .. 31 loop
               Bad := Chunk (Original, Index);
               Bad (Word) := Bad (Word) or Shift_Left (Unsigned_64'(1), Bit);
               Merge (Copy, Index, Bad, Valid);
               pragma Assert (not Valid and Copy = Original);
            end loop;
         end loop;
      end loop;
      Bad := Chunk (Original, Chunk_Index'Last);
      Bad (3) := 1;
      Merge (Copy, Chunk_Index'Last, Bad, Valid);
      pragma Assert (not Valid and Copy = Original);
   end;
   declare
      Good : constant String := "(theme v1 (base alloy-dark) (text (rgb 1 2 3)))";
      Mutation : String := Good;
   begin
      for Last in 0 .. Good'Length - 1 loop Reject (Good (1 .. Last)); end loop;
      -- Every single-byte mutation, including NUL/non-ASCII. Acceptance must
      -- never leak an out-of-range color; failures must retain the fallback.
      for I in Good'Range loop
         for Byte in 0 .. 255 loop
            Mutation := Good; Mutation (I) := Character'Val (Byte);
            Load (Mutation, CuBit_Alloy, R);
            if not R.Success then pragma Assert (R.Value = CuBit_Alloy); end if;
            Count := Count + 1;
         end loop;
      end loop;
   end;
   Put_Line ("PASS appearance loader cases:" & Count'Image);
end Main;

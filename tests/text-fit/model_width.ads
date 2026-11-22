--  A stand-in measure for the Text_Fit proofs and tests: proportional
--  advances with one negative kerning pair ("AV"), like the real faces, so
--  prefix widths are not assumed to grow with every character.
package Model_Width with SPARK_Mode, Pure is
   Narrowest : constant := 4;
   Spread : constant := 7;
   Widest : constant := Narrowest + Spread - 1;
   Pair_Kerning : constant := 2;

   subtype Advance is Natural range Narrowest .. Widest;
   function Advance_Of (Ch : Character) return Advance is
     (Narrowest + Character'Pos (Ch) mod Spread);

   function Width (Text : String) return Natural;
   --  Boundary_At's role: characters left of X, from the start of Text.
   function Hint (Text : String; X : Natural) return Natural;
end Model_Width;

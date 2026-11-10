pragma Ada_2022;
package body Shapes is

   function Kind (S : Shape) return Character is ('?');
   function Twice_Area (S : Shape'Class) return Natural is (2 * S.Area);

   overriding function Area (S : Square) return Natural is (S.Side * S.Side);
   overriding function Kind (S : Square) return Character is ('S');

   overriding function Area (P : Polygon) return Natural is
      Sum : Natural := 0;
   begin
      for L of P.Lengths loop
         Sum := Sum + L;
      end loop;
      return Sum * P.Scale;
   end Area;

   overriding function Name_Length (P : Polygon) return Natural is (P.Points);
   overriding function Factor (P : Polygon) return Natural is (P.Scale);

   overriding procedure Grow (P : in out Polygon; By : Natural) is
   begin
      P.Scale := P.Scale * By;
   end Grow;

   overriding function Area (C : Cube) return Natural is (C.Side * C.Side * C.Depth);

   overriding function Name_Length (L : Label) return Natural is (L.Length);

end Shapes;

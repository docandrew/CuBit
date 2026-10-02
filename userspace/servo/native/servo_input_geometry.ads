with Interfaces;
with Client_Canvas_Geometry;
-- Device-pixel representative of a logical cell's start, with the containing
-- canvas's fractional origin phase. Negative capture coordinates remain signed;
-- only values outside the foreign signed32 range saturate.
package Servo_Input_Geometry with SPARK_Mode, Pure is
   subtype Coordinate is Interfaces.Integer_32;
   subtype Wide is Interfaces.Integer_64;
   subtype Component is Client_Canvas_Geometry.Component;
   subtype Origin is Client_Canvas_Geometry.Logical_Edge;
   use type Coordinate, Wide;
   function Edge (Value : Coordinate; N, D : Component) return Wide is
     (if Value >= 0 then (Wide (Value) * Wide (N) + Wide (D) - 1) / Wide (D)
      else (Wide (Value) * Wide (N)) / Wide (D));
   function Relative (Value : Coordinate; Start : Origin; N, D : Component)
     return Coordinate
     with Post => Wide (Relative'Result) = Wide'Max (Wide (Coordinate'First),
       Wide'Min (Wide (Coordinate'Last),
         Edge (Value, N, D) - Wide (Client_Canvas_Geometry.Edge (Start, N, D))));
end Servo_Input_Geometry;

package body Servo_Input_Geometry with SPARK_Mode is
   function Relative (Value : Coordinate; Start : Origin; N, D : Component)
     return Coordinate
   is
      Shifted : constant Wide := Edge (Value, N, D) -
        Wide (Client_Canvas_Geometry.Edge (Start, N, D));
   begin
      return (if Shifted < Wide (Coordinate'First) then Coordinate'First
        elsif Shifted > Wide (Coordinate'Last) then Coordinate'Last
        else Coordinate (Shifted));
   end Relative;
end Servo_Input_Geometry;

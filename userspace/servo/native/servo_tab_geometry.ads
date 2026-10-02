package Servo_Tab_Geometry with SPARK_Mode, Pure is
   subtype Extent is Natural range 0 .. 65_535;
   type Rectangle is record X, Y, W, H : Extent; end record;
   function Page (Width, Height : Extent; Vertical : Boolean) return Rectangle is
     (if Vertical then
        (Natural'Min (192, Width), Natural'Min (64, Height),
         (if Width > 192 then Width - 192 else 0),
         (if Height > 88 then Height - 88 else 0))
      else (0, Natural'Min (104, Height), Width,
         (if Height > 128 then Height - 128 else 0)))
     with Post => Page'Result.X + Page'Result.W <= Width and
       Page'Result.Y + Page'Result.H <= Height;
   subtype Window_Width is Extent range 800 .. Extent'Last;
   subtype Tab_Rank is Natural range 0 .. 31;
   subtype Tab_Count is Positive range 1 .. 32;
   function Visible (Width : Window_Width; Height : Extent;
                     Vertical : Boolean; Count : Tab_Count) return Natural is
     (Natural'Min (Count,
       (if Vertical then (if Height > 132 then (Height - 132) / 36 else 0)
        else (Width - 112) / 48)))
     with Post => Visible'Result <= Count and
       (if not Vertical then Visible'Result <= (Width - 112) / 48);
   function Stride (Width : Window_Width; Count : Tab_Count) return Extent is
     (Natural'Min (224, (Width - 112) / Count));
   function Tab (Width : Window_Width; Rank : Tab_Rank; Vertical : Boolean;
                 Count : Tab_Count := 8) return Rectangle is
     (if Vertical then (8, 104 + Rank * 36, 176, 32)
      else (40 + Rank * Stride (Width, Count), 68,
            Stride (Width, Count) - 4, 28))
     with Pre => Rank < Count and
       (Vertical or else Count <= (Width - 112) / 48),
       Post => Tab'Result.X + Tab'Result.W <= Width and
       Tab'Result.Y + Tab'Result.H <= 1252 and Tab'Result.W >= 44;
end Servo_Tab_Geometry;

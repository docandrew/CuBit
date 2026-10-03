package Servo_Tab_Geometry with SPARK_Mode, Pure is
   Menu_Height : constant := 22;
   Toolbar_Height : constant := 30;
   Strip_Top : constant := Menu_Height + Toolbar_Height;
   Page_Top : constant := Strip_Top + 30;
   -- Match Desktop Settings' contiguous vertical tab strip. The browser uses
   -- compact 26-pixel native headers, with no extra space between rows.
   Vertical_Tab_Height : constant := 26;
   subtype Extent is Natural range 0 .. 65_535;
   type Rectangle is record X, Y, W, H : Extent; end record;
   subtype Rail_Size is Extent range 128 .. 400;
   function Rail (Wanted : Integer; Width : Extent) return Rail_Size is
     (Rail_Size (Integer'Max (128, Integer'Min (Wanted,
       Integer'Min (400, Integer (Width) - 320)))));
   function Page (Width, Height : Extent; Vertical : Boolean;
                  Rail_Width : Rail_Size := 192) return Rectangle is
     (if Vertical then
        (Natural'Min (Rail_Width, Width), Natural'Min (Strip_Top, Height),
         (if Width > Rail_Width then Width - Rail_Width else 0),
         (if Height > Strip_Top + 24 then Height - Strip_Top - 24 else 0))
      else (0, Natural'Min (Page_Top, Height), Width,
         (if Height > Page_Top + 24 then Height - Page_Top - 24 else 0)))
     with Post => Page'Result.X + Page'Result.W <= Width and
       Page'Result.Y + Page'Result.H <= Height;
   subtype Window_Width is Extent range 800 .. Extent'Last;
   subtype Tab_Rank is Natural range 0 .. 31;
   subtype Tab_Count is Positive range 1 .. 32;
   function Visible (Width : Window_Width; Height : Extent;
                     Vertical : Boolean; Count : Tab_Count) return Natural is
     (Natural'Min (Count,
       (if Vertical then (if Height > Page_Top + 24 then (Height - Page_Top - 24) / Vertical_Tab_Height else 0)
        else (Width - 112) / 48)))
     with Post => Visible'Result <= Count and
       (if not Vertical then Visible'Result <= (Width - 112) / 48);
   function Stride (Width : Window_Width; Count : Tab_Count) return Extent is
     (Natural'Min (224, (Width - 112) / Count));
   function Tab (Width : Window_Width; Rank : Tab_Rank; Vertical : Boolean;
                 Count : Tab_Count := 8; Rail_Width : Rail_Size := 192) return Rectangle is
     (if Vertical then (8, Page_Top + Rank * Vertical_Tab_Height, Rail_Width - 16, Vertical_Tab_Height)
      else (40 + Rank * Stride (Width, Count), Strip_Top + 4,
            Stride (Width, Count) - 4, 26))
     with Pre => Rank < Count and
       (Vertical or else Count <= (Width - 112) / 48),
       Post => Tab'Result.X + Tab'Result.W <= Width and
       Tab'Result.Y + Tab'Result.H <= 1252 and Tab'Result.W >= 44;
end Servo_Tab_Geometry;

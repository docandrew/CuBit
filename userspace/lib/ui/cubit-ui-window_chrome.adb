package body CuBit.UI.Window_Chrome with SPARK_Mode is
   function Muted (App, Bar : Theme_Color) return Theme_Color is
      subtype Channel is Unsigned_32 range 0 .. 255;
      Red   : constant := 16#1_0000#;
      Green : constant := 16#100#;
      function Part (Value : Theme_Color; Weight : Unsigned_32) return Channel is
        ((Value / Weight) mod 16#100#)
        with Pre => Weight in 1 | Green | Red;
      --  Rounded App * (100 - P)% + Bar * P%: at most 255.
      function Mix (A, B : Channel) return Channel is
        ((A * (100 - Muted_Percent) + B * Muted_Percent + 50) / 100);
   begin
      return Mix (Part (App, Red), Part (Bar, Red)) * Red +
             Mix (Part (App, Green), Part (Bar, Green)) * Green +
             Mix (Part (App, 1), Part (Bar, 1));
   end Muted;
end CuBit.UI.Window_Chrome;

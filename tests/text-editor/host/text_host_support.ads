with Interfaces; use Interfaces;
with CuBit.Appearance;
with CuBit.UI;
with Text_Shell;

--  What the hosted tests, benchmarks and window share: an in-memory canvas,
--  a monotonic clock, PPM screenshots, the desktop's themes, and pumping a
--  shell until its work settles.
package Text_Host_Support is
   type Pixels is array (Natural range <>) of Unsigned_32;
   type Pixels_Access is access Pixels;
   type Surface is record
      Width, Height : Natural := 0;
      Image : Pixels_Access;
   end record;

   function New_Surface (Width, Height : Positive) return Surface;
   function Canvas (S : Surface) return CuBit.UI.Canvas;
   function Canvas (S : Surface; Damage : CuBit.UI.Rect) return CuBit.UI.Canvas;
   function Bounds (S : Surface) return CuBit.UI.Rect is (0, 0, S.Width, S.Height);
   procedure Save_PPM (S : Surface; Path : String);
   function Pixel (S : Surface; X, Y : Natural) return Unsigned_32;

   function Now_Us return Unsigned_64;

   --  TEXT_THEME_LIGHT and TEXT_THEME_DARK hold system.ccl's themes (run.sh
   --  takes them from ccl-config's plan), loaded over the built-in palette.
   procedure Install_Themes (Scheme : CuBit.Appearance.Color_Scheme);

   --  Pump (and render, as a frame would) until nothing is busy or
   --  Timeout_Ms passes; False on timeout.
   function Settle (Shell : in out Text_Shell.Shell; Target : Surface; Timeout_Ms : Positive := 60_000) return Boolean;
   --  One frame: damage, render into the surface's damaged area.
   procedure Frame (Shell : in out Text_Shell.Shell; Target : Surface);
end Text_Host_Support;

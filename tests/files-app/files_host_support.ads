with CuBit.Appearance;
with Interfaces; use Interfaces;
with CuBit.UI;
with CuBit.UI.Controls;
with CuBit.UI.State;
with Files_View;

--  What the hosted tests, benchmarks and window share: an in-memory
--  canvas, a monotonic clock, PPM screenshots, the toolkit's retained
--  pointer dispatch (as CuBit.UI.App.Run does natively) and pumping the
--  view until its panes settle.
package Files_Host_Support is
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

   --  The scratch place's directory (FILES_SCRATCH, else the default) and
   --  the mock service set up with it and a host root.
   --  The desktop's themes, as on CuBit: FILES_THEME_LIGHT and
   --  FILES_THEME_DARK hold system.ccl's desktop.appearance.theme.* values
   --  (run.sh takes them from ccl-config's plan); each is loaded with
   --  CuBit.UI.Theme_CCL over the built-in palette, which stays when unset or
   --  invalid. Then Scheme is selected.
   procedure Install_Themes (Scheme : CuBit.Appearance.Color_Scheme);
   function Scratch_Root return String;
   procedure Configure_Service (Host_Root : String);

   type View_Access is access Files_View.View_State;
   type UI_Access is access CuBit.UI.State.UI_State;
   type Map_Access is access CuBit.UI.Controls.Control_Map;

   --  Pump until both panes settle (or Timeout_Ms passes); False on
   --  timeout.
   --  The service's wake (OP_FS_WAKE): Configure_Service hooks it to this
   --  event; wait for it, at most Timeout_Ms, when the view is idle with
   --  requests out.
   procedure Wait_For_Wake (Timeout_Ms : Positive);

   function Settle (View : in out Files_View.View_State; Timeout_Ms : Positive := 10_000) return Boolean;
   --  A pointer event at X, Y through the retained dispatch, then the view.
   procedure Pointer
     (View : in out Files_View.View_State; UI : in out CuBit.UI.State.UI_State;
      Map : in out CuBit.UI.Controls.Control_Map; Action : CuBit.UI.Controls.Pointer_Action; X, Y : Natural;
      Time_Ms : Unsigned_64 := 0; Control, Shift : Boolean := False; Secondary : Boolean := False;
      Middle : Boolean := False);
end Files_Host_Support;

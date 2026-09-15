with CuBit.Appearance;
with CuBit.UI;

--  Desktop-owned appearance page. No IPC handlers and no file authority.
--  Uses shared toolkit drawing/hit semantics, separate from compositor state.
package Desktop_Settings is
   type Control is (None, Light, Dark, Image, Cubie, Slate, Ocean,
                    Fill, Fit, Center, Apply, Revert);
   type Save_Status is (Unchanged, Saved_In_Config, Session_Only);
   type State is record
      Pending : CuBit.Appearance.Preferences := CuBit.Appearance.Default;
      Applied : CuBit.Appearance.Preferences := CuBit.Appearance.Default;
      Hovered, Pressed : Control := None;
      Focused : Control := Light;
      Status : Save_Status := Unchanged;
   end record;
   procedure Open (View : out State; Current : CuBit.Appearance.Preferences);
   procedure Draw (View : State; C : CuBit.UI.Canvas; Bounds : CuBit.UI.Rect);
   procedure Pointer
     (View : in out State; Bounds : CuBit.UI.Rect;
      Pointer : CuBit.UI.Pointer_State; Apply_Requested : out Boolean);
   procedure Key
     (View : in out State; Scan_Code : Natural; Backwards : Boolean;
      Apply_Requested : out Boolean);
end Desktop_Settings;

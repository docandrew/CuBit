package body Desktop_Wallpaper is
   procedure Render (Target : System.Address; Width, Height, Pitch : Positive) is
   begin raise Program_Error with "unexpected legacy wallpaper render"; end;
   procedure Paint (Target : System.Address; Width, Height, Pitch : Positive;
                    X, Y, W, H : Natural;
                    Style : CuBit.Appearance.Preferences := CuBit.Appearance.Default) is
   begin raise Program_Error with "unexpected legacy wallpaper paint"; end;
   procedure Paint_Output
     (Target : System.Address; Pitch : Positive;
      Screen : CuBit.Display_Geometry.Output;
      Bounds : CuBit.Display_Geometry.Logical_Rectangle;
      Damage : CuBit.Display_Geometry.Physical_Rectangle;
      Style : CuBit.Appearance.Preferences := CuBit.Appearance.Default) is
   begin raise Program_Error with "unexpected native wallpaper paint"; end;
end Desktop_Wallpaper;

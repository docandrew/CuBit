--  Versioned appearance preferences. Pure data, not authority to change them.
package CuBit.Appearance with SPARK_Mode, Pure is
   type Color_Scheme is (Alloy_Light, Alloy_Dark);
   --  Append-only encoding: existing saved background selections stay valid.
   type Background is (Wallpaper, Slate, Ocean, Cubie);
   type Placement is (Fill, Fit, Center);
   type Preferences is record
      Scheme : Color_Scheme := Alloy_Light;
      Backdrop : Background := Wallpaper;
      Position : Placement := Fill;
   end record;
   Default : constant Preferences := (others => <>);
   Config_Key : constant String := "desktop.appearance.v1";
   --  One atomic Config value: no partially applied theme/background tuple.
   subtype Encoding is String (1 .. 3);
   function Encode (Item : Preferences) return Encoding;
   function Valid (Text : String) return Boolean;
   function Decode (Text : String) return Preferences
     with Pre => Valid (Text),
          Post => Encode (Decode'Result) = Text;
end CuBit.Appearance;

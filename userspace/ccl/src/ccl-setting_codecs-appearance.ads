--  Generated from userspace/ccl/interfaces/settings/ by
--  ccl-manifest --setting-codecs (CAP-003); do not edit. Regenerate with
--  make -C kernel setting-kinds after changing the catalog or a type.
pragma Ada_2022;
with CuBit.Canonical_Readers;

--  The typed setting desktop.appearance.v2: a CCL Appearance (CCL.Interfaces.Desktop_Display.APPEARANCE_TYPE_SOURCE),
--  as Ada types, and Decode, which reads its canonical source
--  (CCL.Typed_Settings renders it; CuBit.Canonical_Readers).
package CCL.Setting_Codecs.Appearance with SPARK_Mode, Pure is
   subtype Text is CuBit.Canonical_Readers.Text;
   type Color_Scheme_Value is (Alloy_Light, Alloy_Dark);
   type Placement_Value is (Fill, Fit, Center);
   type Wallpaper_Choice_Value is record
      name : Text := CuBit.Canonical_Readers.Empty_Text;
      placement : Placement_Value := Placement_Value'First;
   end record;
   type Solid_Color_Value is (Slate, Ocean);
   type Backdrop_Choice is (Image, Solid);
   type Backdrop_Value is record
      Choice : Backdrop_Choice := Backdrop_Choice'First;
      Image_Payload : Wallpaper_Choice_Value := (others => <>);
      Solid_Payload : Solid_Color_Value := Solid_Color_Value'First;
   end record;
   type Appearance_Value is record
      scheme : Color_Scheme_Value := Color_Scheme_Value'First;
      backdrop : Backdrop_Value := (others => <>);
   end record;

   --  Item is the value when Success: Source is exactly the canonical
   --  source of a Appearance. Otherwise Item is (others => <>).
   procedure Decode (Source : String; Item : out Appearance_Value; Success : out Boolean)
   with Global => null, Always_Terminates;
end CCL.Setting_Codecs.Appearance;


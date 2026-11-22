--  Generated from CCL.Interfaces.Wallpapers.TYPE_SOURCE by
--  ccl-manifest --asset-codecs; do not edit. Regenerate with
--  make -C kernel asset-codecs after changing the type.
pragma Ada_2022;
with CuBit.Canonical_Readers;

--  One line of a wallpaper package's catalog: a CCL Wallpaper
--  (CCL.Interfaces.Wallpapers.TYPE_SOURCE), as Ada types, and Decode,
--  which reads its canonical source (CCL.Typed_Values renders it;
--  CuBit.Canonical_Readers).
package CCL.Asset_Codecs.Wallpaper with SPARK_Mode, Pure is
   subtype Text is CuBit.Canonical_Readers.Text;
   type Wallpaper_Value is record
      name : Text := CuBit.Canonical_Readers.Empty_Text;
      label : Text := CuBit.Canonical_Readers.Empty_Text;
      file : Text := CuBit.Canonical_Readers.Empty_Text;
      attribution : Text := CuBit.Canonical_Readers.Empty_Text;
   end record;

   --  Item is the value when Success: Source is exactly the canonical
   --  source of a Wallpaper. Otherwise Item is (others => <>).
   procedure Decode (Source : String; Item : out Wallpaper_Value; Success : out Boolean)
   with Global => null, Always_Terminates;
end CCL.Asset_Codecs.Wallpaper;


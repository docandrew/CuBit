--  Generated from CCL.Interfaces.Wallpapers.TYPE_SOURCE by
--  ccl-manifest --asset-codecs; do not edit. Regenerate with
--  make -C kernel asset-codecs after changing the type.
pragma Ada_2022;

package body CCL.Asset_Codecs.Wallpaper with SPARK_Mode is
   package R renames CuBit.Canonical_Readers;

   procedure Read_Wallpaper
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out Wallpaper_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
   begin
      R.Expect (Source, Position, OK, "(Wallpaper");
      R.Expect (Source, Position, OK, " name => ");
      R.Read_Text (Source, Position, OK, Item.name);
      R.Expect (Source, Position, OK, " label => ");
      R.Read_Text (Source, Position, OK, Item.label);
      R.Expect (Source, Position, OK, " file => ");
      R.Read_Text (Source, Position, OK, Item.file);
      R.Expect (Source, Position, OK, " attribution => ");
      R.Read_Text (Source, Position, OK, Item.attribution);
      R.Expect (Source, Position, OK, ")");
   end Read_Wallpaper;

   procedure Decode (Source : String; Item : out Wallpaper_Value; Success : out Boolean) is
   begin
      Item := (others => <>);
      Success := R.Readable (Source) and then Source'Length in 1 .. R.Maximum_Canonical;
      if not Success then
         return;
      end if;
      declare
         Position : Positive := Source'First;
      begin
         Read_Wallpaper (Source, Position, Success, Item);
         R.Finish (Source, Position, Success);
      end;
      if not Success then
         Item := (others => <>);
      end if;
   end Decode;
end CCL.Asset_Codecs.Wallpaper;


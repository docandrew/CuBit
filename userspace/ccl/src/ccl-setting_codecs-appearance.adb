--  Generated from userspace/ccl/interfaces/settings/ by
--  ccl-manifest --setting-codecs (CAP-003); do not edit. Regenerate with
--  make -C kernel setting-kinds after changing the catalog or a type.
pragma Ada_2022;

package body CCL.Setting_Codecs.Appearance with SPARK_Mode is
   package R renames CuBit.Canonical_Readers;

   procedure Read_Color_Scheme
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out Color_Scheme_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
   begin
      Item := Color_Scheme_Value'First;
      if not OK then
         return;
      elsif R.Next_Is_Word (Source, Position, "Color_Scheme.Alloy_Light") then
         R.Expect_Word (Source, Position, OK, "Color_Scheme.Alloy_Light");
         Item := Alloy_Light;
      elsif R.Next_Is_Word (Source, Position, "Color_Scheme.Alloy_Dark") then
         R.Expect_Word (Source, Position, OK, "Color_Scheme.Alloy_Dark");
         Item := Alloy_Dark;
      else
         OK := False;
      end if;
   end Read_Color_Scheme;

   procedure Read_Placement
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out Placement_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
   begin
      Item := Placement_Value'First;
      if not OK then
         return;
      elsif R.Next_Is_Word (Source, Position, "Placement.Fill") then
         R.Expect_Word (Source, Position, OK, "Placement.Fill");
         Item := Fill;
      elsif R.Next_Is_Word (Source, Position, "Placement.Fit") then
         R.Expect_Word (Source, Position, OK, "Placement.Fit");
         Item := Fit;
      elsif R.Next_Is_Word (Source, Position, "Placement.Center") then
         R.Expect_Word (Source, Position, OK, "Placement.Center");
         Item := Center;
      else
         OK := False;
      end if;
   end Read_Placement;

   procedure Read_Wallpaper_Choice
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out Wallpaper_Choice_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
   begin
      R.Expect (Source, Position, OK, "(Wallpaper_Choice");
      R.Expect (Source, Position, OK, " name => ");
      R.Read_Text (Source, Position, OK, Item.name);
      R.Expect (Source, Position, OK, " placement => ");
      Read_Placement (Source, Position, OK, Item.placement);
      R.Expect (Source, Position, OK, ")");
   end Read_Wallpaper_Choice;

   procedure Read_Solid_Color
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out Solid_Color_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
   begin
      Item := Solid_Color_Value'First;
      if not OK then
         return;
      elsif R.Next_Is_Word (Source, Position, "Solid_Color.Slate") then
         R.Expect_Word (Source, Position, OK, "Solid_Color.Slate");
         Item := Slate;
      elsif R.Next_Is_Word (Source, Position, "Solid_Color.Ocean") then
         R.Expect_Word (Source, Position, OK, "Solid_Color.Ocean");
         Item := Ocean;
      else
         OK := False;
      end if;
   end Read_Solid_Color;

   procedure Read_Backdrop
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out Backdrop_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
   begin
      Item := (others => <>);
      if not OK then
         return;
      elsif R.Next_Is (Source, Position, "(Backdrop.Image ") then
         R.Expect (Source, Position, OK, "(Backdrop.Image ");
         Read_Wallpaper_Choice (Source, Position, OK, Item.Image_Payload);
         R.Expect (Source, Position, OK, ")");
         Item.Choice := Image;
      elsif R.Next_Is (Source, Position, "(Backdrop.Solid ") then
         R.Expect (Source, Position, OK, "(Backdrop.Solid ");
         Read_Solid_Color (Source, Position, OK, Item.Solid_Payload);
         R.Expect (Source, Position, OK, ")");
         Item.Choice := Solid;
      else
         OK := False;
      end if;
   end Read_Backdrop;

   procedure Read_Appearance
     (Source : String; Position : in out Positive; OK : in out Boolean; Item : out Appearance_Value)
   with Always_Terminates,
     Pre => R.In_Source (Source, Position),
     Post => R.In_Source (Source, Position) and then Position >= Position'Old
   is
   begin
      R.Expect (Source, Position, OK, "(Appearance");
      R.Expect (Source, Position, OK, " scheme => ");
      Read_Color_Scheme (Source, Position, OK, Item.scheme);
      R.Expect (Source, Position, OK, " backdrop => ");
      Read_Backdrop (Source, Position, OK, Item.backdrop);
      R.Expect (Source, Position, OK, ")");
   end Read_Appearance;

   procedure Decode (Source : String; Item : out Appearance_Value; Success : out Boolean) is
   begin
      Item := (others => <>);
      Success := R.Readable (Source) and then Source'Length in 1 .. R.Maximum_Canonical;
      if not Success then
         return;
      end if;
      declare
         Position : Positive := Source'First;
      begin
         Read_Appearance (Source, Position, Success, Item);
         R.Finish (Source, Position, Success);
      end;
      if not Success then
         Item := (others => <>);
      end if;
   end Decode;
end CCL.Setting_Codecs.Appearance;


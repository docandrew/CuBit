------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
with CCL.Interfaces.Desktop_Display;
with CCL.Setting_Codecs.Appearance;
with CuBit.Canonical_Readers;

package body Desktop_Appearance_Settings is
   package A renames CuBit.Appearance;
   package DD renames CCL.Interfaces.Desktop_Display;
   package Codec renames CCL.Setting_Codecs.Appearance;
   use type Codec.Backdrop_Choice;

   --  The schema's type, member and field names (DD.APPEARANCE_TYPE_SOURCE), for Encode.
   Scheme_Type : constant String := "Color_Scheme";
   Backdrop_Type : constant String := "Backdrop";
   Choice_Type : constant String := "Wallpaper_Choice";
   Placement_Type : constant String := "Placement";
   Solid_Type : constant String := "Solid_Color";
   Image_Member : constant String := "Image";
   Solid_Member : constant String := "Solid";

   --  The Ada and CCL enumerations name the same members in the same order.
   pragma Compile_Time_Error
     (Codec.Color_Scheme_Value'Pos (Codec.Color_Scheme_Value'Last) /= A.Color_Scheme'Pos (A.Color_Scheme'Last)
      or else Codec.Placement_Value'Pos (Codec.Placement_Value'Last) /= A.Placement'Pos (A.Placement'Last)
      or else Codec.Solid_Color_Value'Pos (Codec.Solid_Color_Value'Last) /= A.Solid_Color'Pos (A.Solid_Color'Last),
      "the CCL appearance enumerations must be CuBit.Appearance's");

   procedure Decode (Source : String; Item : out A.Preferences; Success : out Boolean) is
      Setting : Codec.Appearance_Value;
   begin
      Item := A.Default;
      Codec.Decode (Source, Setting, Success);
      if not Success then
         return;
      end if;
      Item.Scheme := A.Color_Scheme'Val (Codec.Color_Scheme_Value'Pos (Setting.scheme));
      if Setting.backdrop.Choice = Codec.Image then
         declare
            Name : constant String := CuBit.Canonical_Readers.Image (Setting.backdrop.Image_Payload.name);
         begin
            --  Empty: the default wallpaper. Anything else must be spelled
            --  as a catalog name could be.
            if Name'Length > 0 and then not A.Valid_Image_Name (Name) then
               Item := A.Default;
               Success := False;
               return;
            end if;
            Item.Kind := A.Image;
            Item.Image := A.To_Image_Name (Name);
            Item.Position := A.Placement'Val (Codec.Placement_Value'Pos (Setting.backdrop.Image_Payload.placement));
         end;
      else
         Item.Kind := A.Solid;
         Item.Color := A.Solid_Color'Val (Codec.Solid_Color_Value'Pos (Setting.backdrop.Solid_Payload));
      end if;
   end Decode;

   --  The schema's member names. Spelled out: the CuBit runtime has no
   --  enumeration images ('Image gives the position).
   function Member (Item : A.Color_Scheme) return String is
     (case Item is when A.Alloy_Light => "Alloy_Light", when A.Alloy_Dark => "Alloy_Dark");
   function Member (Item : A.Placement) return String is
     (case Item is when A.Fill => "Fill", when A.Fit => "Fit", when A.Center => "Center");
   function Member (Item : A.Solid_Color) return String is
     (case Item is when A.Slate => "Slate", when A.Ocean => "Ocean");

   function Encode (Item : A.Preferences) return String is
     ("(" & DD.APPEARANCE_ROOT_TYPE_NAME & " scheme => " & Scheme_Type & "." & Member (Item.Scheme) &
      " backdrop => (" & Backdrop_Type & "." &
      (case Item.Kind is
         --  A name is a-z, 0-9 and '-' (Valid_Image_Name): nothing to escape.
         when A.Image =>
            Image_Member & " (" & Choice_Type & " name => """ & A.Name (Item.Image) & """ placement => " &
            Placement_Type & "." & Member (Item.Position) & ")",
         when A.Solid =>
            Solid_Member & " " & Solid_Type & "." & Member (Item.Color)) &
      "))");
end Desktop_Appearance_Settings;

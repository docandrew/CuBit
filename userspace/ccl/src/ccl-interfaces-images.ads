with Interfaces;
with CCL.Catalog;
with CCL.Objects;
with CCL.Types;

--  The image interface. An Image is
--  the value (Image width height id): id names immutable pixels in the
--  process's CCL.Image_Store, by content. The operations draw data as
--  images: (image.plot (list 3 1 4 1 5)), (image.heatmap (Grid 16 16 xs)).
--  This package holds the types, the catalog publication and the result a
--  host builds; CCL_Image_Bindings answers the operations.
package CCL.Interfaces.Images with SPARK_Mode is
   use Standard.Interfaces;

   --  The types, as CCL source: checked by the CCL type checker when the
   --  interface is published. The digest and keys are SHA-256 of it (then
   --  "#" and the type's name); tests/ccl-console checks them.
   TYPE_SOURCE : constant String :=
     "(type Image (record (width Integer) (height Integer) (id Integer))) (type Size (record " &
     "(width Integer) (height Integer))) (type Grid (record (width Integer) (height Integer) " &
     "(values (List Integer)))) (type Scaled (record (image Image) (factor Integer)))";
   DIGEST : constant CCL.Catalog.Descriptor_Digest :=
     [16#E376_00BD_F714_2E71#,
      16#92E7_4616_B4BA_9103#,
      16#CD65_970D_3D3A_A020#,
      16#1067_9B42_5B87_81FF#];
   IMAGE_KEY : constant CCL.Objects.Schema_Key :=
     [16#E25A_4BB4_BA46_4473#,
      16#5383_7773_5D08_79BE#,
      16#927A_05F9_12B7_1BA7#,
      16#93ED_A041_51B0_7E79#];
   SIZE_KEY : constant CCL.Objects.Schema_Key :=
     [16#8779_EE46_E9BF_D24C#,
      16#11B2_5306_C238_6546#,
      16#1F2B_F7BC_9B9E_0C3C#,
      16#07FE_0FC7_071F_46F2#];
   SERIES_KEY : constant CCL.Objects.Schema_Key :=
     [16#07D8_AB71_A544_54A7#,
      16#AEE7_0D9E_1E2E_4040#,
      16#9BA6_6211_AEB3_05D2#,
      16#6C9B_5842_30C2_984E#];
   GRID_KEY : constant CCL.Objects.Schema_Key :=
     [16#7A7F_EE6E_F733_2181#,
      16#C795_EB62_DDE7_9103#,
      16#2A17_0F1C_0C25_061F#,
      16#1A87_A6C6_EB5E_5623#];

   IMAGES_KEY : constant CCL.Objects.Schema_Key :=
     [16#51CA_D6F8_66A8_0702#,
      16#87C0_E77B_6635_04AE#,
      16#9920_18F6_95F7_2879#,
      16#EFCF_ECC2_B1B2_48F4#];
   SCALED_KEY : constant CCL.Objects.Schema_Key :=
     [16#26B0_0949_40A0_0462#,
      16#768A_F2CD_5758_CF9D#,
      16#771C_057C_8C28_0A3A#,
      16#DD7F_7DBF_ED31_5D36#];

   --  Load reads a QOI or PPM file from the host's authorized workspace;
   --  the others draw their argument.
   --  Stack and Beside join images (top to bottom, left to right); Scale
   --  enlarges one by a whole factor. Images stay immutable: each makes a
   --  new image.
   type Operation is (Plot, Bars, Heatmap, Pixels, Gradient, Stack, Beside, Scale, Load);
   subtype Drawing is Operation range Plot .. Scale;
   function Name (Item : Operation) return String is
     (case Item is
         when Plot => "plot", when Bars => "bars", when Heatmap => "heatmap",
         when Pixels => "pixels", when Gradient => "gradient", when Stack => "stack",
         when Beside => "beside", when Scale => "scale", when Load => "load");
   type Argument_Shape is
     (Series_Argument, Grid_Argument, Size_Argument, Images_Argument, Scaled_Argument,
      Name_Argument);
   function Shape_Of (Item : Operation) return Argument_Shape is
     (case Item is
         when Plot | Bars => Series_Argument,
         when Heatmap | Pixels => Grid_Argument,
         when Gradient => Size_Argument,
         when Stack | Beside => Images_Argument,
         when Scale => Scaled_Argument,
         when Load => Name_Argument);
   --  Scale's largest factor.
   MAXIMUM_SCALE : constant := 16;
   --  The runtime binding of each operation, the same in every host.
   FIRST_BINDING : constant Unsigned_32 := 16#0006_0001#;
   function Binding_Of (Item : Operation) return Unsigned_32 is
     (FIRST_BINDING + Operation'Pos (Item));
   --  A file name in (image.load "name.qoi").
   MAX_FILE_NAME : constant := 64;

   --  Image, Size, Series (List<Integer>), Grid, Images (List<Image>) and
   --  Scaled in Types, each bound to its key.
   type Contracts is record
      Image, Size, Series, Grid, Images, Scaled : CCL.Objects.Binding;
   end record;
   procedure Define_Types
     (Types : in out CCL.Types.Registry; Bound : out Contracts; Accepted : out Boolean);

   --  The schemas and the image interface, for discovery only: a host
   --  separately installs a binding for the operations it answers.
   procedure Publish
     (Item : in out CCL.Catalog.Interface_Catalog; Error : out CCL.Catalog.Catalog_Error);

   --  The Image value for stored pixels.
   procedure Image_Value
     (Contract : CCL.Objects.Binding; Width, Height : Natural; Id : Integer_64;
      Result : out CCL.Objects.Image; Built : out Boolean);
end CCL.Interfaces.Images;

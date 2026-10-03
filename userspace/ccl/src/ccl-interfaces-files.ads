with Interfaces;
with CCL.Catalog;
with CCL.Objects;
with CCL.Types;

--  Places and their files (docs/ccl-places.md).
--  A Place names a directory inside a scope the process's manifest grants
--  (its root) and a path below it; it grants nothing by itself, and the
--  filesystem service checks the scope on every request.
package CCL.Interfaces.Files with SPARK_Mode is
   use Standard.Interfaces;

   --  The types, as CCL source: checked by the CCL type checker when the
   --  interface is published. The digest and keys are SHA-256 of it (then
   --  "#" and the type's name); tests/ccl-console checks them.
   TYPE_SOURCE : constant String :=
     "(type File_Kind (enum File Directory Link Other)) (type Bytes (range 0 " &
     "9223372036854775807)) (type Timestamp (range 0 9223372036854775807)) (type " &
     "UNIX_File_Permissions (range 0 65535)) (type Place (record (root String) (path " &
     "String))) (type Child (record (place Place) (name String))) (type File_Metadata " &
     "(record (name String) (kind File_Kind) (size Bytes) (modified Timestamp) (mode " &
     "UNIX_File_Permissions) (links Integer)))";
   DIGEST : constant CCL.Catalog.Descriptor_Digest :=
     [16#9BCA_94D2_FCCA_1D1F#,
      16#0583_72BE_00CC_B154#,
      16#4A21_48B7_20F6_AABF#,
      16#E9BD_C0F3_63ED_0DCF#];
   KIND_KEY : constant CCL.Objects.Schema_Key :=
     [16#FE4D_E07A_E250_6E67#,
      16#1EFB_D73F_2A7C_1860#,
      16#840A_23E3_D146_1019#,
      16#EA2D_D279_3C83_3A9D#];
   PLACE_KEY : constant CCL.Objects.Schema_Key :=
     [16#356D_AE37_BF46_A730#,
      16#298F_A719_AD98_D599#,
      16#AC6F_4FA0_8EBD_0D3E#,
      16#E73C_AB7D_8E73_15E6#];
   CHILD_KEY : constant CCL.Objects.Schema_Key :=
     [16#6712_7D91_4C07_6FAA#,
      16#8583_9FAE_37C7_7123#,
      16#1DE0_A6D2_EA9A_8B4E#,
      16#FB74_5FE3_D615_F1DC#];
   METADATA_KEY : constant CCL.Objects.Schema_Key :=
     [16#3440_61FB_7EB1_F18C#,
      16#F2EA_66F0_3693_0029#,
      16#FC21_B501_CC78_238E#,
      16#FA3B_006E_1B33_1F8A#];
   LISTING_KEY : constant CCL.Objects.Schema_Key :=
     [16#E525_4CC7_C7FC_8336#,
      16#9C9A_1A74_F34E_7D20#,
      16#3BB3_9C5B_A07F_AF54#,
      16#0058_EE10_553D_76B5#];

   type File_Kind is (File, Directory, Link, Other);
   PLACE_FIELDS : constant := 2;
   CHILD_FIELDS : constant := 2;
   METADATA_FIELDS : constant := 6;
   --  A File_Metadata takes its product cell, its name, two cells of kind
   --  and four Integers; a listing's own count cell comes first. So one
   --  result image (CCL.Objects.Maximum_Cells) lists at most this many.
   METADATA_CELLS : constant := 1 + 1 + 2 + 4;
   MAXIMUM_LISTED : constant := (CCL.Objects.Maximum_Cells - 1) / METADATA_CELLS;
   --  UNIX_File_Permissions: the POSIX type and permission bits an
   --  ext-family volume stores (16 of them). Named for what they are: a
   --  native CuBit filesystem will have its own, richer model.
   MAXIMUM_MODE : constant := 16#FFFF#;
   --  A place's root and path, and a name, as text.
   MAXIMUM_PATH : constant := 255;
   MAXIMUM_NAME : constant := 255;

   type Operation is (Home, Enter, Up, List);
   function Name (Item : Operation) return String is
     (case Item is
         when Home => "home", when Enter => "enter", when Up => "up", when List => "list");
   FIRST_BINDING : constant Unsigned_32 := 16#0008_0001#;
   function Binding_Of (Item : Operation) return Unsigned_32 is
     (FIRST_BINDING + Operation'Pos (Item));

   type Contracts is record
      Kind, Place, Child, Metadata, Listing : CCL.Objects.Binding;
   end record;
   procedure Define_Types
     (Types : in out CCL.Types.Registry; Bound : out Contracts; Accepted : out Boolean);
   procedure Publish
     (Item : in out CCL.Catalog.Interface_Catalog; Error : out CCL.Catalog.Catalog_Error);
end CCL.Interfaces.Files;

with Interfaces;
with CCL.Binding_Blocks;
with CCL.Catalog;
with CCL.Objects;
with CCL.Types;

--  Places and their files (docs/ccl-places.md).
--  A Place names a directory inside a scope the process's manifest grants
--  (its root) and a path below it; it grants nothing by itself, and the
--  filesystem service checks the scope on every request.
package CCL.Interfaces.Files with SPARK_Mode is

   --  What Publish adds to a catalog: one interface and this many data
   --  schemas. CCL_Host_Environment sums these to check the catalog's
   --  capacity at compile time; tests/ccl-install's probe checks a real run.
   PUBLISHED_INTERFACES : constant := 1;
   PUBLISHED_SCHEMAS : constant := 7;
   --  Declared types Publish adds to the catalog's registry (its named
   --  types and the list types it specializes).
   PUBLISHED_TYPES : constant := 10;
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
     "UNIX_File_Permissions) (links Integer))) (type Excerpt_Length (range 1 8192)) " &
     "(type Excerpt (record (child Child) (offset Bytes) (length Excerpt_Length)))";
   DIGEST : constant CCL.Catalog.Descriptor_Digest :=
     [16#F764_23DE_B011_9804#,
      16#8758_5D61_9A25_7B83#,
      16#DB50_CC5B_5415_E1D5#,
      16#5913_E3EA_CA7D_B0E2#];
   KIND_KEY : constant CCL.Objects.Schema_Key :=
     [16#B9E5_A6C7_92CB_187D#,
      16#02EF_E470_AAE7_82CC#,
      16#A84A_0268_04B4_9537#,
      16#1CA1_4213_A8D0_1280#];
   PLACE_KEY : constant CCL.Objects.Schema_Key :=
     [16#417C_F7D6_0FE2_98A8#,
      16#4E88_C4F5_78F5_0B85#,
      16#9851_ACC8_2011_FF0E#,
      16#98A5_8967_F907_53A5#];
   CHILD_KEY : constant CCL.Objects.Schema_Key :=
     [16#25AA_A9E4_2D75_681A#,
      16#46B8_88C5_E20F_28AB#,
      16#8D26_609B_EDC4_1F6A#,
      16#8BE1_EEBA_18CD_138D#];
   METADATA_KEY : constant CCL.Objects.Schema_Key :=
     [16#2DC9_8992_FB8A_969A#,
      16#2BB8_74C4_C2EF_F10A#,
      16#3D5A_1E34_064E_7CA5#,
      16#EC98_1270_653D_2CBF#];
   LISTING_KEY : constant CCL.Objects.Schema_Key :=
     [16#9774_2CDD_3B3F_78EC#,
      16#1C94_4900_AC95_7E79#,
      16#D429_BF69_8019_0D3C#,
      16#D768_7964_E989_1E35#];

   --  fs.read's argument (a span of a file) and its result (the text).
   EXCERPT_KEY : constant CCL.Objects.Schema_Key :=
     [16#6D41_DA29_B6A1_98FE#,
      16#0F1B_8E59_4465_CB4B#,
      16#936F_04AE_419B_B143#,
      16#EED1_7C24_4F47_7D31#];
   TEXT_KEY : constant CCL.Objects.Schema_Key :=
     [16#301D_552D_EE34_EDAE#,
      16#51BA_3AD8_8AD5_CEE6#,
      16#826E_CC3B_81E1_E9C3#,
      16#B1FF_7CE9_3ADD_BF9B#];

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

   --  (fs.read (Excerpt child => c offset => 0 length => 4096)): at most
   --  length bytes of the file from offset (fewer at its end), as text.
   MAXIMUM_EXCERPT : constant := 8_192;
   EXCERPT_FIELDS : constant := 3;

   type Operation is (Home, Enter, Up, List, Read);
   function Name (Item : Operation) return String is
     (case Item is
         when Home => "home", when Enter => "enter", when Up => "up", when List => "list",
         when Read => "read");
   FIRST_BINDING : constant Unsigned_32 := CCL.Binding_Blocks.First (CCL.Binding_Blocks.Files);
   function Binding_Of (Item : Operation) return Unsigned_32 is
     (FIRST_BINDING + Operation'Pos (Item));

   type Contracts is record
      Kind, Place, Child, Metadata, Listing, Excerpt, Text : CCL.Objects.Binding;
   end record;
   procedure Define_Types
     (Types : in out CCL.Types.Registry; Bound : out Contracts; Accepted : out Boolean);
   procedure Publish
     (Item : in out CCL.Catalog.Interface_Catalog; Error : out CCL.Catalog.Catalog_Error);
end CCL.Interfaces.Files;

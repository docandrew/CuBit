------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  archive.*: unpack an archive into a granted place (SELF-003 step 1).
--    (archive.unpack (Unpack_Request
--       from => (Place root => "@system" path => "work") name => "a.tar"
--       into => (Place root => "@system" path => "work") directory => "a"
--       format => Archive_Format.Tar specials => Archive_Specials.Refuse
--       max_entries => 100000 max_bytes => 1073741824))
--                     Unpack_Reply: the archive's files and directories,
--                     made inside a new directory "a" in into, or a typed
--                     refusal (the reader's reason and the entry it named).
--  The reader (userspace/lib/archive, proved) confines every path below
--  the new directory and refuses links, devices and FIFOs unless specials
--  => Skip. The whole tree is written under a temporary name and renamed
--  into place last; a refusal or a failed write removes what was made.
--  The types are CCL source; the digest and keys are SHA-256 of it (then
--  "#" and the type's name), and tests/ccl-console checks them.
------------------------------------------------------------------------------
with Interfaces;
with CCL.Binding_Blocks;
with CCL.Catalog;
with CCL.Objects;

package CCL.Interfaces.Archive with SPARK_Mode is

   PUBLISHED_INTERFACES : constant := 1;
   PUBLISHED_SCHEMAS : constant := 2;
   --  Place is Files' own declaration again (identical, so not a new type).
   PUBLISHED_TYPES : constant := 5;
   use Standard.Interfaces;

   TYPE_SOURCE : constant String :=
     "(type Place (record (root String) (path String))) (type Archive_Format (enum " &
     "Tar)) (type Archive_Specials (enum Refuse Skip)) (type Unpack_Request (record " &
     "(from Place) (name String) (into Place) (directory String) (format " &
     "Archive_Format) (specials Archive_Specials) (max_entries Integer) (max_bytes " &
     "Integer))) (type Unpack_Outcome (enum Unpacked Not_Granted Not_Found Exists " &
     "Refused Too_Large Write_Failed)) (type Unpack_Reply (record (outcome " &
     "Unpack_Outcome) (entries Integer) (bytes Integer) (skipped Integer) (reason " &
     "String) (entry String) (detail String)))";
   DIGEST : constant CCL.Catalog.Descriptor_Digest :=
     [16#2BF7_6FB8_63AD_04EF#,
      16#9A55_DE72_FD68_C066#,
      16#E6DF_F533_1267_76F2#,
      16#E6E1_3AF7_4B6D_8491#];
   REQUEST_KEY : constant CCL.Objects.Schema_Key :=
     [16#7CB4_926C_6990_CEB8#,
      16#FD10_B225_6D27_3A6D#,
      16#3805_E7BF_827F_AB0B#,
      16#3A55_BAB9_DAE9_0CD1#];
   REPLY_KEY : constant CCL.Objects.Schema_Key :=
     [16#017E_FF4A_07FE_C725#,
      16#3137_E7B5_23B5_B72B#,
      16#9256_A2BA_868D_AAA9#,
      16#3CDB_935B_A7B3_A357#];

   --  The limits' bounds, checked by the host.
   MAXIMUM_ENTRIES : constant := 10_000_000;
   MAXIMUM_BYTES : constant := 1_099_511_627_776;

   REQUEST_FROM : constant := 1;
   REQUEST_NAME : constant := 2;
   REQUEST_INTO : constant := 3;
   REQUEST_DIRECTORY : constant := 4;
   REQUEST_FORMAT : constant := 5;
   REQUEST_SPECIALS : constant := 6;
   REQUEST_MAX_ENTRIES : constant := 7;
   REQUEST_MAX_BYTES : constant := 8;
   PLACE_ROOT : constant := 1;
   PLACE_PATH : constant := 2;
   REPLY_FIELDS : constant := 7;
   SPECIALS_REFUSE : constant := 1;
   SPECIALS_SKIP : constant := 2;

   type Operation is (Unpack);
   function Name (Item : Operation) return String is (case Item is when Unpack => "unpack");
   FIRST_BINDING : constant Unsigned_32 := CCL.Binding_Blocks.First (CCL.Binding_Blocks.Archive);
   function Binding_Of (Item : Operation) return Unsigned_32 is
     (FIRST_BINDING + Operation'Pos (Item));

   type Contracts is record
      Request, Reply : CCL.Objects.Binding;
   end record;

   procedure Publish
     (Item : in out CCL.Catalog.Interface_Catalog; Error : out CCL.Catalog.Catalog_Error);
end CCL.Interfaces.Archive;

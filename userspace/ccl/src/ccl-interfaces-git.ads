------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  git.*: clone a repository over HTTPS into a granted place (SELF-003
--  step 2), git's smart HTTP protocol version 2, SHA-1 or SHA-256.
--    (git.clone (Clone_Request url => "https://example.com/team/repo.git"
--       into => (Place root => "@system" path => "work") directory => "repo"
--       max_refs => 10000 max_objects => 1000000 max_bytes => 1073741824
--       max_redirects => 5 deadline_ms => 600000))
--                     Git_Clone: the clone, started at once. Every limit is
--                     spelled out (no defaults) and a range type.
--    (git.reply c)    Task<Git_Reply>: how it ended (a typed outcome; reason
--                     names the exact cause), the refs, objects and files,
--                     the object format, the URL after redirects.
--    (git.status c)   Git_Reply now (outcome Running while it runs).
--    (git.cancel c)   stops it (Git_Outcome.Cancelled).
--  The clone is made under ".DIRECTORY.cloning" beside the target and
--  renamed to DIRECTORY only when every object checked out: a clone that
--  fails leaves nothing. The result is an ordinary git repository (git's
--  tools open it). The filesystem service checks the place against the
--  program's filesystem scope on every request, and tls.svc every host,
--  redirects included, against its TLS scope. The types are CCL source; the
--  digest and keys are SHA-256 of it (then "#" and the type's name), and
--  tests/ccl-console checks them.
------------------------------------------------------------------------------
with Interfaces;
with CCL.Binding_Blocks;
with CCL.Catalog;
with CCL.Objects;

package CCL.Interfaces.Git with SPARK_Mode is

   PUBLISHED_INTERFACES : constant := 1;
   PUBLISHED_SCHEMAS : constant := 3;
   --  Place is Files' own declaration again (identical, so not a new type).
   PUBLISHED_TYPES : constant := 10;
   use Standard.Interfaces;

   TYPE_SOURCE : constant String :=
     "(type Place (record (root String) (path String))) (type Git_Ref_Limit (range 1" &
     " 1000000)) (type Git_Object_Limit (range 1 100000000)) (type Git_Byte_Limit " &
     "(range 1 1099511627776)) (type Git_Redirect_Limit (range 0 10)) (type " &
     "Git_Deadline (range 1 86400000)) (type Clone_Request (record (url String) " &
     "(into Place) (directory String) (max_refs Git_Ref_Limit) (max_objects " &
     "Git_Object_Limit) (max_bytes Git_Byte_Limit) (max_redirects " &
     "Git_Redirect_Limit) (deadline_ms Git_Deadline))) (type Git_Clone (record (id " &
     "Integer))) (type Git_Outcome (enum Running Cloned Not_Granted Exists Bad_Url " &
     "Too_Many_Redirects Connect_Failed Tls_Failed Http_Status Bad_Response Deadline" &
     " Cancelled Write_Failed Refused Too_Large Empty)) (type Git_Format (enum Sha1 " &
     "Sha256)) (type Git_Reply (record (outcome Git_Outcome) (reason String) (status" &
     " Integer) (refs Integer) (objects Integer) (files Integer) (bytes Integer) " &
     "(format Git_Format) (redirects Integer) (url String) (detail String)))";
   DIGEST : constant CCL.Catalog.Descriptor_Digest :=
     [16#7F12_FA6B_61E0_F930#,
      16#1040_5FA2_1272_B49C#,
      16#1294_5B19_8193_3BD1#,
      16#0969_01F2_5F54_1962#];
   REQUEST_KEY : constant CCL.Objects.Schema_Key :=
     [16#2DCF_B228_CA09_DF83#,
      16#0368_1670_4DB8_B0E3#,
      16#B6CC_9749_A8BC_8E65#,
      16#CD9F_136F_8773_1FD9#];
   CLONE_KEY : constant CCL.Objects.Schema_Key :=
     [16#CBF4_C2ED_C526_F930#,
      16#BE9F_ACF4_4F3F_74C5#,
      16#34B9_ECAE_2815_AB55#,
      16#7CE2_0E94_9C43_6E9D#];
   REPLY_KEY : constant CCL.Objects.Schema_Key :=
     [16#622E_5269_6FFA_FF08#,
      16#E9FD_FC0D_2D3E_C597#,
      16#B9C0_D871_AB73_A219#,
      16#5012_6DA3_99F1_E519#];

   --  The limits' bounds (the range types'), checked again by the host.
   MAXIMUM_REFS : constant := 1_000_000;
   MAXIMUM_OBJECTS : constant := 100_000_000;
   MAXIMUM_BYTES : constant := 1_099_511_627_776;
   MAXIMUM_REDIRECTS : constant := 10;
   MAXIMUM_DEADLINE_MS : constant := 86_400_000;

   --  Fields, in declaration order (1-based, as CCL.Objects.Views numbers them).
   REQUEST_URL : constant := 1;
   REQUEST_INTO : constant := 2;
   REQUEST_DIRECTORY : constant := 3;
   REQUEST_MAX_REFS : constant := 4;
   REQUEST_MAX_OBJECTS : constant := 5;
   REQUEST_MAX_BYTES : constant := 6;
   REQUEST_MAX_REDIRECTS : constant := 7;
   REQUEST_DEADLINE : constant := 8;
   PLACE_ROOT : constant := 1;
   PLACE_PATH : constant := 2;
   CLONE_FIELDS : constant := 1;
   REPLY_FIELDS : constant := 11;

   --  Git_Outcome, in the source's order.
   type Outcome is
     (Running, Cloned, Not_Granted, Exists, Bad_Url, Too_Many_Redirects, Connect_Failed, Tls_Failed,
      Http_Status, Bad_Response, Deadline, Cancelled, Write_Failed, Refused, Too_Large, Empty);
   --  Git_Format, in the source's order.
   type Format is (Sha1, Sha256);

   type Operation is (Clone, Reply, Status, Cancel);
   function Name (Item : Operation) return String is
     (case Item is
         when Clone => "clone", when Reply => "reply", when Status => "status", when Cancel => "cancel");
   FIRST_BINDING : constant Unsigned_32 := CCL.Binding_Blocks.First (CCL.Binding_Blocks.Git);
   function Binding_Of (Item : Operation) return Unsigned_32 is
     (FIRST_BINDING + Operation'Pos (Item));

   type Contracts is record
      Request, Clone, Reply : CCL.Objects.Binding;
   end record;

   procedure Publish
     (Item : in out CCL.Catalog.Interface_Catalog; Error : out CCL.Catalog.Catalog_Error);
end CCL.Interfaces.Git;

------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  net.*: HTTPS downloads into a granted place (SELF-003 step 1).
--    (net.fetch (Fetch_Request url => "https://example.com/a.tar"
--       into => (Place root => "@system" path => "work") name => "a.tar"
--       pin => (Fetch_Pin.Sha256 "<64 hex digits>") existing => Fetch_Existing.Refuse
--       max_bytes => 104857600 max_redirects => 5 deadline_ms => 600000))
--                     Fetch: the download, started at once. Every limit is
--                     spelled out (no defaults); pin => Fetch_Pin.Unpinned
--                     fetches without a pinned SHA-256.
--    (net.reply f)    Task<Fetch_Reply>: how it ended (a typed outcome), the
--                     HTTP status, the bytes and their SHA-256, the URL that
--                     answered after redirects.
--    (net.status f)   Fetch_Reply now (outcome Running while it runs).
--    (net.cancel f)   stops it (Fetch_Outcome.Cancelled).
--  The bytes go to a temporary name beside name and are renamed to it
--  only when the body is whole and its SHA-256 matches the pin, so a fetch
--  that fails leaves nothing. The filesystem service checks the place
--  against the program's filesystem scope on every request, and tls.svc
--  checks every host (redirects included) against its TLS scope. The types
--  are CCL source; the digest and keys are SHA-256 of it (then "#" and the
--  type's name), and tests/ccl-console checks them.
------------------------------------------------------------------------------
with Interfaces;
with CCL.Binding_Blocks;
with CCL.Catalog;
with CCL.Objects;

package CCL.Interfaces.Net with SPARK_Mode is

   PUBLISHED_INTERFACES : constant := 1;
   PUBLISHED_SCHEMAS : constant := 3;
   --  Place is Files' own declaration again (identical, so not a new type).
   PUBLISHED_TYPES : constant := 7;
   use Standard.Interfaces;

   TYPE_SOURCE : constant String :=
     "(type Place (record (root String) (path String))) (type Fetch_Pin (variant " &
     "(Unpinned) (Sha256 String))) (type Fetch_Existing (enum Refuse Replace)) (type " &
     "Fetch_Request (record (url String) (into Place) (name String) (pin Fetch_Pin) " &
     "(existing Fetch_Existing) (max_bytes Integer) (max_redirects Integer) " &
     "(deadline_ms Integer))) (type Fetch (record (id Integer))) (type Fetch_Outcome " &
     "(enum Running Stored Not_Granted Exists Bad_Url Too_Many_Redirects " &
     "Connect_Failed Tls_Failed Http_Status Bad_Response Too_Large Truncated " &
     "Digest_Mismatch Deadline Cancelled Write_Failed)) (type Fetch_Expected " &
     "(variant (Unknown) (Bytes Integer))) (type Fetch_Reply (record (outcome " &
     "Fetch_Outcome) (status Integer) (bytes Integer) (expected Fetch_Expected) " &
     "(redirects Integer) (sha256 String) (url String) (detail String)))";
   DIGEST : constant CCL.Catalog.Descriptor_Digest :=
     [16#92F8_BE96_A8E5_E4DE#,
      16#B4E7_EFEF_E69B_ED33#,
      16#5EA4_6983_020F_852D#,
      16#3622_8C68_A3C5_7001#];
   REQUEST_KEY : constant CCL.Objects.Schema_Key :=
     [16#13D0_63E5_B2F2_E48E#,
      16#BFC4_4BC3_D9E0_54CA#,
      16#D8B4_2814_EA2A_54EB#,
      16#1800_C55E_A5AD_A8E9#];
   FETCH_KEY : constant CCL.Objects.Schema_Key :=
     [16#651B_0278_9BAF_285F#,
      16#7746_AE0C_2679_B344#,
      16#069F_0D12_FBF8_F1A2#,
      16#AE8C_357D_D2C0_E8C8#];
   REPLY_KEY : constant CCL.Objects.Schema_Key :=
     [16#5D90_D208_B07B_99A5#,
      16#718A_68ED_53D7_DCEF#,
      16#FB21_38B7_4315_1F90#,
      16#91D6_6FC7_3CA0_4218#];

   --  The limits' bounds, checked by the host (plain Integers: the type
   --  registry has no room for three more range types).
   MAXIMUM_BYTES : constant := 1_099_511_627_776;
   MAXIMUM_REDIRECTS : constant := 10;
   MAXIMUM_DEADLINE_MS : constant := 86_400_000;

   --  Fields, in declaration order (1-based, as CCL.Objects.Views numbers them).
   REQUEST_URL : constant := 1;
   REQUEST_INTO : constant := 2;
   REQUEST_NAME : constant := 3;
   REQUEST_PIN : constant := 4;
   REQUEST_EXISTING : constant := 5;
   REQUEST_MAX_BYTES : constant := 6;
   REQUEST_MAX_REDIRECTS : constant := 7;
   REQUEST_DEADLINE : constant := 8;
   PLACE_ROOT : constant := 1;
   PLACE_PATH : constant := 2;
   FETCH_FIELDS : constant := 1;
   REPLY_FIELDS : constant := 8;
   --  Alternatives, numbered from 1.
   PIN_UNPINNED : constant := 1;
   PIN_SHA256 : constant := 2;
   EXISTING_REFUSE : constant := 1;
   EXISTING_REPLACE : constant := 2;
   EXPECTED_UNKNOWN : constant := 1;
   EXPECTED_BYTES : constant := 2;

   type Operation is (Fetch, Reply, Status, Cancel);
   function Name (Item : Operation) return String is
     (case Item is
         when Fetch => "fetch", when Reply => "reply", when Status => "status", when Cancel => "cancel");
   FIRST_BINDING : constant Unsigned_32 := CCL.Binding_Blocks.First (CCL.Binding_Blocks.Net);
   function Binding_Of (Item : Operation) return Unsigned_32 is
     (FIRST_BINDING + Operation'Pos (Item));

   type Contracts is record
      Request, Fetch, Reply : CCL.Objects.Binding;
   end record;

   procedure Publish
     (Item : in out CCL.Catalog.Interface_Catalog; Error : out CCL.Catalog.Catalog_Error);
end CCL.Interfaces.Net;

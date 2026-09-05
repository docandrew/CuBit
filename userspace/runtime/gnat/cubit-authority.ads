------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary Stable launch-authority provenance identifiers
--
--  These identifiers describe why procmgr requested or installed authority.
--  A future inspection protocol will expose authenticated, bounded snapshots;
--  these constants are not themselves an inspection or enforcement API.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package CuBit.Authority with SPARK_Mode => On is

   AUTH_SOURCE_MANIFEST         : constant Unsigned_8 := 1;
   AUTH_SOURCE_KERNEL_BOOTSTRAP : constant Unsigned_8 := 2;
   AUTH_SOURCE_COMPATIBILITY    : constant Unsigned_8 := 3;
   AUTH_SOURCE_IDENTITY_POLICY  : constant Unsigned_8 := 4;
   AUTH_SOURCE_CONFIG_POLICY    : constant Unsigned_8 := 5;

   AUTH_REASON_MANIFEST_REQUEST : constant Unsigned_8 := 1;
   AUTH_REASON_SELF_BOOTSTRAP   : constant Unsigned_8 := 2;
   AUTH_REASON_FS_BOOTSTRAP     : constant Unsigned_8 := 3;
   AUTH_REASON_INPUT_COMPAT     : constant Unsigned_8 := 4;
   AUTH_REASON_PROCESS_COMPAT   : constant Unsigned_8 := 5;
   AUTH_REASON_PACKAGE_ID       : constant Unsigned_8 := 6;
   AUTH_REASON_SERVICE_MISSING  : constant Unsigned_8 := 7;
   AUTH_REASON_MINT_FAILED      : constant Unsigned_8 := 8;
   AUTH_REASON_CONFIG_QUOTA     : constant Unsigned_8 := 9;

end CuBit.Authority;

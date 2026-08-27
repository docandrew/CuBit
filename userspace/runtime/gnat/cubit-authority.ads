------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary Bounded explainable-authority inspection protocol
--
--  Launch provenance is reported by procmgr.  Effective capability state is
--  independently read from the kernel; consumers must not treat this protocol
--  as an enforcement oracle.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package CuBit.Authority with SPARK_Mode => On is

   OP_AUTHORITY_QUERY : constant Unsigned_32 := 16#0102#;

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

   AUTH_FLAG_REQUESTED : constant Unsigned_8 := 1;
   AUTH_FLAG_GRANTED   : constant Unsigned_8 := 2;

   --  OP_AUTHORITY_QUERY request:
   --    word 0: target PID
   --    word 1: capability slot
   --
   --  REPLY_OK response:
   --    word 0: authority_id[31:0], slot[39:32], source[47:40],
   --            reason[55:48], flags[63:56]
   --    word 1: capability_type[7:0], rights[15:8]
   --    word 2: object reference
   --    word 3: object parameter

end CuBit.Authority;

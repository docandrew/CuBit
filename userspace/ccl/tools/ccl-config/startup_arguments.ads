------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The image build's check of a startup plan's typed program arguments
--  (docs/ccl-launch-parameters.md): ccl-config PROFILE --programs DIR.
--
--  For each (start "x" ...) whose program file DIR/x exists, the program's
--  .cubit.parameters descriptor (CuBit.Parameter_Signatures) is read from
--  its ELF and validated, its identity is checked against an independent
--  SHA-256 of its signature (SPARKTLSCrypto), and the entry's
--  (arguments ...) is checked against it with the launcher's own check
--  (CCL.Typed_Values), as procmgr does at launch. A program that declares
--  parameters needs arguments, one that declares none takes none, and an
--  entry with arguments whose program is not in DIR cannot be checked.
--  Each refusal names the profile's line and column. Linux-hosted.
------------------------------------------------------------------------------
pragma Ada_2022;
with CCL.Configurations;

package Startup_Arguments is
   procedure Check
     (Profile_Name, Source : String; Plan : CCL.Configurations.Configuration_Plan;
      Directory : String; Success : out Boolean);
end Startup_Arguments;

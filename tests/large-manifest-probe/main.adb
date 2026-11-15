------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  large-manifest-probe (tests/large-manifest-probe, headless test
--  large-manifest): a program whose typed manifest is well past the CCL
--  limits the manifest compiler had (tests/ccl-manifest-capacity). That it
--  runs at all is the test: procmgr loaded its sections and launched it.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Messages; use CuBit.Messages;
with Startup;
pragma Unreferenced (Startup);

procedure Main is
   use ASCII;
begin
   debugPrint ("large-manifest-probe: started with its large manifest PASS" & LF);
end Main;

-------------------------------------------------------------------------------
-- CuBit OS
-- Copyright (C) 2026 Jon Andrew
-- SPDX-License-Identifier: GPL-3.0-or-later
-------------------------------------------------------------------------------
package body Build_Signing is

   procedure Sign (Directory, Input, Output : String) is
      pragma Unreferenced (Directory, Input, Output);
   begin
      raise Signing_Unavailable with
        "ccl-build.app holds no signing key; on CuBit, secrets.svc will sign (SIGN-001 slice 5)";
   end Sign;

end Build_Signing;

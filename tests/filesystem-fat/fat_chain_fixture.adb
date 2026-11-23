------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
package body Fat_Chain_Fixture is
   procedure Read (C : Data_Cluster; Raw : out Unsigned_32; OK : out Boolean) is
   begin
      OK := C /= Fail_At and then C in Table'Range;
      Raw := (if OK then Table (C) else 0);
   end Read;
end Fat_Chain_Fixture;

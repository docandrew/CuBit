------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
------------------------------------------------------------------------------
with Fat_Chain_Fixture;

package body Fat_Chain_Instances with SPARK_Mode => Off is
   procedure Read_Entry (C : Data_Cluster; Raw : out Unsigned_32; OK : out Boolean) is
   begin
      Fat_Chain_Fixture.Read (C, Raw, OK);
   end Read_Entry;
end Fat_Chain_Instances;

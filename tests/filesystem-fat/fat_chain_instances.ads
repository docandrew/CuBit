------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  The chain walks and the free search over a FAT read through an opaque
--  procedure: proving these instances proves the generic bodies for any
--  Read_Entry (the driver's reads the device), since the provers know
--  nothing of the entries it returns. The hosted tests read a FAT held in
--  an array (Set_Fat).
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with Fat_Format; use Fat_Format;
with Fat_Chains;

package Fat_Chain_Instances with SPARK_Mode is
   procedure Read_Entry (C : Data_Cluster; Raw : out Unsigned_32; OK : out Boolean)
     with Global => null;
   package Walks is new Fat_Chains.Walks (Read_Entry);
   package Searches is new Fat_Chains.Searches (Read_Entry);
end Fat_Chain_Instances;

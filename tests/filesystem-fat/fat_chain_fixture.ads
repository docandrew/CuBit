------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  The hosted tests' FAT: an array of raw entries, and reads that fail
--  from a chosen cluster on.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with Fat_Format; use Fat_Format;

package Fat_Chain_Fixture is
   type Entries is array (Cluster_Number range <>) of Unsigned_32;
   type Entries_Access is access Entries;
   Table : Entries_Access;
   Fail_At : Cluster_Number := Cluster_Number'Last;
   procedure Read (C : Data_Cluster; Raw : out Unsigned_32; OK : out Boolean);
end Fat_Chain_Fixture;

------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  `logs.retention`, a typed CCL Log_Retention (CCL.Interfaces.Log_Retention),
--  for example (Log_Retention per_source => 4096 boot => 256 total => 16384):
--  the ceilings of logstore's retained history (Log_Fanout.Ceilings). The
--  setting catalog's codec (CAP-003). Decode refuses a value whose boot
--  window is not below its per-source ceiling, or whose per-source ceiling
--  is above the total (Log_Fanout.Consistent): the CCL type bounds each
--  field, not how they relate.
------------------------------------------------------------------------------
with Log_Fanout;

package Logstore_Retention_Settings is
   subtype Value is Log_Fanout.Ceilings;
   procedure Decode (Source : String; Item : out Value; Success : out Boolean);
end Logstore_Retention_Settings;

------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  ccl-remote's lines ("started", "a host paired", refusals: never the
--  pairing code, the token or contents), one Information record each in
--  its logstore ring (ABI-002: its debug text is routed there, and the
--  record is marked as console text, so test builds mirror it once).
------------------------------------------------------------------------------
package Remote_Log is
   procedure Note (Text : String);
end Remote_Log;

------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Shared system UI theme colors
------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package CuBit.Theme is
   subtype Color is Unsigned_32;

   Desktop : constant Color := 16#0025_2A2E#;
   Panel   : constant Color := 16#00E7_EAED#;
   Face    : constant Color := 16#00F7_F8FA#;
   Edge    : constant Color := 16#00C4_CBD3#;
   Shadow  : constant Color := 16#008F_98A3#;
   Text    : constant Color := 16#001E_252B#;
   Muted   : constant Color := 16#0068_727D#;
   Accent  : constant Color := 16#002F_7D8C#;
   Good    : constant Color := 16#002F_875B#;
   Danger  : constant Color := 16#00C9_4D42#;
   White   : constant Color := 16#00FF_FFFF#;
   Black   : constant Color := 16#0000_0000#;
end CuBit.Theme;

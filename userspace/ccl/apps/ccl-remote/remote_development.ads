------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The DEVELOPMENT-ONLY authority this endpoint holds so that app replace
--  can swap in any shipped app (SELF-002): what its manifest's marked
--  block asks for, and whether the startup plan granted each. Named in
--  the startup log line and in (hello), so it is never held unseen.
------------------------------------------------------------------------------
pragma Ada_2022;

package Remote_Development is

   --  "development-only authority for app replace: services log-control,
   --  lexicon (absent), ...; outlet com.cubit-os.stdmetric; scopes ...".
   --  An entry is "(absent)" when its provider was not running at start
   --  (it is then not passed on to a swapped app either).
   function Summary return String;

end Remote_Development;

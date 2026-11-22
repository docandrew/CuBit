------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The Bible app's remembered choices, as other apps remember theirs: typed
--  settings in Config (userspace/ccl/interfaces/settings/applications.ccl),
--  bible.reading-face and bible.translation, the manifest's "bible." scope.
------------------------------------------------------------------------------
with Bible_Translations;

package Bible_Preferences is
   function Saved (Item : Bible_Translations.Reader_Preference) return String;
   procedure Save (Item : Bible_Translations.Reader_Preference; Value : String);
end Bible_Preferences;

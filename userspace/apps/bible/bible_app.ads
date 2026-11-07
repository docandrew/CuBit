------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The native Bible app (APP-005): this view (Bible_Views) over the
--  cubit-bible asset package (Bible_Loading), set in the reading faces the
--  program embeds and offers (Literata, Source Serif 4, EB Garamond:
--  ccl-build's pinned `embedded` blobs) through CuBit.Reading_Faces.
------------------------------------------------------------------------------
with Bible_Loading;
with Bible_Preferences;
with Bible_Views;

package Bible_App is
   package Views is new Bible_Views (Bible_Loading.Load, Bible_Preferences.Saved, Bible_Preferences.Save);

   --  Open the window at John 1 and run until it closes.
   procedure Run;
end Bible_App;

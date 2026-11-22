with Bible_Translations;

--  Where a CCL host's Bible images come from (APP-005): natively the
--  cubit-bible asset package through the program's filesystem grant
--  (native/, Bible_Asset_Files), on Linux the files in the directory named
--  by CCL_BIBLE_DIR (tools/ccl-ui-preview/host/). Each translation is read
--  once, when a call first needs it; Bible_Translations.Open checks it.
package CCL_Bible_Source is

   procedure Load (Which : Bible_Translations.Translation; Data : out Bible_Translations.Image_Access;
                   Status : out Bible_Translations.Load_Status);

   --  What would make an unavailable translation available.
   function Unavailable_Remedy return String;

end CCL_Bible_Source;

------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Reading a Bible image natively (APP-005): the asset
--  <system.assets.root>cubit-bible/1/<translation>.bible, read once when
--  the translation is first wanted, through the filesystem's request queue
--  (submit, then wait for the completion, one explicit deadline for the
--  whole read) into one heap block sized from the image's own header.
--  Bible_Translations.Open then opens and indexes it in place.
--
--  Generic over the program's grants: the Bible app instantiates it with
--  its Startup bindings, the CCL hosts with theirs (CCL_Native_Bindings).
------------------------------------------------------------------------------
with CuBit.Handles;
with Bible_Translations;

generic
   --  The program's "filesystem" endpoint.
   with function Filesystem return CuBit.Handles.Endpoint;
   --  The system.assets.root setting's text ("" when it is absent).
   with function Asset_Root return String;
package Bible_Asset_Files is

   --  The whole read of one translation (about 9 MB).
   LOAD_BUDGET_MS : constant := 4_000;

   procedure Load (Which : Bible_Translations.Translation; Data : out Bible_Translations.Image_Access;
                   Status : out Bible_Translations.Load_Status);

   --  The last load's size and time, for the log.
   function Last_Bytes return Natural;
   function Last_Milliseconds return Natural;

end Bible_Asset_Files;

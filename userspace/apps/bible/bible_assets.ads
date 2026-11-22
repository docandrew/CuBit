------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The Bible app's grants for reading the cubit-bible asset package: its
--  "filesystem" endpoint and the system.assets.root setting (manifest.ccl).
------------------------------------------------------------------------------
with CuBit.Handles;

package Bible_Assets is
   function Filesystem return CuBit.Handles.Endpoint;
   function Asset_Root return String;
end Bible_Assets;

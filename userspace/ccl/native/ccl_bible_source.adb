--  Native: the cubit-bible asset package (docs/assets.md layout) through the
--  program's "filesystem" grant, at the system.assets.root setting read
--  through its "config" grant (the console reads all Config).
with CuBit.Handles;
with CuBit.Handles.Services;
with CuBit.Settings;
with CuBit.Settings.Codecs;
with CuBit.Settings.Readable;
with Bible_Asset_Files;
with CCL_Native_Bindings;

package body CCL_Bible_Source is

   function Config return CuBit.Handles.Services.Config.Endpoint is
     (CuBit.Handles.Services.Config.Granted (CCL_Native_Bindings.Config));
   package Root_Setting is new CuBit.Settings.Readable
     (CuBit.Settings.Codecs.Text_Codec.Value, "system.assets.root", Config,
      CuBit.Settings.Codecs.Text_Codec.Decode);

   function Filesystem return CuBit.Handles.Endpoint is (CCL_Native_Bindings.Filesystem);
   function Asset_Root return String is
      use all type CuBit.Settings.Outcome;
      Stored : constant Root_Setting.Reading := Root_Setting.Read;
   begin
      return (if Stored.Kind = Present then CuBit.Settings.Image (Stored.Item) else "");
   end Asset_Root;

   package Files is new Bible_Asset_Files (Filesystem, Asset_Root);

   procedure Load (Which : Bible_Translations.Translation; Data : out Bible_Translations.Image_Access;
                   Status : out Bible_Translations.Load_Status) renames Files.Load;

   function Unavailable_Remedy return String is
     ("install the cubit-bible asset package (Assets/cubit-bible/1/) and give the program read access to it " &
      "(filesystem-scope @nvme:0/Assets/cubit-bible/)");

end CCL_Bible_Source;

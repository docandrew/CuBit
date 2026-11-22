with CuBit.Settings;
with Startup;

package body Bible_Assets is
   function Filesystem return CuBit.Handles.Endpoint is (Startup.Filesystem);

   function Asset_Root return String is
      use all type CuBit.Settings.Outcome;
      Stored : constant Startup.Settings.System_Assets_Root.Reading := Startup.Settings.System_Assets_Root.Read;
   begin
      return (if Stored.Kind = Present then CuBit.Settings.Image (Stored.Item) else "");
   end Asset_Root;
end Bible_Assets;

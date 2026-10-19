--  Desktop's application icons (docs/assets.md): the atlas file
--  applications.qoi in the read-only package Assets/cubit-icons/<version>/,
--  read once at startup before the first frame (Desktop_Asset_Files) and
--  decoded into Desktop_Icon_Store (every icon at every exported edge). A
--  missing, unreadable, malformed or wrong-size atlas, or no memory for it,
--  is logged as a warning naming why, and every icon is drawn as the
--  store's built-in fallback glyph; it is not retried.
package Desktop_Icon_Assets is
   --  The icon package Desktop was built against (its exact version, like
   --  any other dependency): the atlas order is Desktop_Icons.Icon_ID's.
   Package_Path : constant String := "cubit-icons/3/";
   File_Name    : constant String := "applications.qoi";

   --  Load the atlas if it has not been tried.
   procedure Load;
end Desktop_Icon_Assets;

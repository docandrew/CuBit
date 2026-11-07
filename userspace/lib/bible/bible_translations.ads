------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The translations CuBit ships (APP-005) and a loaded one's image:
--  read-only asset files in the cubit-bible package (docs/assets.md
--  layout), <system.assets.root>cubit-bible/1/<name>.bible, built by
--  tools/build_bible.py from the pinned public-domain sources in
--  assets/bible/sources.json. A program reads only the translations it
--  shows, each once, into one heap block (Bible_Images reads it in place).
------------------------------------------------------------------------------
with Bible_Images;

package Bible_Translations is

   type Translation is
     (KJV,    --  King James Version (1769) with the Apocrypha
      BSB,    --  Berean Standard Bible
      WEB);   --  World English Bible with the deuterocanon

   --  What a reader sees first.
   DEFAULT : constant Translation := KJV;

   --  The asset package (docs/assets.md): its directory under the root.
   PACKAGE_DIRECTORY : constant String := "cubit-bible/1/";

   function File_Name (Which : Translation) return String is
     (case Which is when KJV => "kjv.bible", when BSB => "bsb.bible", when WEB => "web.bible");
   function Abbreviation (Which : Translation) return String is
     (case Which is when KJV => "KJV", when BSB => "BSB", when WEB => "WEB");
   function Title (Which : Translation) return String is
     (case Which is
         when KJV => "King James Version",
         when BSB => "Berean Standard Bible",
         when WEB => "World English Bible");

   type Image_Access is access Bible_Images.Byte_Array;

   type Load_Status is
     (Not_Loaded,      --  not asked for yet
      Loaded,          --  read whole, opened and indexed
      No_Asset_Root,   --  system.assets.root missing or malformed
      No_Filesystem,   --  the filesystem request queue could not be opened
      Open_Failed,     --  the file could not be opened (not installed)
      Read_Failed,     --  a read failed, passed its deadline, or came up short
      Not_An_Image,    --  the header or tables are not a Bible image
      Not_Indexed);    --  its books or chapters failed their checks

   function Describe (Status : Load_Status) return String is
     (case Status is
         when Not_Loaded => "not loaded",
         when Loaded => "loaded",
         when No_Asset_Root => "the system has no asset root (system.assets.root)",
         when No_Filesystem => "the filesystem is not available",
         when Open_Failed => "the cubit-bible asset package is not installed",
         when Read_Failed => "the translation could not be read",
         when Not_An_Image => "the translation's file is damaged",
         when Not_Indexed => "the translation's books are damaged");

   --  What a reader's choices are remembered as (the app's settings
   --  bible.reading-face, bible.translation and bible.red-letters): a face
   --  name, an abbreviation, and RED_LETTERS_ON or RED_LETTERS_OFF.
   type Reader_Preference is (Reading_Face_Preference, Translation_Preference, Red_Letters_Preference);
   RED_LETTERS_ON  : constant String := "on";
   RED_LETTERS_OFF : constant String := "off";

   --  One translation, once loaded.
   type Edition is record
      Status : Load_Status := Not_Loaded;
      Data   : Image_Access;
      Layout : Bible_Images.Layout;
      Index  : Bible_Images.Book_Index;
   end record;

   --  Open and index Item.Data (read by the caller): Status Loaded, or
   --  Not_An_Image / Not_Indexed (Data is then released).
   procedure Open (Item : in out Edition);

end Bible_Translations;

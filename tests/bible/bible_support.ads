--  Hosted helpers for the Bible tests (APP-005): an image file in memory,
--  opened and indexed, and verse text read whole (Linux only).
with Bible_Images; use Bible_Images;
with Bible_References; use Bible_References;
with Bible_Translations;

package Bible_Support is

   subtype Image_Access is Bible_Translations.Image_Access;

   type Opened_Image is record
      Data  : Image_Access;
      L     : Layout;
      Index : Book_Index;
   end record;
   type Translation_Access is access Opened_Image;

   --  The image file at Path, opened and indexed (Program_Error if not).
   function Load (Path : String) return Translation_Access;
   function Load_Bytes (Path : String) return Image_Access;

   --  Bytes of the image.
   function Slice (T : Opened_Image; S : Text_Slice) return String;

   --  The verse's text (its verse-text segments joined by spaces), UTF-8.
   function Verse_Text (T : Opened_Image; Row : Image_Size) return String;
   function Verse_Text (T : Opened_Image; Item : Reference) return String;

end Bible_Support;

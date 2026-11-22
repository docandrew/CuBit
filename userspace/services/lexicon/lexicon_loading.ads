--  Reading the lexicon image (docs/lexicon.md, "Shipping"): the asset
--  <system.assets.root>cubit-lexicon/<version>/english.lexicon, read once at
--  startup through the filesystem's request queue into one heap block sized
--  from the image's own header, then opened (Lexicon_Images.Open).
with Interfaces;
with Lexicon_Images;

package Lexicon_Loading is

   --  The asset package and file (docs/assets.md layout).
   PACKAGE_FILE : constant String := "cubit-lexicon/1/english.lexicon";

   type Image_Access is access Lexicon_Images.Byte_Array;

   type Load_Status is
     (Loaded,          --  read whole and opened
      No_Asset_Root,   --  system.assets.root missing or malformed
      No_Filesystem,   --  the filesystem request queue could not be opened
      Open_Failed,     --  the file could not be opened
      Read_Failed,     --  a read failed, passed the deadline, or came up short
      Not_An_Image);   --  the header or tables are not a lexicon image

   type Load_Result is record
      Status     : Load_Status := No_Asset_Root;
      Bytes      : Interfaces.Unsigned_64 := 0;
      Elapsed_Ms : Interfaces.Unsigned_64 := 0;
      Open       : Lexicon_Images.Open_Status := Lexicon_Images.Too_Short;
   end record;

   --  Read and open the image within Budget_Ms (one deadline for the whole
   --  read). Data is null unless Result.Status = Loaded.
   procedure Load (Budget_Ms : Natural; Data : out Image_Access; Layout : out Lexicon_Images.Layout;
                   Result : out Load_Result);

   function Describe (Result : Load_Result) return String;

end Lexicon_Loading;

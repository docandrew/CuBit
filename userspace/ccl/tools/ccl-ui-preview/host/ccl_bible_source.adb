--  Hosted (Linux): the image files in CCL_BIBLE_DIR (tests/bible/build/bible
--  after tests/bible/run.sh).
with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Streams.Stream_IO;
with Interfaces;
with Bible_Images;

package body CCL_Bible_Source is
   use Bible_Translations;

   DIRECTORY_VARIABLE : constant String := "CCL_BIBLE_DIR";

   procedure Load (Which : Translation; Data : out Image_Access; Status : out Load_Status) is
      use Ada.Streams;
   begin
      Data := null;
      if not Ada.Environment_Variables.Exists (DIRECTORY_VARIABLE) then
         Status := No_Asset_Root;
         return;
      end if;
      declare
         Path : constant String := Ada.Environment_Variables.Value (DIRECTORY_VARIABLE) & "/" & File_Name (Which);
         File : Stream_IO.File_Type;
         Buffer : Stream_Element_Array (1 .. 2 ** 20);
         Last : Stream_Element_Offset;
         At_Byte : Natural := 0;
      begin
         if not Ada.Directories.Exists (Path) then
            Status := Open_Failed;
            return;
         end if;
         Data := new Bible_Images.Byte_Array (0 .. Natural (Ada.Directories.Size (Path)) - 1);
         Stream_IO.Open (File, Stream_IO.In_File, Path);
         while not Stream_IO.End_Of_File (File) loop
            Stream_IO.Read (File, Buffer, Last);
            for I in Buffer'First .. Last loop
               exit when At_Byte > Data'Last;
               Data (At_Byte) := Interfaces.Unsigned_8 (Buffer (I));
               At_Byte := At_Byte + 1;
            end loop;
         end loop;
         Stream_IO.Close (File);
         Status := (if At_Byte = Data'Length then Loaded else Read_Failed);
      end;
   end Load;

   function Unavailable_Remedy return String is
     ("set " & DIRECTORY_VARIABLE & " to a directory of Bible images (tests/bible/run.sh builds them)");

end CCL_Bible_Source;

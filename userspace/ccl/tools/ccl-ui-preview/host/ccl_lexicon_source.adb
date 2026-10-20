--  HOSTED/LINUX: the lexicon image file named by CCL_LEXICON_IMAGE, read
--  once into memory and answered in process by Lexicon_Answers (the same
--  pages lexicon.svc writes). A stand-in for the service, not CuBit.
with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Streams.Stream_IO;
with Lexicon_Images;

package body CCL_Lexicon_Source is
   use Interfaces;

   VARIABLE : constant String := "CCL_LEXICON_IMAGE";

   type Image_Access is access Lexicon_Images.Byte_Array;
   Data : Image_Access;
   Layout : Lexicon_Images.Layout;
   Tried : Boolean := False;

   procedure Load is
      use Ada.Streams;
      use type Lexicon_Images.Open_Status;
      File : Stream_IO.File_Type;
      Status : Lexicon_Images.Open_Status;
   begin
      Tried := True;
      if not Ada.Environment_Variables.Exists (VARIABLE)
        or else not Ada.Directories.Exists (Ada.Environment_Variables.Value (VARIABLE))
      then
         return;
      end if;
      declare
         CHUNK : constant := 2 ** 20;
         Path : constant String := Ada.Environment_Variables.Value (VARIABLE);
         Size : constant Natural := Natural (Ada.Directories.Size (Path));
         Buffer : Stream_Element_Array (1 .. CHUNK);
         Last : Stream_Element_Offset;
         At_Byte : Natural := 0;
      begin
         Data := new Lexicon_Images.Byte_Array (0 .. Size - 1);
         Stream_IO.Open (File, Stream_IO.In_File, Path);
         while not Stream_IO.End_Of_File (File) and then At_Byte < Size loop
            Stream_IO.Read (File, Buffer, Last);
            for I in 1 .. Natural'Min (Natural (Last), Size - At_Byte) loop
               Data (At_Byte) := Unsigned_8 (Buffer (Stream_Element_Offset (I)));
               At_Byte := At_Byte + 1;
            end loop;
         end loop;
         Stream_IO.Close (File);
      end;
      Lexicon_Images.Open (Data.all, Layout, Status);
      if Status /= Lexicon_Images.Opened then
         Data := null;
      end if;
   end Load;

   procedure Ask
     (Op : Lexicon_Answers.Operation; Text : String; Resume : Unsigned_32;
      Suggestions : Lexicon_Answers.Suggestion_Count; Into : out Page; Used : out Natural;
      Status : out Ask_Status) is
   begin
      if not Tried then
         Load;
      end if;
      if Data = null then
         Into := [others => 0];
         Used := 0;
         Status := Unavailable;
         return;
      end if;
      Lexicon_Answers.Answer (Data.all, Layout, Op, Text, Resume, Suggestions, Into, Used);
      Status := Answered;
   end Ask;

   function Unavailable_Reason return String is
     ("no lexicon image: " & VARIABLE & " does not name one");
   function Unavailable_Remedy return String is
     ("build it with tools/build_lexicon.py and set " & VARIABLE & " to its path");
end CCL_Lexicon_Source;

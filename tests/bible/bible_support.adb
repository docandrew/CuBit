with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces; use Interfaces;

package body Bible_Support is

   function Load_Bytes (Path : String) return Image_Access is
      use Ada.Streams;
      Size : constant Natural := Natural (Ada.Directories.Size (Path));
      Result : constant Image_Access := new Byte_Array (0 .. Size - 1);
      File : Stream_IO.File_Type;
      Buffer : Stream_Element_Array (1 .. 2 ** 20);
      Last : Stream_Element_Offset;
      At_Byte : Natural := 0;
   begin
      Stream_IO.Open (File, Stream_IO.In_File, Path);
      while not Stream_IO.End_Of_File (File) loop
         Stream_IO.Read (File, Buffer, Last);
         for I in Buffer'First .. Last loop
            Result (At_Byte) := Unsigned_8 (Buffer (I));
            At_Byte := At_Byte + 1;
         end loop;
      end loop;
      Stream_IO.Close (File);
      return Result;
   end Load_Bytes;

   function Load (Path : String) return Translation_Access is
      Result : constant Translation_Access := new Opened_Image;
      Opened : Open_Status;
      Read : Index_Status;
   begin
      Result.Data := Load_Bytes (Path);
      Open (Result.Data.all, Result.L, Opened);
      if Opened /= Bible_Images.Opened then
         raise Program_Error with Path & ": " & Opened'Image;
      end if;
      Index (Result.Data.all, Result.L, Result.Index, Read);
      if Read /= Index_Read then
         raise Program_Error with Path & ": " & Read'Image;
      end if;
      return Result;
   end Load;

   function Slice (T : Opened_Image; S : Text_Slice) return String is
      Result : String (1 .. S.Length);
   begin
      for I in Result'Range loop
         Result (I) := Character'Val (T.Data (S.First + I - 1));
      end loop;
      return Result;
   end Slice;

   function Verse_Text (T : Opened_Image; Row : Image_Size) return String is
      V : constant Verse_Record := Read_Verse (T.Data.all, T.L, Row);
      Result : Unbounded_String;
   begin
      if not V.Valid then
         return "";
      end if;
      for S in V.First_Segment .. V.First_Segment + V.Segments - 1 loop
         declare
            Seg : constant Segment_Record := Read_Segment (T.Data.all, T.L, S);
         begin
            if Seg.Valid and then Is_Verse_Text (Seg.Kind) then
               if Length (Result) > 0 then
                  Append (Result, ' ');
               end if;
               Append (Result, Slice (T, Seg.Content));
            end if;
         end;
      end loop;
      return To_String (Result);
   end Verse_Text;

   function Verse_Text (T : Opened_Image; Item : Reference) return String is
     (Verse_Text (T, Verse_Row (T.L, T.Index, Item)));

end Bible_Support;

with Ada.Unchecked_Deallocation;
with Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Filesystem_Queues;
with CuBit.Filesystem_Sessions;
with CuBit.Filesystems;
with CuBit.Messages;
with Bible_Images;

package body Bible_Asset_Files is
   use Interfaces;
   use Bible_Translations;
   use type Bible_Images.Open_Status;
   package FS renames CuBit.Filesystem_Sessions;
   package FQ renames CuBit.Filesystem_Queues;
   use type FS.Token;
   use type FS.Wait_Result;

   --  The transfer window: the path in its first page, data in the rest.
   ARENA_PAGES : constant := 256;
   PATH_AT     : constant := 0;
   DATA_AT     : constant := FQ.Page_Bytes;
   CHUNK_BYTES : constant := (ARENA_PAGES - 1) * FQ.Page_Bytes;
   pragma Compile_Time_Error (ARENA_PAGES > FQ.Transfer_Pages, "arena too large");

   MAXIMUM_ROOT : constant := 128;
   MAXIMUM_FILE : constant := 16;
   MAXIMUM_PATH : constant := MAXIMUM_ROOT + PACKAGE_DIRECTORY'Length + MAXIMUM_FILE;
   pragma Compile_Time_Error (MAXIMUM_PATH > FQ.Page_Bytes, "path page too small");
   VOLUME_MARK : constant Character := '@';
   SEPARATOR   : constant Character := '/';

   Session : FS.Session;
   Bytes_Read : Natural := 0;
   Elapsed : Natural := 0;

   procedure Free is new Ada.Unchecked_Deallocation (Bible_Images.Byte_Array, Image_Access);

   function Last_Bytes return Natural is (Bytes_Read);
   function Last_Milliseconds return Natural is (Elapsed);

   --  The configured asset root: a volume path ending in a separator, made
   --  of printable characters, with no parent references.
   procedure Read_Root (Root : out String; Length : out Natural) is
      Copy : constant String := Asset_Root;
      Size : constant Natural := Copy'Length;
   begin
      Length := 0;
      if Size not in 2 .. MAXIMUM_ROOT then
         return;
      end if;
      begin
         if Copy (Copy'First) /= VOLUME_MARK or else Copy (Copy'Last) /= SEPARATOR or else
           (for some C of Copy => C not in '!' .. '~') or else
           (for some I in Copy'First .. Copy'Last - 1 => Copy (I .. I + 1) = "..")
         then
            return;
         end if;
         Root (Root'First .. Root'First + Size - 1) := Copy;
         Length := Size;
      end;
   end Read_Root;

   --  One request, waited for until Deadline.
   function Call (Item : FQ.Request; Deadline : Unsigned_64; Value : out Unsigned_64) return Unsigned_32 is
      Tag : FS.Token;
      Answer : FQ.Queues.Completion;
      Got : Boolean;
      Waited : FS.Wait_Result;
   begin
      Value := 0;
      if not FS.Can_Submit (Session) then
         return CuBit.Filesystems.REPLY_ERR;
      end if;
      FS.Submit (Session, Item, Tag);
      if Tag = 0 then
         return CuBit.Filesystems.REPLY_ERR;
      end if;
      FS.Wait_Answer (Session, Deadline, Waited);
      if Waited /= FS.Answer_Waiting then
         return CuBit.Messages.REPLY_TIMEOUT;
      end if;
      FS.Reap (Session, Answer, Got);
      if not Got or else Answer.Tag /= Tag then
         return CuBit.Filesystems.REPLY_ERR;
      end if;
      Value := Answer.Answer.Value;
      return Answer.Answer.Status;
   end Call;

   procedure Load (Which : Translation; Data : out Image_Access; Status : out Load_Status) is
      Started : constant Unsigned_64 := CuBit.Messages.syscall (CuBit.Messages.SYSCALL_GETTIME);
      Deadline : constant Unsigned_64 := CuBit.Messages.Deadline_After (LOAD_BUDGET_MS);
      Root : String (1 .. MAXIMUM_ROOT);
      Root_Length : Natural;
      Queue_Opened : Boolean;
      Handle, Ignore : Unsigned_64 := 0;
      Ignore_Close : Unsigned_32;
      Size : Bible_Images.Image_Size := 0;

      procedure Finish (Outcome : Load_Status) is
      begin
         Status := Outcome;
         Elapsed := Natural (Unsigned_64'Min (CuBit.Messages.syscall (CuBit.Messages.SYSCALL_GETTIME) - Started,
                                             Unsigned_64 (Natural'Last)));
      end Finish;

      --  Read the open file into Data, sized from its header.
      procedure Read_File is
         Count : Unsigned_64;
         Answer : Unsigned_32;
         At_Byte : Natural := 0;
         Declared : Bible_Images.Open_Status;
      begin
         loop
            Answer := Call ((Operation => FQ.Queue_Read_At, Handle => Handle,
                             Position => Unsigned_64 (At_Byte), Length => CHUNK_BYTES,
                             Arena_Offset => DATA_AT, others => <>),
                            Deadline, Count);
            if Answer /= CuBit.Filesystems.REPLY_OK or else Count > CHUNK_BYTES then
               Finish (Read_Failed);
               return;
            end if;
            exit when Count = 0;
            declare
               Shared : constant Bible_Images.Byte_Array (0 .. Natural (Count) - 1)
                 with Import, Address => FS.Arena (Session) + Storage_Offset (DATA_AT);
            begin
               if Data = null then
                  --  The first chunk holds the header, which gives the size.
                  if Count < Bible_Images.HEADER_BYTES then
                     Finish (Not_An_Image);
                     return;
                  end if;
                  Bible_Images.Declared_Size (Shared (0 .. Bible_Images.HEADER_BYTES - 1), Size, Declared);
                  if Declared /= Bible_Images.Opened or else Size < Bible_Images.HEADER_BYTES then
                     Finish (Not_An_Image);
                     return;
                  end if;
                  Data := new Bible_Images.Byte_Array (0 .. Size - 1);
               end if;
               if Natural (Count) > Size - At_Byte then
                  Finish (Not_An_Image);   --  longer than its header says
                  return;
               end if;
               Data (At_Byte .. At_Byte + Natural (Count) - 1) := Shared;
               At_Byte := At_Byte + Natural (Count);
               Bytes_Read := At_Byte;
            end;
         end loop;
         if Data = null or else At_Byte /= Size then
            Finish (Read_Failed);
            return;
         end if;
         Finish (Loaded);
      end Read_File;
   begin
      Data := null;
      Status := Read_Failed;
      Bytes_Read := 0;
      Read_Root (Root, Root_Length);
      if Root_Length = 0 then
         Finish (No_Asset_Root);
         return;
      end if;
      FS.Open (Session, Filesystem, ARENA_PAGES, Queue_Opened);
      if not Queue_Opened then
         Finish (No_Filesystem);
         return;
      end if;
      declare
         Path : constant String := Root (1 .. Root_Length) & PACKAGE_DIRECTORY & File_Name (Which);
         Shared : String (1 .. Path'Length)
           with Import, Address => FS.Arena (Session) + Storage_Offset (PATH_AT);
      begin
         Shared := Path;
         if Call ((Operation => FQ.Queue_Open,
                   Options => Unsigned_32 (CuBit.Filesystems.OPEN_READ_ONLY),
                   Length => Unsigned_64 (Path'Length), Arena_Offset => PATH_AT, others => <>),
                  Deadline, Handle) /= CuBit.Filesystems.REPLY_OK
         then
            Finish (Open_Failed);
         else
            Read_File;
            Ignore_Close := Call ((Operation => FQ.Queue_Close, Handle => Handle, others => <>),
                                  Deadline, Ignore);
         end if;
      end;
      --  One image, read once: the queue is not needed afterwards.
      FS.Close (Session);
      if Status /= Loaded then
         Free (Data);
      end if;
   end Load;

end Bible_Asset_Files;

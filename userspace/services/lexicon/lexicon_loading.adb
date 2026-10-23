with Ada.Unchecked_Deallocation;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Settings;
with CuBit.Filesystem_Queues;
with CuBit.Filesystem_Sessions;
with CuBit.Filesystems;
with CuBit.Messages;
with Startup;

package body Lexicon_Loading is
   use Interfaces;
   use Lexicon_Images;
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
   MAXIMUM_PATH : constant := MAXIMUM_ROOT + PACKAGE_FILE'Length;
   pragma Compile_Time_Error (MAXIMUM_PATH > FQ.Page_Bytes, "path page too small");
   VOLUME_MARK : constant Character := '@';
   SEPARATOR   : constant Character := '/';

   Session : FS.Session;

   procedure Free is new Ada.Unchecked_Deallocation (Byte_Array, Image_Access);

   function Describe (Result : Load_Result) return String is
     (case Result.Status is
        when Loaded => "loaded" & Result.Bytes'Image & " bytes in" & Result.Elapsed_Ms'Image & " ms",
        when No_Asset_Root => Startup.Settings.System_Assets_Root.Setting_Key & " is not a valid asset root",
        when No_Filesystem => "no filesystem queue",
        when Open_Failed => "the lexicon file could not be opened",
        when Read_Failed => "reading failed after" & Result.Bytes'Image & " bytes",
        when Not_An_Image => "not a lexicon image (" & Open_Status'Image (Result.Open) & ")");

   --  The configured asset root: a volume path ending in a separator, made
   --  of printable characters, with no parent references.
   procedure Read_Root (Root : out String; Length : out Natural) is
      use all type CuBit.Settings.Outcome;
      Stored : constant Startup.Settings.System_Assets_Root.Reading := Startup.Settings.System_Assets_Root.Read;
   begin
      Length := 0;
      if Stored.Kind /= Present or else Stored.Item.Length not in 2 .. MAXIMUM_ROOT then
         return;
      end if;
      declare
         Copy : constant String := CuBit.Settings.Image (Stored.Item);
         Size : constant Natural := Copy'Length;
      begin
         if Copy (1) /= VOLUME_MARK or else Copy (Size) /= SEPARATOR or else
           (for some C of Copy => C not in '!' .. '~') or else
           (for some I in 1 .. Size - 1 => Copy (I .. I + 1) = "..")
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

   procedure Load (Budget_Ms : Natural; Data : out Image_Access; Layout : out Lexicon_Images.Layout;
                   Result : out Load_Result)
   is
      Started : constant Unsigned_64 := CuBit.Messages.syscall (CuBit.Messages.SYSCALL_GETTIME);
      Deadline : constant Unsigned_64 := CuBit.Messages.Deadline_After (Unsigned_64 (Budget_Ms));
      Root : String (1 .. MAXIMUM_ROOT);
      Root_Length : Natural;
      Queue_Opened : Boolean;
      Handle, Ignore : Unsigned_64 := 0;
      Ignore_Close : Unsigned_32;
      Size : Image_Size := 0;

      procedure Finish (Status : Load_Status) is
      begin
         Result.Status := Status;
         Result.Elapsed_Ms := CuBit.Messages.syscall (CuBit.Messages.SYSCALL_GETTIME) - Started;
      end Finish;

      --  Read the open file into Data, sized from its header.
      procedure Read_File is
         Count : Unsigned_64;
         Status : Unsigned_32;
         At_Byte : Natural := 0;
         Declared : Open_Status;
      begin
         loop
            Status := Call ((Operation => FQ.Queue_Read_At, Handle => Handle,
                             Position => Unsigned_64 (At_Byte), Length => CHUNK_BYTES,
                             Arena_Offset => DATA_AT, others => <>),
                            Deadline, Count);
            if Status /= CuBit.Filesystems.REPLY_OK or else Count > CHUNK_BYTES then
               Finish (Read_Failed);
               return;
            end if;
            exit when Count = 0;
            declare
               Shared : constant Byte_Array (0 .. Natural (Count) - 1)
                 with Import, Address => FS.Arena (Session) + Storage_Offset (DATA_AT);
            begin
               if Data = null then
                  --  The first chunk holds the header, which gives the size.
                  if Count < HEADER_BYTES then
                     Finish (Not_An_Image);
                     return;
                  end if;
                  Declared_Size (Shared (0 .. HEADER_BYTES - 1), Size, Declared);
                  Result.Open := Declared;
                  if Declared /= Opened or else Size < HEADER_BYTES then
                     Finish (Not_An_Image);
                     return;
                  end if;
                  Data := new Byte_Array (0 .. Size - 1);
               end if;
               if Natural (Count) > Size - At_Byte then
                  Finish (Not_An_Image);   --  longer than its header says
                  return;
               end if;
               Data (At_Byte .. At_Byte + Natural (Count) - 1) := Shared;
               At_Byte := At_Byte + Natural (Count);
               Result.Bytes := Unsigned_64 (At_Byte);
            end;
         end loop;
         if Data = null or else At_Byte /= Size then
            Finish (Read_Failed);
            return;
         end if;
         Open (Data.all, Layout, Result.Open);
         Finish (if Result.Open = Opened then Loaded else Not_An_Image);
      end Read_File;
   begin
      Data := null;
      Layout := (others => <>);
      Result := (others => <>);
      Read_Root (Root, Root_Length);
      if Root_Length = 0 then
         Finish (No_Asset_Root);
         return;
      end if;
      FS.Open (Session, Startup.Filesystem, ARENA_PAGES, Queue_Opened);
      if not Queue_Opened then
         Finish (No_Filesystem);
         return;
      end if;
      declare
         Path : constant String := Root (1 .. Root_Length) & PACKAGE_FILE;
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
      if Result.Status /= Loaded then
         Free (Data);   --  the service then answers No_Lexicon
      end if;
   end Load;

end Lexicon_Loading;

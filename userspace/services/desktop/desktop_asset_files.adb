with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with Desktop_Endpoints;
with CuBit.Settings;
with Startup;
with CuBit.Filesystem_Queues;
with CuBit.Filesystem_Sessions;
with CuBit.Filesystems;
with CuBit.Messages;

package body Desktop_Asset_Files is
   use Interfaces;
   package FS renames CuBit.Filesystem_Sessions;
   package FQ renames CuBit.Filesystem_Queues;
   package Q renames CuBit.QOI;
   use type FS.Token;
   use type FS.Wait_Result;

   --  A transfer window of Arena_Pages: the path in its first page, file
   --  data read into the rest.
   Arena_Pages : constant := 64;
   Path_At     : constant := 0;
   Data_At     : constant := FQ.Page_Bytes;
   Chunk_Bytes : constant := (Arena_Pages - 1) * FQ.Page_Bytes;
   pragma Compile_Time_Error (Arena_Pages > FQ.Transfer_Pages, "arena too large");
   pragma Compile_Time_Error (Maximum_Path > FQ.Page_Bytes, "path page too small");

   Maximum_Root : constant := 128;
   subtype Root_Length is Natural range 0 .. Maximum_Root;
   Volume_Mark  : constant Character := '@';
   Separator    : constant Character := '/';

   --  Private copy of each chunk: the arena is shared with the service.
   --  One read at a time (Desktop's startup and settings are one task).
   Chunk : Q.Byte_Array (1 .. Chunk_Bytes) := [others => 0];
   --  The request queue stays open between reads: startup reads several
   --  assets back to back, and reopening a just-closed queue can be refused
   --  while the service still retires the old one. A read that did not end
   --  cleanly closes it, so no stale request survives into the next read.
   Session : FS.Session;

   function Status_Name (Status : Unsigned_32) return String is
     (if Status = CuBit.Filesystems.REPLY_NOT_FOUND then "not found"
      elsif Status = CuBit.Filesystems.REPLY_ACCESS_DENIED then "access denied"
      elsif Status = CuBit.Messages.REPLY_TIMEOUT then "deadline passed"
      else "status" & Status'Image);

   function Describe (Result : Read_Result) return String is
     (case Result.Status is
        when Complete => "read" & Result.Bytes'Image & " bytes",
        when Refused => "rejected after" & Result.Bytes'Image & " bytes",
        when No_Asset_Root => Startup.Settings.System_Assets_Root.Setting_Key & " is not a valid asset root",
        when No_Filesystem => "no filesystem queue",
        when Open_Failed => "open failed (" & Status_Name (Result.Open_Reply) & ")",
        when Read_Failed => "read failed after" & Result.Bytes'Image & " bytes",
        when Too_Large => "larger than" & Result.Bytes'Image & " bytes");

   --  The configured asset root: a volume path ending in a separator, made
   --  of printable characters, with no parent references.
   procedure Read_Root (Root : out String; Length : out Root_Length) is
      use all type CuBit.Settings.Outcome;
      Stored : constant Startup.Settings.System_Assets_Root.Reading := Startup.Settings.System_Assets_Root.Read;
   begin
      Length := 0;
      if Stored.Kind /= Present or else Stored.Item.Length not in 2 .. Maximum_Root then
         return;
      end if;
      declare
         Copy : constant String := CuBit.Settings.Image (Stored.Item);
         Size : constant Natural := Copy'Length;
      begin
         if Copy (1) /= Volume_Mark or else Copy (Size) /= Separator or else
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
   function Call
     (Item : FQ.Request; Deadline : Unsigned_64; Value : out Unsigned_64) return Unsigned_32
   is
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

   procedure Read
     (File        : String;
      Max_Bytes   : Unsigned_64;
      Budget_Ms   : Natural;
      Result      : out Read_Result;
      Path        : out String;
      Path_Length : out Natural)
   is
      Started : constant Unsigned_64 := CuBit.Messages.syscall (CuBit.Messages.SYSCALL_GETTIME);
      Deadline : constant Unsigned_64 := CuBit.Messages.Deadline_After (Unsigned_64 (Budget_Ms));
      Root : String (1 .. Maximum_Root);
      Length : Root_Length;

      procedure Finish (Status : Read_Status) is
      begin
         Result.Status := Status;
         Result.Elapsed_Ms :=
           CuBit.Messages.syscall (CuBit.Messages.SYSCALL_GETTIME) - Started;
      end Finish;

      --  Read the open file Handle to its end, into the sink.
      procedure Read_File (Handle : Unsigned_64) is
         Count : Unsigned_64;
         Status : Unsigned_32;
      begin
         loop
            Status := Call ((Operation => FQ.Queue_Read_At, Handle => Handle,
                             Position => Result.Bytes, Length => Chunk_Bytes,
                             Arena_Offset => Data_At, others => <>),
                            Deadline, Count);
            if Status /= CuBit.Filesystems.REPLY_OK or else Count > Chunk_Bytes then
               Finish (Read_Failed);
               return;
            end if;
            if Count = 0 then
               Finish (Complete);
               return;
            end if;
            Result.Bytes := Result.Bytes + Count;
            if Result.Bytes > Max_Bytes then
               Finish (Too_Large);
               return;
            end if;
            declare
               Shared : constant Q.Byte_Array (1 .. Positive (Count))
                 with Import, Address => FS.Arena (Session) + Storage_Offset (Data_At);
            begin
               Chunk (1 .. Positive (Count)) := Shared;
            end;
            Feed (Chunk (1 .. Positive (Count)));
            if not Wants_More then
               --  Rejected already; the rest of the file changes nothing.
               Finish (Refused);
               return;
            end if;
         end loop;
      end Read_File;
   begin
      Result := (others => <>);
      Path := [others => ' '];
      Path_Length := 0;
      Read_Root (Root, Length);
      if Length = 0 or else Length + File'Length > Maximum_Path then
         Finish (No_Asset_Root);
         return;
      end if;
      Path_Length := Length + File'Length;
      Path (1 .. Path_Length) := Root (1 .. Length) & File;
      declare
         Opened : Boolean;
         Handle, Ignore : Unsigned_64 := 0;
         Ignore_Close : Unsigned_32;
      begin
         FS.Open (Session, Desktop_Endpoints.Files, Arena_Pages, Opened);
         if not Opened then
            Finish (No_Filesystem);
            return;
         end if;
         declare
            Shared : String (1 .. Path_Length)
              with Import, Address => FS.Arena (Session) + Storage_Offset (Path_At);
         begin
            Shared := Path (1 .. Path_Length);
         end;
         Result.Open_Reply :=
           Call ((Operation => FQ.Queue_Open,
                  Options => Unsigned_32 (CuBit.Filesystems.OPEN_READ_ONLY),
                  Length => Unsigned_64 (Path_Length), Arena_Offset => Path_At, others => <>),
                 Deadline, Handle);
         if Result.Open_Reply = CuBit.Filesystems.REPLY_OK then
            Read_File (Handle);
            --  A failed close leaves nothing to undo: the queue closes next.
            Ignore_Close := Call ((Operation => FQ.Queue_Close, Handle => Handle, others => <>),
                                  Deadline, Ignore);
         else
            Finish (Open_Failed);
         end if;
         if not Read_Whole (Result) and then
           (Result.Status /= Open_Failed or else
            Result.Open_Reply = CuBit.Messages.REPLY_TIMEOUT or else
            Result.Open_Reply = CuBit.Filesystems.REPLY_ERR)
         then
            FS.Close (Session);
         end if;
      end;
   end Read;
end Desktop_Asset_Files;

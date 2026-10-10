with Interfaces; use Interfaces;
with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with CCL_Manifest_Bindings;
with CuBit.Config;
with CuBit.Filesystem_Queues;
with CuBit.Filesystem_Sessions;
with CuBit.Filesystems;
with CuBit.Messages;
with CuBit.QOI;
with Desktop_Backdrop_Style;
with Desktop_Logs;
with Desktop_Wallpaper_Store;

package body Desktop_Wallpaper_Assets is
   package Store renames Desktop_Wallpaper_Store;
   package FS renames CuBit.Filesystem_Sessions;
   package FQ renames CuBit.Filesystem_Queues;
   package Q renames CuBit.QOI;
   use type CuBit.Appearance.Background;
   use type CuBit.Config.ConfigStatus;
   use type FS.Token;
   use type FS.Wait_Result;
   use type Store.Status;

   LF : constant Character := Character'Val (10);

   --  The whole load, from opening the queue to closing it, ends by this
   --  deadline; startup never waits longer on the filesystem for a
   --  wallpaper.
   Load_Budget_Ms : constant := 2_000;
   --  A transfer window of Arena_Pages: the path in its first page, file
   --  data read into the rest.
   Arena_Pages : constant := 64;
   Path_At     : constant := 0;
   Data_At     : constant := FQ.Page_Bytes;
   Chunk_Bytes : constant := (Arena_Pages - 1) * FQ.Page_Bytes;
   pragma Compile_Time_Error (Arena_Pages > FQ.Transfer_Pages, "arena too large");

   Maximum_Root : constant := 128;
   subtype Root_Length is Natural range 0 .. Maximum_Root;
   Volume_Mark  : constant Character := '@';
   Separator    : constant Character := '/';
   Extension    : constant String := ".qoi";

   --  Each image's file in the package.
   function File_Name (Asset : Store.Image) return String is
     ((case Asset is
         when CuBit.Appearance.Wallpaper => "cubes",
         when CuBit.Appearance.Cubie => "cubie") & Extension);

   --  The worst QOI stream for Asset's raster: a five-byte Op_RGBA chunk per
   --  pixel. Longer files are rejected without reading them all.
   function Maximum_Bytes (Asset : Store.Image) return Unsigned_64 is
     (Q.Header_Bytes + Q.Marker_Bytes + 5 * Unsigned_64
        (Desktop_Backdrop_Style.Width (Asset)) *
        Unsigned_64 (Desktop_Backdrop_Style.Height (Asset)));

   --  Private copy of each chunk: the arena is shared with the service.
   Chunk : Q.Byte_Array (1 .. Chunk_Bytes) := [others => 0];
   Session : FS.Session;

   --  Readable reasons: the runtime has no enumeration images.
   function Reason (Error : Q.Failure) return String is
     (case Error is
        when Q.No_Failure => "no image",
        when Q.Bad_Magic => "not a QOI file",
        when Q.Bad_Size => "invalid size",
        when Q.Over_Limit => "larger than the raster",
        when Q.Bad_Channels => "invalid channel count",
        when Q.Bad_Colorspace => "invalid colour space",
        when Q.Run_Past_End => "run past the last pixel",
        when Q.Bad_Marker => "bad end marker",
        when Q.Trailing_Data => "data after the end marker",
        when Q.Truncated => "truncated");
   function Status_Name (Status : Unsigned_32) return String is
     (if Status = CuBit.Filesystems.REPLY_NOT_FOUND then "not found"
      elsif Status = CuBit.Filesystems.REPLY_ACCESS_DENIED then "access denied"
      elsif Status = CuBit.Messages.REPLY_TIMEOUT then "deadline passed"
      else "status" & Status'Image);

   procedure Warn (Text : String) is
   begin
      Desktop_Logs.Warn (Text);
   end Warn;

   --  The configured asset root: a volume path ending in a separator, made
   --  of printable characters, with no parent references.
   procedure Read_Root (Root : out String; Length : out Root_Length) is
      Value : System.Address;
      Size : Natural;
      Status : CuBit.Config.ConfigStatus;
   begin
      Length := 0;
      CuBit.Config.get (Root_Setting, Value, Size, Status);
      if Status /= CuBit.Config.OK or else Value = System.Null_Address or else
        Size not in 2 .. Maximum_Root
      then
         return;
      end if;
      declare
         Text : constant String (1 .. Size) with Import, Address => Value;
         Copy : constant String (1 .. Size) := Text;
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

   --  Read the open file Handle into the store until its end, or until the
   --  store has rejected the stream. Read_Failed: a request failed.
   procedure Read_File
     (Asset : Store.Image; Handle : Unsigned_64; Deadline : Unsigned_64;
      Bytes : out Unsigned_64; Read_Failed : out Boolean)
   is
      Count : Unsigned_64;
      Status : Unsigned_32;
   begin
      Bytes := 0;
      Read_Failed := True;
      loop
         Status := Call ((Operation => FQ.Queue_Read_At, Handle => Handle, Position => Bytes,
                          Length => Chunk_Bytes, Arena_Offset => Data_At, others => <>),
                         Deadline, Count);
         if Status /= CuBit.Filesystems.REPLY_OK or else Count > Chunk_Bytes then
            return;
         end if;
         if Count = 0 then
            Read_Failed := False;
            return;
         end if;
         Bytes := Bytes + Count;
         if Bytes > Maximum_Bytes (Asset) then
            return;
         end if;
         declare
            Shared : constant Q.Byte_Array (1 .. Positive (Count))
              with Import, Address => FS.Arena (Session) + Storage_Offset (Data_At);
         begin
            Chunk (1 .. Positive (Count)) := Shared;
         end;
         Store.Feed (Asset, Chunk (1 .. Positive (Count)));
         if not Store.Wants_More (Asset) then
            --  Rejected already; the rest of the file changes nothing.
            Read_Failed := False;
            return;
         end if;
      end loop;
   end Read_File;

   procedure Load (Asset : Store.Image) is
      Root : String (1 .. Maximum_Root);
      Length : Root_Length;
      Started : constant Unsigned_64 := CuBit.Messages.syscall (CuBit.Messages.SYSCALL_GETTIME);
      Deadline : constant Unsigned_64 := CuBit.Messages.Deadline_After (Load_Budget_Ms);
   begin
      Read_Root (Root, Length);
      if Length = 0 then
         Store.Give_Up (Asset);
         Warn ("desktop: wallpaper unavailable: " & Root_Setting &
               " is not a valid asset root; using the flat theme colour");
         return;
      end if;
      declare
         Path : constant String := Root (1 .. Length) & Package_Path & File_Name (Asset);
         Opened : Boolean;
         Handle, Bytes, Ignore : Unsigned_64 := 0;
         Open_Status, Ignore_Close : Unsigned_32;
         Read_Failed : Boolean := True;
         Result : Store.Load_Result;
      begin
         FS.Open (Session, CCL_Manifest_Bindings.Slot_filesystem, Arena_Pages, Opened);
         if not Opened then
            Store.Give_Up (Asset);
            Warn ("desktop: wallpaper " & Path &
                  " unavailable: no filesystem queue; using the flat theme colour");
            return;
         end if;
         declare
            Shared : String (1 .. Path'Length)
              with Import, Address => FS.Arena (Session) + Storage_Offset (Path_At);
         begin
            Shared := Path;
         end;
         Store.Begin_Load (Asset);
         Open_Status := Call ((Operation => FQ.Queue_Open,
                               Options => Unsigned_32 (CuBit.Filesystems.OPEN_READ_ONLY),
                               Length => Path'Length, Arena_Offset => Path_At, others => <>),
                              Deadline, Handle);
         if Open_Status = CuBit.Filesystems.REPLY_OK then
            Read_File (Asset, Handle, Deadline, Bytes, Read_Failed);
            --  A failed close leaves nothing to undo: the queue closes next.
            Ignore_Close := Call ((Operation => FQ.Queue_Close, Handle => Handle, others => <>),
                                  Deadline, Ignore);
         end if;
         FS.Close (Session);
         Store.End_Load (Asset, Read_Failed, Result);
         if Result.Loaded then
            Desktop_Logs.Write ("desktop: wallpaper loaded " & Path & " bytes=" &
              Bytes'Image & " ms=" &
              Unsigned_64'Image (CuBit.Messages.syscall (CuBit.Messages.SYSCALL_GETTIME) - Started) & LF);
         elsif Open_Status /= CuBit.Filesystems.REPLY_OK then
            Warn ("desktop: wallpaper " & Path & " unavailable: open failed (" &
                  Status_Name (Open_Status) & "); using the flat theme colour");
         elsif Read_Failed then
            Warn ("desktop: wallpaper " & Path & " unavailable: read failed after" &
                  Bytes'Image & " bytes; using the flat theme colour");
         elsif Result.Wrong_Size then
            Warn ("desktop: wallpaper " & Path & " rejected: size" & Result.Width'Image &
                  " x" & Result.Height'Image & " is not the expected" &
                  Desktop_Backdrop_Style.Width (Asset)'Image & " x" &
                  Desktop_Backdrop_Style.Height (Asset)'Image &
                  "; using the flat theme colour");
         else
            Warn ("desktop: wallpaper " & Path & " rejected: " & Reason (Result.Error) &
                  "; using the flat theme colour");
         end if;
      end;
   end Load;

   procedure Prepare (Backdrop : CuBit.Appearance.Background) is
   begin
      if Backdrop in Store.Image and then not Store.Busy and then
        Store.Current (Backdrop) = Store.Not_Loaded
      then
         Load (Backdrop);
      end if;
   end Prepare;

   procedure Resolve (Requested : CuBit.Appearance.Preferences;
                      Shown : out CuBit.Appearance.Preferences) is
   begin
      Prepare (Requested.Backdrop);
      Shown := Store.Shown (Requested);
   end Resolve;
end Desktop_Wallpaper_Assets;

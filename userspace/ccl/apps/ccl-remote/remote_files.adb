with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Filesystems; use CuBit.Filesystems;
with CuBit.Memory_Grants;
with SPARKNaCl;
with SPARKTLSCrypto.Hashing.SHA256;
with CCL.Interfaces.Files;
with CCL_Native_Bindings;
with CCL_Places;

package body Remote_Files is
   use Interfaces;
   use type W.Status;
   use type W.Place;
   use type W.Digest;
   package SHA renames SPARKTLSCrypto.Hashing.SHA256;

   Page_Bytes : constant := 4_096;
   --  One chunk, or two paths for a rename.
   Buffer_Bytes : constant := W.Maximum_Chunk_Bytes;
   pragma Compile_Time_Error
     (Buffer_Bytes < 2 * MAXIMUM_PATH_BYTES, "a rename's two paths must fit the buffer");
   Buffer_Pages : constant := Buffer_Bytes / Page_Bytes;

   Work_Root : String (1 .. Maximum_Root_Bytes) := [others => ' '];
   Work_Length : Natural range 0 .. Maximum_Root_Bytes := 0;
   Loan : CuBit.Memory_Grants.Grant_Reference;
   Buffer_Address : Unsigned_64 := 0;
   Ready : Boolean := False;

   Workspace_Directory : constant String := "/remote";
   Mail_Directory      : constant String := "/mail";
   To_Device_Directory : constant String := "/mail/to-device";
   To_Host_Directory   : constant String := "/mail/to-host";
   Captures_Directory  : constant String := "/captures";

   function Root (Where : W.Place) return String is
     (Work_Root (1 .. Work_Length) &
      (case Where is
          when W.Workspace => Workspace_Directory,
          when W.Mail_To_Device => To_Device_Directory,
          when W.Mail_To_Host => To_Host_Directory,
          when W.Captures => Captures_Directory,
          when W.None => ""));

   function Path (Where : W.Place; Name : String) return String is
     (if Name'Length = 0 then Root (Where) else Root (Where) & "/" & Name);

   function Outcome (Label : Unsigned_32) return W.Status is
     (case Label is
         when REPLY_OK => W.OK,
         when REPLY_NOT_FOUND => W.Not_Found,
         when REPLY_ACCESS_DENIED | REPLY_READ_ONLY => W.Not_Granted,
         when REPLY_ALREADY_EXISTS | REPLY_SHARING_VIOLATION | REPLY_IS_DIRECTORY |
              REPLY_WRONG_OBJECT_TYPE | REPLY_NOT_EMPTY => W.Conflict,
         when REPLY_NO_SPACE => W.Too_Large,
         when REPLY_BUSY => W.Busy,
         when REPLY_VOLUME_UNAVAILABLE => W.Unavailable,
         when others => W.Failed);

   procedure Place_Text (Text : String) is
      Bytes : String (1 .. Text'Length)
        with Import, Address => To_Address (Integer_Address (Buffer_Address));
   begin
      Bytes := Text;
   end Place_Text;

   function Call (Request : in out Message) return W.Status is
   begin
      Request.tag := capCall (CCL_Native_Bindings.Filesystem, Request, CuBit.Messages.Wait_Forever);
      return Outcome (Request.tag.label);
   end Call;

   --  OPEN's word layout: UNLINK and MKDIR use it (word 2: no parked handle).
   function Path_Request (Label : Unsigned_32; Length : Natural) return Message is
     (tag => (label => Label, length => 4, protocol => 0, ipc => <>, reserved => 0),
      authorityTag => 0,
      words => [0 => Loan.slot, 1 => Unsigned_64 (Length), 2 => 0, 3 => Loan.generation]);

   procedure Make_Directory (Text : String; Result : out W.Status) is
      Request : Message;
   begin
      Place_Text (Text);
      Request := Path_Request (OP_MKDIR, Text'Length);
      Result := Call (Request);
      if Result = W.Conflict then
         Result := W.OK;   --  it exists already
      end if;
   end Make_Directory;

   procedure Initialize (Work : String; Success : out Boolean) is
      Raw : Unsigned_64;
      Made : W.Status;
      --  Either way, the places below must be made.
      Ignore_Root : W.Status;
   begin
      Success := False;
      Work_Length := Work'Length;
      Work_Root (1 .. Work_Length) := Work;
      Raw := syscall (SYSCALL_SBRK, Buffer_Bytes + Page_Bytes);
      if Raw = Unsigned_64'Last then
         return;
      end if;
      Buffer_Address := (Raw + Page_Bytes - 1) and not Unsigned_64 (Page_Bytes - 1);
      CuBit.Memory_Grants.Create_Via_Capability
        (CCL_Native_Bindings.Filesystem, To_Address (Integer_Address (Buffer_Address)),
         Buffer_Pages, True, Loan, Ready);
      if not Ready then
         return;
      end if;
      --  The work root may exist already (and its parent may be outside
      --  this process's scopes): only the places below it must be made.
      Make_Directory (Work_Root (1 .. Work_Length), Ignore_Root);
      --  The places, made once (each exists afterwards, or this fails).
      Make_Directory (Work_Root (1 .. Work_Length) & Workspace_Directory, Made);
      if Made = W.OK then
         Make_Directory (Work_Root (1 .. Work_Length) & Mail_Directory, Made);
      end if;
      if Made = W.OK then
         Make_Directory (Work_Root (1 .. Work_Length) & To_Device_Directory, Made);
      end if;
      if Made = W.OK then
         Make_Directory (Work_Root (1 .. Work_Length) & To_Host_Directory, Made);
      end if;
      if Made = W.OK then
         Make_Directory (Work_Root (1 .. Work_Length) & Captures_Directory, Made);
      end if;
      Success := Made = W.OK;
   end Initialize;

   procedure Open (Text : String; Options : Open_Options; File : out File_Handle; Result : out W.Status) is
      Request : Message;
   begin
      File := INVALID_FILE_HANDLE;
      if Text'Length = 0 or else Text'Length > MAXIMUM_PATH_BYTES then
         Result := W.Malformed;
         return;
      end if;
      Place_Text (Text);
      Request := Open_Request (Loan, Text'Length, Options);
      Result := Call (Request);
      if Result = W.OK then
         File := File_Handle (Request.words (0));
      end if;
   end Open;

   procedure Close (File : File_Handle; Result : in out W.Status) is
      Request : Message := Close_Request (File);
      Closed : constant W.Status := Call (Request);
   begin
      if Result = W.OK then
         Result := Closed;
      end if;
   end Close;

   --  Read up to Count bytes at Offset into the buffer: Got of them.
   procedure Read_At (File : File_Handle; Offset : Unsigned_64; Count : Natural;
                      Got : out Natural; Result : out W.Status) is
      Request : Message := Read_At_Request (File, Loan, Unsigned_64 (Count), Offset);
   begin
      Result := Call (Request);
      Got := (if Result = W.OK and then Request.words (0) <= Unsigned_64 (Count)
              then Natural (Request.words (0)) else 0);
   end Read_At;

   procedure List
     (Where : W.Place; Name : String; Output : out W.Byte_Array; Length : out Natural;
      Result : out W.Status)
   is
      package Places renames CCL_Places;
      Entries : Places.Listing;
      Count : Places.Listed_Count;
      Total : Natural;
      Listed : Places.Result_Kind;

      procedure Append (Text : String) is
      begin
         for C of Text loop
            exit when Length = Output'Length;
            Length := Length + 1;
            Output (Length) := Character'Pos (C);
         end loop;
      end Append;

      function Decimal (Value : Unsigned_64) return String is
         Image : constant String := Value'Image;
      begin
         return Image (Image'First + 1 .. Image'Last);
      end Decimal;
   begin
      Output := [others => 0];
      Length := 0;
      Places.List (Path (Where, Name), Entries, Count, Total, Listed);
      case Listed is
         when Places.Listed_All => Result := W.OK;
         when Places.Not_Found => Result := W.Not_Found;
         when Places.Access_Denied => Result := W.Not_Granted;
         when Places.Unavailable => Result := W.Unavailable;
         when Places.Failed => Result := W.Failed;
      end case;
      if Result /= W.OK then
         return;
      end if;
      for I in 1 .. Count loop
         declare
            E : Places.Listed renames Entries (I);
         begin
            Append ((case E.Kind is
                        when CCL.Interfaces.Files.File => "file",
                        when CCL.Interfaces.Files.Directory => "dir",
                        when CCL.Interfaces.Files.Link => "link",
                        when CCL.Interfaces.Files.Other => "other") &
                    " " & Decimal (E.Size) & " " & E.Name (1 .. E.Name_Length) & ASCII.LF);
         end;
      end loop;
      if Total > Count then
         Append ("... " & Decimal (Unsigned_64 (Total - Count)) & " more" & ASCII.LF);
      end if;
   end List;

   --  The size and SHA-256 of the file the filesystem names Text.
   procedure Hash_Path
     (Text : String; Size : out Unsigned_64; Hash : out W.Digest; Result : out W.Status)
   is
      File : File_Handle;
      Got : Natural;
      Context : SHA.Context;
      Output : SHA.Digest;
   begin
      Size := 0;
      Hash := [others => 0];
      Open (Text, OPEN_READ_ONLY, File, Result);
      if Result /= W.OK then
         return;
      end if;
      SHA.Init (Context);
      loop
         Read_At (File, Size, Buffer_Bytes, Got, Result);
         exit when Result /= W.OK or else Got = 0;
         if Size + Unsigned_64 (Got) > W.Maximum_File_Bytes then
            Result := W.Too_Large;
            exit;
         end if;
         declare
            Chunk : SPARKNaCl.Byte_Seq (0 .. SPARKNaCl.N32 (Got) - 1)
              with Import, Address => To_Address (Integer_Address (Buffer_Address));
         begin
            SHA.Update (Context, Chunk);
         end;
         Size := Size + Unsigned_64 (Got);
         exit when Got < Buffer_Bytes;
      end loop;
      Close (File, Result);
      if Result /= W.OK then
         Size := 0;
         return;
      end if;
      SHA.Final (Context, Output);
      for I in W.Digest_Index loop
         Hash (I) := Output (SPARKNaCl.N32 (I) - 1);
      end loop;
   end Hash_Path;

   procedure Stat
     (Where : W.Place; Name : String; Size : out Unsigned_64; Hash : out W.Digest;
      Result : out W.Status) is
   begin
      Hash_Path (Path (Where, Name), Size, Hash, Result);
   end Stat;

   procedure Get
     (Where : W.Place; Name : String; Offset : W.File_Offset;
      Output : out W.Byte_Array; Length : out Natural; Result : out W.Status)
   is
      File : File_Handle;
   begin
      Output := [others => 0];
      Length := 0;
      Open (Path (Where, Name), OPEN_READ_ONLY, File, Result);
      if Result /= W.OK then
         return;
      end if;
      Read_At (File, Offset, W.Maximum_Chunk_Bytes, Length, Result);
      if Result = W.OK and then Length > 0 then
         declare
            Chunk : W.Byte_Array (1 .. Length)
              with Import, Address => To_Address (Integer_Address (Buffer_Address));
         begin
            Output (1 .. Length) := Chunk;
         end;
      end if;
      Close (File, Result);
      if Result /= W.OK then
         Length := 0;
      end if;
   end Get;

   function Staging (Name : String) return String is (Name & W.Staging_Suffix);

   procedure Put
     (Where : W.Place; Name : String; Offset : W.File_Offset; Data : W.Byte_Array;
      Result : out W.Status)
   is
      File : File_Handle;
      Request : Message;
   begin
      if Offset = 0 then
         --  A fresh upload: its directories, then an empty staging file.
         for I in Name'Range loop
            if Name (I) = '/' then
               Make_Directory (Path (Where, Name (Name'First .. I - 1)), Result);
               if Result /= W.OK then
                  return;
               end if;
            end if;
         end loop;
         Open (Path (Where, Staging (Name)), OPEN_READ_WRITE or OPEN_CREATE or OPEN_TRUNCATE, File, Result);
      else
         Open (Path (Where, Staging (Name)), OPEN_READ_WRITE, File, Result);
      end if;
      if Result /= W.OK then
         return;
      end if;
      if Data'Length > 0 then
         declare
            Chunk : W.Byte_Array (1 .. Data'Length)
              with Import, Address => To_Address (Integer_Address (Buffer_Address));
         begin
            Chunk := Data;
         end;
         Request := Write_At_Request (File, Loan, Unsigned_64 (Data'Length), Offset);
         Result := Call (Request);
         if Result = W.OK and then Request.words (0) /= Unsigned_64 (Data'Length) then
            Result := W.Failed;
         end if;
      end if;
      Close (File, Result);
   end Put;

   procedure Commit
     (Where : W.Place; Name : String; Size : W.File_Offset; Hash : W.Digest;
      Result : out W.Status)
   is
      Actual_Size : Unsigned_64;
      Actual_Hash : W.Digest;
      Request : Message;
   begin
      Stat (Where, Staging (Name), Actual_Size, Actual_Hash, Result);
      if Result /= W.OK then
         return;
      end if;
      if Actual_Size /= Size or else Actual_Hash /= Hash then
         Result := W.Integrity_Mismatch;
         return;
      end if;
      declare
         From : constant String := Path (Where, Staging (Name));
         To : constant String := Path (Where, Name);
      begin
         Place_Text (From & To);
         Request := Rename_Request (Loan, From'Length, To'Length);
         Result := Call (Request);
      end;
   end Commit;

   procedure Remove (Where : W.Place; Name : String; Result : out W.Status) is
      Text : constant String := Path (Where, Name);
      Request : Message;
   begin
      Place_Text (Text);
      Request := Path_Request (OP_UNLINK, Text'Length);
      Result := Call (Request);
   end Remove;

   ---------------------------------------------------------------------------
   --  The work root's own files
   ---------------------------------------------------------------------------
   function Work_Path (Name : String) return String is
     (Work_Root (1 .. Work_Length) & "/" & Name);

   procedure Hash_Work
     (Name : String; Size : out Unsigned_64; Hash : out W.Digest; Result : out W.Status) is
   begin
      Hash_Path (Work_Path (Name), Size, Hash, Result);
   end Hash_Work;

   procedure Make_Work_Directory (Name : String; Result : out W.Status) is
   begin
      Make_Directory (Work_Path (Name), Result);
   end Make_Work_Directory;

   --  Rename the work root's From over To.
   procedure Rename_Work (From, To : String; Result : out W.Status) is
      Old_Path : constant String := Work_Path (From);
      New_Path : constant String := Work_Path (To);
      Request : Message;
   begin
      Place_Text (Old_Path & New_Path);
      Request := Rename_Request (Loan, Old_Path'Length, New_Path'Length);
      Result := Call (Request);
   end Rename_Work;

   --  Write the buffer's first Count bytes to File at Offset.
   procedure Write_At (File : File_Handle; Offset : Unsigned_64; Count : Natural; Result : out W.Status) is
      Request : Message := Write_At_Request (File, Loan, Unsigned_64 (Count), Offset);
   begin
      Result := Call (Request);
      if Result = W.OK and then Request.words (0) /= Unsigned_64 (Count) then
         Result := W.Failed;
      end if;
   end Write_At;

   procedure Copy_To_Work
     (Where : W.Place; Name : String; Target : String;
      Size : out Unsigned_64; Hash : out W.Digest; Result : out W.Status)
   is
      Source, Copy : File_Handle;
      Got : Natural;
      Context : SHA.Context;
      Output : SHA.Digest;
      Closed : W.Status := W.OK;
   begin
      Size := 0;
      Hash := [others => 0];
      Open (Path (Where, Name), OPEN_READ_ONLY, Source, Result);
      if Result /= W.OK then
         return;
      end if;
      Open (Work_Path (Staging (Target)), OPEN_READ_WRITE or OPEN_CREATE or OPEN_TRUNCATE, Copy, Result);
      if Result /= W.OK then
         Close (Source, Closed);
         return;
      end if;
      SHA.Init (Context);
      loop
         Read_At (Source, Size, Buffer_Bytes, Got, Result);
         exit when Result /= W.OK or else Got = 0;
         if Size + Unsigned_64 (Got) > W.Maximum_File_Bytes then
            Result := W.Too_Large;
            exit;
         end if;
         declare
            Chunk : SPARKNaCl.Byte_Seq (0 .. SPARKNaCl.N32 (Got) - 1)
              with Import, Address => To_Address (Integer_Address (Buffer_Address));
         begin
            SHA.Update (Context, Chunk);
         end;
         Write_At (Copy, Size, Got, Result);
         exit when Result /= W.OK;
         Size := Size + Unsigned_64 (Got);
         exit when Got < Buffer_Bytes;
      end loop;
      Close (Source, Result);
      Close (Copy, Result);
      if Result = W.OK then
         Rename_Work (Staging (Target), Target, Result);
      end if;
      if Result /= W.OK then
         Size := 0;
         return;
      end if;
      SHA.Final (Context, Output);
      for I in W.Digest_Index loop
         Hash (I) := Output (SPARKNaCl.N32 (I) - 1);
      end loop;
   end Copy_To_Work;

   procedure Write_Work_Text (Name : String; Text : String; Result : out W.Status) is
      File : File_Handle;
   begin
      Open (Work_Path (Staging (Name)), OPEN_READ_WRITE or OPEN_CREATE or OPEN_TRUNCATE, File, Result);
      if Result /= W.OK then
         return;
      end if;
      if Text'Length > 0 then
         Place_Text (Text);
         Write_At (File, 0, Text'Length, Result);
      end if;
      Close (File, Result);
      if Result = W.OK then
         Rename_Work (Staging (Name), Name, Result);
      end if;
   end Write_Work_Text;

   procedure Read_Work_Text
     (Name : String; Text : out String; Length : out Natural; Result : out W.Status)
   is
      File : File_Handle;
      Got : Natural;
   begin
      Text := [others => ' '];
      Length := 0;
      Open (Work_Path (Name), OPEN_READ_ONLY, File, Result);
      if Result /= W.OK then
         return;
      end if;
      --  One byte more than Text holds tells a longer file.
      Read_At (File, 0, Text'Length + 1, Got, Result);
      if Result = W.OK and then Got > Text'Length then
         Result := W.Too_Large;
      elsif Result = W.OK and then Got > 0 then
         declare
            Bytes : String (1 .. Got)
              with Import, Address => To_Address (Integer_Address (Buffer_Address));
         begin
            Text (Text'First .. Text'First + Got - 1) := Bytes;
            Length := Got;
         end;
      end if;
      Close (File, Result);
      if Result /= W.OK then
         Length := 0;
      end if;
   end Read_Work_Text;
end Remote_Files;

with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Filesystems; use CuBit.Filesystems;
with CuBit.Filesystem_Queues;
with CuBit.Directory_Pages;
with CuBit.Channel_Protocol;
with CuBit.Channels;
with CuBit.Memory_Grants;
with CCL_Native_Bindings;

--  Places through the filesystem's request queue (docs/filesystem-data-
--  plane.md): one queue and arena lent once, then a listing is a handful of
--  queue entries (open, a few pairs of directory and inspection pages per
--  request, close) instead of a message and a grant per page.
package body CCL_Places is
   use Interfaces;
   package FQ renames CuBit.Filesystem_Queues;
   package Q renames FQ.Queues;
   use type Q.Token;

   --  The roots a CCL app's manifest grants (filesystem-scope), preferred
   --  first: persistent storage, then the in-memory volume.
   ROOT_COUNT : constant := 2;
   function Root_Text (Index : Positive) return String is
     (if Index = 1 then "@nvme:0/work" else "@mem:0/work");

   PAGE_BYTES : constant := DIRECTORY_PAGE_BYTES;
   --  Room for sixteen Directory.Page.V2 pages per request.
   PAGES_PER_REQUEST : constant := 16;
   ARENA_BYTES : constant := PAGES_PER_REQUEST * PAGE_BYTES;
   ARENA_PAGES : constant := ARENA_BYTES / PAGE_BYTES;
   MAXIMUM_REQUESTS : constant := 64;   --  per listing: 512 pages
   ANSWER_SPINS : constant := 256;

   --  The queue's channels (FQ): a small transfer arena, then the pair.
   Transfer_Link, Queue_Link : CuBit.Channels.Channel;
   Queue_Address, Server_Address, Arena_Address : Unsigned_64 := 0;
   Ready, Refused : Boolean := False;
   Client : Q.Client;
   Next_Tag : Q.Token := 0;
   Kicked : Unsigned_32 := 0;

   --  This client's region (it writes) and the service's (read-only here).
   function Word (Offset : Natural) return System.Address is
     (To_Address (Integer_Address (Queue_Address + Unsigned_64 (Offset))));
   function Server_Word (Offset : Natural) return System.Address is
     (To_Address (Integer_Address (Server_Address + Unsigned_64 (Offset))));

   --  Open the queue's channels once: no dirty arena (listings never
   --  write).
   function Initialize return Boolean is
      use type CuBit.Channels.Open_Result;
      Result : CuBit.Channels.Open_Result;
      Ignore_Refusal : CuBit.Channel_Protocol.Open_Refusal;
   begin
      if Ready then return True; end if;
      if Refused then return False; end if;
      Refused := True;
      CuBit.Channels.Open
        (CCL_Native_Bindings.Filesystem, (FQ.TRANSFER_CONTRACT with delta Buffers => ARENA_PAGES),
         CuBit.Channels.Producing, Transfer_Link, Result, Ignore_Refusal,
         Connector => FQ.Transfer_Connector);
      if Result /= CuBit.Channels.Opened then return False; end if;
      CuBit.Channels.Open
        (CCL_Native_Bindings.Filesystem, FQ.QUEUE_CONTRACT, CuBit.Channels.Producing, Queue_Link, Result,
         Ignore_Refusal, Connector => FQ.Queue_Connector);
      if Result /= CuBit.Channels.Opened then
         CuBit.Channels.Close (Transfer_Link);
         return False;
      end if;
      Queue_Address := Queue_Link.Own_Base;
      Server_Address := Queue_Link.Peer_Base;
      Arena_Address := Unsigned_64 (To_Integer (CuBit.Channels.Buffer_Address (Transfer_Link, 0)));
      Ready := True;
      Refused := False;
      return Ready;
   end Initialize;

   --  One request through the queue, waiting for its answer.
   procedure Call
     (Operation : Unsigned_32; Handle, Length : Unsigned_64;
      Status : out Unsigned_32; Value : out Unsigned_64; Options : Unsigned_32 := 0)
   is
      --  The rings are written and read only inside the proved Submit and
      --  Reap; the shared header words are volatile.
      Requests : Q.Submissions.Ring with Import, Address => Word (FQ.Client_Requests_At);
      Answers : constant Q.Completions.Ring with Import, Address => Server_Word (FQ.Server_Answers_At);
      Submitted : Unsigned_32 with Import, Volatile, Address => Word (FQ.Client_Submitted_At);
      Taken : constant Unsigned_32 with Import, Volatile, Address => Server_Word (FQ.Server_Taken_At);
      Wake : constant Unsigned_32 with Import, Volatile, Address => Server_Word (FQ.Server_Wake_At);
      Answered : constant Unsigned_32 with Import, Volatile, Address => Server_Word (FQ.Server_Answered_At);
      Reaped : Unsigned_32 with Import, Volatile, Address => Word (FQ.Client_Reaped_At);
      Result : Q.Completion;
      Accepted : Boolean;
      Waiting : Message := NULL_MESSAGE;
      Tag : MessageTag;
      Armed : Unsigned_32;
   begin
      Status := REPLY_ERR;
      Value := 0;
      Q.Accept_Taken (Client, Q.Submissions.Index (Taken), Accepted);
      if not Q.Can_Submit (Client) then return; end if;
      Next_Tag := Next_Tag + 1;
      Q.Submit (Client, Requests, Next_Tag,
                (Operation => Operation, Options => Options, Handle => Handle, Length => Length,
                 others => <>));
      Submitted := Unsigned_32 (Client.Requests.Produced);
      --  The count before the wake word: kick only a service that sleeps.
      Armed := Wake;
      if Armed /= 0 and then Armed /= Kicked then
         Kicked := Armed;
         CuBit.Channels.Kick (Queue_Link);
      end if;
      for Look in 1 .. Natural'Last loop
         Q.Completions.Accept_Produced (Client.Answers, Q.Completions.Index (Answered), Accepted);
         exit when Client.Answers.Available > 0;
         if Look > ANSWER_SPINS then
            --  Block until an answer waits (the wake is answered then).
            Waiting.tag := (label => FQ.OP_FS_WAKE, length => 0, protocol => 0, ipc => <>, reserved => 0);
            Tag := capCall (CCL_Native_Bindings.Filesystem, Waiting, CuBit.Messages.Wait_Forever);
         end if;
      end loop;
      Q.Reap (Client, Answers, Result, Accepted);
      Reaped := Unsigned_32 (Client.Answers.Consumed);
      if Accepted and then Result.Tag = Next_Tag then
         Status := Result.Answer.Status;
         Value := Result.Answer.Value;
      end if;
   end Call;

   function Outcome (Label : Unsigned_32) return Result_Kind is
     (case Label is
         when REPLY_OK => Listed_All,
         when REPLY_NOT_FOUND => Not_Found,
         when REPLY_ACCESS_DENIED => Access_Denied,
         when others => Failed);

   procedure Open (Path : String; Directory : out Unsigned_64; Result : out Result_Kind) is
      Status : Unsigned_32;
   begin
      Directory := 0;
      if Path'Length = 0 or else Path'Length > ARENA_BYTES then
         Result := Not_Found; return;
      end if;
      declare
         Bytes : String (1 .. Path'Length)
           with Import, Address => To_Address (Integer_Address (Arena_Address));
      begin
         Bytes := Path;
      end;
      Call (FQ.Queue_Open_Directory, 0, Path'Length, Status, Directory);
      Result := Outcome (Status);
   end Open;

   procedure Close (Directory : Unsigned_64) is
      Status : Unsigned_32;
      Ignored : Unsigned_64;
   begin
      Call (FQ.Queue_Close_Directory, Directory, 0, Status, Ignored);
   end Close;

   Home_Index : Natural := 0;

   function Home return String is
      Directory : Unsigned_64;
      Result : Result_Kind;
   begin
      if Home_Index = 0 and then Initialize then
         for Index in 1 .. ROOT_COUNT loop
            Open (Root_Text (Index), Directory, Result);
            if Result = Listed_All then
               Close (Directory);
               Home_Index := Index;
               exit;
            end if;
         end loop;
      end if;
      return (if Home_Index = 0 then "" else Root_Text (Home_Index));
   end Home;

   --  Take one Directory.Page.V2 from the arena: copied, then checked.
   procedure Take_Page
     (Index : Natural; Entries : in out Listing; Count : in out Listed_Count;
      Total : in out Natural; Valid, At_End : out Boolean)
   is
      package DP renames CuBit.Directory_Pages;
      Shared : constant DP.Page
        with Import, Address => To_Address (Integer_Address (Arena_Address + Unsigned_64 (Index * PAGE_BYTES)));
      Copy : constant DP.Page := Shared;
      Entry_Count : DP.Entry_Count;
      Used : DP.Used_Bytes;
      Resume, Stamp : Unsigned_64;
      At_Entry : Natural := DP.Header_Bytes;
      Item : DP.Facts;
      Name : DP.Name_Bytes;
      Length : DP.Name_Length;
      Next : Natural;
      OK : Boolean;
   begin
      DP.Check (Copy, Valid, Entry_Count, Used, At_End, Resume, Stamp);
      if not Valid then return; end if;
      for Ordinal in 1 .. Entry_Count loop
         DP.Get (Copy, At_Entry, Used, Item, Name, Length, Next, OK);
         exit when not OK;   --  Check accepted every entry
         At_Entry := Next;
         if Total < Natural'Last then Total := Total + 1; end if;
         if Count < Files.MAXIMUM_LISTED then
            Count := Count + 1;
            Length := Natural'Min (Length, Files.MAXIMUM_NAME);
            for C in 1 .. Length loop
               Entries (Count).Name (C) := Character'Val (Name (C));
            end loop;
            Entries (Count).Name_Length := Length;
            Entries (Count).Kind :=
              (case Item.Kind is
                  when DIRECTORY_KIND_FILE => Files.File,
                  when DIRECTORY_KIND_DIRECTORY => Files.Directory,
                  when DIRECTORY_KIND_SYMLINK => Files.Link,
                  when others => Files.Other);
            Entries (Count).Size := (if (Item.Valid and INSPECTED_SIZE) /= 0 then Item.Size else 0);
            if (Item.Valid and INSPECTED_TIMES) /= 0 then
               Entries (Count).Modified_Ms := Item.Modified;
            end if;
            if (Item.Valid and INSPECTED_MODE) /= 0 then
               Entries (Count).Mode := Natural (Item.Mode and 16#FFFF#);
            end if;
            if (Item.Valid and INSPECTED_LINKS) /= 0 then
               Entries (Count).Links := Natural (Item.Links and 16#FFFF#);
            end if;
         end if;
      end loop;
   end Take_Page;

   procedure List
     (Path : String; Entries : out Listing; Count : out Listed_Count;
      Total : out Natural; Result : out Result_Kind)
   is
      Directory : Unsigned_64;
      Status : Unsigned_32;
      Filled : Unsigned_64;
      Valid, At_End : Boolean := False;
   begin
      Entries := [others => <>];
      Count := 0;
      Total := 0;
      if not Initialize then Result := Unavailable; return; end if;
      Open (Path, Directory, Result);
      if Result /= Listed_All then return; end if;
      for Request in 1 .. MAXIMUM_REQUESTS loop
         Call (FQ.Queue_Read_Directory, Directory, ARENA_BYTES, Status, Filled,
               Options => FQ.Directory_Metadata);
         Result := Outcome (Status);
         exit when Result /= Listed_All;
         if Filled > PAGES_PER_REQUEST then Result := Failed; exit; end if;
         for Index in 0 .. Natural (Filled) - 1 loop
            Take_Page (Index, Entries, Count, Total, Valid, At_End);
            if not Valid then Result := Failed; end if;
            exit when not Valid or else At_End;
         end loop;
         exit when Result /= Listed_All or else At_End or else Filled < PAGES_PER_REQUEST;
      end loop;
      Close (Directory);
   end List;

   ---------------------------------------------------------------------------
   --  Files: the filesystem's message requests through one page lent once
   --  (a file's bytes a page at a time; never a path outside the scopes the
   --  service checks on every open).
   ---------------------------------------------------------------------------
   File_Loan : CuBit.Memory_Grants.Grant_Reference;
   File_Page : Unsigned_64 := 0;
   File_Ready : Boolean := False;

   function File_Initialize return Boolean is
      Raw : Unsigned_64;
   begin
      if File_Ready then return True; end if;
      if File_Page = 0 then
         Raw := syscall (SYSCALL_SBRK, 2 * PAGE_BYTES);
         if Raw = Unsigned_64'Last then return False; end if;
         File_Page := (Raw + PAGE_BYTES - 1) and not Unsigned_64 (PAGE_BYTES - 1);
      end if;
      CuBit.Memory_Grants.Create_Via_Capability
        (CCL_Native_Bindings.Filesystem, To_Address (Integer_Address (File_Page)), 1, True, File_Loan, File_Ready);
      return File_Ready;
   end File_Initialize;

   procedure Put_Page (Text : String) is
      Bytes : String (1 .. Text'Length) with Import, Address => To_Address (Integer_Address (File_Page));
   begin
      Bytes := Text;
   end Put_Page;

   function File_Call (Request : in out Message) return Result_Kind is
   begin
      Request.tag := capCall (CCL_Native_Bindings.Filesystem, Request, CuBit.Messages.Wait_Forever);
      return (if Request.tag.label = REPLY_ALREADY_EXISTS then Listed_All else Outcome (Request.tag.label));
   end File_Call;

   procedure Open_File (Path : String; Options : Open_Options; File : out File_Handle; Result : out Result_Kind) is
      Request : Message;
   begin
      File := File_Handle (0);
      if not File_Initialize then Result := Unavailable; return; end if;
      if Path'Length = 0 or else Path'Length > PAGE_BYTES then Result := Not_Found; return; end if;
      Put_Page (Path);
      Request := Open_Request (File_Loan, Path'Length, Options);
      Request.tag := capCall (CCL_Native_Bindings.Filesystem, Request, CuBit.Messages.Wait_Forever);
      Result := Outcome (Request.tag.label);
      if Result = Listed_All then
         File := File_Handle (Request.words (0));
      end if;
   end Open_File;

   procedure Close_File (File : File_Handle) is
      Request : Message := Close_Request (File);
      Ignored : Result_Kind;
   begin
      Ignored := File_Call (Request);
   end Close_File;

   procedure Read_At
     (Path : String; Offset : Natural; Into : out String; Last : out Natural; Result : out Result_Kind)
   is
      File : File_Handle;
      Request : Message;
      Got : Natural := 0;
   begin
      Into := [others => ' '];
      Last := Into'First - 1;
      Open_File (Path, OPEN_READ_ONLY, File, Result);
      if Result /= Listed_All then return; end if;
      while Got < Into'Length loop
         declare
            Wanted : constant Natural := Natural'Min (PAGE_BYTES, Into'Length - Got);
            Bytes : String (1 .. PAGE_BYTES) with Import, Address => To_Address (Integer_Address (File_Page));
            N : Natural;
         begin
            Request := Read_At_Request (File, File_Loan, Unsigned_64 (Wanted), Unsigned_64 (Offset + Got));
            Result := File_Call (Request);
            exit when Result /= Listed_All;
            N := Natural (Unsigned_64'Min (Request.words (0), Unsigned_64 (Wanted)));
            exit when N = 0;
            Into (Into'First + Got .. Into'First + Got + N - 1) := Bytes (1 .. N);
            Got := Got + N;
            exit when N < Wanted;
         end;
      end loop;
      Last := Into'First + Got - 1;
      Close_File (File);
   end Read_At;

   procedure Create (Path : String; Result : out Result_Kind) is
      File : File_Handle;
   begin
      Open_File (Path, OPEN_READ_WRITE or OPEN_CREATE or OPEN_EXCLUSIVE, File, Result);
      if Result = Listed_All then
         Close_File (File);
      elsif Result = Failed then
         --  It exists already (REPLY_ALREADY_EXISTS): left as it is.
         Open_File (Path, OPEN_READ_ONLY, File, Result);
         if Result = Listed_All then Close_File (File); end if;
      end if;
   end Create;

   procedure Write_At (Path : String; Offset : Natural; Text : String; Result : out Result_Kind) is
      File : File_Handle;
      Request : Message;
      Done : Natural := 0;
   begin
      Open_File (Path, OPEN_READ_WRITE, File, Result);
      if Result /= Listed_All then return; end if;
      while Done < Text'Length loop
         declare
            N : constant Natural := Natural'Min (PAGE_BYTES, Text'Length - Done);
         begin
            Put_Page (Text (Text'First + Done .. Text'First + Done + N - 1));
            Request := Write_At_Request (File, File_Loan, Unsigned_64 (N), Unsigned_64 (Offset + Done));
            Result := File_Call (Request);
            if Result = Listed_All and then Request.words (0) /= Unsigned_64 (N) then
               Result := Failed;
            end if;
            exit when Result /= Listed_All;
            Done := Done + N;
         end;
      end loop;
      Close_File (File);
   end Write_At;
end CCL_Places;

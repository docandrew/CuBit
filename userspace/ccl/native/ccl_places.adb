with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Filesystems; use CuBit.Filesystems;
with CuBit.Filesystem_Queues;
with CuBit.Memory_Grants;

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
   QUEUE_PAGES : constant := FQ.Queue_Bytes / PAGE_BYTES;
   --  Room for eight page and inspection pairs per request.
   PAIRS_PER_REQUEST : constant := 8;
   ARENA_BYTES : constant := PAIRS_PER_REQUEST * DIRECTORY_INSPECTED_BYTES;
   ARENA_PAGES : constant := ARENA_BYTES / PAGE_BYTES;
   MAXIMUM_REQUESTS : constant := 64;   --  per listing: 512 pages
   ANSWER_SPINS : constant := 256;
   NO_COMPLETION_TOKEN : constant Unsigned_64 := Unsigned_64'Last;

   Queue_Address, Arena_Address : Unsigned_64 := 0;
   Ready, Refused : Boolean := False;
   Client : Q.Client;
   Next_Tag : Q.Token := 0;
   Kicked : Unsigned_32 := 0;

   function Word (Offset : Natural) return System.Address is
     (To_Address (Integer_Address (Queue_Address + Unsigned_64 (Offset))));

   --  Page-aligned memory of Pages pages.
   function Pages_Of (Pages : Positive) return Unsigned_64 is
      Raw : constant Unsigned_64 := syscall (SYSCALL_SBRK, (Unsigned_64 (Pages) + 1) * PAGE_BYTES);
   begin
      if Raw = Unsigned_64'Last then return 0; end if;
      return (Raw + PAGE_BYTES - 1) and not Unsigned_64 (PAGE_BYTES - 1);
   end Pages_Of;

   function Lend (Address : Unsigned_64; Pages : Positive) return Unsigned_64 is
      Loan : CuBit.Memory_Grants.Grant_Reference;
      OK : Boolean;
      GENERATION_SHIFT : constant := 32;
   begin
      CuBit.Memory_Grants.Create_Via_Capability
        (CAP_SLOT_FS, To_Address (Integer_Address (Address)), Pages, True, Loan, OK);
      return (if OK then Shift_Left (Loan.generation, GENERATION_SHIFT) or Loan.slot else 0);
   end Lend;

   --  Lend the queue and arena once.
   function Initialize return Boolean is
      Request : Message := NULL_MESSAGE;
      Queue_Ref, Arena_Ref : Unsigned_64;
   begin
      if Ready then return True; end if;
      if Refused then return False; end if;
      Refused := True;
      Queue_Address := Pages_Of (QUEUE_PAGES);
      Arena_Address := Pages_Of (ARENA_PAGES);
      if Queue_Address = 0 or else Arena_Address = 0 then return False; end if;
      Queue_Ref := Lend (Queue_Address, QUEUE_PAGES);
      Arena_Ref := Lend (Arena_Address, ARENA_PAGES);
      if Queue_Ref = 0 or else Arena_Ref = 0 then return False; end if;
      Request.tag := (label => FQ.OP_FS_QUEUE, length => 4, flags => 0, reserved => 0);
      Request.words (0) := Queue_Ref;
      Request.words (1) := Arena_Ref;
      Request.words (2) := ARENA_BYTES;
      Request.words (3) := 0;   --  no dirty arena: listings never write
      Request.tag := capCall (CAP_SLOT_FS, Request);
      Ready := Request.tag.label = REPLY_OK;
      Refused := not Ready;
      return Ready;
   end Initialize;

   --  One request through the queue, waiting for its answer.
   procedure Call
     (Operation : Unsigned_32; Handle, Length : Unsigned_64;
      Status : out Unsigned_32; Value : out Unsigned_64)
   is
      --  The rings are written and read only inside the proved Submit and
      --  Reap; the shared header words are volatile.
      Requests : Q.Submissions.Ring with Import, Address => Word (FQ.Requests_At);
      Answers : Q.Completions.Ring with Import, Address => Word (FQ.Answers_At);
      Submitted : Unsigned_32 with Import, Volatile, Address => Word (FQ.Submissions_At + FQ.Produced_At);
      Taken : Unsigned_32 with Import, Volatile, Address => Word (FQ.Submissions_At + FQ.Consumed_At);
      Wake : Unsigned_32 with Import, Volatile, Address => Word (FQ.Submissions_At + FQ.Wake_At);
      Answered : Unsigned_32 with Import, Volatile, Address => Word (FQ.Completions_At + FQ.Produced_At);
      Reaped : Unsigned_32 with Import, Volatile, Address => Word (FQ.Completions_At + FQ.Consumed_At);
      Result : Q.Completion;
      Accepted : Boolean;
      Waiting : Message := NULL_MESSAGE;
      Tag : MessageTag;
      Sent : Boolean;
      Armed : Unsigned_32;
   begin
      Status := REPLY_ERR;
      Value := 0;
      Q.Accept_Taken (Client, Q.Submissions.Index (Taken), Accepted);
      if not Q.Can_Submit (Client) then return; end if;
      Next_Tag := Next_Tag + 1;
      Q.Submit (Client, Requests, Next_Tag,
                (Operation => Operation, Handle => Handle, Length => Length, others => <>));
      Submitted := Unsigned_32 (Client.Requests.Produced);
      --  The count before the wake word: kick only a service that sleeps.
      Armed := Wake;
      if Armed /= 0 and then Armed /= Kicked then
         Kicked := Armed;
         Sent := capSubmit (CAP_SLOT_FS,
                            (tag => (label => FQ.OP_FS_KICK, length => 0, flags => 0, reserved => 0),
                             authorityTag => 0, words => [others => 0]),
                            NO_COMPLETION_TOKEN);
      end if;
      for Look in 1 .. Natural'Last loop
         Q.Completions.Accept_Produced (Client.Answers, Q.Completions.Index (Answered), Accepted);
         exit when Client.Answers.Available > 0;
         if Look > ANSWER_SPINS then
            --  Block until an answer waits (OP_FS_WAIT completes then).
            Waiting.tag := (label => FQ.OP_FS_WAIT, length => 0, flags => 0, reserved => 0);
            Tag := capCall (CAP_SLOT_FS, Waiting);
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

   --  Take one page and inspection pair from the arena.
   procedure Take_Pair
     (Pair : Natural; Entries : in out Listing; Count : in out Listed_Count;
      Total : in out Natural; Valid, At_End : out Boolean)
   is
      Base : constant Unsigned_64 := Arena_Address + Unsigned_64 (Pair * DIRECTORY_INSPECTED_BYTES);
      Header : Directory_Page_Header with Import, Address => To_Address (Integer_Address (Base));
      Page_Entries : Directory_Entries
        with Import, Address => To_Address (Integer_Address (Base + DIRECTORY_PAGE_HEADER_BYTES));
      Inspections : Directory_Inspections
        with Import, Address => To_Address (Integer_Address (Base + PAGE_BYTES));
   begin
      Valid := Header.version = PROTOCOL_VERSION and then
        Header.headerBytes = DIRECTORY_PAGE_HEADER_BYTES and then
        Header.entryBytes = DIRECTORY_ENTRY_BYTES and then
        Header.entryCount <= MAXIMUM_DIRECTORY_PAGE_ENTRIES and then
        (Header.flags and not DIRECTORY_PAGE_END) = 0;
      At_End := (Header.flags and DIRECTORY_PAGE_END) /= 0;
      if not Valid then return; end if;
      for Index in 0 .. Natural (Header.entryCount) - 1 loop
         declare
            E : Directory_Entry renames Page_Entries (Index);
            I : Entry_Inspection renames Inspections (Index);
            Length : constant Natural := Natural'Min (Natural (E.nameLength), Files.MAXIMUM_NAME);
         begin
            if Total < Natural'Last then Total := Total + 1; end if;
            if Count < Files.MAXIMUM_LISTED and then Length > 0 then
               Count := Count + 1;
               for C in 1 .. Length loop
                  Entries (Count).Name (C) := Character'Val (E.name (C));
               end loop;
               Entries (Count).Name_Length := Length;
               Entries (Count).Kind :=
                 (case E.kind is
                     when DIRECTORY_KIND_FILE => Files.File,
                     when DIRECTORY_KIND_DIRECTORY => Files.Directory,
                     when DIRECTORY_KIND_SYMLINK => Files.Link,
                     when others => Files.Other);
               Entries (Count).Size :=
                 (if (I.valid and INSPECTED_SIZE) /= 0 then I.sizeBytes
                  elsif (E.flags and DIRECTORY_ENTRY_SIZE_VALID) /= 0 then E.sizeBytes
                  else 0);
               if (I.valid and INSPECTED_TIMES) /= 0 then
                  Entries (Count).Modified_Ms := I.modifiedMs;
               end if;
               if (I.valid and INSPECTED_MODE) /= 0 then
                  Entries (Count).Mode := Natural (I.mode and 16#FFFF#);
               end if;
               if (I.valid and INSPECTED_LINKS) /= 0 then
                  Entries (Count).Links := Natural (I.links and 16#FFFF#);
               end if;
            end if;
         end;
      end loop;
   end Take_Pair;

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
         Call (FQ.Queue_Read_Directory_Inspected, Directory, ARENA_BYTES, Status, Filled);
         Result := Outcome (Status);
         exit when Result /= Listed_All;
         if Filled > PAIRS_PER_REQUEST then Result := Failed; exit; end if;
         for Pair in 0 .. Natural (Filled) - 1 loop
            Take_Pair (Pair, Entries, Count, Total, Valid, At_End);
            if not Valid then Result := Failed; end if;
            exit when not Valid or else At_End;
         end loop;
         exit when Result /= Listed_All or else At_End or else Filled < PAIRS_PER_REQUEST;
      end loop;
      Close (Directory);
   end List;
end CCL_Places;

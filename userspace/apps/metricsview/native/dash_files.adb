pragma Ada_2022;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Filesystem_Queues;
with CuBit.Filesystem_Sessions;
with CuBit.Filesystems;
with CuBit.Monotonic;

package body Dash_Files is
   package FQ renames CuBit.Filesystem_Queues;
   package FS renames CuBit.Filesystem_Sessions;
   package Files renames CuBit.Filesystems;
   use type FS.Token;
   use type Files.Open_Options;
   use type FS.Wait_Result;

   --  The transfer arena: two paths back to back (a rename names both),
   --  then the data. A dashboard is a few kilobytes; larger values go in
   --  chunks of the data area.
   Arena_Pages : constant := 16;
   Path_At : constant := 0;
   Maximum_Path : constant := 1_024;
   Data_At : constant := 2 * Maximum_Path;
   Sibling_Suffix : constant String := ".saving";

   Session : FS.Session;
   Reply : Unsigned_32 := 0;

   procedure Open (Endpoint : CuBit.Handles.Endpoint; Ready : out Boolean) is
   begin
      FS.Open (Session, Endpoint, Arena_Pages, Ready);
   end Open;

   function Is_Open return Boolean is (FS.Is_Open (Session));
   function Last_Reply return Unsigned_32 is (Reply);

   function Data_Bytes return Unsigned_64 is
     (if FS.Arena_Bytes (Session) > Data_At then FS.Arena_Bytes (Session) - Data_At else 0);

   procedure Put (Offset : Unsigned_64; Bytes : String) is
      Target : String (1 .. Bytes'Length) with Import, Address => FS.Arena (Session) + Storage_Offset (Offset);
   begin
      Target := Bytes;
   end Put;

   --  Copy, then validate: the service also writes the arena.
   procedure Take (Offset : Unsigned_64; Into : out String) is
      Source : constant String (1 .. Into'Length)
        with Import, Address => FS.Arena (Session) + Storage_Offset (Offset);
   begin
      Into := Source;
   end Take;

   --  One request and its answer, waiting until Deadline_Ms.
   procedure Call (Item : FQ.Request; Deadline_Ms : Unsigned_64; Status : out Unsigned_32; Value : out Unsigned_64;
                   Result : out Outcome);
   procedure Call (Item : FQ.Request; Deadline_Ms : Unsigned_64; Status : out Unsigned_32; Value : out Unsigned_64;
                   Result : out Outcome)
   is
      Tag : FS.Token;
      Answer : FS.Q.Completion;
      Got : Boolean;
      Waited : FS.Wait_Result;
   begin
      Status := 0;
      Value := 0;
      if not FS.Is_Open (Session) or else not FS.Can_Submit (Session) then
         Result := Unavailable;
         return;
      end if;
      FS.Submit (Session, Item, Tag);
      loop
         FS.Reap (Session, Answer, Got);
         if Got and then Answer.Tag = Tag then
            Status := Answer.Answer.Status;
            Value := Answer.Answer.Value;
            Result := (if Status = Files.REPLY_OK then Done
                       elsif Status = Files.REPLY_NOT_FOUND then Not_Found
                       elsif Status = Files.REPLY_ACCESS_DENIED then Not_Granted
                       else Failed);
            if Result /= Done then
               Reply := Status;
            end if;
            return;
         elsif not Got then
            if CuBit.Monotonic.Milliseconds >= Deadline_Ms then
               Result := Late;
               return;
            end if;
            FS.Wait_Answer (Session, Deadline_Ms, Waited);
            if Waited = FS.Queue_Ended then
               Result := Unavailable;
               return;
            end if;
         end if;
      end loop;
   end Call;

   procedure Close_Handle (Handle : Unsigned_64; Deadline_Ms : Unsigned_64; Result : out Outcome);
   procedure Close_Handle (Handle : Unsigned_64; Deadline_Ms : Unsigned_64; Result : out Outcome) is
      Status : Unsigned_32;
      Value : Unsigned_64;
   begin
      Call ((Operation => FQ.Queue_Close, Handle => Handle, others => <>), Deadline_Ms, Status, Value, Result);
   end Close_Handle;

   procedure Load (Name : String; Into : out String; Got : out Natural; Deadline_Ms : Unsigned_64;
                   Result : out Outcome)
   is
      Status : Unsigned_32;
      Handle, Count : Unsigned_64;
      Closed : Outcome;
   begin
      Got := 0;
      Into := [others => ' '];
      if Name'Length = 0 or else Name'Length > Maximum_Path then
         Result := Failed;
         return;
      end if;
      Put (Path_At, Name);
      Call ((Operation => FQ.Queue_Open, Options => Unsigned_32 (Files.OPEN_READ_ONLY),
             Length => Unsigned_64 (Name'Length), Arena_Offset => Path_At, others => <>),
            Deadline_Ms, Status, Handle, Result);
      if Result /= Done then
         return;
      end if;
      loop
         declare
            Room : constant Unsigned_64 := Unsigned_64'Min (Data_Bytes, Unsigned_64 (Into'Length - Got) + 1);
         begin
            Call ((Operation => FQ.Queue_Read_At, Handle => Handle, Position => Unsigned_64 (Got), Length => Room,
                   Arena_Offset => Data_At, others => <>), Deadline_Ms, Status, Count, Result);
            exit when Result /= Done;
            Count := Unsigned_64'Min (Count, Room);
            if Unsigned_64 (Got) + Count > Unsigned_64 (Into'Length) then
               Result := Too_Large;
               exit;
            end if;
            Take (Data_At, Into (Into'First + Got .. Into'First + Got + Natural (Count) - 1));
            Got := Got + Natural (Count);
            exit when Count < Room;
         end;
      end loop;
      Close_Handle (Handle, Deadline_Ms, Closed);
   end Load;

   procedure Save (Name, Bytes : String; Deadline_Ms : Unsigned_64; Result : out Outcome) is
      Slash : Natural := Name'First - 1;
      Status : Unsigned_32;
      Handle, Value : Unsigned_64;
      Ignored : Outcome;
   begin
      for I in Name'Range loop
         if Name (I) = '/' then
            Slash := I;
         end if;
      end loop;
      declare
         Sibling : constant String := Name (Name'First .. Slash) & "." & Name (Slash + 1 .. Name'Last) & Sibling_Suffix;
         Written : Natural := 0;
         Chunk : constant Natural := Natural (Unsigned_64'Min (Data_Bytes, Unsigned_64 (Natural'Last)));
      begin
         if Name'Length = 0 or else Sibling'Length + Name'Length > 2 * Maximum_Path or else Chunk = 0 then
            Result := Failed;
            return;
         end if;
         for Attempt in 1 .. 2 loop
            Put (Path_At, Sibling);
            Call ((Operation => FQ.Queue_Open,
                   Options => Unsigned_32 (Files.OPEN_WRITE_ONLY or Files.OPEN_CREATE or Files.OPEN_TRUNCATE),
                   Length => Unsigned_64 (Sibling'Length), Arena_Offset => Path_At, others => <>),
                  Deadline_Ms, Status, Handle, Result);
            exit when Result /= Not_Found or else Attempt = 2 or else Slash <= Name'First;
            --  The place's folder is not there yet: made once, then again.
            Put (Path_At, Name (Name'First .. Slash - 1));
            Call ((Operation => FQ.Queue_Mkdir, Length => Unsigned_64 (Slash - Name'First), Arena_Offset => Path_At,
                   others => <>), Deadline_Ms, Status, Value, Ignored);
         end loop;
         if Result /= Done then
            return;
         end if;
         while Written < Bytes'Length and then Result = Done loop
            declare
               Count : constant Natural := Natural'Min (Chunk, Bytes'Length - Written);
            begin
               Put (Data_At, Bytes (Bytes'First + Written .. Bytes'First + Written + Count - 1));
               Call ((Operation => FQ.Queue_Write_At, Handle => Handle, Position => Unsigned_64 (Written),
                      Length => Unsigned_64 (Count), Arena_Offset => Data_At, others => <>),
                     Deadline_Ms, Status, Value, Result);
               Written := Written + Count;
            end;
         end loop;
         if Result = Done then
            Call ((Operation => FQ.Queue_Flush, Handle => Handle, others => <>), Deadline_Ms, Status, Value, Result);
         end if;
         if Result = Done then
            Close_Handle (Handle, Deadline_Ms, Result);
         else
            Close_Handle (Handle, Deadline_Ms, Ignored);
         end if;
         if Result = Done then
            Put (Path_At, Sibling & Name);
            Call ((Operation => FQ.Queue_Rename, Position => Unsigned_64 (Sibling'Length),
                   Length => Unsigned_64 (Sibling'Length + Name'Length), Arena_Offset => Path_At, others => <>),
                  Deadline_Ms, Status, Value, Result);
         end if;
         if Result /= Done then
            --  The partial sibling goes; the file was never touched.
            Put (Path_At, Sibling);
            Call ((Operation => FQ.Queue_Unlink, Length => Unsigned_64 (Sibling'Length), Arena_Offset => Path_At,
                   others => <>), Deadline_Ms, Status, Value, Ignored);
         end if;
      end;
   end Save;
end Dash_Files;

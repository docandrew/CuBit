with CuBit.Filesystems;

package body Files_Reader is
   package FS renames CuBit.Filesystems;
   package FQ renames Files_Queue.FQ;
   use type Files_Queue.Token;

   PATH_AREA : constant := 4_096;
   type Buffer is array (1 .. VIEW_LIMIT) of Unsigned_8;
   type Buffer_Access is access Buffer;

   Data : Buffer_Access;
   Current : Read_State := Idle;
   Reply : Unsigned_32 := 0;
   Filled : View_Length := 0;
   Went_On : Boolean := False;
   Changes : Unsigned_64 := 0;
   Base : Unsigned_64 := 0;
   Handle : Unsigned_64 := 0;
   Deadline_At : Unsigned_64 := 0;
   Path_Length : Natural := 0;
   --  The request in flight, and ones submitted for an earlier read that
   --  still owe an answer (an open among them may still return a handle).
   Pending : Files_Queue.Token := Files_Queue.NO_TOKEN;
   Pending_Is_Open : Boolean := False;
   MAXIMUM_ORPHANS : constant := 8;
   type Orphan is record
      Tag : Files_Queue.Token := Files_Queue.NO_TOKEN;
      Is_Open : Boolean := False;
   end record;
   Orphans : array (1 .. MAXIMUM_ORPHANS) of Orphan;
   --  A request that found the queue full, sent from Pump.
   Waiting : Boolean := False;

   procedure Changed is
   begin
      Changes := Changes + 1;
   end Changed;

   procedure Fire_And_Forget (Request : FQ.Request) is
      Tag : Files_Queue.Token;
   begin
      if Files_Queue.Ready and then Files_Queue.Can_Submit then
         Files_Queue.Submit (Request, Tag);
      end if;
   end Fire_And_Forget;

   procedure Close_Handle is
   begin
      if Handle /= 0 then
         Fire_And_Forget ((Operation => FQ.Queue_Close, Handle => Handle, others => <>));
         Handle := 0;
      end if;
   end Close_Handle;

   --  The read is over: whatever is in flight becomes an orphan.
   procedure Finish (Result : Finished) is
   begin
      if Pending /= Files_Queue.NO_TOKEN then
         for O of Orphans loop
            if O.Tag = Files_Queue.NO_TOKEN then
               O := (Pending, Pending_Is_Open);
               exit;
            end if;
         end loop;
         Pending := Files_Queue.NO_TOKEN;
      end if;
      Close_Handle;
      Waiting := False;
      Current := Result;
      Changed;
   end Finish;

   procedure Send_Next is
      Request : FQ.Request;
   begin
      if Current = Opening then
         Request := (Operation => FQ.Queue_Open, Options => Unsigned_32 (FS.OPEN_READ_ONLY),
                     Length => Unsigned_64 (Path_Length), Arena_Offset => Base, others => <>);
      else
         Request := (Operation => FQ.Queue_Read_At, Handle => Handle, Position => Unsigned_64 (Filled),
                     Length => Unsigned_64 (Natural'Min (CHUNK_BYTES, VIEW_LIMIT - Filled)),
                     Arena_Offset => Base + PATH_AREA, others => <>);
      end if;
      Waiting := not (Files_Queue.Ready and then Files_Queue.Can_Submit);
      if not Waiting then
         Files_Queue.Submit (Request, Pending);
         Pending_Is_Open := Current = Opening;
      end if;
   end Send_Next;

   procedure Start (Path : String; Arena_Base : Unsigned_64; Now_Us, Deadline_Us : Unsigned_64) is
      Text : Files_Listing.Name_Bytes (1 .. Path'Length);
      pragma Unreferenced (Now_Us);
   begin
      if Current in Opening | Reading then
         Finish (Cancelled);
      end if;
      if Data = null then
         Data := new Buffer;
      end if;
      Base := Arena_Base;
      Deadline_At := Deadline_Us;
      Filled := 0;
      Went_On := False;
      Reply := 0;
      Path_Length := Path'Length;
      for K in Text'Range loop
         Text (K) := Character'Pos (Path (Path'First + K - 1));
      end loop;
      Files_Queue.Write_Arena (Base, Text);
      Current := Opening;
      Send_Next;
      Changed;
   end Start;

   procedure Cancel is
   begin
      if Current in Opening | Reading then
         Finish (Cancelled);
      end if;
   end Cancel;

   procedure Take (Answer : Files_Queue.Answer; Owned : out Boolean) is
   begin
      Owned := False;
      if Answer.Tag = Files_Queue.NO_TOKEN then
         return;
      end if;
      for O of Orphans loop
         if O.Tag = Answer.Tag then
            --  Too late: a handle it opened is closed at once.
            if O.Is_Open and then Answer.Status = FS.REPLY_OK then
               Fire_And_Forget ((Operation => FQ.Queue_Close, Handle => Answer.Value, others => <>));
            end if;
            O := (others => <>);
            Owned := True;
            return;
         end if;
      end loop;
      if Answer.Tag /= Pending then
         return;
      end if;
      Owned := True;
      Pending := Files_Queue.NO_TOKEN;
      if Answer.Status /= FS.REPLY_OK then
         Reply := Answer.Status;
         Finish (Failed);
         return;
      end if;
      if Current = Opening then
         Handle := Answer.Value;
         Current := Reading;
         Send_Next;
      else
         declare
            Got : constant Natural := Natural (Unsigned_64'Min (Answer.Value, CHUNK_BYTES));
            Room : constant Natural := VIEW_LIMIT - Filled;
            Taken : constant Natural := Natural'Min (Got, Room);
            Chunk : Files_Listing.Name_Bytes (1 .. Taken);
         begin
            Files_Queue.Read_Arena (Base + PATH_AREA, Chunk);
            for K in 1 .. Taken loop
               Data (Filled + K) := Chunk (K);
            end loop;
            Filled := Filled + Taken;
            if Got = 0 or else Filled = VIEW_LIMIT or else Got < Natural'Min (CHUNK_BYTES, Room) then
               Went_On := Filled = VIEW_LIMIT and then Got > 0;
               Finish (Done);
            else
               Send_Next;
            end if;
         end;
      end if;
      Changed;
   end Take;

   procedure Pump (Now_Us : Unsigned_64) is
   begin
      if Current in Opening | Reading then
         if Now_Us > Deadline_At then
            Finish (Timed_Out);
         elsif Waiting then
            Send_Next;
         end if;
      end if;
   end Pump;

   function State return Read_State is (Current);
   function Status return Unsigned_32 is (Reply);
   function Length return View_Length is (Filled);
   function Byte (Position : Positive) return Unsigned_8 is (Data (Position));
   function Bytes return Files_Listing.Name_Bytes is
     (if Data = null then [1 .. 0 => 0] else Files_Listing.Name_Bytes (Data (1 .. Filled)));
   function Truncated return Boolean is (Went_On);
   function Revision return Unsigned_64 is (Changes);
   function Deadline return Unsigned_64 is (if Current in Opening | Reading then Deadline_At else Unsigned_64'Last);
end Files_Reader;

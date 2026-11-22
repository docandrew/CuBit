with System; use System;
with System.Machine_Code;
with System.Storage_Elements; use System.Storage_Elements;
with Files_Link;
with Files_Mock_Service;

package body Files_Queue is
   package Q renames FQ.Queues;

   Is_Open : Boolean := False;
   Client : Q.Client;
   Next_Tag : Token := NO_TOKEN;
   Kicked : Unsigned_32 := 0;
   Events_Opened : Boolean := False;

   --  The compiler keeps ring writes before index stores (x86 keeps the
   --  order in hardware).
   procedure Barrier is
   begin
      System.Machine_Code.Asm ("", Volatile => True, Clobber => "memory");
   end Barrier;

   function Client_Word (Offset : Natural) return Address is (Files_Link.Client_Region + Storage_Offset (Offset));
   function Server_Word (Offset : Natural) return Address is (Files_Link.Server_Region + Storage_Offset (Offset));

   procedure Open (Arena_Pages : Positive; Success : out Boolean) is
   begin
      if not Is_Open then
         Files_Link.Open (Arena_Pages, Is_Open);
         Client := (others => <>);
         Kicked := 0;
      end if;
      Success := Is_Open;
   end Open;

   function Ready return Boolean is (Is_Open);
   function Arena_Bytes return Unsigned_64 is (if Is_Open then Files_Link.Arena_Bytes else 0);

   procedure Accept_Taken is
      Taken : constant Unsigned_32 with Import, Volatile, Address => Server_Word (FQ.Server_Taken_At);
      Accepted : Boolean;
   begin
      Q.Accept_Taken (Client, Q.Submissions.Index (Taken), Accepted);
   end Accept_Taken;

   function Can_Submit return Boolean is
   begin
      if not Is_Open then
         return False;
      end if;
      Accept_Taken;
      return Q.Can_Submit (Client);
   end Can_Submit;

   function Outstanding return Natural is (Client.Pending);

   procedure Submit (Request : FQ.Request; Tag : out Token) is
      Requests : Q.Submissions.Ring with Import, Address => Client_Word (FQ.Client_Requests_At);
   begin
      Next_Tag := Next_Tag + 1;
      Tag := Next_Tag;
      Q.Submit (Client, Requests, Tag, Request);
   end Submit;

   procedure Flush is
      Submitted : Unsigned_32 with Import, Volatile, Address => Client_Word (FQ.Client_Submitted_At);
      Wake : constant Unsigned_32 with Import, Volatile, Address => Server_Word (FQ.Server_Wake_At);
      Armed : Unsigned_32;
   begin
      if not Is_Open or else Submitted = Unsigned_32 (Client.Requests.Produced) then
         return;
      end if;
      Barrier;
      Submitted := Unsigned_32 (Client.Requests.Produced);
      Barrier;
      --  Kick only a service that sleeps, once per arming.
      Armed := Wake;
      if Armed /= 0 and then Armed /= Kicked then
         Kicked := Armed;
         Files_Link.Kick;
      end if;
   end Flush;

   procedure Reap (Item : out Answer; Got : out Boolean) is
      Answers : constant Q.Completions.Ring with Import, Address => Server_Word (FQ.Server_Answers_At);
      Answered : constant Unsigned_32 with Import, Volatile, Address => Server_Word (FQ.Server_Answered_At);
      Reaped : Unsigned_32 with Import, Volatile, Address => Client_Word (FQ.Client_Reaped_At);
      Result : Q.Completion;
      Accepted : Boolean;
   begin
      Item := (others => <>);
      Got := False;
      if not Is_Open then
         return;
      end if;
      Q.Completions.Accept_Produced (Client.Answers, Q.Completions.Index (Answered), Accepted);
      if Client.Answers.Available = 0 then
         return;
      end if;
      Barrier;
      Q.Reap (Client, Answers, Result, Accepted);
      Barrier;
      Reaped := Unsigned_32 (Client.Answers.Consumed);
      if Accepted then
         Item := (Tag => Result.Tag, Status => Result.Answer.Status, Value => Result.Answer.Value);
         Got := True;
      end if;
   end Reap;

   procedure Arm_Wake is
   begin
      if Is_Open and then (Client.Pending > 0 or else Events_Opened) then
         Files_Link.Arm_Wake;
      end if;
   end Arm_Wake;

   procedure Complete (Receipt : CuBit.Messages.CompletionEntry; Consumed, Woken : out Boolean) is
      pragma Unreferenced (Receipt);
   begin
      Consumed := False;
      Woken := False;
   end Complete;

   procedure Open_Events (Success : out Boolean) is
   begin
      if Is_Open and then not Events_Opened then
         Files_Mock_Service.Enable_Events;
         Events_Opened := True;
      end if;
      Success := Is_Open and then Events_Opened;
   end Open_Events;

   function Events_Open return Boolean is (Is_Open and then Events_Opened);

   --  The mock's ring stands in for the event channel; records are copied,
   --  then checked, as on CuBit.
   procedure Take_Event
     (Item : out FE.Event; Name : out FE.Name_Bytes; Length : out FE.Name_Length; Result : out Event_Result)
   is
      Bytes : FE.Record_Bytes;
      Used : Natural;
      Got, OK : Boolean;
   begin
      Item := (others => <>);
      Name := [others => 0];
      Length := 0;
      Result := Empty;
      if not Events_Open then
         return;
      end if;
      Files_Mock_Service.Take_Record (Bytes, Used, Got);
      if not Got then
         return;
      end if;
      FE.Decode (Bytes, Used, Item, Name, Length, OK);
      Result := (if OK then Taken else Malformed);
   end Take_Event;

   function Copy_Progress (Tag : Token) return Unsigned_64 is
   begin
      if not Is_Open or else Tag = NO_TOKEN then
         return 0;
      end if;
      for K in 0 .. FQ.Maximum_Copies - 1 loop
         declare
            Base : constant Address := Server_Word (FQ.Server_Copies_At + K * FQ.Copy_Entry_Bytes);
            Owner : constant Unsigned_64 with Import, Volatile, Address => Base + FQ.Copy_Token_At;
            Done : constant Unsigned_64 with Import, Volatile, Address => Base + FQ.Copy_Done_At;
            First, Count, Last : Unsigned_64;
         begin
            First := Owner;
            Count := Done;
            Last := Owner;
            if First = Unsigned_64 (Tag) and then Last = Unsigned_64 (Tag) then
               return Count;
            end if;
         end;
      end loop;
      return 0;
   end Copy_Progress;

   procedure Read_Arena (Offset : Unsigned_64; Into : out Files_Listing.Name_Bytes) is
   begin
      Into := [others => 0];
      if not Is_Open or else not FQ.In_Arena (Offset, Into'Length, Files_Link.Arena_Bytes) then
         return;
      end if;
      declare
         Source : constant Files_Listing.Name_Bytes (Into'Range)
           with Import, Address => Files_Link.Arena + Storage_Offset (Offset);
      begin
         Into := Source;
      end;
   end Read_Arena;

   procedure Write_Arena (Offset : Unsigned_64; From : Files_Listing.Name_Bytes) is
   begin
      if not Is_Open or else not FQ.In_Arena (Offset, From'Length, Files_Link.Arena_Bytes) then
         return;
      end if;
      declare
         Target : Files_Listing.Name_Bytes (From'Range)
           with Import, Address => Files_Link.Arena + Storage_Offset (Offset);
      begin
         Target := From;
      end;
   end Write_Arena;

   procedure Close is
   begin
      if Is_Open then
         Files_Link.Close;
         Is_Open := False;
         Events_Opened := False;
      end if;
   end Close;
end Files_Queue;

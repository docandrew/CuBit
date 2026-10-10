with System; use System;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Async_Requests;
with CuBit.Filesystem_Sessions;
with CuBit.UI.App;
with CCL_Manifest_Bindings;

--  On CuBit: Files' queue is a CuBit.Filesystem_Sessions session on the
--  filesystem endpoint its manifest grants. Submit and Reap never block;
--  the wake (OP_FS_WAKE) is armed while requests are out and its completion
--  arrives in CuBit.UI.App.Run's one activity wait (Activity_Wait), which
--  hands it to Complete.
package body Files_Queue is
   package FS renames CuBit.Filesystem_Sessions;

   Session : FS.Session;
   --  Wake tokens: Files' own range, above the window's input waits.
   Last_Wake_Token : CuBit.Async_Requests.Token := CuBit.UI.App.APPLICATION_TOKEN_FIRST;

   procedure Open (Arena_Pages : Positive; Success : out Boolean) is
   begin
      if not FS.Is_Open (Session) then
         FS.Open (Session, CCL_Manifest_Bindings.Slot_Files_Filesystem,
                  Positive'Min (Arena_Pages, FQ.Transfer_Pages), Success);
      end if;
      Success := FS.Is_Open (Session);
   end Open;

   function Ready return Boolean is (FS.Is_Open (Session));
   function Arena_Bytes return Unsigned_64 is (if Ready then FS.Arena_Bytes (Session) else 0);
   function Can_Submit return Boolean is (Ready and then FS.Can_Submit (Session));
   function Outstanding return Natural is (FS.Outstanding (Session));

   procedure Submit (Request : FQ.Request; Tag : out Token) is
   begin
      FS.Submit (Session, Request, Tag);
   end Submit;

   --  Each Submit publishes and kicks (only a sleeping service) already.
   procedure Flush is null;

   procedure Reap (Item : out Answer; Got : out Boolean) is
      Result : FQ.Queues.Completion;
   begin
      Item := (others => <>);
      Got := False;
      if not Ready then
         return;
      end if;
      FS.Reap (Session, Result, Got);
      if Got then
         Item := (Tag => Result.Tag, Status => Result.Answer.Status, Value => Result.Answer.Value);
      end if;
   end Reap;

   procedure Arm_Wake is
      Accepted : Boolean;
   begin
      if Ready and then (FS.Outstanding (Session) > 0 or else FS.Events_Open (Session))
        and then not FS.Wake_Armed (Session)
        and then Last_Wake_Token < CuBit.Async_Requests.Token'Last - 1
      then
         Last_Wake_Token := Last_Wake_Token + 1;
         FS.Arm_Wake (Session, Last_Wake_Token, Accepted);
      end if;
   end Arm_Wake;

   procedure Complete (Receipt : CuBit.Messages.CompletionEntry; Consumed, Woken : out Boolean) is
   begin
      FS.Complete_Wake (Session, Receipt, Consumed, Woken);
   end Complete;

   procedure Open_Events (Success : out Boolean) is
   begin
      if Ready and then not FS.Events_Open (Session) then
         FS.Open_Events (Session, Success);
      end if;
      Success := Ready and then FS.Events_Open (Session);
   end Open_Events;

   function Events_Open return Boolean is (Ready and then FS.Events_Open (Session));

   procedure Take_Event
     (Item : out FE.Event; Name : out FE.Name_Bytes; Length : out FE.Name_Length; Result : out Event_Result)
   is
      Outcome : FS.Event_Result;
   begin
      Item := (others => <>);
      Name := [others => 0];
      Length := 0;
      Result := Empty;
      if not Events_Open then
         return;
      end if;
      FS.Take_Event (Session, Item, Name, Length, Outcome);
      Result := (case Outcome is
                    when FS.Taken => Files_Queue.Taken, when FS.Empty => Files_Queue.Empty,
                    when FS.Malformed => Files_Queue.Malformed);
   end Take_Event;

   --  The service's progress words (FQ.Server_Copies_At): the token, the
   --  count and the token again; the count counts only if both are Tag.
   function Copy_Progress (Tag : Token) return Unsigned_64 is
   begin
      if not Ready or else Tag = NO_TOKEN then
         return 0;
      end if;
      for K in 0 .. FQ.Maximum_Copies - 1 loop
         declare
            Base : constant System.Address :=
              FS.Server_Region (Session) + Storage_Offset (FQ.Server_Copies_At + K * FQ.Copy_Entry_Bytes);
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
      if not Ready or else not FQ.In_Arena (Offset, Into'Length, FS.Arena_Bytes (Session)) then
         return;
      end if;
      declare
         Source : constant Files_Listing.Name_Bytes (Into'Range)
           with Import, Address => FS.Arena (Session) + Storage_Offset (Offset);
      begin
         Into := Source;
      end;
   end Read_Arena;

   procedure Write_Arena (Offset : Unsigned_64; From : Files_Listing.Name_Bytes) is
   begin
      if not Ready or else not FQ.In_Arena (Offset, From'Length, FS.Arena_Bytes (Session)) then
         return;
      end if;
      declare
         Target : Files_Listing.Name_Bytes (From'Range)
           with Import, Address => FS.Arena (Session) + Storage_Offset (Offset);
      begin
         Target := From;
      end;
   end Write_Arena;

   procedure Close is
   begin
      if Ready then
         FS.Close (Session);
      end if;
   end Close;
end Files_Queue;

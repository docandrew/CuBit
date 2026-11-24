pragma Ada_2022;
with CuBit.Async_Requests;
with CuBit.Handles;
with Desktop_Endpoints;
with Desktop_Status_Machines;

package body Desktop_Power_Refresh is
   use Interfaces;
   use CuBit.Messages;
   package AR renames CuBit.Async_Requests;
   package PS renames CuBit.Power_Sources;
   use type AR.Phase, PS.Summary_Mode, PS.Summary;

   Read_Flight, Watch_Flight : AR.Tracker;
   Started : Boolean := False;
   Have_Value, Fresh_Value : Boolean := False;
   Value_Now : PS.Summary;
   Seen : Unsigned_64 := 0;
   Retry_At : Unsigned_64 := 0;
   Next_Id : Unsigned_64 := 1;

   function Granted return Boolean is (CuBit.Handles.Valid (Desktop_Endpoints.Power_Sources));

   function Token_Of (F : AR.Tracker) return Unsigned_64 is
     (if AR.State (F) = AR.In_Flight then AR.Pending_Token (F) else 0);

   function Matches (Token : Unsigned_64) return Boolean is
     (Token /= 0 and then (Token = Token_Of (Read_Flight) or else Token = Token_Of (Watch_Flight)));

   function Next_Due return Unsigned_64 is (Retry_At);

   function Submit (Flight : in out AR.Tracker; Mode : PS.Summary_Mode; Sequence : in out Unsigned_64)
     return Boolean
   is
      Msg : Message := NULL_MESSAGE;
      Accepted : Boolean;
   begin
      if not Granted or else AR.State (Flight) /= AR.Idle or else Sequence >= Unsigned_64'Last - 1
        or else Next_Id = Unsigned_64'Last
      then
         return False;
      end if;
      Sequence := Sequence + 1;
      AR.Reserve (Flight, Sequence, Accepted);
      if not Accepted then
         return False;
      end if;
      Msg.tag := (label => PS.Summary_Label, length => PS.Summary_Request_Words, protocol => 0, ipc => <>,
                  reserved => 0);
      Msg.words (PS.Request_Word) := Next_Id;
      Msg.words (PS.Seen_Word) := Seen;
      Msg.words (PS.Mode_Word) := PS.Summary_Mode'Enum_Rep (Mode);
      Next_Id := Next_Id + 1;
      Accepted := capSubmit (Desktop_Endpoints.Power_Sources, Msg, Sequence);
      AR.Submitted (Flight, Accepted);
      return Accepted;
   end Submit;

   procedure Watch (Now : Unsigned_64; Sequence : in out Unsigned_64) is
   begin
      --  The first read answers before the first watch.
      if AR.State (Read_Flight) /= AR.Idle or else AR.State (Watch_Flight) /= AR.Idle or else Seen = 0 then
         return;
      end if;
      if not Submit (Watch_Flight, PS.Watch, Sequence) then
         Retry_At := Now + Desktop_Status_Machines.Retry_Ms;
      end if;
   end Watch;

   procedure Start (Now : Unsigned_64; Sequence : in out Unsigned_64) is
      Ignore : Boolean;
   begin
      if not Granted then
         return;
      end if;
      if not Started then
         Started := Submit (Read_Flight, PS.Read_Now, Sequence);
         if not Started then
            Retry_At := Now + Desktop_Status_Machines.Retry_Ms;
         end if;
         return;
      end if;
      if Retry_At /= 0 and then Now >= Retry_At then
         Retry_At := 0;
         if Seen = 0 then
            Ignore := Submit (Read_Flight, PS.Read_Now, Sequence);
         else
            Watch (Now, Sequence);
         end if;
      end if;
   end Start;

   procedure Read_Now (Sequence : in out Unsigned_64) is
      Ignore : Boolean;
   begin
      Ignore := Submit (Read_Flight, PS.Read_Now, Sequence);
   end Read_Now;

   --  A summary reply: True when it was one, and the newest one is kept.
   function Decode (Reply : Message) return Boolean is
   begin
      if Reply.tag.label /= PS.Summary_Label or else Reply.tag.length /= PS.Reply_Words
        or else Reply.words (PS.Generation_Word) = 0
        or else not PS.Valid_Summary (Reply.words (PS.Payload_Word), Reply.words (PS.Payload_Word + 1))
      then
         return False;
      end if;
      --  An older read answered after a newer watch changes nothing.
      if Reply.words (PS.Generation_Word) >= Seen then
         declare
            Next : constant PS.Summary :=
              PS.To_Summary (Reply.words (PS.Payload_Word), Reply.words (PS.Payload_Word + 1));
         begin
            if not Have_Value or else Next /= Value_Now then
               Fresh_Value := True;
            end if;
            Value_Now := Next;
            Have_Value := True;
            Seen := Reply.words (PS.Generation_Word);
         end;
      end if;
      return True;
   end Decode;

   procedure Drain (Flight : in out AR.Tracker; C : CompletionEntry) is
      Captured : Boolean;
   begin
      AR.Capture (Flight, C.token, True, Captured);
      if Captured then
         AR.Release (Flight);
      end if;
   end Drain;

   procedure Collect (C_Entry : CompletionEntry; Now : Unsigned_64; Sequence : in out Unsigned_64) is
      Delivered : constant Boolean := C_Entry.valid and then C_Entry.status = COMPLETION_OK;
      Decoded : Boolean := False;
   begin
      if C_Entry.token = Token_Of (Read_Flight) then
         Drain (Read_Flight, C_Entry);
      elsif C_Entry.token = Token_Of (Watch_Flight) then
         Drain (Watch_Flight, C_Entry);
      else
         return;
      end if;
      if Delivered then
         Decoded := Decode (C_Entry.msg);
      end if;
      if not Delivered then
         --  power.svc is gone (or refused the request outright).
         if Have_Value then
            Have_Value := False;
            Fresh_Value := True;
         end if;
         Retry_At := Now + Desktop_Status_Machines.Retry_Ms;
      elsif not Decoded then
         --  Refused (No_Room, Superseded, ...): watch again later.
         Retry_At := Now + Desktop_Status_Machines.Retry_Ms;
      else
         Watch (Now, Sequence);
      end if;
   end Collect;

   procedure Take (Value : out PS.Summary; Have, Fresh : out Boolean) is
   begin
      Value := Value_Now;
      Have := Have_Value;
      Fresh := Fresh_Value;
      Fresh_Value := False;
   end Take;
end Desktop_Power_Refresh;

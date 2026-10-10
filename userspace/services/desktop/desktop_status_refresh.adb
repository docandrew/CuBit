with Compositor_Requests;
package body Desktop_Status_Refresh is
   use Interfaces;
   use CuBit.Messages;
   package CR renames Compositor_Requests;
   package Clocks renames CuBit.Clocks;
   package Audio renames CuBit.Audio_Control;
   --  Reply envelope shared by both services (CuBit.Clocks.Read and
   --  CuBit.Audio_Control.Read check the same fields).
   Reply_OK : constant Unsigned_32 := 16#F000#;
   Reply_Words : constant := 4;
   Seconds_Per_Day : constant := 86_400;
   Maximum_Offset_Encoding : constant := 2 * Seconds_Per_Day;
   First_Year : constant := 1970;
   Last_Year : constant := 2399;

   Clock_Flight, Audio_Flight : CR.State;
   Clock_Value : Clocks.Snapshot;
   Audio_Value : Audio.State;
   Clock_Fresh, Audio_Fresh : Boolean := False;
   type Audio_Setting is record
      Queued : Boolean := False;
      Level : Audio.Percent := 0;
      Muted : Boolean := False;
   end record;
   Wanted : Audio_Setting;

   function Clock_Token return Unsigned_64 is
     (if CR.Busy (Clock_Flight) then CR.Token (Clock_Flight) else 0);
   function Audio_Token return Unsigned_64 is
     (if CR.Busy (Audio_Flight) then CR.Token (Audio_Flight) else 0);

   procedure Submit (Flight : in out CR.State; Slot : Unsigned_64; Msg : Message;
                     Sequence : in out Unsigned_64) is
      Token : Unsigned_64;
      Accepted : Boolean;
   begin
      if not CR.Available (Flight) then return; end if;
      CR.Allocate (Sequence, Token);
      if Token = 0 then CR.Quarantine (Flight); return; end if;
      CR.Begin_Request (Flight, Token, Accepted);
      if not Accepted then CR.Quarantine (Flight); return; end if;
      --  A refused submission published nothing: the flight is free again.
      if not capSubmit (Slot, Msg, Token) then
         CR.Complete (Flight, Token, True);
      end if;
   end Submit;

   procedure Request_Clock (Sequence : in out Unsigned_64) is
      Msg : Message := NULL_MESSAGE;
   begin
      Msg.tag.label := Clocks.Snapshot_Operation;
      Submit (Clock_Flight, Clocks.Endpoint_Slot, Msg, Sequence);
   end Request_Clock;

   procedure Request_Audio (Sequence : in out Unsigned_64) is
      Msg : Message := NULL_MESSAGE;
   begin
      if not CR.Available (Audio_Flight) then return; end if;
      if Wanted.Queued then
         Msg.tag.label := Audio.Set_State;
         Msg.tag.length := 2;
         Msg.words (0) := Unsigned_64 (Wanted.Level);
         Msg.words (1) := Boolean'Pos (Wanted.Muted);
         Wanted.Queued := False;
      else
         Msg.tag.label := Audio.Get_State;
      end if;
      Submit (Audio_Flight, Audio.Endpoint_Slot, Msg, Sequence);
   end Request_Audio;

   procedure Set_Audio (Level : Audio.Percent; Muted : Boolean;
                        Sequence : in out Unsigned_64) is
   begin
      Wanted := (True, Level, Muted);
      Request_Audio (Sequence);
   end Set_Audio;

   function Envelope (Reply : Message) return Boolean is
     (Reply.tag.label = Reply_OK and then Reply.tag.length = Reply_Words
      and then Reply.tag.flags = 0 and then Reply.tag.reserved = 0);

   procedure Decode_Clock (Reply : Message) is
      Y : constant Unsigned_64 := Shift_Right (Reply.words (1), 40);
      M : constant Unsigned_64 := Shift_Right (Reply.words (1), 32) and 255;
      D : constant Unsigned_64 := Shift_Right (Reply.words (1), 24) and 255;
      H : constant Unsigned_64 := Shift_Right (Reply.words (1), 16) and 255;
      N : constant Unsigned_64 := Shift_Right (Reply.words (1), 8) and 255;
      S : constant Unsigned_64 := Reply.words (1) and 255;
   begin
      if Envelope (Reply)
        and then Reply.words (2) <= Maximum_Offset_Encoding
        and then Reply.words (3) <= Clocks.Time_Quality'Enum_Rep (Clocks.Time_Quality'Last)
        and then Y in First_Year .. Last_Year and then M in 1 .. 12
        and then D in 1 .. 31 and then H <= 23 and then N <= 59 and then S <= 59
      then
         Clock_Value :=
           (Reply.words (0), Natural (Y), Natural (M), Natural (D),
            Natural (H), Natural (N), Natural (S),
            Integer (Reply.words (2)) - Seconds_Per_Day,
            Clocks.Time_Quality'Enum_Val (Reply.words (3)));
         Clock_Fresh := True;
      end if;
   end Decode_Clock;

   procedure Decode_Audio (Reply : Message) is
   begin
      if Envelope (Reply)
        and then Reply.words (0) <= Unsigned_64 (Audio.Percent'Last)
        and then Reply.words (1) <= 1 and then Reply.words (2) <= 1
        and then Reply.words (3) = 0
      then
         Audio_Value := (Audio.Percent (Reply.words (0)), Reply.words (1) = 1,
                         Reply.words (2) = 1);
         Audio_Fresh := True;
      end if;
   end Decode_Audio;

   procedure Collect (C : CompletionEntry; Sequence : in out Unsigned_64) is
      Delivered : constant Boolean := C.valid and then C.status = COMPLETION_OK;
   begin
      if C.token = Clock_Token then
         CR.Complete (Clock_Flight, C.token, Delivered);
         if Delivered then Decode_Clock (C.msg); end if;
      elsif C.token = Audio_Token then
         CR.Complete (Audio_Flight, C.token, Delivered);
         if Delivered then Decode_Audio (C.msg); end if;
         if Wanted.Queued then Request_Audio (Sequence); end if;
      end if;
   end Collect;

   procedure Take_Clock (Value : out Clocks.Snapshot; Fresh : out Boolean) is
   begin
      Value := Clock_Value; Fresh := Clock_Fresh; Clock_Fresh := False;
   end Take_Clock;

   procedure Take_Audio (Value : out Audio.State; Fresh : out Boolean) is
   begin
      Value := Audio_Value; Fresh := Audio_Fresh; Audio_Fresh := False;
   end Take_Audio;
end Desktop_Status_Refresh;

pragma Ada_2022;
with CuBit.Async_Requests;
with CuBit.Handles;
with Desktop_Endpoints;
with Desktop_Status_Machines;
package body Desktop_Status_Refresh is
   use Interfaces;
   use CuBit.Messages;
   package AR renames CuBit.Async_Requests;
   package SM renames Desktop_Status_Machines;
   package Clocks renames CuBit.Clocks;
   package Audio renames CuBit.Audio_Control;
   use type AR.Phase, SM.Clock_Action, SM.Audio_Action_Kind, SM.Outcome;
   --  Reply envelope shared by both services (CuBit.Clocks.Read and
   --  CuBit.Audio_Control.Read check the same fields).
   Reply_OK : constant Unsigned_32 := 16#F000#;
   Reply_Words : constant := 4;
   Seconds_Per_Day : constant := 86_400;
   Maximum_Offset_Encoding : constant := 2 * Seconds_Per_Day;
   First_Year : constant := 1970;
   Last_Year : constant := 2399;
   Months : constant := 12;
   Days : constant := 31;
   Last_Hour : constant := 23;
   Last_Minute : constant := 59;
   Byte_Mask : constant := 255;
   pragma Compile_Time_Error
     (SM.Percent'First /= Audio.Percent'First or else SM.Percent'Last /= Audio.Percent'Last,
      "the audio machine's levels are the mixer's");

   --  One request in flight per service (CuBit.Async_Requests): any
   --  completion, delivered or not, returns the flight to Idle, so a
   --  failed request is asked again (Desktop_Status_Machines).
   Clock_Flight, Audio_Flight : AR.Tracker;
   Clock_Machine : SM.Clock_State := SM.Idle;
   Audio_Machine : SM.Audio_State;
   Clock_Value : Clocks.Snapshot;
   Audio_Value : Audio.State;
   Clock_Fresh, Audio_Fresh : Boolean := False;

   function Token_Of (F : AR.Tracker) return Unsigned_64 is
     (if AR.State (F) = AR.In_Flight then AR.Pending_Token (F) else 0);
   function Clock_Token return Unsigned_64 is (Token_Of (Clock_Flight));
   function Audio_Token return Unsigned_64 is (Token_Of (Audio_Flight));

   --  Submit Msg on a free flight; False when it was not submitted.
   function Submit (Flight : in out AR.Tracker; Service : CuBit.Handles.Endpoint; Msg : Message;
                    Sequence : in out Unsigned_64) return Boolean is
      Accepted : Boolean;
   begin
      if Sequence >= Unsigned_64'Last - 1 or else not CuBit.Handles.Valid (Service) then
         return False;
      end if;
      Sequence := Sequence + 1;
      AR.Reserve (Flight, Sequence, Accepted);
      if not Accepted then
         return False;
      end if;
      Accepted := capSubmit (Service, Msg, Sequence);
      AR.Submitted (Flight, Accepted);
      return Accepted;
   end Submit;

   procedure Request_Clock (Sequence : in out Unsigned_64) is
      Msg : Message := NULL_MESSAGE;
      Act : SM.Clock_Action;
   begin
      SM.Step (Clock_Machine, SM.Due, Act);
      if Act = SM.Submit_Read then
         Msg.tag.label := Clocks.Snapshot_Operation;
         if not Submit (Clock_Flight, Desktop_Endpoints.Clock_Service, Msg, Sequence) then
            SM.Step (Clock_Machine, SM.Done, Act);
         end if;
      end if;
   end Request_Clock;

   procedure Perform_Audio (Act : SM.Audio_Action; Sequence : in out Unsigned_64) is
      Msg : Message := NULL_MESSAGE;
      Again : SM.Audio_Action;
   begin
      case Act.Kind is
         when SM.None =>
            return;
         when SM.Submit_Get =>
            Msg.tag.label := Audio.Get_State;
         when SM.Submit_Set =>
            Msg.tag.label := Audio.Set_State;
            Msg.tag.length := 2;
            Msg.words (0) := Unsigned_64 (Act.Value.Level);
            Msg.words (1) := Boolean'Pos (Act.Value.Muted);
      end case;
      if not Submit (Audio_Flight, Desktop_Endpoints.Mixer_Control, Msg, Sequence) then
         --  Not sent: a setting is queued again for the next due time.
         SM.Step (Audio_Machine, (Kind => SM.Done, Result => SM.Not_Delivered), Again);
      end if;
   end Perform_Audio;

   procedure Request_Audio (Sequence : in out Unsigned_64) is
      Act : SM.Audio_Action;
   begin
      SM.Step (Audio_Machine, (Kind => SM.Due), Act);
      Perform_Audio (Act, Sequence);
   end Request_Audio;

   procedure Set_Audio (Level : Audio.Percent; Muted : Boolean;
                        Sequence : in out Unsigned_64) is
      Act : SM.Audio_Action;
   begin
      SM.Step (Audio_Machine, (Kind => SM.Set, Value => (Level, Muted)), Act);
      Perform_Audio (Act, Sequence);
   end Set_Audio;

   function Envelope (Reply : Message) return Boolean is
     (Reply.tag.label = Reply_OK and then Reply.tag.length = Reply_Words
      and then Reply.tag.protocol = 0 and then Reply.tag.reserved = 0);

   function Decode_Clock (Reply : Message) return Boolean is
      Y : constant Unsigned_64 := Shift_Right (Reply.words (1), 40);
      M : constant Unsigned_64 := Shift_Right (Reply.words (1), 32) and Byte_Mask;
      D : constant Unsigned_64 := Shift_Right (Reply.words (1), 24) and Byte_Mask;
      H : constant Unsigned_64 := Shift_Right (Reply.words (1), 16) and Byte_Mask;
      N : constant Unsigned_64 := Shift_Right (Reply.words (1), 8) and Byte_Mask;
      S : constant Unsigned_64 := Reply.words (1) and Byte_Mask;
   begin
      if Envelope (Reply)
        and then Reply.words (2) <= Maximum_Offset_Encoding
        and then Reply.words (3) <= Clocks.Time_Quality'Enum_Rep (Clocks.Time_Quality'Last)
        and then Y in First_Year .. Last_Year and then M in 1 .. Months
        and then D in 1 .. Days and then H <= Last_Hour and then N <= Last_Minute and then S <= Last_Minute
      then
         Clock_Value :=
           (Reply.words (0), Natural (Y), Natural (M), Natural (D),
            Natural (H), Natural (N), Natural (S),
            Integer (Reply.words (2)) - Seconds_Per_Day,
            Clocks.Time_Quality'Enum_Val (Reply.words (3)));
         Clock_Fresh := True;
         return True;
      end if;
      return False;
   end Decode_Clock;

   function Decode_Audio (Reply : Message) return Boolean is
   begin
      if Envelope (Reply)
        and then Reply.words (0) <= Unsigned_64 (Audio.Percent'Last)
        and then Reply.words (1) <= 1 and then Reply.words (2) <= 1
        and then Reply.words (3) = 0
      then
         Audio_Value := (Audio.Percent (Reply.words (0)), Reply.words (1) = 1,
                         Reply.words (2) = 1);
         Audio_Fresh := True;
         return True;
      end if;
      return False;
   end Decode_Audio;

   --  Take Flight's completion (whatever its status) back to Idle.
   procedure Drain (Flight : in out AR.Tracker; C : CompletionEntry) is
      Captured : Boolean;
   begin
      AR.Capture (Flight, C.token, True, Captured);
      if Captured then
         AR.Release (Flight);
      end if;
   end Drain;

   procedure Collect (C : CompletionEntry; Sequence : in out Unsigned_64) is
      Delivered : constant Boolean := C.valid and then C.status = COMPLETION_OK;
   begin
      if C.token = Clock_Token then
         Drain (Clock_Flight, C);
         declare
            Act : SM.Clock_Action;
            Ignore : Boolean;
         begin
            if Delivered then
               Ignore := Decode_Clock (C.msg);
            end if;
            SM.Step (Clock_Machine, SM.Done, Act);
         end;
      elsif C.token = Audio_Token then
         Drain (Audio_Flight, C);
         declare
            Result : constant SM.Outcome :=
              (if not Delivered then SM.Not_Delivered
               elsif Decode_Audio (C.msg) then SM.Answered else SM.Refused);
            Act : SM.Audio_Action;
         begin
            SM.Step (Audio_Machine, (Kind => SM.Done, Result => Result), Act);
            Perform_Audio (Act, Sequence);
         end;
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

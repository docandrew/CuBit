with Interfaces; use Interfaces;
with CuBit.Handles;
with CuBit.Messages;
with CCL_Native_Bindings;

--  power.* on CuBit: Read_Now on power.svc's power-sources endpoint
--  (CuBit.Power_Sources), then every page of every source against the
--  generation it answered; a snapshot that changed meanwhile is read again
--  (at most Attempts times).
package body CCL_Power is
   package PS renames CuBit.Power_Sources;
   use type CuBit.Messages.Call_Ending;

   Attempts : constant := 3;
   Next_Id : Unsigned_64 := 1;

   function Available return Boolean is (CuBit.Handles.Valid (CCL_Native_Bindings.Power_Sources));

   type Outcome is (Done, Changed, Failed);

   --  One call; Answered when power.svc replied with Label, the same id;
   --  Refused when it refused it (its reason in Msg's Refusal_Word).
   procedure Call (Msg : in out CuBit.Messages.Message; Label : Unsigned_32; Answered, Refused : out Boolean) is
      Id : constant Unsigned_64 := Msg.words (PS.Request_Word);
      Tag : CuBit.Messages.MessageTag;
      Replied : Boolean;
   begin
      Tag := CuBit.Messages.capCall
        (CCL_Native_Bindings.Power_Sources, Msg,
         CuBit.Messages.Deadline_After (PS.Answer_Budget_Milliseconds));
      Replied := CuBit.Messages.Ending_Of (Tag) = CuBit.Messages.Call_Answered
        and then Msg.words (PS.Reply_Request_Word) = Id;
      Answered := Replied and then Tag.label = Label;
      Refused := Replied and then Tag.label = PS.Refused_Label;
   end Call;

   function Request (Label : Unsigned_32; Length : Unsigned_8) return CuBit.Messages.Message is
      Msg : CuBit.Messages.Message := CuBit.Messages.NULL_MESSAGE;
   begin
      Msg.tag := (label => Label, length => Length, protocol => 0, ipc => <>, reserved => 0);
      Msg.words (PS.Request_Word) := Next_Id;
      Next_Id := (if Next_Id = Unsigned_64'Last then 1 else Next_Id + 1);
      return Msg;
   end Request;

   procedure Read_Once
     (Sources : out PS.Source_Array; Count : out PS.Source_Count; Result : out Outcome;
      Why : out CuBit.Failures.Failure)
   is
      Msg : CuBit.Messages.Message := Request (PS.Summary_Label, PS.Summary_Request_Words);
      Answered, Refused, Valid : Boolean;
      Generation : Unsigned_64;
      Summary : PS.Summary;
   begin
      Sources := [others => (others => <>)];
      Count := 0;
      Result := Failed;
      Why := (others => <>);
      Msg.words (PS.Seen_Word) := 0;
      Msg.words (PS.Mode_Word) := PS.Summary_Mode'Enum_Rep (PS.Read_Now);
      Call (Msg, PS.Summary_Label, Answered, Refused);
      if not Answered or else Msg.words (PS.Generation_Word) = 0
        or else not PS.Valid_Summary (Msg.words (PS.Payload_Word), Msg.words (PS.Payload_Word + 1))
      then
         Why := CuBit.Failures.Failed (CuBit.Failures.Unavailable, "power: power.svc did not answer its summary");
         return;
      end if;
      Generation := Msg.words (PS.Generation_Word);
      Summary := PS.To_Summary (Msg.words (PS.Payload_Word), Msg.words (PS.Payload_Word + 1));
      for I in 1 .. Summary.Sources loop
         for P in PS.Page loop
            Msg := Request (PS.Source_Label, PS.Source_Request_Words);
            Msg.words (PS.Expected_Word) := Generation;
            Msg.words (PS.Index_Word) := Unsigned_64 (I);
            Msg.words (PS.Page_Word) := PS.Page'Enum_Rep (P);
            Call (Msg, PS.Source_Label, Answered, Refused);
            if not Answered then
               --  A refusal of the generation: the snapshot moved on.
               if Refused and then Msg.words (PS.Refusal_Word) = PS.Refusal'Enum_Rep (PS.Stale_Generation) then
                  Result := Changed;
               else
                  Why := CuBit.Failures.Failed
                    (CuBit.Failures.Unavailable, "power: power.svc did not answer a source's page");
               end if;
               return;
            end if;
            PS.Read_Page (P, [Msg.words (PS.Payload_Word), Msg.words (PS.Payload_Word + 1)], Sources (I), Valid);
            if not Valid then
               Why := CuBit.Failures.Failed (CuBit.Failures.Refused, "power: power.svc sent a page it never writes");
               return;
            end if;
         end loop;
      end loop;
      Count := Summary.Sources;
      Result := Done;
   end Read_Once;

   procedure Read
     (Sources : out PS.Source_Array; Count : out PS.Source_Count;
      Success : out Boolean; Why : out CuBit.Failures.Failure)
   is
      Result : Outcome := Changed;
   begin
      Sources := [others => (others => <>)];
      Count := 0;
      Success := False;
      Why := (others => <>);
      if not Available then
         Why := CuBit.Failures.Failed
           (CuBit.Failures.Not_Granted, "power: this program has no endpoint to power.svc's power sources",
            "the program's manifest must request (service ""power-sources"" ""power-sources"") " &
            "from the startup plan");
         return;
      end if;
      for Attempt in 1 .. Attempts loop
         Read_Once (Sources, Count, Result, Why);
         exit when Result /= Changed;
      end loop;
      case Result is
         when Done => Success := True;
         when Changed =>
            Why := CuBit.Failures.Failed (CuBit.Failures.Unavailable, "power: the sources kept changing while read");
         when Failed => null;
      end case;
   end Read;
end CCL_Power;

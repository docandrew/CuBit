pragma Ada_2022;
with CuBit.Log;
with CuBit.Handles;
with CuBit.Handles.IPC;
with CuBit.Introductions;
with CuBit.Monotonic;
with CuBit.Power_Sources;
with CuBit.Wake_Words;
with Power_ACPI_Facts;
with Power_ACPI_Sources;
with Power_ACPI_Watch;
with Power_Source_Mapping;

package body Power_Source_Service is
   use CuBit.Messages;
   package PS renames CuBit.Power_Sources;
   package AF renames Power_ACPI_Facts;
   package PW renames Power_ACPI_Watch;
   package PM renames Power_Source_Mapping;
   use type PS.Generation, PS.Source_Array, PS.Summary_Mode, PS.Runtime_Kind, PW.Step;

   --  Each request to acpi.svc: its observer pages are copies of its cache,
   --  answered at once; this bounds a stuck acpi.svc.
   ACPI_Read_Budget_Milliseconds : constant := 250;
   Microseconds_Per_Millisecond : constant := 1_000;

   --  The completion tokens of power.svc's submits for the sources (main's
   --  own are 1 .. 3).
   Watch_Token  : constant Unsigned_64 := 16#5057_0001#;
   Demand_Token : constant Unsigned_64 := 16#5057_0003#;

   --  The startup-table entry acpi.svc's observer endpoint arrives in.
   Observer_Entry : constant String := "acpi-observer";

   --  Named ceiling: the watches kept at once (one per reader), and as
   --  many Read_Now answers waiting for one demand read.
   Maximum_Watchers : constant := 8;

   type Kept is record
      Present : Boolean := False;
      Handle  : CuBit.Handles.Reply;
      From    : Process_ID := No_Process;
      Id      : Unsigned_64 := 0;
   end record;
   type Kept_Array is array (1 .. Maximum_Watchers) of Kept;
   Watchers, Waiting : Kept_Array;

   ACPI : CuBit.Handles.Startup.Optional_Endpoint;
   --  Written by the introduction thread, then Arrived_Late set and the
   --  main thread's activity wait woken (CuBit.Wake_Words): Advance takes
   --  the endpoint on the main thread.
   Introduced : CuBit.Handles.Endpoint with Volatile;
   Arrived_Late : Boolean := False with Atomic;

   Age_Us : AF.Word := 0;
   Current : PS.Generation := PS.No_Generation;
   Sources : PS.Source_Array := [others => (others => <>)];
   Count : PS.Source_Count := 0;
   Summary : PS.Summary;

   --  The watch: one in flight at a time.
   Watch_In_Flight, Watch_Ended : Boolean := False;
   Watch_Id : Unsigned_64 := 0;
   Next_Watch_Id : Unsigned_64 := 1;
   Seen : PW.Generation := 0;
   Retry_At : Unsigned_64 := 0;
   Refusals : Natural := 0;
   --  acpi.svc answered the watch Changed: it serves its sources.
   Serving : Boolean := False;

   Demand_In_Flight : Boolean := False;
   --  Requests acpi.svc did not answer during one reading.
   Unanswered : Natural := 0;

   function Image (V : Unsigned_64) return String is
      T : constant String := Unsigned_64'Image (V);
   begin
      return T (T'First + 1 .. T'Last);
   end Image;

   procedure Ask
     (Request : AF.Words; Label : out Unsigned_32; Reply : out AF.Words; Delivered : out Boolean)
   is
      Msg : Message := NULL_MESSAGE;
      Tag : MessageTag;
   begin
      Label := 0;
      Reply := [others => 0];
      Delivered := False;
      if not ACPI.Present then
         return;
      end if;
      Msg.tag := (label => AF.Read_Metrics, length => AF.Request_Words, protocol => 0, ipc => <>, reserved => 0);
      for I in AF.Words'Range loop
         Msg.words (I) := Request (I);
      end loop;
      Tag := capCall (ACPI.Value, Msg, Deadline_After (ACPI_Read_Budget_Milliseconds));
      if Ending_Of (Tag) = Call_Answered and then Tag.length = AF.Request_Words then
         Delivered := True;
         Label := Tag.label;
         for I in AF.Words'Range loop
            Reply (I) := Msg.words (I);
         end loop;
      elsif Unanswered < Natural'Last then
         Unanswered := Unanswered + 1;
      end if;
   end Ask;
   package Reader is new Power_ACPI_Sources (Ask);

   function Quantity_Image (Q : PS.Quantity; Unit : String) return String is
     (if Q.Known then Image (Unsigned_64 (Q.Value)) & " " & Unit else "unknown");

   procedure Log_Source (S : PS.Source) is
   begin
      CuBit.Log.Info
        ("power: source " & PS.Image (S.Name) & " " & PS.Source_Kind'Image (S.Kind) & " " &
         PS.Source_State'Image (S.State) &
         (if PS.Is_Battery (S.Kind) then
            " charge " & (if S.Charge.Known then Image (Unsigned_64 (S.Charge.Value)) & "/1000" else "unknown") &
            " left " & PS.Runtime_Kind'Image (S.Left.Kind) &
            (if S.Left.Kind = PS.Unknown then "" else " " & Image (Unsigned_64 (S.Left.Seconds)) & " s") &
            " energy " & Quantity_Image (S.Energy_Now, "mWh") & " of " & Quantity_Image (S.Energy_Full, "mWh") &
            " rate " & Quantity_Image (S.Rate, "mW") & " low " & PS.Flag'Image (S.Condition.Low)
          else "") &
         " " & PS.Freshness'Image (S.Fresh));
   end Log_Source;

   procedure Log_Summary (Why : String; Requests : Natural) is
   begin
      CuBit.Log.Info
        ("power: sources generation " & Image (Unsigned_64 (Current)) & ":" & Natural'Image (Count) &
         " sources," & Natural'Image (Summary.Batteries) & " batteries, " &
         PS.Source_State'Image (Summary.State) & ", runs on " & PS.Supply'Image (Summary.Runs_On) &
         ", charge " & (if Summary.Charge.Known then Image (Unsigned_64 (Summary.Charge.Value)) & "/1000"
                        else "unknown") &
         ", " & PS.Freshness'Image (Summary.Fresh) & " (" & Why & "," & Natural'Image (Requests) &
         " acpi.svc requests)");
      for I in 1 .. Count loop
         Log_Source (Sources (I));
      end loop;
   end Log_Summary;

   function Summary_Answer (Id : Unsigned_64) return Message is
      Msg : Message := NULL_MESSAGE;
   begin
      Msg.tag := (label => PS.Summary_Label, length => PS.Reply_Words, protocol => 0, ipc => <>, reserved => 0);
      Msg.words (PS.Reply_Request_Word) := Id;
      Msg.words (PS.Generation_Word) := Unsigned_64 (Current);
      Msg.words (PS.Payload_Word) := PS.Summary_A (Summary);
      Msg.words (PS.Payload_Word + 1) := PS.Summary_B (Summary);
      return Msg;
   end Summary_Answer;

   function Refusal_Answer (Id : Unsigned_64; Why : PS.Refusal) return Message is
      Msg : Message := NULL_MESSAGE;
   begin
      Msg.tag := (label => PS.Refused_Label, length => PS.Refusal_Words, protocol => 0, ipc => <>, reserved => 0);
      Msg.words (PS.Reply_Request_Word) := Id;
      Msg.words (PS.Refusal_Word) := PS.Refusal'Enum_Rep (Why);
      return Msg;
   end Refusal_Answer;

   --  Every kept answer of List, with the snapshot now.
   procedure Answer_All (List : in out Kept_Array) is
      Ignore : Boolean;
   begin
      for K of List loop
         if K.Present then
            Ignore := CuBit.Handles.IPC.Answer (K.Handle, Summary_Answer (K.Id));
            K := (others => <>);
         end if;
      end loop;
   end Answer_All;

   --  The snapshot is Next now; the generation moves on when it changed.
   procedure Publish (Next : PS.Source_Array; Next_Count : PS.Source_Count; Why : String; Requests : Natural) is
   begin
      if Next_Count /= Count or else Next /= Sources then
         Sources := Next;
         Count := Next_Count;
         Summary := PM.Summarize (Sources, Count);
         if Current < PS.Generation'Last then
            Current := Current + 1;
         end if;
         Log_Summary (Why, Requests);
         Answer_All (Watchers);
      end if;
   end Publish;

   --  One reading (after the demand read): acpi.svc answers each page at
   --  once. One it did not answer in full keeps the snapshot, marked Stale.
   procedure Read_Sources (Why : String) is
      Reading : Reader.Reading;
   begin
      Unanswered := 0;
      --  While the watch is live, the adapter's state is acpi.svc's copy
      --  as its last Notify left it: current, whatever its age.
      Reader.Read (Age_Us, Reading, AC_Maximum_Age => (if Serving then AF.Any_Age else 0));
      if Unanswered = 0 then
         Publish (Reading.Sources, Reading.Count, Why, Reading.Requests);
      else
         CuBit.Log.Warning ("power: acpi.svc did not answer" & Natural'Image (Unanswered) &
                            " of its sources' requests; the last reading is kept, as Stale");
         Publish (PM.Aged (Sources), Count, Why & ", incomplete", Reading.Requests);
      end if;
   end Read_Sources;

   --  acpi.svc no longer serves its sources: what was read stays, Stale.
   procedure Lose_Sources (Why : String) is
   begin
      Serving := False;
      Watch_Ended := True;
      CuBit.Log.Warning ("power: acpi.svc's power-source watch ended (" & Why &
                         "); the last reading is kept, as Stale");
      Publish (PM.Aged (Sources), Count, "watch ended", 0);
      Answer_All (Waiting);
   end Lose_Sources;

   procedure Submit_Watch is
      Msg : Message := NULL_MESSAGE;
      Words : PW.Words;
   begin
      if not ACPI.Present or else Watch_In_Flight or else Watch_Ended then
         return;
      end if;
      if Next_Watch_Id = Unsigned_64'Last then
         Lose_Sources ("no watch id is left");
         return;
      end if;
      Watch_Id := Next_Watch_Id;
      Next_Watch_Id := Next_Watch_Id + 1;
      Words := PW.Request (Watch_Id, Seen);
      Msg.tag := (label => PW.Watch_Label, length => PW.Request_Words, protocol => 0, ipc => <>, reserved => 0);
      for I in PW.Words'Range loop
         Msg.words (I) := Words (I);
      end loop;
      Watch_In_Flight := capSubmit (ACPI.Value, Msg, Watch_Token);
      if not Watch_In_Flight then
         Retry_At := CuBit.Monotonic.Milliseconds + PW.Unavailable_Retry_Milliseconds;
         CuBit.Log.Warning ("power: the power-source watch could not be submitted to acpi.svc; again in" &
                            Unsigned_64'Image (PW.Unavailable_Retry_Milliseconds) & " ms");
      end if;
   end Submit_Watch;

   --  The demand read (acpi.svc's battery inventory within Age_Us): its
   --  completion starts the reading. One at a time; a reading asked for
   --  meanwhile is the one after it.
   procedure Demand is
      Msg : Message := NULL_MESSAGE;
      Words : constant AF.Words := AF.Inventory_Request (Age_Us);
   begin
      if Demand_In_Flight then
         return;
      end if;
      if ACPI.Present then
         Msg.tag := (label => AF.Read_Metrics, length => AF.Request_Words, protocol => 0, ipc => <>,
                     reserved => 0);
         for I in AF.Words'Range loop
            Msg.words (I) := Words (I);
         end loop;
         Demand_In_Flight := capSubmit (ACPI.Value, Msg, Demand_Token);
      end if;
      if not Demand_In_Flight then
         Read_Sources ("read without a demand");
         Answer_All (Waiting);
      end if;
   end Demand;

   procedure Watch_Answer (Completion : CompletionEntry; Id : Unsigned_64) is
      Next : PW.Step;
      New_Seen : PW.Generation;
      Why : PW.Status;
      Answered : Boolean;
      Reply : constant PW.Words := [for I in PW.Words'Range => Completion.msg.words (I)];
   begin
      Watch_In_Flight := False;
      if Completion.status /= COMPLETION_OK then
         Lose_Sources ("not delivered, completion status " & Image (Completion.status));
         return;
      end if;
      PW.Decide (Completion.msg.tag.label, Natural (Completion.msg.tag.length), Reply, Id, Seen,
                 Next, New_Seen, Why, Answered);
      case Next is
         when PW.Read_Then_Watch =>
            if not Serving then
               CuBit.Log.Info ("power: acpi.svc serves its power sources (watch generation " &
                               Image (New_Seen) & ", after" & Natural'Image (Refusals) & " refusals)");
            end if;
            Serving := True;
            Refusals := 0;
            Seen := New_Seen;
            Submit_Watch;
            Demand;
         when PW.Watch_Again =>
            Submit_Watch;
         when PW.Retry_Later =>
            if Refusals = 0 then
               CuBit.Log.Info ("power: acpi.svc does not take the power-source watch yet (" & PW.Name (Why) &
                               "); watching again every" &
                               Unsigned_64'Image (PW.Unavailable_Retry_Milliseconds) & " ms until it does");
            end if;
            if Refusals < Natural'Last then
               Refusals := Refusals + 1;
            end if;
            Retry_At := CuBit.Monotonic.Milliseconds + PW.Unavailable_Retry_Milliseconds;
         when PW.Resynchronize =>
            CuBit.Log.Info ("power: acpi.svc's watch generation is behind the one seen (a new acpi.svc); " &
                            "watching from its start");
            Seen := New_Seen;
            Submit_Watch;
         when PW.Give_Up =>
            Lose_Sources (if Answered then PW.Name (Why)
                          else "an answer acpi.svc never writes, label " &
                               Image (Unsigned_64 (Completion.msg.tag.label)));
      end case;
   end Watch_Answer;

   procedure Collect (Completion : CompletionEntry) is
   begin
      if Completion.token = Watch_Token then
         Watch_Answer (Completion, Watch_Id);
      elsif Completion.token = Demand_Token then
         Demand_In_Flight := False;
         Read_Sources (if Serving then "watched" else "read");
         Answer_All (Waiting);
      end if;
   end Collect;

   function Handles_Completion (Token : Unsigned_64) return Boolean is
     (Token in Watch_Token | Demand_Token);

   function Next_Due return Unsigned_64 is (Retry_At);

   procedure Advance (Now : Unsigned_64) is
   begin
      if Arrived_Late and then not ACPI.Present then
         ACPI := (Present => True, Value => Introduced);
         CuBit.Log.Info ("power: introduced to acpi.svc's observer endpoint");
         Submit_Watch;
      end if;
      if Retry_At /= 0 and then Now >= Retry_At then
         Retry_At := 0;
         Submit_Watch;
      end if;
   end Advance;

   --  On the introduction thread: keep the endpoint and wake the main
   --  thread, which takes it (Advance). Nothing else is touched here.
   procedure Arrived (Value : CuBit.Handles.Endpoint) is
   begin
      Introduced := Value;
      Arrived_Late := True;
      CuBit.Wake_Words.Wake;
   end Arrived;

   procedure Start (Observer : CuBit.Handles.Startup.Optional_Endpoint; Maximum_Age_Ms : Unsigned_64) is
      Waits : Boolean := False;
   begin
      ACPI := Observer;
      Age_Us := AF.Word'Min (AF.Any_Age, Maximum_Age_Ms * Microseconds_Per_Millisecond);
      --  The snapshot before anything is read: no sources yet.
      Current := 1;
      Summary := PM.Summarize (Sources, Count);
      if ACPI.Present then
         CuBit.Log.Info ("power: watching acpi.svc's power sources (no sources until it serves them)");
         Submit_Watch;
      else
         CuBit.Introductions.In_Background (Observer_Entry, Arrived'Access, Waits);
         CuBit.Log.Info (if Waits then "power: acpi.svc is not running yet: its power sources are watched " &
                                       "when procmgr introduces it"
                         else "power: no acpi.svc observer endpoint: no ACPI power sources");
      end if;
   end Start;

   function Handles (Label : Unsigned_32) return Boolean is
     (Label in PS.Summary_Label | PS.Source_Label);

   procedure Watch (From : Process_ID; Id, Seen_By_Reader : Unsigned_64) is
      Free : Natural := 0;
      Ignore : Unsigned_64;
      Ignore_Answer : Boolean;
   begin
      for I in Watchers'Range loop
         if Watchers (I).Present and then Watchers (I).From = From then
            --  One watch per reader: the newer one takes its place.
            Ignore_Answer := CuBit.Handles.IPC.Answer (Watchers (I).Handle, Refusal_Answer (Watchers (I).Id, PS.Superseded));
            Watchers (I) := (others => <>);
         end if;
         if not Watchers (I).Present and then Free = 0 then
            Free := I;
         end if;
      end loop;
      if Seen_By_Reader /= Unsigned_64 (Current) then
         Ignore := reply (From, Summary_Answer (Id));
         return;
      end if;
      if Free = 0 then
         Ignore := reply (From, Refusal_Answer (Id, PS.No_Room));
         return;
      end if;
      declare
         Saved : constant CuBit.Handles.IPC.Saved_Reply := CuBit.Handles.IPC.Save_Reply;
      begin
         if Saved.Saved then
            Watchers (Free) := (Present => True, Handle => Saved.Handle, From => From, Id => Id);
         else
            Ignore := reply (From, Refusal_Answer (Id, PS.No_Room));
         end if;
      end;
   end Watch;

   --  Read_Now: while acpi.svc serves its sources, answered after one
   --  demand read and reading; otherwise (or with no room to keep the
   --  answer) at once, with the snapshot as it is.
   procedure Read_Now (From : Process_ID; Id : Unsigned_64) is
      Free : Natural := 0;
      Ignore : Unsigned_64;
   begin
      if Serving and then ACPI.Present then
         for I in Waiting'Range loop
            if not Waiting (I).Present then
               Free := I;
               exit;
            end if;
         end loop;
         if Free /= 0 then
            declare
               Saved : constant CuBit.Handles.IPC.Saved_Reply := CuBit.Handles.IPC.Save_Reply;
            begin
               if Saved.Saved then
                  Waiting (Free) := (Present => True, Handle => Saved.Handle, From => From, Id => Id);
                  Demand;
                  return;
               end if;
            end;
         end if;
      end if;
      Ignore := reply (From, Summary_Answer (Id));
   end Read_Now;

   procedure Serve (From : Process_ID; Request : Message) is
      Id : constant Unsigned_64 := Request.words (PS.Request_Word);
      Ignore : Unsigned_64;
      Msg : Message := NULL_MESSAGE;
   begin
      if not PS.Is_Observer (Request.authorityTag) then
         Ignore := reply (From, Refusal_Answer (Id, PS.Not_Approved));
         return;
      end if;
      if Request.tag.label = PS.Summary_Label then
         if Request.tag.length /= PS.Summary_Request_Words or else Id = 0
           or else not PS.Valid_Mode (Request.words (PS.Mode_Word))
         then
            Ignore := reply (From, Refusal_Answer (Id, PS.Malformed));
         elsif PS.To_Mode (Request.words (PS.Mode_Word)) = PS.Read_Now then
            Read_Now (From, Id);
         else
            Watch (From, Id, Request.words (PS.Seen_Word));
         end if;
         return;
      end if;
      --  A page of one source.
      if Request.tag.length /= PS.Source_Request_Words or else Id = 0
        or else not PS.Valid_Page (Request.words (PS.Page_Word))
      then
         Ignore := reply (From, Refusal_Answer (Id, PS.Malformed));
      elsif Request.words (PS.Expected_Word) /= Unsigned_64 (Current) then
         Ignore := reply (From, Refusal_Answer (Id, PS.Stale_Generation));
      elsif Request.words (PS.Index_Word) not in 1 .. Unsigned_64 (Count) then
         Ignore := reply (From, Refusal_Answer (Id, PS.No_Such_Source));
      else
         declare
            Data : constant PS.Payload :=
              PS.Page_Words (Sources (PS.Source_Index (Request.words (PS.Index_Word))),
                             PS.To_Page (Request.words (PS.Page_Word)));
         begin
            Msg.tag := (label => PS.Source_Label, length => PS.Reply_Words, protocol => 0, ipc => <>,
                        reserved => 0);
            Msg.words (PS.Reply_Request_Word) := Id;
            Msg.words (PS.Generation_Word) := Unsigned_64 (Current);
            Msg.words (PS.Payload_Word) := Data (0);
            Msg.words (PS.Payload_Word + 1) := Data (1);
            Ignore := reply (From, Msg);
         end;
      end if;
   end Serve;
end Power_Source_Service;

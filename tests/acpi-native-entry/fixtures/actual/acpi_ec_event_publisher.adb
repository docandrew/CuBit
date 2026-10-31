pragma Ada_2022;
package body ACPI_EC_Event_Publisher with SPARK_Mode is
   package T renames ACPI_EC_Types;
   package E renames ACPI_EC_Event_Types;
   use type T.Byte, T.Outcome, T.Work_Budget;
   use type E.Controller_ID, E.Epoch_ID, E.Publication, E.Notice;
   use type CuBit.Process_IDs.Process_ID;
   use type Registers.Submission, Registers.Operation_ID;
   Current : Phase := Unconfigured;
   Source : E.Controller_ID := 0;
   Generation : E.Epoch_ID := 0;
   Token : Registers.Operation_ID := 0;
   Hardware : T.Outcome := T.Idle;
   Notice : E.Notice_Option;
   Remaining, Attempt_Count : T.Work_Budget := 0;
   function Observe return Report is
     ((Stage => Current, Token => Token, Hardware => Hardware,
       Hardware_Recovery => Registers.Retained_For_Recovery,
       Publication_Attempts => Attempt_Count, Remaining => Remaining));
   function Retained return E.Notice_Option is (Notice);
   procedure Configure
     (Controller : E.Controller_ID; Epoch : E.Epoch_ID; Accepted : out Boolean) is
   begin
      Accepted := False;
      if Current /= Unconfigured then return; end if;
      Current := Configuration_Refused;
      if Controller = 0 or else Epoch = 0 then return; end if;
      Source := Controller; Generation := Epoch;
      Current := Ready; Accepted := True;
   end Configure;
   procedure Start_Query
     (Deadline : T.Microseconds; Budget : T.Work_Budget;
      Delivery_Attempts : E.Delivery_Budget; Result : out Registers.Submission) is
   begin
      Result := Registers.Busy;
      if Current /= Ready then return; end if;
      Registers.Submit_Query (Deadline, Budget, Result, Token);
      if Result /= Registers.Started then return; end if;
      Hardware := T.Idle;
      Remaining := T.Work_Budget (Delivery_Attempts); Attempt_Count := 0;
      Current := Querying;
   end Start_Query;
   procedure Advance is
      Fact : Registers.Query_Completion;
      Result : E.Publication;
   begin
      if Current = Querying then
         Registers.Step;
         Registers.Take_Query (Fact);
         if not Fact.Present then return; end if;
         if Fact.Token /= Token then Current := Correlation_Failed; return; end if;
         Hardware := Fact.Status;
         if Fact.Needs_Recovery then Current := Query_Failed;
         elsif Fact.Status = T.No_Event
           or else (Fact.Status = T.Succeeded and Fact.Event = 0)
         then
            Current := Ready; Token := 0;
         elsif Fact.Status = T.Succeeded then
            Notice := (Present => True,
              Item => (Source, Generation, E.Token_ID (Token), E.Event_Number (Fact.Event)));
            Current := Publication_Pending;
         else Current := Query_Failed;
         end if;
      elsif Current = Publication_Pending then
         if not Notice.Present or else Remaining = 0 then
            Current := Correlation_Failed; return;
         end if;
         Remaining := Remaining - 1; Attempt_Count := Attempt_Count + 1;
         --  An exceptional/unknown enqueue might have committed. Retain the
         --  exact notice and disable automatic retry before calling outward.
         Current := Delivery_Unknown;
         Publish (Notice.Item, Result);
         case Result is
            when E.Enqueued =>
               Notice := (Present => False); Current := Ready; Token := 0;
            when E.Backpressure =>
               Current := (if Remaining = 0 then Delivery_Stalled else Publication_Pending);
            when E.Indeterminate => null;
         end case;
      end if;
   end Advance;
   procedure Resume_Publication (Attempts : E.Delivery_Budget; Accepted : out Boolean) is
   begin
      Accepted := False;
      if Current /= Delivery_Stalled or else not Notice.Present then return; end if;
      --  New bounded retry episode; the event identity and hardware query stay
      --  unchanged. Episode attempt counts cannot overflow Work_Budget.
      Remaining := T.Work_Budget (Attempts);
      Attempt_Count := 0;
      Current := Publication_Pending; Accepted := True;
   end Resume_Publication;
   procedure Resume_Queries (Accepted : out Boolean) is
   begin
      Accepted := False;
      if Current /= Query_Failed or else Registers.Active
        or else Registers.Retained_For_Recovery or else Notice.Present
      then return; end if;
      Token := 0; Remaining := 0; Current := Ready; Accepted := True;
   end Resume_Queries;
   procedure Confirm_Claim
     (Sender : CuBit.Process_IDs.Process_ID; Item : E.Notice;
      Accepted : out Boolean) is
   begin
      Accepted := False;
      if Expected_Consumer = CuBit.Process_IDs.No_Process
        or else Sender /= Expected_Consumer
        or else Current /= Delivery_Unknown
        or else not Notice.Present
        or else Item /= Notice.Item
      then return; end if;
      Notice := (Present => False);
      Token := 0; Remaining := 0; Current := Ready; Accepted := True;
   end Confirm_Claim;
end ACPI_EC_Event_Publisher;

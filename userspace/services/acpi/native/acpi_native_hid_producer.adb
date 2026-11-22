pragma Ada_2022;
with ACPI_One_Service;
with ACPI_Native_Pre_Device;
with ACPI_Native_Initializer_History;
with ACPI_Native_HID_Offers;
with ACPI_Consumer_Request_IDs;
with CuBit.Messages;
with CuBit.GPIO_Bootstrap_Exchange;
with CuBit.HID_Offer_Exchange;
package body ACPI_Native_HID_Producer is
   package One renames ACPI_One_Service;
   package Collection renames ACPI_Native_HID_Collection.Controller;
   package Provenance renames ACPI_Native_HID_Collection.Provenance;
   package IDs renames ACPI_Consumer_Request_IDs;
   use type Interfaces.Unsigned_64;
   use type One.Runtime.Status.Outcome;
   use type Collection.Outcome, ACPI_Native_HID_Detection.Outcome;
   use type CuBit.HID_Offer_Exchange.Disposition;
   View : Report;
   AML_Ended : Boolean := False;
   Collector : Collection.Run;
   Last_Collection : Collection.Report;
   Sequence : IDs.State;
   -- The collection is one historical observation group for this process.
   Initial_Observation_Sequence : constant Interfaces.Unsigned_64 := 1;
   function Send_Boundary return Boolean is
     (View.Working and then View.Status = Submitting and then AML_Ended
      and then not View.Window_Open and then not View.Recovery_Needed
      and then Owner_Quiet);
   package Transport is new ACPI_Native_HID_Offers (Send_Boundary);
   Saved_Transport : Transport.Report;
   type Session_Array is array (Collection.Candidate_Index) of Transport.Session;
   Sessions : Session_Array;
   function Name (Status : Outcome) return String is
     (case Status is
        when Not_Attempted => "NOT_ATTEMPTED",
        when Collecting => "COLLECTING",
        when Submitting => "SUBMITTING",
        when No_Devices => "NO_DEVICES",
        when Discovery_Incomplete => "DISCOVERY_INCOMPLETE",
        when Admission_Unavailable => "ADMISSION_UNAVAILABLE",
        when History_Incomplete => "HISTORY_INCOMPLETE",
        when Boundary_Refused => "BOUNDARY_REFUSED",
        when Window_Refused => "WINDOW_REFUSED",
        when Collection_Failed => "COLLECTION_FAILED",
        when Device_Unavailable => "DEVICE_UNAVAILABLE",
        when Recovery_Required => "RECOVERY_REQUIRED",
        when Request_ID_Exhausted => "REQUEST_ID_EXHAUSTED",
        when Offers_Refused => "OFFERS_REFUSED",
        when Offers_Queued => "OFFERS_QUEUED",
        when Handoff_Unsettled => "HANDOFF_UNSETTLED");
   function Observe return Report is (View);
   function Collection_Observation return Collection.Report is (Last_Collection);
   function Last_Transport return String is
     (case Saved_Transport.State is
        when Transport.Not_Attempted => "NOT_ATTEMPTED",
        when Transport.Boundary_Refused => "BOUNDARY_REFUSED",
        when Transport.Payload_Refused => "PAYLOAD_REFUSED",
        when Transport.Endpoint_Refused => "ENDPOINT_REFUSED",
        when Transport.Clock_Refused => "CLOCK_REFUSED",
        when Transport.Allocation_Refused => "ALLOCATION_REFUSED",
        when Transport.Grant_Refused => "GRANT_REFUSED",
        when Transport.Not_Dispatched => "NOT_DISPATCHED",
        when Transport.Completion_Unknown => "COMPLETION_UNKNOWN",
        when Transport.Invalid_Reply => "INVALID_REPLY",
        when Transport.Reply_Confirmed => "REPLY_CONFIRMED");
   function Last_Receipt return String is
     (if not Saved_Transport.Reply_Known then "UNKNOWN" else
        (case Saved_Transport.Answer is
           when CuBit.HID_Offer_Exchange.Queued => "QUEUED",
           when CuBit.HID_Offer_Exchange.Not_Approved => "NOT_APPROVED",
           when CuBit.HID_Offer_Exchange.Malformed => "MALFORMED",
           when CuBit.HID_Offer_Exchange.Capacity => "CAPACITY",
           when CuBit.HID_Offer_Exchange.Unknown_Controller => "UNKNOWN_CONTROLLER",
           when CuBit.HID_Offer_Exchange.Controller_Mismatch => "CONTROLLER_MISMATCH",
           when CuBit.HID_Offer_Exchange.GPIO_Not_Admitted => "GPIO_NOT_ADMITTED",
           when CuBit.HID_Offer_Exchange.Duplicate_Device => "DUPLICATE_DEVICE",
           when CuBit.HID_Offer_Exchange.Copy_Failed => "COPY_FAILED"));
   function Quiet return Boolean is
   begin
      if View.Working or else View.Window_Open or else View.Recovery_Needed then return False; end if;
      for Item of Sessions loop
         if not Transport.Quiet (Item) then return False; end if;
      end loop;
      return True;
   end Quiet;
   function Retention_Required return Boolean is
   begin
      if not Quiet then return True; end if;
      for Item of Sessions loop
         declare Info : constant Transport.Report := Transport.Observe (Item); begin
            if Info.Dispatched and then (not Info.Reply_Known or else Info.Answer = CuBit.HID_Offer_Exchange.Queued)
            then return True; end if;
         end;
      end loop;
      return False;
   end Retention_Required;
   procedure Advance_Cleanup is
   begin
      if View.Working then return; end if;
      View.Working := True;
      for Item of Sessions loop Transport.Advance_Cleanup (Item); end loop;
      View.Working := False;
      if View.Status = Handoff_Unsettled and then Quiet then
         View.Status := (if View.Queued = View.Candidates then Offers_Queued else Offers_Refused);
      end if;
   end Advance_Cleanup;
   procedure Start (Server : in out ACPI_Requests.State;
      Discovery : ACPI_Native_HID_Detection.Report;
      Target : CuBit.Handles.Endpoint;
      Original_Deadline_Microseconds, Original_Deadline_Milliseconds : Interfaces.Unsigned_64;
      Result : out Report) is
      Submitted : ACPI_Native_Pre_Device.Submitted_Request;
      History : ACPI_Native_Initializer_History.Report;
      Devices : Collection.Device_Array := [others => ACPI_Service.Values.No_Node];
      Stamp : Provenance.Stamp;
      Collected : Collection.Report;
      Window_Result : One.Runtime.Status.Outcome;
      Request : IDs.Request_ID;
      Allocated : Boolean;
      Info : Transport.Report;
      procedure Finish (Status : Outcome) is
      begin
         View.Status := Status; View.Working := False; Result := View;
      end Finish;
   begin
      if View.Attempted then Result := View; return; end if;
      View.Attempted := True; View.Working := True;
      View.Node_Errors := Discovery.Node_Errors;
      Submitted := ACPI_Native_Pre_Device.Submitted;
      History := ACPI_Native_Initializer_History.Observe;
      if Discovery.Cleanup_Required or else Discovery.Recovery_Needed then
         View.Recovery_Needed := True; Finish (Recovery_Required); return;
      end if;
      if Discovery.Status = ACPI_Native_HID_Detection.No_Devices
        and then Discovery.Candidates = 0 and then Discovery.Observed = 0 and then Discovery.Failed_Candidates = 0
      then Finish (No_Devices); return; end if;
      if Discovery.Status = ACPI_Native_HID_Detection.Finished
        and then Discovery.Observed = 0 and then Discovery.Candidates > 0
        and then Discovery.Candidates = Discovery.Failed_Candidates
        and then Discovery.Node_Errors >= Discovery.Failed_Candidates
      then Finish (No_Devices); return; end if;
      if Discovery.Status /= ACPI_Native_HID_Detection.Finished
        or else Discovery.Observed = 0
        or else Natural (Discovery.Candidates) /= Natural (Discovery.Observed) + Natural (Discovery.Failed_Candidates)
        or else Discovery.Node_Errors < Discovery.Failed_Candidates
      then Finish (Discovery_Incomplete); return; end if;
      if not Submitted.Present or else not Submitted.Receipt_Known
        or else Submitted.Answer not in CuBit.GPIO_Bootstrap_Exchange.Admitted | CuBit.GPIO_Bootstrap_Exchange.P2SB_Available
        or else Submitted.Data.Epoch /= CuBit.Messages.To_Word (CuBit.Messages.Own_Process)
      then Finish (Admission_Unavailable); return; end if;
      if not History.Complete then Finish (History_Incomplete); return; end if;
      if not Owner_Quiet or else not ACPI_Native_Pre_Device.Quiet
        or else ACPI_Requests.Retained_Results_Pending (Server)
      then Finish (Boundary_Refused); return; end if;
      View.Status := Collecting;
      for I in 1 .. Discovery.Observed loop Devices (I) := Discovery.Items (I).Device; end loop;
      Stamp := (Epoch => CuBit.Messages.To_Word (CuBit.Messages.Own_Process),
        Catalog_Revision => Interfaces.Unsigned_64 (ACPI_Requests.Revision (Server)),
        Observation_Sequence => Initial_Observation_Sequence,
        Initializer_Count => History.Invocations, Initializer_Digest => History.Ordered_Digest);
      -- A refusal never opens another allowance or renews either clock epoch.
      One.Runtime_Access.Begin_Event (Server, Original_Deadline_Microseconds, Window_Result);
      if Window_Result /= One.Runtime.Status.Event_Opened then Finish (Window_Refused); return; end if;
      View.Window_Open := True; View.Recovery_Needed := True;
      Collect_Offers (Collector, Server, Devices, Discovery.Observed,
        Submitted.Device, Submitted.Data, Stamp,
        ACPI_Native_HID_Detection.Maximum_AML_Fuel,
        ACPI_Native_HID_Detection.Maximum_Resource_Bytes, Collected);
      Last_Collection := Collected;
      View.Charged := Collected.Charged;
      View.Node_Errors := View.Node_Errors + Collected.Node_Errors;
      View.Status_Known := Collected.Status_Known; View.Status_Flags := Collected.Status_Flags;
      View.Recovery_Needed := Collected.Working or else Collected.Recovery_Needed
        or else ACPI_Requests.Retained_Results_Pending (Server);
      One.Runtime_Access.End_Event (Server, not View.Recovery_Needed, Window_Result);
      View.Window_Open := Window_Result /= One.Runtime.Status.Event_Completed;
      if View.Window_Open or else View.Recovery_Needed then
         View.Recovery_Needed := True; Finish (Recovery_Required); return;
      end if;
      AML_Ended := True;
      if Collected.Status = Collection.Device_Unavailable then Finish (No_Devices); return; end if;
      if Collected.Status /= Collection.Prepared then Finish (Collection_Failed); return; end if;
      if Collected.Count = 0 then Finish (No_Devices); return; end if;
      View.Candidates := Collected.Count;
      View.Status := Submitting;
      for I in 1 .. Collected.Count loop
         IDs.Take (Sequence, Request, Allocated);
         if not Allocated then Finish (Request_ID_Exhausted); return; end if;
         Transport.Submit (Sessions (I), Target, Collected.Values (I), Interfaces.Unsigned_64 (Request),
           Original_Deadline_Milliseconds, Info);
         -- One explicit same-id confirmation, never another probe/description.
         Transport.Confirm (Sessions (I), Info);
         Transport.Advance_Cleanup (Sessions (I));
         Info := Transport.Observe (Sessions (I));
         Saved_Transport := Info;
         View.Last_Request := Info.Request; View.Last_Detail := Info.Detail;
         if Info.Dispatched then View.Dispatched := View.Dispatched + 1; end if;
         if Info.Reply_Known then
            View.Known := View.Known + 1;
            if Info.Answer = CuBit.HID_Offer_Exchange.Queued then View.Queued := View.Queued + 1; end if;
         end if;
         -- An unresolved first offer ends this handoff; later rows remain
         -- unsent. Retain all state and do not declare initialization complete.
         if not Transport.Quiet (Sessions (I)) then Finish (Handoff_Unsettled); return; end if;
      end loop;
      Finish (if View.Queued = View.Candidates then Offers_Queued else Offers_Refused);
   exception
      when others =>
         -- Preserve the original window, grants and any unknown child debt.
         -- No retry, RPC or evaluator/cleanup work is safe here.
         View.Status := Recovery_Required;
         View.Working := False;
         View.Recovery_Needed := True;
         Result := View;
   end Start;
end ACPI_Native_HID_Producer;

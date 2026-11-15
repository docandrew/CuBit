pragma Ada_2022;
with ACPI_Native_HID_Producer; with ACPI_Native_HID_Detection; with ACPI_Requests;
with Producer_Model; with Sender_Model; with Ada.Command_Line; with Ada.Text_IO;
with Interfaces;
procedure Producer_Tests is
   package M renames Producer_Model; package S renames Sender_Model;
   function Boundary return Boolean is
   begin
      S.Boundary_Reads := S.Boundary_Reads + 1;
      return M.Boundary and then not M.Window_Open and then not M.Pending;
   end Boundary;
   package T is new ACPI_Native_HID_Producer (Boundary);
   use type T.Outcome, Interfaces.Unsigned_64;
   Server : ACPI_Requests.State;
   Discovery : ACPI_Native_HID_Detection.Report;
   Result, Again : T.Report;
   Mode : constant Natural := Natural'Value (Ada.Command_Line.Argument (1));
   MS : Interfaces.Unsigned_64 := 10000;
begin
   S.Reset; M.Mode := Mode; S.Own_ID := 16#F123_4567_0000_0007#;
   S.Times := [others => 100]; S.Second_Retired := True;
   Discovery.Status := ACPI_Native_HID_Detection.Finished;
   Discovery.Candidates := 2; Discovery.Observed := 2;
   Discovery.Items (1).Device := 11; Discovery.Items (2).Device := 12;
   case Mode is
      when 2 => Discovery.Status := ACPI_Native_HID_Detection.No_Devices; Discovery.Candidates := 0; Discovery.Observed := 0;
      when 3 => Discovery.Status := ACPI_Native_HID_Detection.Candidate_Limit;
      when 8 => M.History_Complete := False;
      when 9 => M.Boundary := False;
      when 17 => S.Confirmed := False;
      when 18 => MS := 100;
      when 22 => Discovery.Status := ACPI_Native_HID_Detection.No_Devices; Discovery.Recovery_Needed := True;
      when 23 => Discovery.Candidates := 3;
      when 24 => M.Original_US := 0;
      when 25 => S.Allocate_OK := False;
      when 26 => S.Create_OK := False;
      when 28 => MS := Interfaces.Unsigned_64'Last;
      when 38 => Discovery.Candidates := 3; Discovery.Failed_Candidates := 1; Discovery.Node_Errors := 1;
      when 41 => Discovery.Observed := 0; Discovery.Failed_Candidates := 2; Discovery.Node_Errors := 2;
      when 42 => Discovery.Status := ACPI_Native_HID_Detection.No_Devices; Discovery.Candidates := 0; Discovery.Observed := 0; Discovery.Node_Errors := 1;
      when 43 => Discovery.Failed_Candidates := 1; Discovery.Node_Errors := 1;
      when others => null;
   end case;
   T.Start (Server, Discovery, 77, M.Original_US, MS, Result);
   M.Check (Result.Attempted and then not Result.Working, "one attempt completes owner call");
   M.Check (T.Observe.Status = Result.Status, "copied and observed outcome agree");
   case Mode is
      when 1 | 14 | 29 =>
         M.Check (Result.Status = T.Offers_Queued and then Result.Candidates = 2 and then Result.Queued = 2
           and then Result.Dispatched = 2 and then Result.Known = 2, "both immutable offers queued");
         M.Check (S.Calls = (if Mode = 14 then 3 else 2) and then S.Frees = 2, "one bounded confirmation only when needed");
         M.Check (T.Quiet and then T.Retention_Required, "queued child obligation survives retired grants");
         M.Check (T.Last_Transport = "REPLY_CONFIRMED" and then T.Last_Receipt = "QUEUED",
           "queued diagnostic names remain explicit");
      when 2 => M.Check (Result.Status = T.No_Devices, "no devices is a finished absence");
      when 3 | 23 => M.Check (Result.Status = T.Discovery_Incomplete, "partial/inconsistent discovery not promoted");
      when 4 .. 7 => M.Check (Result.Status = T.Admission_Unavailable, "missing, unknown, refused or foreign-epoch GPIO admission refused");
      when 8 => M.Check (Result.Status = T.History_Incomplete, "incomplete initializer history refused");
      when 36 | 37 =>
         M.Check (Result.Status = T.Recovery_Required and then Result.Recovery_Needed
           and then not Result.Window_Open and then not T.Quiet and then T.Retention_Required,
           "metadata interruption latches before any owner window");
         M.Check (M.Opens = 0 and then M.Collections = 0 and then S.Calls = 0,
           "metadata interruption performs no window/AML/IPC");
      when 35 =>
         M.Check (Result.Status = T.Recovery_Required and then Result.Recovery_Needed
           and then Result.Window_Open and then M.Window_Open and then not T.Quiet
           and then T.Retention_Required, "collection interruption retains original owner window");
         M.Check (M.Collections = 1 and then M.Ends = 0 and then S.Calls = 0
           and then S.Allocations = 0, "interrupted collection performs no cleanup AML or offer IPC");
      when 31 .. 34 =>
         M.Check (Result.Status = T.No_Devices and then Result.Status_Known
           and then T.Quiet and then not T.Retention_Required and then S.Calls = 0,
           "unavailable device never reaches offer transport or child obligation");
         M.Check (Result.Status_Flags = (case Mode is when 31 => 0, when 32 => 1, when 33 => 2, when others => 8),
           "full unavailable status diagnostic preserved");
      when 9 | 30 => M.Check (Result.Status = T.Boundary_Refused, "busy owner/pre-device refused before AML");
      when 10 | 24 => M.Check (Result.Status = T.Window_Refused and then M.Collections = 0, "window refusal never collects");
      when 11 => M.Check (Result.Status = T.Collection_Failed and then T.Quiet and then not T.Retention_Required, "clean collection refusal closes window");
      when 12 | 13 | 22 =>
         M.Check (Result.Status = T.Recovery_Required and then not T.Quiet and then T.Retention_Required, "recovery fences completion/retirement");
      when 15 | 20 | 21 =>
         M.Check (Result.Status = T.Handoff_Unsettled and then not T.Quiet and then T.Retention_Required,
           "unknown receipt preserves child obligation even after page retirement");
         M.Check (Result.Dispatched = 1 and then Result.Known = 0 and then S.Calls = (if Mode = 20 then 1 else 2)
           and then S.Allocations = 1 and then S.Frees = 1, "no later offer or fresh page after unknown first outcome");
         M.Check (T.Last_Receipt = "UNKNOWN", "unknown outcome is never logged as a fabricated receipt");
      when 16 => M.Check (Result.Status = T.Offers_Refused and then T.Quiet and then not T.Retention_Required
        and then Result.Known = 2 and then Result.Queued = 0, "attributable refusal has no child obligation");
         M.Check (T.Last_Receipt = "NOT_APPROVED", "actual refusal reason is available to bring-up logging");
      when 17 =>
         M.Check (Result.Status = T.Handoff_Unsettled and then Result.Queued = 1 and then not T.Quiet
           and then T.Retention_Required and then S.Frees = 0, "revoke acceptance never frees pending grant");
         T.Advance_Cleanup; M.Check (S.Frees = 0 and then not T.Quiet, "bounded cleanup preserves pending page");
         S.Confirmed := True; T.Advance_Cleanup;
         M.Check (S.Frees = 1 and then T.Quiet and then T.Retention_Required
           and then T.Observe.Status = T.Offers_Refused and then T.Observe.Queued = 1,
           "late retirement cannot invent the unsent second offer or clear queued child obligation");
      when 18 | 19 | 25 | 26 | 28 =>
         M.Check (Result.Status = T.Offers_Refused and then T.Quiet and then not T.Retention_Required
           and then S.Calls = 0, "expired deadline, late boundary or allocation/grant failure never dispatches");
      when 38 =>
         M.Check (Result.Status = T.Offers_Queued and then Result.Queued = 2 and then Result.Node_Errors = 1
           and then S.Calls = 2, "known failed discovery candidate does not prevent complete observed offers");
      when 39 =>
         M.Check (Result.Status = T.Offers_Queued and then Result.Candidates = 1 and then Result.Queued = 1
           and then Result.Node_Errors = 1 and then S.Calls = 1, "fresh partial success submits only the completed subset");
      when 40 =>
         M.Check (Result.Status = T.No_Devices and then Result.Node_Errors = 2 and then M.Collections = 1
           and then S.Calls = 0 and then T.Quiet, "all ordinary collection errors complete without transport");
      when 41 | 42 =>
         M.Check (Result.Status = T.No_Devices and then Result.Node_Errors > 0 and then M.Collections = 0
           and then S.Calls = 0, "all known discovery errors require no fresh collection or transport");
      when 43 => M.Check (Result.Status = T.Discovery_Incomplete and then S.Calls = 0,
         "inconsistent observed plus failed discovery counts remain refused");
      when others => raise Program_Error with "unknown mode";
   end case;
   if Mode in 2 .. 10 | 22 .. 24 | 30 then
      M.Check (S.Calls = 0 and then M.Collections = 0, "early refusal performs no collection or IPC");
   end if;
   declare
      Before_Calls : constant Natural := S.Calls;
      Before_Collections : constant Natural := M.Collections;
      Before_Opens : constant Natural := M.Opens;
      Before_Submitted : constant Natural := M.Submitted_Reads;
      Before_History : constant Natural := M.History_Reads;
   begin
      Discovery.Status := ACPI_Native_HID_Detection.Finished;
      T.Start (Server, Discovery, 77, 99999999, 99999999, Again);
      T.Advance_Cleanup;
      M.Check (Again.Status = T.Observe.Status and then S.Calls = Before_Calls
        and then M.Collections = Before_Collections and then M.Opens = Before_Opens,
        "repeat start cannot replace immutable outcome or renew deadlines/replay AML");
      M.Check (M.Submitted_Reads = Before_Submitted and then M.History_Reads = Before_History,
        "repeat start does not refresh metadata before latch");
   end;
   Ada.Text_IO.Put_Line ("HID producer checks" & M.Checks'Image);
end Producer_Tests;

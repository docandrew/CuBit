pragma Ada_2022;
with Ada.Text_IO;
with GPIO_Pin_Leases;
with GPIO_Lease_Control;
with CuBit.Process_IDs;
with System.Storage_Elements;
with GPIO_Mapping_Policy;
with GPIO_Community_Types;
with GPIO_Community_Window;
with Intel_ADLN_GPIO;
with Intel_GPIO_IRQ;
with Intel_GPIO_Controller;
procedure Lease_Tests is
   package P renames GPIO_Mapping_Policy;
   package T renames GPIO_Community_Types;
   package G renames Intel_ADLN_GPIO;
   package I renames Intel_GPIO_IRQ;
   use type P.Address;
   use type G.Outcome;
   use type G.Plan_Result;
   use type T.Map_Outcome;
   use type T.Header_Outcome;
   use type T.Pin_Admission;
   use type I.Word;
   use type I.Access_Status;
   use type I.Ticket;
   use type I.Write_ID;
   type Words is array (Natural range 0 .. 2047) of I.Word;
   Memory : aliased Words := [others => 0] with Alignment => 4096;
   Physical : constant P.Address := 16#1234_0000#;
   Virtual : constant P.Address := P.Address (System.Storage_Elements.To_Integer (Memory'Address));
   Checks, Inspects, Maps, Clears : Natural := 0;
   Clock_Available : Boolean := True; Fault_Pause : Boolean := False;
   procedure Check (OK : Boolean; Why : String) is
   begin if not OK then raise Program_Error with Why; end if; Checks := Checks + 1; end Check;
   procedure Inspect (Authority : Natural; Result : out P.Description) is
   begin
      Check (Authority = 1, "same admitted authority"); Inspects := Inspects + 1;
      Result := (True, True, True, True, Physical, 8192);
   end Inspect;
   procedure Map (Authority : Natural; First, Destination, Pages : P.Address; Mapped : out Boolean) is
   begin
      Check (Authority = 1 and First = Physical and Destination = Virtual and Pages = 2,
        "exact admitted whole-page region"); Maps := Maps + 1; Mapped := True;
   end Map;
   package W is new GPIO_Community_Window (Natural, Inspect, Map);
   procedure Clock (Now : out I.Tick; Available : out Boolean) is
   begin Now := 1; Available := Clock_Available; end Clock;
   procedure Write
     (Plan : G.Plan_Result; ID : I.Write_ID; Value : I.Word; Status : out I.Access_Status) is
      Index : constant Natural := (if Plan.Status = G.Ready then Natural (Plan.Selected.Status_Register) / 4 else 0);
      Before : constant I.Word := Memory (Index);
   begin
      W.Write (Plan, ID, Value, Status);
      if Status = I.Access_OK and ID = I.Clear_Status then
         -- The adapter emits actual volatile stores; model ONLY chip W1C here.
         Clears := Clears + 1; Memory (Index) := Before and not Value;
      end if;
      if Status = I.Access_OK and then ID = I.Write_Enable and then Fault_Pause then Status := I.Unknown_Effect; end if;
   end Write;
   package C is new Intel_GPIO_Controller (2, Clock, W.Read, Write);
   use type C.Pin_ID;
   use type C.Admission;
   use type C.Engine.Phase;
   use type C.Engine.Admission;
   use type C.Engine.Failure;
   Plan_A, Plan_B, Candidate, Bad : G.Plan_Result;
   Bind_Report : T.Map_Report;
   Header : T.Header_Report;
   Admission : T.Pin_Admission;
   Registry_Admission : C.Admission;
   Owner_Admission : C.Engine.Admission;
   Pin_A, Pin_B, Refused : C.Pin_ID;
   Value : I.Word;
   Status : I.Access_Status;
   Snapshot : Words;
   function Index (Offset : G.Register_Offset) return Natural is (Natural (Offset) / 4);
   procedure Finish is
      Steps : Natural := 0;
      Worked : Boolean;
   begin
      while C.Observe.Active /= C.No_Pin loop
         Steps := Steps + 1; Check (Steps <= 32, "bounded integrated controller operation");
         C.Advance (Worked); Check (Worked, "active operation advances");
      end loop;
   end Finish;
   package PID renames CuBit.Process_IDs;
   package L renames GPIO_Lease_Control;
   Lease_Counter : L.Lease_ID := 16#1000_0000_0000_0000#;
   procedure Next_Lease (Lease : out L.Lease_ID; Available : out Boolean) is
      use type L.Lease_ID;
   begin Lease_Counter := Lease_Counter + 1; Lease := Lease_Counter; Available := True; end Next_Lease;
   package Leases is new GPIO_Pin_Leases (C, Clock, Next_Lease);
   use type Leases.Phase, L.Completion_Status, L.Lease_ID, Leases.Event;
   Lease_State, Second_State, Duplicate_State : Leases.State;
   Owner : constant PID.Process_ID := PID.From_Word (16#F123_4567_0000_1234#);
   Stranger : constant PID.Process_ID := PID.From_Word (16#F123_4568_0000_1234#);
   Accepted : Boolean; Reply : L.Reply; Lease_ID, Old_Lease : L.Lease_ID;
   Retained : Leases.Event;
   procedure Drive_Lease (S : in out Leases.State) is
      Worked : Boolean;
   begin
      for N in 1 .. 32 loop
         Leases.Step (S); C.Advance (Worked); Leases.Step (S);
         exit when Leases.Has_Reply (S) or else Leases.Has_Event (S) or else
            Leases.Observe (S).At_Phase = Leases.Armed;
      end loop;
   end Drive_Lease;
begin
   Memory (0) := 16#0094_0000#; Memory (3) := 16#1000#;
   W.Admit_Pin (160, Admission, Plan_A);
   Check (Admission = T.Header_Unavailable, "no plans before mapping/probe");
   W.Bind (1, 1, Physical, 8192, Virtual, 2, Bind_Report);
   Check (Bind_Report.Status = T.Bound and not W.Ready, "map alone enables no pin access");
   W.Admit_Pin (160, Admission, Plan_A);
   Check (Admission = T.Header_Unavailable, "mapped but unprobed cannot admit pin");
   W.Probe (G.Target_HID, Header);
   Check (Header.Status = T.Probed and W.Ready and Header.Revision = 16#0094_0000#,
      "same-resource fixed headers probed once");
   W.Admit_Pin (160, Admission, Plan_A);
   Check (Admission = T.Admitted and Plan_A.Status = G.Ready, "first canonical pin");
   W.Admit_Pin (161, Admission, Plan_B);
   Check (Admission = T.Admitted and Plan_B.Status = G.Ready, "second pin shares mapped community");
   W.Admit_Pin (160, Admission, Candidate);
   Check (Admission = T.Already_Admitted and Candidate = Plan_A and Inspects = 1 and Maps = 1,
      "idempotent internal geometry does not remap/reprobe");
   W.Admit_Pin (0, Admission, Candidate);
   Check (Admission = T.Wrong_Resource, "other community cannot alias resource");
   W.Admit_Pin (G.ACPI_Pin'Last, Admission, Candidate);
   Check (Admission = T.Unsupported_Pin, "no-pin/gap refuses");
   Candidate := G.Plan (G.Target_HID, 162, 16#0094_0000#, 16#1000#, 8192);
   W.Read (Candidate, I.Pad_Configuration, Value, Status);
   Check (Status = I.Denied and Value = 0, "valid geometry is insufficient without canonical admission");
   Snapshot := Memory;
   for Which in 1 .. 8 loop
      Bad := Plan_A;
      case Which is
         when 1 => Bad.PADCFG0 := 0;
         when 2 => Bad.Selected.Interrupt_Mask := Plan_B.Selected.Interrupt_Mask;
         when 3 => Bad.Selected.Resource := 0;
         when 4 => Bad.Selected.Pad := Plan_B.Selected.Pad;
         when 5 => Bad.Selected.Enable_Register := 0;
         when 6 => Bad.Selected.Owner_Register := 0;
         when 7 => Bad.Selected.Status_Register := 0;
         when others => Bad.End_Exclusive := 0;
      end case;
      W.Read (Bad, I.Pad_Configuration, Value, Status);
      Check (Status = I.Denied and Value = 0, "complete canonical plan equality before volatile read");
      W.Write (Bad, I.Write_Enable, 0, Status);
      Check (Status = I.Denied and Memory = Snapshot, "changed canonical field cannot redirect write");
   end loop;
   Memory (Index (Plan_A.Selected.Host_Register)) := 7;
   C.Register_Pin (Plan_A, I.Level_Low, 1000, 32, Pin_A, Registry_Admission, Owner_Admission);
   Check (Registry_Admission = C.Accepted, "actual controller accepts first backend plan");
   C.Register_Pin (Plan_B, I.Rising_Edge, 1000, 32, Refused, Registry_Admission, Owner_Admission);
   Check (Registry_Admission = C.Busy, "single lease before second pin setup"); Finish;
   C.Register_Pin (Plan_B, I.Rising_Edge, 1000, 32, Pin_B, Registry_Admission, Owner_Admission); Finish;
   Check (Memory (Index (Plan_A.Selected.Enable_Register)) = 3,
      "actual volatile backend preserves first enable during second setup");
   Memory (Index (Plan_A.Selected.Status_Register)) := 3;
   C.Poll (Pin_A, 1000, 32, Registry_Admission, Owner_Admission); Finish;
   Check (Memory (Index (Plan_A.Selected.Enable_Register)) = 2 and
      Memory (Index (Plan_A.Selected.Status_Register)) = 2 and
      C.Observe_Pin (Pin_A).Report.At_Phase = C.Engine.Event_Ready,
      "integrated mask/W1C model retains neighboring event");
   C.Poll (Pin_B, 1000, 32, Registry_Admission, Owner_Admission); Finish;
   Check (Memory (Index (Plan_A.Selected.Enable_Register)) = 0 and
      C.Observe_Pin (Pin_B).Report.Event_Pending, "two independently retained events");
   C.Rearm (Pin_A, C.Observe_Pin (Pin_A).Report.Event, 1000, 32, Registry_Admission, Owner_Admission); Finish;
   C.Rearm (Pin_B, C.Observe_Pin (Pin_B).Report.Event, 1000, 32, Registry_Admission, Owner_Admission); Finish;
   Check (Memory (Index (Plan_A.Selected.Enable_Register)) = 3, "both pins active before pause");
   Snapshot := Memory;
   C.Pause (Pin_A, 1, 32, Registry_Admission, Owner_Admission);
   Check (Registry_Admission = C.Owner_Refused and Owner_Admission = C.Engine.Expired and Memory = Snapshot,
      "expired pause has no register IO");
   C.Resume (Pin_A, 1000, 32, Registry_Admission, Owner_Admission);
   Check (Registry_Admission = C.Owner_Refused and Owner_Admission = C.Engine.Busy and Memory = Snapshot,
      "resume requires held mask");
   Clock_Available := False; C.Pause (Pin_A, 1000, 32, Registry_Admission, Owner_Admission);
   Check (Registry_Admission = C.Owner_Refused and Owner_Admission = C.Engine.Clock_Refused and Memory = Snapshot,
      "clock-refused pause does no IO"); Clock_Available := True;
   Memory (Index (Plan_A.Selected.Status_Register)) := 2;
   declare Previous_Clears : constant Natural := Clears; Previous_Event : constant I.Ticket := C.Observe_Pin (Pin_A).Report.Event; begin
      C.Pause (Pin_A, 1000, 32, Registry_Admission, Owner_Admission);
      Check (Registry_Admission = C.Accepted, "pause begins");
      C.Pause (Pin_B, 1000, 32, Registry_Admission, Owner_Admission);
      Check (Registry_Admission = C.Busy, "pause retains controller RMW lease"); Finish;
      Check (C.Observe_Pin (Pin_A).Report.At_Phase = C.Engine.Held_Masked and
         not C.Observe_Pin (Pin_A).Report.Pin_May_Be_Enabled, "pause only succeeds after masked readback");
      Check (Memory (Index (Plan_A.Selected.Enable_Register)) = 2 and
         Memory (Index (Plan_A.Selected.Status_Register)) = 2 and Clears = Previous_Clears,
         "pause preserves neighbor enable and all latched status");
      C.Poll (Pin_A, 1000, 32, Registry_Admission, Owner_Admission);
      Check (Registry_Admission = C.Owner_Refused and Owner_Admission = C.Engine.Busy, "held pin cannot poll");
      -- An actual modeled edge arrives after the mask readback, before resume.
      Memory (Index (Plan_A.Selected.Status_Register)) := Memory (Index (Plan_A.Selected.Status_Register)) or Plan_A.Selected.Interrupt_Mask;
      C.Resume (Pin_A, 1000, 32, Registry_Admission, Owner_Admission); Finish;
      Check (C.Observe_Pin (Pin_A).Report.At_Phase = C.Engine.Listening and
         Memory (Index (Plan_A.Selected.Enable_Register)) = 3 and
         Memory (Index (Plan_A.Selected.Status_Register)) = 3 and Clears = Previous_Clears,
         "resume preserves edges latched during mask");
      C.Poll (Pin_A, 1000, 32, Registry_Admission, Owner_Admission); Finish;
      Check (C.Observe_Pin (Pin_A).Report.Event = Previous_Event + 1 and
         Memory (Index (Plan_A.Selected.Status_Register)) = 2, "retained edge becomes next exact-ticket event");
   end;
   -- A further edge arrives while the previous event remains masked/retained.
   Memory (Index (Plan_A.Selected.Status_Register)) := Memory (Index (Plan_A.Selected.Status_Register)) or Plan_A.Selected.Interrupt_Mask;
   Snapshot := Memory;
   C.Retire_Event (Pin_A, I.No_Ticket, Registry_Admission, Owner_Admission);
   Check (Registry_Admission = C.Owner_Refused and Owner_Admission = C.Engine.Wrong_Ticket and Memory = Snapshot,
      "zero ticket cannot retire a retained event");
   C.Retire_Event (Pin_A, C.Observe_Pin (Pin_A).Report.Event - 1, Registry_Admission, Owner_Admission);
   Check (Registry_Admission = C.Owner_Refused and Owner_Admission = C.Engine.Wrong_Ticket and Memory = Snapshot,
      "stale ticket cannot retire current event");
   Clock_Available := False;
   C.Retire_Event (Pin_A, C.Observe_Pin (Pin_A).Report.Event, Registry_Admission, Owner_Admission);
   Check (Registry_Admission = C.Accepted and C.Observe_Pin (Pin_A).Report.At_Phase = C.Engine.Held_Masked and
      not C.Observe_Pin (Pin_A).Report.Event_Pending and Memory = Snapshot,
      "exact-ticket retirement is local masked cleanup, even without clock"); Clock_Available := True;
   Memory (0) := I.Word'Last; Memory (3) := I.Word'Last;
   W.Admit_Pin (162, Admission, Candidate);
   Check (Admission = T.Admitted and Candidate.Status = G.Ready,
      "header cache is immutable after probe, no new header IO");
   W.Probe (G.Target_HID, Header); W.Bind (1, 1, Physical, 8192, Virtual, 2, Bind_Report);
   Check (Header.Status = T.Already_Probed and Bind_Report.Status = T.Already_Attempted
      and Inspects = 1 and Maps = 1, "mapping/header attempts never repeat");
   C.Resume (Pin_A, 1000, 32, Registry_Admission, Owner_Admission); Finish;
   C.Rearm (Pin_B, C.Observe_Pin (Pin_B).Report.Event, 1000, 32, Registry_Admission, Owner_Admission); Finish;
   Leases.Bind (Lease_State, Pin_A, Owner, 1000, 32, Accepted);
   Check (Accepted, "lease binds admitted pin and opaque owner");
   Snapshot := Memory; Leases.Bind (Duplicate_State, Pin_A, Stranger, 1000, 32, Accepted);
   Check (not Accepted and Memory = Snapshot, "pending bind reserves pin against another state/owner");
   Leases.Step (Lease_State);
   Leases.Bind (Second_State, Pin_B, Stranger, 1, 32, Accepted); Check (Accepted, "expired bind is admitted for bounded evaluation");
   Snapshot := Memory; Leases.Step (Second_State);
   Check (Leases.Has_Reply (Second_State) and Memory = Snapshot, "expired queued bind performs no MMIO");
   Leases.Take_Reply (Second_State, Reply); Check (Reply.Status = L.Refused, "definite pre-start expiry returns refusal");
   Leases.Bind (Second_State, Pin_B, Stranger, 1000, 32, Accepted); Check (Accepted, "second pin bind queued");
   Snapshot := Memory; Leases.Step (Second_State);
   Check (C.Observe.Active = Pin_A and Leases.Observe (Second_State).At_Phase = Leases.Binding and Memory = Snapshot,
      "queued second pin does not interleave first RMW"); Drive_Lease (Lease_State);
   Drive_Lease (Second_State); Leases.Take_Reply (Second_State, Reply);
   Check (Reply.Status = L.Completed and Leases.Quiescent (Second_State), "second queued pin parks after first operation");
   Check (Leases.Has_Reply (Lease_State), "bind completion retained"); Leases.Take_Reply (Lease_State, Reply);
   Check (Reply.Status = L.Completed and Leases.Quiescent (Lease_State), "bind parks before lease grants");
   Leases.Arm (Lease_State, Stranger, 1000, 32, Accepted); Check (not Accepted, "different process lifetime cannot arm");
   Leases.Arm (Lease_State, Owner, 1000, 32, Accepted); Check (Accepted, "arm queued"); Drive_Lease (Lease_State);
   Check (Leases.Has_Reply (Lease_State), "arm response available only after controller resume");
   Leases.Unbind (Lease_State, Accepted); Check (not Accepted, "owned reply/active arm prevents unbind");
   Leases.Take_Reply (Lease_State, Reply); Lease_ID := Reply.Lease;
   Check (Reply.Status = L.Completed and Lease_ID > 2 ** 32 and Leases.Observe (Lease_State).At_Phase = Leases.Armed,
      "full-width lease issued after confirmed listening");
   Snapshot := Memory; Leases.Bind (Duplicate_State, Pin_A, Stranger, 1000, 32, Accepted);
   Check (not Accepted and Memory = Snapshot, "active owner cannot be replaced through duplicate binding");
   Memory (Index (Plan_A.Selected.Status_Register)) := Memory (Index (Plan_A.Selected.Status_Register)) or Plan_A.Selected.Interrupt_Mask;
   Leases.Poll (Lease_State, 1000, 32, Accepted); Check (Accepted, "owned pin poll queued"); Drive_Lease (Lease_State);
   Check (Leases.Has_Event (Lease_State), "masked event retained"); Retained := Leases.Head (Lease_State);
   Leases.Step (Lease_State); Check (Leases.Head (Lease_State) = Retained, "backpressure preserves exact event");
   Check (Retained.Lease = Lease_ID and Retained.Pin = Pin_A and Retained.Ticket /= I.No_Ticket, "event bound to lease/pin/ticket");
   Leases.Acknowledge_Event (Lease_State, Owner, Lease_ID, Retained.Ticket, 1000, 32, Accepted);
   Check (not Accepted, "undelivered event cannot be acknowledged"); Leases.Delivered (Lease_State);
   Leases.Acknowledge_Event (Lease_State, Stranger, Lease_ID, Retained.Ticket, 1000, 32, Accepted);
   Check (not Accepted, "foreign owner cannot acknowledge");
   Leases.Acknowledge_Event (Lease_State, Owner, Lease_ID, Retained.Ticket - 1, 1000, 32, Accepted);
   Check (not Accepted, "stale event ticket cannot rearm");
   Memory (Index (Plan_A.Selected.Status_Register)) := Memory (Index (Plan_A.Selected.Status_Register)) or Plan_A.Selected.Interrupt_Mask;
   Leases.Acknowledge_Event (Lease_State, Owner, Lease_ID, Retained.Ticket, 1000, 32, Accepted); Check (Accepted, "exact event ACK queued"); Drive_Lease (Lease_State);
   Leases.Take_Reply (Lease_State, Reply); Check (Reply.Status = L.Completed and Reply.Lease = Lease_ID, "ACK retains lease");
   Leases.Poll (Lease_State, 1000, 32, Accepted); Drive_Lease (Lease_State);
   Check (Leases.Has_Event (Lease_State) and Leases.Head (Lease_State).Ticket = Retained.Ticket + 1,
      "masked edge survives rearm into next event");
   Clock_Available := False; Leases.Release (Lease_State, Owner, Lease_ID, 0, 32, Accepted);
   Check (Accepted, "release held event accepted"); Drive_Lease (Lease_State); Leases.Take_Reply (Lease_State, Reply);
   Check (Reply.Status = L.Completed and Reply.Lease = Lease_ID and Leases.Quiescent (Lease_State),
      "release returns exact lease after local masked retirement without clock"); Clock_Available := True;
   Old_Lease := Lease_ID;
   declare Previous_Counter : constant L.Lease_ID := Lease_Counter; begin
      Lease_Counter := 0; Leases.Arm (Lease_State, Owner, 1000, 32, Accepted); Drive_Lease (Lease_State); Leases.Take_Reply (Lease_State, Reply);
      Check (Reply.Status = L.Refused and Leases.Quiescent (Lease_State), "reused/lower allocator epoch refuses before unmask");
      Lease_Counter := Previous_Counter;
   end;
   Leases.Arm (Lease_State, Owner, 1000, 32, Accepted); Drive_Lease (Lease_State); Leases.Take_Reply (Lease_State, Reply); Lease_ID := Reply.Lease;
   Check (Lease_ID > Old_Lease, "new lease never reuses old epoch");
   Leases.Release (Lease_State, Owner, Old_Lease, 1000, 32, Accepted); Check (not Accepted, "old lease cannot release new owner epoch");
   Leases.Release (Lease_State, Owner, Lease_ID, 1000, 32, Accepted); Drive_Lease (Lease_State); Leases.Take_Reply (Lease_State, Reply);
   Check (Reply.Status = L.Completed and Reply.Lease = Lease_ID and Leases.Quiescent (Lease_State), "idle lease release parks hardware");
   declare Previous_Lease : constant L.Lease_ID := Lease_ID;
      Before : constant Words := Memory;
   begin
      Leases.Unbind (Lease_State, Accepted);
      Check (Accepted and then Leases.Observe (Lease_State).At_Phase = Leases.Unbound and then Memory = Before,
         "only confirmed parked binding can release reservation without IO");
      Leases.Bind (Lease_State, Pin_A, Stranger, 0, 32, Accepted);
      Check (Accepted and then Leases.Has_Reply (Lease_State) and then Memory = Before,
         "already-masked rebind is local despite zero deadline");
      Leases.Unbind (Lease_State, Accepted); Check (not Accepted, "binding completion must be consumed before unbind");
      Leases.Take_Reply (Lease_State, Reply);
      Check (Reply.Status = L.Completed and then Reply.Lease = 0 and then Leases.Quiescent (Lease_State), "rebound channel starts parked with no lease");
      Leases.Bind (Duplicate_State, Pin_A, Owner, 1000, 32, Accepted);
      Check (not Accepted, "replacement reserves pin exclusively");
      Leases.Arm (Lease_State, Owner, 1000, 32, Accepted); Check (not Accepted, "old process lifetime cannot arm replacement");
      Leases.Arm (Lease_State, Stranger, 1000, 32, Accepted); Check (Accepted, "replacement arm accepted"); Drive_Lease (Lease_State);
      Leases.Take_Reply (Lease_State, Reply); Lease_ID := Reply.Lease;
      Check (Reply.Status = L.Completed and then Lease_ID > Previous_Lease, "unbind never resets global lease epoch");
      Leases.Unbind (Lease_State, Accepted); Check (not Accepted, "armed replacement cannot unbind");
      Leases.Release (Lease_State, Stranger, Previous_Lease, 1000, 32, Accepted); Check (not Accepted, "old lease cannot release replacement");
      Leases.Release (Lease_State, Owner, Lease_ID, 1000, 32, Accepted); Check (not Accepted, "old owner cannot release new lease");
      Leases.Release (Lease_State, Stranger, Lease_ID, 1000, 32, Accepted); Check (Accepted, "exact replacement lease release accepted"); Drive_Lease (Lease_State);
      Leases.Take_Reply (Lease_State, Reply); Check (Reply.Status = L.Completed and then Leases.Quiescent (Lease_State), "replacement returned to confirmed mask");
   end;
   C.Resume (Pin_A, 1000, 32, Registry_Admission, Owner_Admission); Finish;
   Fault_Pause := True; C.Pause (Pin_A, 1000, 32, Registry_Admission, Owner_Admission); Finish;
   Check (C.Observe.Faulted and C.Observe.Last_Fault = Pin_A and
      C.Observe_Pin (Pin_A).Report.Reason = C.Engine.Register_Indeterminate,
      "uncertain mask write freezes controller");
   Check (C.Observe_Pin (Pin_A).Report.Pin_May_Be_Enabled,
      "write effect alone cannot establish confirmed masking");
   Leases.Unbind (Second_State, Accepted); Check (not Accepted, "controller-wide uncertainty prevents freeing even parked neighbor");
   Ada.Text_IO.Put_Line ("GPIO lease/controller checks" & Checks'Image);
end Lease_Tests;

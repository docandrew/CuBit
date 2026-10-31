pragma Ada_2022;
with Ada.Text_IO;
with GPIO_Pin_Leases;
with GPIO_Channel_Owner;
with GPIO_Request_Dispatch;
with GPIO_Control_Wire;
with Ada.Command_Line;
with GPIO_Lease_Control;
with CuBit.Process_IDs;
with System.Storage_Elements;
with GPIO_Mapping_Policy;
with GPIO_Community_Types;
with GPIO_Community_Window;
with Intel_ADLN_GPIO;
with Intel_GPIO_IRQ;
with Intel_GPIO_Controller;
procedure Dispatcher_Channel_Tests is
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
   use type I.Write_ID;
   type Words is array (Natural range 0 .. 2047) of I.Word;
   Memory : aliased Words := [others => 0] with Alignment => 4096;
   Physical : constant P.Address := 16#1234_0000#;
   Virtual : constant P.Address := P.Address (System.Storage_Elements.To_Integer (Memory'Address));
   Checks, Inspects, Maps, Clears : Natural := 0;
   Clock_Available : constant Boolean := True; Fault_Pause : Boolean := False;
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
   use type C.Engine.Admission;
   Plan_A, Plan_B : G.Plan_Result;
   Bind_Report : T.Map_Report;
   Header : T.Header_Report;
   Admission : T.Pin_Admission;
   Registry_Admission : C.Admission;
   Owner_Admission : C.Engine.Admission;
   Pin_A, Pin_B : C.Pin_ID;
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
   use type Leases.Phase;
   package Wire renames GPIO_Control_Wire;
   use type Wire.Completion_Status, Wire.Word, Wire.Control_Kind;
   Reply_Count, Event_Attempts, Events_Accepted : Natural := 0;
   Allow_Answer, Allow_Event : Boolean := True;
   Last_Reply : Wire.Completion := (Wire.Arm, Wire.Refused, 0, 0);
   Last_Event : Wire.Event := (1, 1);
   Last_Authority : Natural := 0;
   procedure Answer (Authority : Natural; Value : Wire.Completion; Delivered : out Boolean) is
   begin
      Check (Wire.Valid (Value), "valid completion before send");
      Last_Authority := Authority; Last_Reply := Value; Reply_Count := Reply_Count + 1; Delivered := Allow_Answer;
   end Answer;
   procedure Try_Event (Sink : Natural; Value : Wire.Event; Accepted : out Boolean) is
   begin
      Check (Sink = 17, "only launcher-installed sink");
      Event_Attempts := Event_Attempts + 1; Last_Event := Value; Accepted := Allow_Event;
      if Accepted then Events_Accepted := Events_Accepted + 1; end if;
   end Try_Event;
   package Channels is new GPIO_Channel_Owner (Leases, 32, Natural, Natural, Answer, Try_Event);
   use type Channels.Admission, Channels.Binding_Phase, Channels.Lifecycle;
   S, Duplicate : Channels.State;
   Owner : constant PID.Process_ID := PID.From_Word (16#F123_4567_0000_1234#);
   Stranger : constant PID.Process_ID := PID.From_Word (16#F123_4568_0000_1234#);
   OK : Boolean;
   Result : Channels.Admission;
   Case_Name : constant String := Ada.Command_Line.Argument (1);
   Lease : Wire.Word := 0;
   Bound_Badge : constant Wire.Word := 16#4750_1000_0000_00A0#;
   Pending_Key : Natural := 0;
   function Authorized (Caller : PID.Process_ID; Badge : Wire.Word) return Boolean is
      use type PID.Process_ID;
   begin return Caller = Owner and then Badge = Bound_Badge; end Authorized;
   procedure Save_Reply (Reply : out Natural; Saved : out Boolean) is
   begin Check (Pending_Key /= 0, "model saved authority exists"); Reply := Pending_Key; Saved := True; end Save_Reply;
   procedure Answer_Saved (Reply : Natural; Label : Wire.Operation; Value : Wire.Completion; Delivered : out Boolean) is
   begin Check (Label = Wire.Label_Of (Value.Kind), "completion echoes admitted control label"); Answer (Reply, Value, Delivered); end Answer_Saved;
   procedure Answer_Current (Caller : PID.Process_ID; Label : Wire.Operation; Value : Wire.Completion; Delivered : out Boolean) is
      pragma Unreferenced (Caller, Label, Value);
   begin Check (False, "unexpected current reply"); Delivered := False; end Answer_Current;
   procedure Gate_Offer (Caller : PID.Process_ID; Badge : Wire.Word; Item : Wire.Request; Reply : Natural; Taken : out Boolean) is
      Admission : Channels.Admission;
   begin
      Check (Authorized (Caller, Badge), "kernel metadata matched existing bound channel");
      Channels.Offer (S, Caller, Item, Reply, Admission); Taken := Admission = Channels.Accepted;
   end Gate_Offer;
   package Dispatch is new GPIO_Request_Dispatch (Natural, Authorized, Save_Reply, Answer_Saved, Answer_Current, Gate_Offer);
   use type Dispatch.Outcome;
   procedure Drive is
      Worked : Boolean;
   begin
      for N in 1 .. 40 loop
         C.Advance (Worked); Channels.Step (S);
      end loop;
   end Drive;
   procedure Offer (Item : Wire.Request; Key : Natural) is
      Report : Dispatch.Report;
   begin
      Pending_Key := Key;
      Dispatch.Handle (Owner, Bound_Badge, Wire.Label_Of (Item.Kind), Wire.Protocol_Version,
         Wire.Encode (Item), True, Report);
      Check (Report.Result = Dispatch.Control_Retained, "dispatcher transferred saved reply to actual channel");
   end Offer;
begin
   Check (Case_Name in "happy" | "answer-lost" | "uncertain" | "expired" |
      "stop-pending" | "stop-expired" | "stop-event", "known test case");
   Memory (0) := 16#0094_0000#; Memory (3) := 16#1000#;
   W.Bind (1, 1, Physical, 8192, Virtual, 2, Bind_Report);
   Check (Bind_Report.Status = T.Bound, "bounded admitted mapping"); W.Probe (G.Target_HID, Header);
   Check (Header.Status = T.Probed, "header probe");
   W.Admit_Pin (160, Admission, Plan_A); Check (Admission = T.Admitted, "admitted pin A");
   W.Admit_Pin (161, Admission, Plan_B); Check (Admission = T.Admitted, "admitted pin B");
   Memory (Index (Plan_A.Selected.Host_Register)) := 7;
   C.Register_Pin (Plan_A, I.Level_Low, 1000, 32, Pin_A, Registry_Admission, Owner_Admission); Finish;
   Check (Registry_Admission = C.Accepted, "pin A configured");
   C.Register_Pin (Plan_B, I.Rising_Edge, 1000, 32, Pin_B, Registry_Admission, Owner_Admission); Finish;
   Check (Registry_Admission = C.Accepted, "neighbor pin configured");
   Channels.Configure (S, Pin_A, Owner, 17, 1000, OK); Check (OK, "trusted channel configure");
   Channels.Configure (Duplicate, Pin_A, Stranger, 99, 1000, OK); Check (not OK, "duplicate physical pin refuses");
   Channels.Offer (S, Owner, (Wire.Arm, (Wire.Within, 1000)), 90, Result);
   Check (Result = Channels.Not_Ready, "cannot request before parked binding");
   Drive;
   Check (Channels.Observe (S).Binding = Channels.Ready and then Channels.Quiescent (S), "bound channel parks before granting lease");
   Check (Reply_Count = 0, "internal binding completion has no requester");
   Check (Memory (Index (Plan_A.Selected.Enable_Register)) = 2, "binding preserves neighbor enable");
   Channels.Offer (S, Stranger, (Wire.Arm, (Wire.Within, 1000)), 91, Result);
   Check (Result = Channels.Unauthorized and then Reply_Count = 0, "full opaque process lifetime equality");
   if Case_Name = "answer-lost" then Allow_Answer := False;
   elsif Case_Name = "uncertain" then Fault_Pause := True; end if;
   Offer ((Wire.Arm, (Wire.Within, (if Case_Name = "expired" then 1 else 1000))), 100);
   Channels.Offer (S, Owner, (Wire.Arm, (Wire.Within, 1000)), 101, Result);
   Check (Result = Channels.Busy, "busy request cannot replace saved authority");
   if Case_Name in "stop-pending" | "stop-expired" then
      Channels.Request_Stop (S, (if Case_Name = "stop-expired" then 1 else 1000));
      Channels.Request_Stop (S, 2000);
      Channels.Offer (S, Owner, (Wire.Arm, (Wire.Within, 1000)), 999, Result);
      Check (Result = Channels.Not_Ready, "stop prevents new control authority transfer");
   end if;
   Drive; Check (Reply_Count = 1 and then Last_Authority = 100, "one exact saved reply used");
   if Case_Name in "stop-pending" | "stop-expired" then
      Check (Last_Reply.Status = Wire.Completed, "pending arm reply completed once before internal cleanup");
      Check (Reply_Count = 1 and then Event_Attempts = 0, "cleanup never replies or sends event");
      if Case_Name = "stop-expired" then
         Check (Channels.Observe (S).Life = Channels.Unresolved and then not Channels.Quiescent (S), "expired cleanup cannot claim parked hardware");
         Check (Channels.Observe (S).Pin.At_Phase = Leases.Armed, "definite refused cleanup preserves known armed lease");
         Channels.Request_Stop (S, 3000); Drive;
         Check (Channels.Observe (S).Life = Channels.Unresolved and then Memory (Index (Plan_A.Selected.Enable_Register)) = 3, "repeated stop cannot extend deadline or retry");
      else
         Check (Channels.Observe (S).Life = Channels.Stopped and then Channels.Quiescent (S), "pending arm drains to confirmed masked stop");
         Check (Memory (Index (Plan_A.Selected.Enable_Register)) = 2, "drain preserves neighboring pin");
      end if;
   elsif Case_Name = "expired" then
      Check (Last_Reply.Status = Wire.Refused and then Last_Reply.Lease = 0, "expiry before effects is definite refusal");
      Check (Channels.Quiescent (S) and then Memory (Index (Plan_A.Selected.Enable_Register)) = 2, "expired arm remains parked");
      Channels.Request_Stop (S, 1000); Drive;
      Check (Channels.Observe (S).Life = Channels.Stopped and then Channels.Quiescent (S), "parked refused arm stops without cleanup writes");
   elsif Case_Name = "uncertain" then
      Check (Last_Reply.Status = Wire.Uncertain and then Last_Reply.Lease > 2 ** 32, "possible write effects are uncertain with retained lease");
      Check (not Channels.Quiescent (S) and then Channels.Observe (S).Pin.At_Phase = Leases.Faulted, "uncertain controller never clean stop");
      Channels.Request_Stop (S, 1000); Drive;
      Check (Channels.Observe (S).Life = Channels.Unresolved and then not Channels.Quiescent (S), "uncertain controller cleanup remains unresolved");
   elsif Case_Name = "answer-lost" then
      Check (Last_Reply.Status = Wire.Completed and then Last_Reply.Lease > 2 ** 32, "lost answer still owns armed lease");
      Check (Channels.Observe (S).Answer_Lost and then not Channels.Quiescent (S), "lost reply retains supervisor obligation");
      Channels.Offer (S, Owner, (Wire.Release_Pin, (Wire.Within, 1000), Last_Reply.Lease), 102, Result);
      Check (Result = Channels.Reply_Lost, "cannot reuse channel after consumed failed reply");
      Drive; Check (Reply_Count = 1 and then Event_Attempts = 0, "consumed authority is never retried");
      Channels.Request_Stop (S, 1000); Drive;
      Check (Channels.Observe (S).Life = Channels.Stopped and then Channels.Quiescent (S), "supervisor cleans known lease after lost answer");
      Check (Channels.Observe (S).Answer_Lost and then Reply_Count = 1, "historical reply loss preserved after hardware cleanup");
      Check (Memory (Index (Plan_A.Selected.Enable_Register)) = 2, "orphan lease release confirms mask and preserves neighbor");
   else
      Check (Last_Reply.Status = Wire.Completed and then Last_Reply.Lease > 2 ** 32, "arm confirmed with full-width lease"); Lease := Last_Reply.Lease;
      Allow_Event := False; Memory (Index (Plan_A.Selected.Status_Register)) := 3;
      Channels.Poll (S, 1000, OK); Check (OK, "poll current armed channel"); Drive;
      Check (Channels.Observe (S).Pin.At_Phase = Leases.Event_Retained and then Events_Accepted = 0 and then Event_Attempts > 0, "masked event retained during backpressure");
      Check (Last_Event.Lease = Lease, "event retains full-width lease and nonzero typed generation");
      Check (Memory (Index (Plan_A.Selected.Enable_Register)) = 2 and then Memory (Index (Plan_A.Selected.Status_Register)) = 2, "mask and W1C preserve neighbor");
      if Case_Name = "stop-event" then
         declare Previous_Attempts : constant Natural := Event_Attempts;
            Previous_Clears : constant Natural := Clears;
         begin
            Channels.Request_Stop (S, 0); Drive;
            Check (Channels.Observe (S).Life = Channels.Stopped and then Channels.Quiescent (S), "retained masked event retires locally despite expired cleanup deadline");
            Check (Event_Attempts = Previous_Attempts and then Events_Accepted = 0, "stop suppresses undelivered event");
            Check (Clears = Previous_Clears and then Memory (Index (Plan_A.Selected.Status_Register)) = 2, "local retirement does not clear neighboring or later status");
            Check (Reply_Count = 1, "local event cleanup has no peer reply");
         end;
         Ada.Text_IO.Put_Line ("GPIO dispatcher composition checks" & Checks'Image); return;
      end if;
      Offer ((Wire.Acknowledge, (Wire.Within, 1000), Lease, Last_Event.Generation), 103); Drive;
      Check (Last_Reply.Status = Wire.Refused and then Events_Accepted = 0, "undelivered event cannot be acknowledged");
      Allow_Event := True; Drive; Check (Events_Accepted = 1 and then Channels.Observe (S).Pin.At_Phase = Leases.Event_Delivered, "accepted event delivered once");
      declare Previous : constant Natural := Event_Attempts; begin Drive; Check (Event_Attempts = Previous, "no duplicate accepted event"); end;
      Offer ((Wire.Acknowledge, (Wire.Within, 1000), Lease, Last_Event.Generation + 1), 104); Drive;
      Check (Last_Reply.Status = Wire.Refused and then Channels.Observe (S).Pin.At_Phase = Leases.Event_Delivered, "wrong generation does not rearm");
      Offer ((Wire.Acknowledge, (Wire.Within, 1000), Lease, Last_Event.Generation), 105); Drive;
      Check (Last_Reply.Status = Wire.Completed and then Last_Reply.Lease = Lease and then Last_Reply.Generation = Last_Event.Generation, "exact ACK confirms rearm");
      Check (Channels.Observe (S).Pin.At_Phase = Leases.Armed and then Memory (Index (Plan_A.Selected.Enable_Register)) = 3, "rearm preserves neighbor");
      Offer ((Wire.Release_Pin, (Wire.Within, 1000), Lease), 106); Drive;
      Check (Last_Reply.Status = Wire.Completed and then Last_Reply.Lease = Lease and then Last_Reply.Generation = 0, "release echoes retired lease");
      Check (Channels.Quiescent (S) and then Memory (Index (Plan_A.Selected.Enable_Register)) = 2, "release confirms parked and preserves neighbor");
      Channels.Request_Stop (S, 1000); Drive;
      Check (Channels.Observe (S).Life = Channels.Stopped and then Channels.Quiescent (S), "released channel stops");
   end if;
   Ada.Text_IO.Put_Line ("GPIO dispatcher composition checks" & Checks'Image);
end Dispatcher_Channel_Tests;

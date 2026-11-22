pragma Ada_2022;
with Ada.Text_IO;
with Interfaces; use Interfaces;
with ACPI_Field_PCI_Provider;
with ACPI_PCI_Protocol;
with AML_Field_Access;
with AML_Field_Layout;
with ACPI_Capacity_Diagnostics;
with ACPI_Target_Heap_Test_Control;
procedure Frontend_Buses_Tests is
   package Q renames ACPI_PCI_Protocol;
   package P renames Q.P;
   package A renames AML_Field_Access;
   package L renames AML_Field_Layout;
   package C renames ACPI_Capacity_Diagnostics;
   package Heap renames ACPI_Target_Heap_Test_Control;
   use type Q.Wire.Time_Instant;
   use type P.Bus_Number, A.Completion, A.Target, C.Cache_Kind, C.Failure_Kind, C.Entry_Count;
   Checks, Exchanges, Reads, Reports : Natural := 0;
   Epoch : constant Unsigned_64 := 16#F000_0000_0000_0001#;
   Record_Base : constant Unsigned_64 := 16#8000_0000_0000_1000#;
   Physical : constant Unsigned_64 := 16#C000_0000#;
   Last_Key : P.Bus_Key;
   Last_Read_ID : Unsigned_64 := 0;
   Duplicate_ID, Overlap, Not_Sent, Unknown, Reenter, No_Bus_Zero : Boolean := False;
   type Channel is null record;
   procedure Check (Good : Boolean; Name : String) is
   begin Checks := Checks + 1; if not Good then raise Program_Error with Name; end if; end Check;
   procedure Clock (Now : out Q.Wire.Time_Instant; Available : out Boolean) is
   begin Now := 1; Available := True; end Clock;
   procedure Microseconds (Now : out Q.Microsecond_Deadline; Available : out Boolean) is
   begin Now := 1_000; Available := True; end Microseconds;
   procedure Deadline (IPC : out Q.Wire.Time_Instant; Data : out Q.Microsecond_Deadline) is
   begin IPC := 10; Data := 9_000; end Deadline;
   procedure Capacity (Item : C.Report) is
   begin
      Reports := Reports + 1;
      Check (Item.Cache = C.PCI_Targets and then Item.Failure = C.Process_Heap
        and then Item.Demand > Item.Capacity, "typed bus capacity refusal");
   end Capacity;
   procedure Exchange
     (State : in out Channel; Destination : Natural; Due : Q.Wire.Time_Instant;
      Item : Q.Packet; Reply : out Q.Packet; Delivered : out Q.Wire.Delivery);
   package F is new ACPI_Field_PCI_Provider
     (Channel, Natural, 0, Clock, Microseconds, Exchange, Deadline, 2, Capacity);
   use type F.Local_Failure, F.Phase;
   IO : F.Context;
   procedure Exchange
     (State : in out Channel; Destination : Natural; Due : Q.Wire.Time_Instant;
      Item : Q.Packet; Reply : out Q.Packet; Delivered : out Q.Wire.Delivery)
   is
      pragma Unreferenced (State);
      Request : constant Q.Request := Q.Decode_Request (Item);
      Inner : Boolean;
      Before : constant Natural := Exchanges;
   begin
      Exchanges := Exchanges + 1;
      Check (Destination = 42 and Due = 10 and Request.Valid, "peer/deadline/framing");
      Delivered := Q.Wire.Reply_Received;
      case Request.Kind is
         when Q.Header =>
            Check (Request.Setup_Deadline = Due, "setup deadline propagated");
            Last_Key := Request.Key;
            if No_Bus_Zero and then Last_Key.Bus = 0 then
               Reply := Q.Error_Reply (Q.Header, Q.Not_Ready); return;
            end if;
            if Reenter then
               F.Prepare_Bus (IO, (7, 55), 10, Inner);
               Check (not Inner and Exchanges = Before + 1, "preparation reentry refused");
            end if;
            Reply := Q.Header_Result (Epoch, Record_Base +
              (if Duplicate_ID then 0 else Unsigned_64 (Last_Key.Bus)));
            if Not_Sent then Delivered := Q.Wire.Not_Dispatched;
            elsif Unknown then Delivered := Q.Wire.Completion_Unknown; end if;
         when Q.Describe =>
            Check (Request.Epoch = Epoch, "description lifetime");
            Reply := Q.Describe_Result
              (Physical + (if Overlap then 0 else Unsigned_64 (Last_Key.Bus) * P.Bus_Bytes), P.Bus_Bytes);
         when Q.Read =>
            Reads := Reads + 1; Last_Read_ID := Request.ID;
            Check (Request.Epoch = Epoch, "read lifetime");
            Reply := Q.Read_Result (165);
      end case;
   end Exchange;
   function Geometry (Bus : P.Bus_Number) return L.Register_Geometry is
     ((Region => (Space => L.PCI_Config_Space, Base => 0, Length => 256,
        PCI => (Known => True, Device => ((if Bus = 0 then 0 else 7), Bus, 2, 3))),
       Byte_Offset => 16#60#, Width => P.T.Dword_Access));
   Accepted : Boolean;
   Old_Target, New_Target, Alias_Target : A.Target;
   Response : A.Response;
   Before : Natural;
begin
   F.Initialize (IO, 42, 17, 10, Accepted);
   Check (Accepted and F.Status (IO) = F.Ready and Exchanges = 2, "initial binding");
   F.Install (IO, Geometry (0), Old_Target, Accepted); Check (Accepted, "old target");
   Before := Exchanges;
   F.Install (IO, Geometry (1), New_Target, Accepted);
   Check (not Accepted and Exchanges = Before, "unprepared install has no IPC");
   Reenter := True; F.Prepare_Bus (IO, (7, 1), 10, Accepted); Reenter := False;
   Check (Accepted and Exchanges = Before + 2 and Reads = 0, "explicit preparation only");
   Before := Exchanges;
   F.Prepare_Bus (IO, (7, 1), 10, Accepted);
   Check (Accepted and Exchanges = Before, "prepared binding reused");
   F.Prepare_Bus (IO, (7, 1), Q.Wire.No_Finite_Deadline, Accepted);
   Check (not Accepted and Exchanges = Before, "cached preparation still checks deadline");
   F.Install (IO, Geometry (1), New_Target, Accepted);
   Check (Accepted and Exchanges = Before, "new target install remains metadata only");
   declare Alias_View : L.Register_Geometry := Geometry (1); begin
      Alias_View.Region.Space := L.Memory_Space; Alias_View.Region.PCI := (Known => False);
      Alias_View.Region.Base := Physical + P.Bus_Bytes; Alias_View.Region.Length := 256;
      F.Install (IO, Alias_View, Alias_Target, Accepted);
      Check (Accepted and Exchanges = Before, "prepared nonzero bus alias metadata");
   end;
   F.Dispatch (IO, (New_Target, False, 0), Response);
   Check (Response.Status = A.Completed and Last_Read_ID = Record_Base + 1, "new target uses new record");
   F.Dispatch (IO, (Old_Target, False, 0), Response);
   Check (Response.Status = A.Completed and Last_Read_ID = Record_Base, "old target keeps original record");
   F.Dispatch (IO, (Alias_Target, False, 0), Response);
   Check (Response.Status = A.Completed and Last_Read_ID = Record_Base + 1, "alias uses correct record");
   Duplicate_ID := True; F.Prepare_Bus (IO, (7, 2), 10, Accepted); Duplicate_ID := False;
   Check (not Accepted and F.Problem (IO) = F.Bad_Reply and F.Status (IO) = F.Ready, "duplicate record refused without losing old bindings");
   Overlap := True; F.Prepare_Bus (IO, (7, 3), 10, Accepted); Overlap := False;
   Check (not Accepted and F.Problem (IO) = F.Bad_Reply, "overlapping alias windows refused");
   Not_Sent := True; Before := Exchanges;
   F.Prepare_Bus (IO, (7, 4), 10, Accepted); Not_Sent := False;
   Check (not Accepted and F.Problem (IO) = F.Not_Sent and Exchanges = Before + 1, "no implicit preparation retry");
   Unknown := True; F.Prepare_Bus (IO, (7, 5), 10, Accepted); Unknown := False;
   Check (not Accepted and F.Problem (IO) = F.Completion_Unknown and not F.Pending (IO), "metadata delivery uncertainty is not a PCI read");
   F.Prepare_Bus (IO, (7, 2), 10, Accepted); Check (Accepted, "third binding");
   F.Prepare_Bus (IO, (7, 3), 10, Accepted); Check (Accepted, "fourth binding");
   Heap.Successful_Allocation_Budget := 0;
   F.Prepare_Bus (IO, (7, 4), 10, Accepted);
   Check (not Accepted and F.Problem (IO) = F.Capacity_Refused and Reports = 1, "bus metadata allocation refused");
   F.Dispatch (IO, (Old_Target, False, 0), Response);
   Check (Response.Status = A.Completed and Last_Read_ID = Record_Base, "old binding survives allocation refusal");
   Heap.Successful_Allocation_Budget := Natural'Last;
   for Bus in P.Bus_Number range 4 .. 20 loop
      F.Prepare_Bus (IO, (7, Bus), 10, Accepted); Check (Accepted, "growable bus bindings");
   end loop;
   Before := Exchanges;
   F.Install (IO, Geometry (1), Alias_Target, Accepted);
   Check (Accepted and Alias_Target = New_Target and Exchanges = Before, "target identity stable after binding growth");
   F.Dispatch (IO, (New_Target, False, 0), Response);
   Check (Response.Status = A.Completed and Last_Read_ID = Record_Base + 1, "record identity stable after binding growth");
   declare
      Lazy, Invalid, Missing : F.Context;
      Local : A.Target;
   begin
      Before := Exchanges;
      F.Bind (Invalid, 42, 17, Q.Wire.No_Finite_Deadline, Accepted);
      Check (not Accepted and F.Status (Invalid) = F.Failed
        and Exchanges = Before, "binding rejects infinite deadline without IPC");
      F.Bind (Missing, 0, 17, 10, Accepted);
      Check (not Accepted and F.Status (Missing) = F.Failed
        and Exchanges = Before, "binding rejects missing endpoint without IPC");
      F.Bind (Lazy, 42, 17, 10, Accepted);
      Check (Accepted and F.Status (Lazy) = F.Ready and Exchanges = Before,
        "transport binding has no bus assumption or IPC");
      F.Install (Lazy, Geometry (1), Local, Accepted);
      Check (not Accepted and Exchanges = Before, "bound transport has no invented bus authority");
      F.Bind (Lazy, 43, 18, 10, Accepted);
      Check (not Accepted and Exchanges = Before, "bound transport cannot be rebound");
      No_Bus_Zero := True;
      F.Prepare_Bus (Lazy, (0, 0), 10, Accepted);
      Check (not Accepted and F.Status (Lazy) = F.Ready
        and F.Problem (Lazy) = F.Backend_Refused, "bus-zero absence preserves transport");
      F.Prepare_Bus (Lazy, (7, 1), 10, Accepted);
      Check (Accepted, "first actual bus may be nonzero segment and bus");
      F.Install (Lazy, Geometry (1), Local, Accepted);
      Check (Accepted, "first nonzero bus target installed");
      F.Dispatch (Lazy, (Local, False, 0), Response);
      Check (Response.Status = A.Completed and Last_Read_ID = Record_Base + 1,
        "first nonzero bus read uses its own record");
   end;
   Ada.Text_IO.Put_Line ("PCI frontend prepared-bus checks" & Checks'Image);
end Frontend_Buses_Tests;

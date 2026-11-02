pragma Ada_2022;
with AML_Objects;
with Ada.Command_Line;
with Ada.Text_IO;
with Interfaces;
with AML_Field_Access;
with ACPI_PCI_Protocol;
with ACPI_Field_PCI_Provider;
with ACPI_Field_Combined_Provider;
with ACPI_Field_Inventory_Owner;
with ACPI_Field_NVS_Provider;
with ACPI_Inventory_Protocol;
with ACPI_NVS_Protocol;
with ACPI_Service_Core;
with ACPI_Field_Startup_Owner;
with AML_Delays;
with Ada.Streams.Stream_IO;
with Firmware_Tables;
procedure Startup_Tests is
   package Q renames ACPI_PCI_Protocol;
   package A renames AML_Field_Access;
   use type Interfaces.Unsigned_32;
   use type Q.P.Device_Number, Q.P.Function_Number;
   use type Interfaces.Unsigned_64, A.Completion;
   use type Q.Wire.Time_Instant, Q.Microsecond_Deadline, Q.Operation;
   Mode : constant Positive := Positive'Value (Ada.Command_Line.Argument (1));
   Checks, Exchanges, Reads : Natural := 0;
   Millis : Q.Wire.Time_Instant := 1;
   Micros : Q.Microsecond_Deadline := 1_000;
   Micro_Available : constant Boolean := True;
   IPC_Due : constant Q.Wire.Time_Instant := 10;
   Data_Due : constant Q.Microsecond_Deadline := 9_000;
   Peer_Epoch : constant Interfaces.Unsigned_64 := 16#FEDC_BA98_7654_3210#;
   Peer_ID : constant Interfaces.Unsigned_64 := 16#8123_4567_89AB_CDEF#;
   type Channel is null record;
   procedure Check (Good : Boolean; Name : String) is
   begin
      Checks := Checks + 1;
      if not Good then raise Program_Error with Name; end if;
   end Check;
   procedure Read_Clock (Now : out Q.Wire.Time_Instant; Available : out Boolean) is
   begin Now := Millis; Available := True; end Read_Clock;
   procedure Read_Microseconds (Now : out Q.Microsecond_Deadline; Available : out Boolean) is
   begin Now := Micros; Available := Micro_Available; end Read_Microseconds;
   procedure Read_Deadline (IPC_Deadline : out Q.Wire.Time_Instant;
      Data_Deadline : out Q.Microsecond_Deadline) is
   begin IPC_Deadline := IPC_Due; Data_Deadline := Data_Due; end Read_Deadline;
   procedure Exchange (State : in out Channel; Destination : Natural;
      Deadline : Q.Wire.Time_Instant; Item : Q.Packet; Reply : out Q.Packet;
      Delivered : out Q.Wire.Delivery);
   package PCI is new ACPI_Field_PCI_Provider (Channel, Natural, 0, Read_Clock,
     Read_Microseconds, Exchange, Read_Deadline, 2);
   procedure Window (Now, Deadline : out Q.Wire.Time_Instant) is
   begin Now := Millis; Deadline := 10; end Window;
   procedure NVS_Deadline (Deadline : out Q.Wire.Time_Instant) is
   begin Deadline := 10; end NVS_Deadline;
   package Scalar is new ACPI_Field_Inventory_Owner
     (Channel, Natural, 0, Read_Clock, Exchange, Window);
   package Memory is new ACPI_Field_NVS_Provider
     (Channel, Natural, 0, Read_Clock, Exchange, NVS_Deadline);
   package F is new ACPI_Field_Combined_Provider
     (Natural, Scalar, Memory, PCI, Read_Clock, Window, Enable_PCI => True);
   package Core is new ACPI_Service_Core
     (AML_Delays.Unavailable_Provider, Namespace_Node_Capacity => 64,
      Retained_Result_Capacity => 16, Enable_PCI_Context => True);
   procedure Lease_Clock (Now : out Interfaces.Unsigned_64; Available : out Boolean) is
   begin Now := Interfaces.Unsigned_64 (Micros); Available := True; end Lease_Clock;
   package Owner is new ACPI_Field_Startup_Owner
     (Core, Channel, Natural, 0, Read_Clock, Exchange, Read_Lease_Clock => Lease_Clock,
      Enable_PCI => True);
   IO : F.Context;
   Original : constant A.Target := A.No_Target;
   Accepted : Boolean;
   procedure Exchange (State : in out Channel; Destination : Natural;
      Deadline : Q.Wire.Time_Instant; Item : Q.Packet; Reply : out Q.Packet;
      Delivered : out Q.Wire.Delivery) is
      pragma Unreferenced (State);
      Request : constant Q.Request := Q.Decode_Request (Item);
      Inner : A.Response;
   begin
      Exchanges := Exchanges + 1;
      Check (Destination = 42 and then Deadline = 10, "peer and explicit finite IPC deadline");
      Delivered := Q.Wire.Reply_Received;
      if Item.Label = ACPI_Inventory_Protocol.Header_Operation then
         Reply := (Label => ACPI_Inventory_Protocol.Header_Reply,
           Data => [ACPI_Inventory_Protocol.Outcome'Pos (ACPI_Inventory_Protocol.Ready),
             1, 0, ACPI_Inventory_Protocol.Version], others => <>); return;
      elsif Item.Label = ACPI_NVS_Protocol.Header_Operation then
         Reply := ACPI_NVS_Protocol.Header_Result (1, 0); return;
      end if;
      Check (Request.Valid, "request framing");
      Delivered := Q.Wire.Reply_Received;
      if Request.Kind = Q.Header then
         Reply := Q.Header_Result (Peer_Epoch, Peer_ID);
         if Mode = 11 then Reply.Length := 0;
         elsif Mode = 12 then Millis := 10;
         end if;
         return;
      end if;
      if Request.Kind = Q.Describe then
         Check (Request.Epoch = Peer_Epoch and then Request.ID = Peer_ID, "description identity");
         Reply := Q.Describe_Result (16#E000_0000#, Q.P.Bus_Bytes); return;
      end if;
      Reads := Reads + 1;
      if Reads = 2 then
         Check (Request.Region.Device.Device = 27 and then Request.Region.Device.Func = 0
           and then Request.Offset = 16#400# and then Request.Region.Length = 16#480#,
           "actual AML memory alias beyond legacy config extent");
      end if;
      Check (Request.Epoch = Peer_Epoch and then Request.ID = Peer_ID
        and then Request.Deadline = 10_000, "full identity and data deadline");
      if Mode = 14 then
         F.Dispatch (IO, (Original, False, 0), Inner);
         Check (Inner.Status = A.Denied and then Reads = 1, "reentry refused before exchange");
      end if;
      Reply := Q.Read_Result (16#E000_0000#);
      case Mode is
         when 2 => Delivered := Q.Wire.Completion_Unknown;
         when 3 => Reply.Length := 0;
         when 4 => Delivered := Q.Wire.Not_Dispatched;
         when 5 => Reply := Q.Error_Reply (Q.Read, Q.Stale);
         when 6 => Reply := Q.Error_Reply (Q.Read, Q.Fault);
         when 7 => Millis := 9; Micros := 9_500;
         when 8 => Millis := 0;
         when others => null;
      end case;
   end Exchange;
   Service : aliased Core.State (1, 4_096, 4_096, 1, Value_Object_Capacity => AML_Objects.Max_Objects, Value_Byte_Capacity => AML_Objects.Max_Bytes, Value_Element_Capacity => AML_Objects.Max_Elements);
   Staged : Core.Stage_Status;
   Frozen : Core.Freeze_Status;
   Report : Core.Execution_Report;
   Status : Core.Values.Access_Status;
   Initial : Core.Namespace.Initialization_Report;
   File : Ada.Streams.Stream_IO.File_Type;
   use type Core.Stage_Status, Core.Freeze_Status, Core.Values.Access_Status, Core.Values.Module_Outcome;
   use type Ada.Streams.Stream_Element_Offset;
begin
   Owner.Begin_Startup (42, 10, Accepted);
   Check (Accepted, "native-shaped startup provider initialization");
   Ada.Streams.Stream_IO.Open (File, Ada.Streams.Stream_IO.In_File, Ada.Command_Line.Argument (2));
   declare
      Bytes : Ada.Streams.Stream_Element_Array (1 .. Ada.Streams.Stream_Element_Offset (Ada.Streams.Stream_IO.Size (File)));
      Last : Ada.Streams.Stream_Element_Offset;
      Data : Firmware_Tables.Bytes (1 .. Bytes'Length);
   begin
      Ada.Streams.Stream_IO.Read (File, Bytes, Last);
      Ada.Streams.Stream_IO.Close (File);
      Check (Last = Bytes'Last, "complete table fixture");
      for I in Data'Range loop Data (I) := Firmware_Tables.Byte (Bytes (Ada.Streams.Stream_Element_Offset (I))); end loop;
      Core.Stage_Table (Service, 1, Core.DSDT, Data, Staged);
      Check (Staged = Core.Staged, "stage DSDT");
   end;
   Core.Freeze_Catalog (Service, Frozen);
   Check (Frozen = Core.Catalog_Frozen_OK, "freeze DSDT");
   Owner.Execute_Module_Startup (Service, 10_000, Report, Status);
   Check (Status = Core.Values.Available, "module status");
   Check (Report.Modules.Outcome = Core.Values.Modules_Completed, "PCI module completes");
   Check (Reads = 2, "module performed PCI and memory-alias reads");
   Core.Initialize_Members (Service, Initial, Status);
   Check (Status = Core.Values.Available, "initialize before value query");
   declare Node : Core.Namespace.Node_ID; Item : Core.Node_Descriptor; Value : Core.Values.Result;
   begin
      Core.Find_Child (Service, Core.Namespace.Root, "OUT0", Node, Status);
      Check (Status = Core.Values.Available, "find OUT0");
      Core.Describe_Node (Service, Node, Item, Status);
      Check (Status = Core.Values.Available, "describe OUT0");
      Core.Evaluate_Named (Service, Item, 100, Value, Status);
      Check (Status = Core.Values.Available and then Value.Number = 56, "actual AML PXBR extraction");
   end;
   declare Node : Core.Namespace.Node_ID; Item : Core.Node_Descriptor; Value : Core.Values.Result;
   begin
      Core.Find_Child (Service, Core.Namespace.Root, "ALV0", Node, Status);
      Check (Status = Core.Values.Available, "find alias result");
      Core.Describe_Node (Service, Node, Item, Status);
      Check (Status = Core.Values.Available, "describe alias result");
      Core.Evaluate_Named (Service, Item, 100, Value, Status);
      Check (Status = Core.Values.Available and then Value.Number = 56, "actual AML alias extraction");
   end;
   Owner.Close_Startup;
   Ada.Text_IO.Put_Line ("startup checks" & Checks'Image & " PASS");
end Startup_Tests;

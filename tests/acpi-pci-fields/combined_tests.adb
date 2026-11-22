pragma Ada_2022;
with Ada.Command_Line;
with Ada.Text_IO;
with Interfaces;
with AML_Field_Access;
with AML_Field_Layout;
with ACPI_PCI_Protocol;
with ACPI_Field_PCI_Provider;
with ACPI_Field_Combined_Provider;
with ACPI_Field_Inventory_Owner;
with ACPI_Field_NVS_Provider;
with ACPI_Inventory_Protocol;
with ACPI_NVS_Protocol;
procedure Combined_Tests is
   package Q renames ACPI_PCI_Protocol;
   package A renames AML_Field_Access;
   package L renames AML_Field_Layout;
   package P renames Q.P;
   use type Interfaces.Unsigned_32;
   use type Interfaces.Unsigned_64, A.Register_ID, A.Target, A.Completion;
   use type Q.Wire.Time_Instant, Q.Microsecond_Deadline, Q.Operation, P.T.Access_Width;
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
   use type F.Startup_State;
   IO, Foreign : F.Context;
   Target, Original, Other : A.Target;
   Accepted : Boolean;
   Response : A.Response;
   Geometry : constant L.Register_Geometry :=
     (Region => (Space => L.PCI_Config_Space, Base => 0, Length => 256,
       PCI => (Known => True, Device => (0, 0, 0, 0))),
      Byte_Offset => 16#60#, Width => P.T.Dword_Access);
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
      Check (Request.Epoch = Peer_Epoch and then Request.ID = Peer_ID
        and then Request.Deadline = 9_000, "full identity and data deadline");
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
begin
   F.Initialize (IO, 42, 17, 10, Accepted);
   Check (Accepted and then F.Status (IO) = F.Ready, "combined init");
   declare Alias_Geometry : L.Register_Geometry := Geometry;
   begin
      Alias_Geometry.Region.Space := L.Memory_Space;
      Alias_Geometry.Region.PCI := (Known => False);
      Alias_Geometry.Region.Base := 16#E000_0000#;
      F.Resolve_Module_Target (IO, Alias_Geometry, Target, Accepted);
      Check (Accepted and then Reads = 0, "combined module alias fallback");
      F.Select_Target (IO, Alias_Geometry, False, Other, Accepted);
      Check (Accepted and then Other = Target, "combined named alias reuse");
      F.Select_Target (IO, Alias_Geometry, True, Other, Accepted);
      Check (not Accepted and then Reads = 0, "combined alias write refused");
   end;
   F.Resolve_Module_Target (IO, Geometry, Original, Accepted);
   Check (Accepted and then Reads = 0, "module route metadata only");
   F.Select_Target (IO, Geometry, False, Target, Accepted);
   Check (Accepted and then Target = Original, "module and named share target");
   F.Select_Target (IO, Geometry, True, Other, Accepted);
   Check (not Accepted and then Reads = 0, "combined write denied");
   F.Authorize (Foreign, Original, False, Accepted);
   Check (not Accepted, "foreign combined target denied");
   F.Dispatch (IO, (Original, False, 0), Response);
   Check (Reads = 1, "exactly one data read");
   if Mode in 1 | 14 then
      Check (Response.Status = A.Completed and then Response.Value = 16#E000_0000#, "combined datum");
   elsif Mode in 2 | 3 then
      Check (Response.Status = A.Indeterminate and then F.Uncertain (IO).Present
        and then F.Uncertain (IO).Item.Destination = Original, "outer receipt retains outer target");
   elsif Mode in 6 .. 8 then
      Check (Response.Status = A.Failed_Completed and then F.Completed_Fault (IO).Present,
        "combined completed fault");
   else Check (Response.Status = A.Denied, "combined typed refusal");
   end if;
   if Mode not in 1 | 14 then
      declare Before : constant Natural := Exchanges;
      begin
         F.Dispatch (IO, (Original, False, 0), Response);
         Check (Response.Status = A.Denied and then Exchanges = Before, "combined stopped without replay");
      end;
   end if;
   Ada.Text_IO.Put_Line ("combined mode" & Mode'Image & " checks" & Checks'Image & " PASS");
end Combined_Tests;

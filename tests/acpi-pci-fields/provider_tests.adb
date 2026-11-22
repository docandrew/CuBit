pragma Ada_2022;
with Ada.Command_Line;
with Ada.Text_IO;
with Interfaces;
with AML_Field_Access;
with AML_Field_Layout;
with ACPI_PCI_Protocol;
with ACPI_Field_PCI_Provider;
with ACPI_Capacity_Diagnostics;
with ACPI_Target_Heap_Test_Control;
procedure Provider_Tests is
   package Q renames ACPI_PCI_Protocol;
   package A renames AML_Field_Access;
   package L renames AML_Field_Layout;
   package P renames Q.P;
   package C renames ACPI_Capacity_Diagnostics;
   package Heap renames ACPI_Target_Heap_Test_Control;
   use type Interfaces.Unsigned_64, A.Register_ID, A.Operation_ID, A.Target, A.Completion;
   use type P.Device_Number, P.Function_Number, P.Segment_Number, P.Bus_Number;
   use type Q.Wire.Time_Instant, Q.Microsecond_Deadline, Q.Operation, P.T.Access_Width;
   use type C.Cache_Kind, C.Entry_Count, C.Failure_Kind, L.Register_Geometry;
   Mode : constant Positive := Positive'Value (Ada.Command_Line.Argument (1));
   Checks, Exchanges, Reads, Reports : Natural := 0;
   Millis : Q.Wire.Time_Instant := 1;
   Micros : Q.Microsecond_Deadline := 1_000;
   Micro_Available : Boolean := True;
   IPC_Due : constant Q.Wire.Time_Instant := 10;
   Data_Due : Q.Microsecond_Deadline := 9_000;
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
   procedure Capacity (Item : C.Report) is
   begin
      Reports := Reports + 1;
      Check (Item.Cache = C.PCI_Targets and then Item.Failure = C.Process_Heap
        and then Item.Demand = 257 and then Item.Capacity = 256, "typed allocation refusal");
   end Capacity;
   procedure Exchange (State : in out Channel; Destination : Natural;
      Deadline : Q.Wire.Time_Instant; Item : Q.Packet; Reply : out Q.Packet;
      Delivered : out Q.Wire.Delivery);
   package F is new ACPI_Field_PCI_Provider (Channel, Natural, 0, Read_Clock,
     Read_Microseconds, Exchange, Read_Deadline, 2, Capacity);
   use type F.Phase, F.Local_Failure;
   IO, Foreign : F.Context;
   Target, Original, Other : A.Target;
   Accepted : Boolean;
   Response : A.Response;
   Geometry : constant L.Register_Geometry :=
     (Region => (Space => L.PCI_Config_Space, Base => 0, Length => 256,
       PCI => (Known => True, Device => ((if Mode in 29 | 30 then 7 else 0), (if Mode in 29 | 30 then 1 else 0), 0, 0))),
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
      Check (Request.Valid, "request framing");
      Delivered := Q.Wire.Reply_Received;
      if Request.Kind = Q.Header then
         if Mode in 29 | 30 then
            Check (Request.Key.Segment = 7 and then Request.Key.Bus = 1, "selected nonzero bus");
         end if;
         Reply := Q.Header_Result (Peer_Epoch, Peer_ID);
         if Mode = 11 then Reply.Length := 0;
         elsif Mode = 12 then Millis := 10;
         end if;
         return;
      end if;
      if Request.Kind = Q.Describe then
         Check (Request.Epoch = Peer_Epoch and then Request.ID = Peer_ID, "description identity");
         Reply := Q.Describe_Result (16#E000_0000#, P.Bus_Bytes);
         case Mode is
            when 16 => Reply.Length := 0;
            when 17 => Reply := Q.Error_Reply (Q.Describe, Q.Stale);
            when 18 => Reply.Data (1) := 16#E000_0000# + 1;
            when 19 => Reply.Data (1) := Interfaces.Unsigned_64'Last - P.Function_Bytes + 1;
            when 20 => Reply.Data (2) := P.Bus_Bytes - 1;
            when 21 => Delivered := Q.Wire.Completion_Unknown;
            when 22 => Delivered := Q.Wire.Not_Dispatched;
            when 23 => Millis := 10;
            when 24 => Millis := 0;
            when 28 =>
               F.Initialize (IO, 42, 17, 10, Accepted);
               Check (not Accepted, "description reinitialization denied");
               F.Dispatch (IO, (A.No_Target, False, 0), Inner);
               Check (Inner.Status = A.Denied, "description reentry denied");
            when others => null;
         end case;
         return;
      end if;
      Reads := Reads + 1;
      Check (Request.Epoch = Peer_Epoch and then Request.ID = Peer_ID
        and then Request.Deadline = 9_000, "full identity and data deadline");
      if Mode = 15 then
         Check (Request.Region.Device.Device = 27 and then Request.Region.Device.Func = 0
           and then Request.Region.Base = 0 and then Request.Region.Length = 16#480#
           and then Request.Offset = 16#400#, "beyond legacy config alias");
      elsif Mode = 26 then
         Check (Request.Region.Device.Device = 0 and then Request.Region.Device.Func = 1
           and then Request.Region.Base = 0 and then Request.Region.Length = 4
           and then Request.Offset = 0, "cross-function declared region clipped");
      elsif Mode = 27 then
         Check (Request.Region.Device.Device = 31 and then Request.Region.Device.Func = 7
           and then Request.Region.Base = 0 and then Request.Region.Length = P.Function_Bytes
           and then Request.Offset = P.Function_Bytes - 4, "last full-window alias");
      end if;
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
   procedure Deny_Install (Candidate : L.Register_Geometry; Write : Boolean := False) is
      Attempts : constant Natural := Heap.Attempts;
      Calls : constant Natural := Exchanges;
   begin
      F.Install (IO, Candidate, Other, Accepted, Write);
      Check (not Accepted and then Other = A.No_Target, "invalid geometry rejected");
      Check (Attempts = Heap.Attempts and then Calls = Exchanges, "no allocation or exchange on refusal");
   end Deny_Install;
   procedure Send is
   begin F.Dispatch (IO, (Original, False, 0), Response); end Send;
begin
   F.Initialize (IO, (if Mode = 10 then 0 else 42), 17, 10, Accepted,
     Key => (if Mode in 29 | 30 then (7, 1) else (0, 0)));
   if Mode in 10 .. 12 | 16 .. 24 then
      Check (not Accepted and then F.Status (IO) = F.Failed, "header admission refused");
      Check (Reads = 0 and then Exchanges = (if Mode = 10 then 0 elsif Mode in 11 | 12 then 1 else 2), "header no read");
      Deny_Install (Geometry);
   else
      Check (Accepted and then F.Status (IO) = F.Ready and then Exchanges = 2, "initialize");
      F.Install (IO, Geometry, Original, Accepted);
      Check (Accepted and then Exchanges = 2, "install metadata only");
      if Mode in 15 | 26 | 27 | 30 then
         declare Alias_Geometry : L.Register_Geometry := Geometry;
            Described : L.Register_Geometry;
            Before : constant Natural := Exchanges;
         begin
            Alias_Geometry.Region.Space := L.Memory_Space;
            Alias_Geometry.Region.PCI := (Known => False);
            Alias_Geometry.Region.Base := 16#E000_0000# + 16#D8000#;
            Alias_Geometry.Region.Length := 16#480#;
            Alias_Geometry.Byte_Offset := 16#400#;
            if Mode = 26 then
               Alias_Geometry.Region.Base := 16#E000_0000# + P.Function_Bytes - 4;
               Alias_Geometry.Region.Length := 8; Alias_Geometry.Byte_Offset := 4;
            elsif Mode = 27 then
               Alias_Geometry.Region.Base := 16#E000_0000#;
               Alias_Geometry.Region.Length := P.Bus_Bytes;
               Alias_Geometry.Byte_Offset := P.Bus_Bytes - 4;
            end if;
            F.Install (IO, Alias_Geometry, Original, Accepted);
            Check (Accepted and then Exchanges = Before, "alias installation metadata only");
            F.Describe (IO, Original, Described, Accepted);
            Check (Accepted and then Described = Alias_Geometry, "original AML alias geometry retained");
            Deny_Install (Alias_Geometry, True);
            Alias_Geometry.Region.Base := 16#E000_0000# - 1; Deny_Install (Alias_Geometry);
            Alias_Geometry.Region.Base := 16#E000_0000# + P.Bus_Bytes - 1;
            Alias_Geometry.Region.Length := 2; Alias_Geometry.Byte_Offset := 0;
            Deny_Install (Alias_Geometry);
            Alias_Geometry.Region.Base := 16#E000_0000#; Alias_Geometry.Region.Length := P.Bus_Bytes + 1;
            Deny_Install (Alias_Geometry);
         end;
      end if;
      if Mode = 1 then
         declare Bad : L.Register_Geometry := Geometry; Described : L.Register_Geometry;
         begin
            Bad.Region.PCI := (Known => False); Deny_Install (Bad);
            Bad := Geometry; Bad.Region.Space := L.Memory_Space; Deny_Install (Bad);
            Bad := Geometry; Bad.Region.PCI.Device.Bus := 1; Deny_Install (Bad);
            Bad := Geometry; Bad.Region.PCI.Device.Segment := 1; Deny_Install (Bad);
            Bad := Geometry; Bad.Width := P.T.Qword_Access; Deny_Install (Bad);
            Bad := Geometry; Bad.Byte_Offset := 16#61#; Deny_Install (Bad);
            Bad := Geometry; Bad.Byte_Offset := 256; Deny_Install (Bad);
            Deny_Install (Geometry, True);
            for Device in P.Device_Number loop
               for Func in P.Function_Number loop
                  Bad := Geometry; Bad.Region.PCI.Device.Device := Device;
                  Bad.Region.PCI.Device.Func := Func;
                  F.Install (IO, Bad, Target, Accepted);
                  Check (Accepted and then Target.Register = A.Register_ID
                    (Natural (Device) * 8 + Natural (Func) + 1), "all BDF ordinals stable");
               end loop;
            end loop;
            Check (Exchanges = 2, "all associations metadata only");
            Heap.Successful_Allocation_Budget := 0;
            Bad := Geometry; Bad.Byte_Offset := 16#64#;
            F.Install (IO, Bad, Target, Accepted);
            Check (not Accepted and then Reports = 1 and then F.Status (IO) = F.Ready,
              "growth refusal preserves provider");
            F.Install (IO, Geometry, Target, Accepted);
            Check (Accepted and then Target = Original, "reuse survives OOM");
            F.Describe (IO, Original, Described, Accepted);
            Check (Accepted and then Described = Geometry, "describe retained route");
            -- The previous checks covered operation while growth is refused.
            -- A separate owner now needs storage for its first bus binding.
            Heap.Successful_Allocation_Budget := Natural'Last;
            F.Initialize (Foreign, 42, 17, 10, Accepted);
            Check (Accepted, "independent identity");
            F.Authorize (Foreign, Original, False, Accepted);
            Check (not Accepted, "foreign target rejected");
            Target := Original; Target.Epoch := Target.Epoch + 1;
            F.Authorize (IO, Target, False, Accepted); Check (not Accepted, "modified epoch rejected");
            Target := Original; Target.Register := 257;
            F.Authorize (IO, Target, False, Accepted); Check (not Accepted, "unissued ordinal rejected");
            F.Dispatch (IO, (Original, True, 0), Response);
            Check (Response.Status = A.Denied and then Reads = 0, "write refused without exchange");
            F.Dispatch (IO, (Original, False, 1), Response);
            Check (Response.Status = A.Denied and then Reads = 0, "read payload refused");
         end;
      elsif Mode = 9 then Micro_Available := False;
      elsif Mode = 13 then Data_Due := 11_000;
      end if;
      Send;
      case Mode is
         when 1 | 14 | 15 | 26 | 27 | 28 | 29 | 30 =>
            Check (Response.Status = A.Completed and then Response.Value = 16#E000_0000#
              and then Interfaces.Shift_Right (Response.Value, 26) = 56, "PXBR datum and six-bit extraction");
            Check (F.Status (IO) = F.Ready and then not F.Pending (IO), "normal complete");
         when 2 | 3 =>
            Check (Response.Status = A.Indeterminate and then F.Pending (IO)
              and then F.Uncertain (IO).Present and then Response.Operation /= A.No_Operation,
              "unknown completion retained");
            F.Reconciled (IO, F.Uncertain (IO), Accepted);
            Check (not Accepted, "no invented reconciliation");
            declare Before : constant Natural := Exchanges; begin
               F.Prepare_Bus (IO, (7, 1), 10, Accepted);
               Check (not Accepted and then F.Pending (IO) and then Exchanges = Before,
                 "preparation cannot replace an uncertain PCI read");
            end;
         when 4 => Check (Response.Status = A.Denied and then F.Status (IO) = F.Ready
             and then not F.Pending (IO) and then F.Problem (IO) = F.Not_Sent, "not dispatched is distinct");
         when 5 => Check (Response.Status = A.Denied and then F.Status (IO) = F.Failed
             and then F.Last_Reply (IO).Valid and then not F.Pending (IO), "typed refusal retained");
         when 6 .. 8 =>
            Check (Response.Status = A.Failed_Completed and then F.Completed_Fault (IO).Present
              and then not F.Pending (IO) and then F.Status (IO) = F.Failed, "completed unusable result");
            if Mode = 7 then Check (F.Problem (IO) = F.Late_Reply, "late reply reason");
            elsif Mode = 8 then Check (F.Problem (IO) = F.Clock_Reversed, "reversed clock reason"); end if;
         when 9 | 13 =>
            Check (Response.Status = A.Denied and then Reads = 0, "preflight no exchange");
            Check (F.Problem (IO) = (if Mode = 9 then F.Clock_Unavailable else F.Invalid_Deadline),
              "preflight reason");
         when others => raise Program_Error;
      end case;
      Check (Reads = (if Mode in 9 | 13 then 0 else 1), "no internal retry");
      if Mode in 2 | 3 | 5 .. 9 then
         declare Before : constant Natural := Exchanges;
         begin Send; Check (Response.Status = A.Denied and then Exchanges = Before, "stopped provider no replay"); end;
      end if;
   end if;
   Ada.Text_IO.Put_Line ("mode" & Mode'Image & " checks" & Checks'Image & " PASS");
end Provider_Tests;

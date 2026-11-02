pragma Ada_2022;
with AML_Decode;
with ACPI_PCI_Memory_Reads;
with AML_Identity.Issuer;
package body ACPI_Field_PCI_Provider is
   package A renames AML_Field_Access;
   package L renames AML_Field_Layout;
   package P renames Q.P;
   use type Interfaces.Unsigned_64, AML_Decode.Byte, AML_Identity.Identity;
   use type A.Endpoint_ID, Q.Wire.Time_Instant, Q.Microsecond_Deadline;
   use type Q.Outcome, Q.Encoding_Outcome, Q.Wire.Delivery, P.Outcome, L.Register_Geometry, Storage.Outcome;
   Provider_Epoch : constant A.Counter := 1;
   function Status (IO : Context) return Phase is (IO.Stage);
   function Problem (IO : Context) return Local_Failure is (IO.Failure);
   function Last_Reply (IO : Context) return Q.Reply is (IO.Last);
   function Identity (IO : Context) return AML_Identity.Identity is (IO.Issuer);
   function Pending (IO : Context) return Boolean is (IO.Working or else IO.Captured.Present);
   function Uncertain (IO : Context) return A.Observation is (IO.Captured);
   function Completed_Fault (IO : Context) return A.Observation is (IO.Fault);
   function Live (IO : Context) return Boolean is (IO.Stage = Ready and then not Pending (IO));
   procedure Check_IPC (IO : in out Context; Deadline : Q.Wire.Time_Instant; Good : out Boolean) is
      Now : Q.Wire.Time_Instant;
      Available : Boolean;
   begin
      Good := False;
      if Deadline = Q.Wire.No_Finite_Deadline then IO.Failure := Invalid_Deadline; return; end if;
      Read_Clock (Now, Available);
      if not Available then IO.Failure := Clock_Unavailable; return; end if;
      if IO.Seen_IPC and then Now < IO.Last_IPC then IO.Failure := Clock_Reversed; return; end if;
      IO.Last_IPC := Now; IO.Seen_IPC := True;
      if Now >= Deadline then IO.Failure := Expired; return; end if;
      Good := True;
   end Check_IPC;
   procedure Check_Time (IO : in out Context; IPC_Deadline : Q.Wire.Time_Instant;
      Data_Deadline : Q.Microsecond_Deadline; Good : out Boolean) is
      Now : Q.Microsecond_Deadline;
      Available : Boolean;
   begin
      Good := False;
      if Data_Deadline = 0 or else Data_Deadline = Q.No_Finite_Deadline
        or else Q.IPC_Deadline_For (Data_Deadline) > IPC_Deadline
      then IO.Failure := Invalid_Deadline; return; end if;
      Check_IPC (IO, IPC_Deadline, Good); if not Good then return; end if;
      Read_Microseconds (Now, Available); Good := False;
      if not Available then IO.Failure := Clock_Unavailable; return; end if;
      if IO.Seen_Data and then Now < IO.Last_Data then IO.Failure := Clock_Reversed; return; end if;
      IO.Last_Data := Now; IO.Seen_Data := True;
      if Now >= Data_Deadline then IO.Failure := Expired; return; end if;
      Good := True;
   end Check_Time;
   procedure Initialize
     (IO : in out Context; Destination : Peer; Symbol : A.Endpoint_ID;
      Deadline : Q.Wire.Time_Instant; Accepted : out Boolean) is
      Packet : Q.Packet;
      Delivered : Q.Wire.Delivery;
      Good : Boolean;
   begin
      Accepted := False;
      if IO.Stage /= Fresh or else IO.Working then return; end if;
      IO.Stage := Provisioning; IO.Working := True;
      if Destination = No_Peer or else Symbol = A.No_Endpoint then
         IO.Stage := Failed; IO.Working := False; return;
      end if;
      Check_IPC (IO, Deadline, Good);
      if not Good then IO.Stage := Failed; IO.Working := False; return; end if;
      Exchange (IO.Channel, Destination, Deadline, Q.Header_Request, Packet, Delivered);
      if Delivered /= Q.Wire.Reply_Received then
         IO.Failure := (if Delivered = Q.Wire.Not_Dispatched then Not_Sent else Completion_Unknown);
         IO.Stage := Failed; IO.Working := False; return;
      end if;
      IO.Last := Q.Decode_Reply (Packet, Q.Header);
      Check_IPC (IO, Deadline, Good);
      if not Good or else not IO.Last.Valid or else IO.Last.Status /= Q.Ready then
         if Good then IO.Failure := (if IO.Last.Valid then Backend_Refused else Bad_Reply); end if;
         IO.Stage := Failed; IO.Working := False; return;
      end if;
      IO.Epoch := IO.Last.Epoch; IO.ID := IO.Last.ID;
      Check_IPC (IO, Deadline, Good);
      if not Good then IO.Stage := Failed; IO.Working := False; return; end if;
      Exchange (IO.Channel, Destination, Deadline, Q.Describe_Request (IO.Epoch, IO.ID), Packet, Delivered);
      if Delivered /= Q.Wire.Reply_Received then
         IO.Failure := (if Delivered = Q.Wire.Not_Dispatched then Not_Sent else Completion_Unknown);
         IO.Stage := Failed; IO.Working := False; return;
      end if;
      IO.Last := Q.Decode_Reply (Packet, Q.Describe);
      Check_IPC (IO, Deadline, Good);
      if not Good or else not IO.Last.Valid or else IO.Last.Status /= Q.Ready then
         if Good then IO.Failure := (if IO.Last.Valid then Backend_Refused else Bad_Reply); end if;
         IO.Stage := Failed; IO.Working := False; return;
      end if;
      IO.Bus_First := IO.Last.First;
      AML_Identity.Issuer.Issue (IO.Issuer, Accepted);
      if not Accepted then IO.Stage := Failed; IO.Working := False; return; end if;
      IO.Destination := Destination; IO.Symbol := Symbol;
      IO.Stage := Ready; IO.Working := False;
   end Initialize;
   procedure Install
     (IO : in out Context; Geometry : L.Register_Geometry;
      Target : out A.Target; Accepted : out Boolean; For_Write : Boolean := False) is
      Plan : P.View;
      Config : P.Region;
      Offset : Interfaces.Unsigned_64;
      Slot : Natural;
      Stored : Storage.Outcome;
   begin
      Target := A.No_Target; Accepted := False;
      if not Live (IO) or else For_Write then return; end if;
      if Geometry.Region.Space = L.PCI_Config_Space and then Geometry.Region.PCI.Known then
         Config := (Geometry.Region.PCI.Device, Geometry.Region.Base, Geometry.Region.Length);
         Offset := Geometry.Byte_Offset;
         Plan := P.Plan (Config, Offset, Geometry.Width, P.Bus_Bytes);
         if Plan.Status /= P.Read_Ready then return; end if;
      elsif Geometry.Region.Space = L.Memory_Space and then not Geometry.Region.PCI.Known then
         declare Alias_View : constant ACPI_PCI_Memory_Reads.Translation := ACPI_PCI_Memory_Reads.Plan
           (IO.Bus_First, P.Bus_Bytes, Geometry.Region.Base, Geometry.Region.Length,
            Geometry.Byte_Offset, Geometry.Width);
         begin
            if Alias_View.Status /= P.Read_Ready then return; end if;
            Config := Alias_View.Region; Offset := Alias_View.Offset;
         end;
      else return; end if;
      for I in 1 .. Storage.Length (IO.Items) loop
         if Storage.Read (IO.Items,I).Geometry = Geometry then
            Target := (IO.Issuer, IO.Symbol, Provider_Epoch, A.Register_ID (I)); Accepted := True; return;
         end if;
      end loop;
      IO.Working := True;
      Storage.Append (IO.Items, (Geometry, Config, Offset), Slot, Stored);
      if Stored /= Storage.Stored then
         IO.Failure := Capacity_Refused;
         Record_Capacity_Failure (Storage.Capacity_Failure (IO.Items, ACPI_Capacity_Diagnostics.PCI_Targets));
      else Target := (IO.Issuer, IO.Symbol, Provider_Epoch, A.Register_ID (Slot)); Accepted := True;
      end if;
      IO.Working := False;
   end Install;
   function Matches (IO : Context; Target : A.Target) return Boolean is
     (IO.Issuer /= AML_Identity.No_Identity and then Target.Provider = IO.Issuer
      and then Target.Endpoint = IO.Symbol and then Target.Epoch = Provider_Epoch
      and then Target.Register in 1 .. A.Register_ID (Storage.Length (IO.Items)));
   procedure Describe
     (IO : Context; Target : A.Target; Geometry : out L.Register_Geometry; Accepted : out Boolean) is
   begin
      Geometry := (others => <>); Accepted := Live (IO) and then Matches (IO,Target);
      if Accepted then Geometry := Storage.Read (IO.Items,Positive (Target.Register)).Geometry; end if;
   end Describe;
   procedure Authorize
     (IO : Context; Target : A.Target; For_Write : Boolean; Accepted : out Boolean) is
   begin Accepted := not For_Write and then Live (IO) and then Matches (IO,Target) and then IO.Issued < A.Counter'Last; end Authorize;
   procedure Dispatch (IO : in out Context; Item : A.Request; Reply : out A.Response) is
      Accepted, Good : Boolean;
      IPC_Deadline : Q.Wire.Time_Instant;
      Data_Deadline : Q.Microsecond_Deadline;
      Packet : Q.Packet;
      Delivered : Q.Wire.Delivery;
   begin
      Reply := (others => <>); Authorize (IO, Item.Destination, Item.For_Write, Accepted);
      if not Accepted or else Item.Value /= 0 then return; end if;
      IO.Working := True; IO.Issued := IO.Issued + 1;
      IO.Failure := No_Failure; IO.Last := (others => <>);
      Read_Deadline (IPC_Deadline, Data_Deadline);
      Check_Time (IO, IPC_Deadline, Data_Deadline, Good);
      if not Good then
         if IO.Failure not in Invalid_Deadline | Expired then IO.Stage := Failed; end if;
         IO.Working := False; return;
      end if;
      declare
         Row : constant Association := Storage.Read (IO.Items,Positive (Item.Destination.Register));
         Encoded : constant Q.Encoded_Request := Q.Read_Request (IO.Epoch, IO.ID,
           Row.Config, Row.Offset, Row.Geometry.Width, Data_Deadline);
      begin
         if Encoded.Status /= Q.Encoded then IO.Stage := Failed; IO.Working := False; return; end if;
         Exchange (IO.Channel, IO.Destination, IPC_Deadline, Encoded.Item, Packet, Delivered);
      end;
      if Delivered = Q.Wire.Not_Dispatched then IO.Failure := Not_Sent; IO.Working := False; return; end if;
      if Delivered /= Q.Wire.Reply_Received then
         IO.Failure := Completion_Unknown;
         IO.Captured := (True,IO.Issuer,Item,A.Operation_ID (IO.Issued)); IO.Stage := Failed;
         Reply := (A.Indeterminate,IO.Issuer,0,IO.Captured.Operation); IO.Working := False; return;
      end if;
      IO.Last := Q.Decode_Reply (Packet,Q.Read,Storage.Read (IO.Items,Positive (Item.Destination.Register)).Geometry.Width);
      if not IO.Last.Valid then
         IO.Failure := Bad_Reply;
         IO.Captured := (True,IO.Issuer,Item,A.Operation_ID (IO.Issued)); IO.Stage := Failed;
         Reply := (A.Indeterminate,IO.Issuer,0,IO.Captured.Operation); IO.Working := False; return;
      end if;
      if IO.Last.Status = Q.Done then
         Check_Time (IO,IPC_Deadline,Data_Deadline,Good);
         if Good then Reply := (A.Completed,IO.Issuer,IO.Last.Value,A.No_Operation);
         else
            if IO.Failure = Expired then IO.Failure := Late_Reply; end if;
            IO.Fault := (True,IO.Issuer,Item,A.No_Operation); IO.Stage := Failed;
            Reply := (A.Failed_Completed,IO.Issuer,0,A.No_Operation);
         end if;
      elsif IO.Last.Status = Q.Fault then
         IO.Failure := Backend_Refused; IO.Fault := (True,IO.Issuer,Item,A.No_Operation); IO.Stage := Failed;
         Reply := (A.Failed_Completed,IO.Issuer,0,A.No_Operation);
      else IO.Failure := Backend_Refused; IO.Stage := Failed;
      end if;
      IO.Working := False;
   end Dispatch;
   procedure Reconciled
     (IO : Context; Expected : A.Observation; Accepted : out Boolean) is
      pragma Unreferenced (IO,Expected);
   begin Accepted := False; end Reconciled;
end ACPI_Field_PCI_Provider;

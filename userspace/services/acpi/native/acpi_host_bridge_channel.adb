pragma Ada_2022;
with System.Storage_Elements;
with CuBit.Channel_Bells;
with CuBit.Channel_Protocol;
with CuBit.Memory_Grants;
package body ACPI_Host_Bridge_Channel is
   package C renames CuBit.Channels;
   package CC renames CuBit.Channel_Contracts;
   use type W.Word, P.Process_ID, GR.Reference, CC.Contract;
   use type C.Side, C.Open_Result, C.Signal_Result;
   use type S.Outcome, CuBit.Protocols.Wire_Size_Kind, CuBit.Protocols.Protocol_Version;
   use type Interfaces.Unsigned_32, CuBit.Channel_Bells.Watch_Result;
   use type CuBit.Protocols.Schema_Contract, M.Acceptance;
   use type CuBit.Control_Events.Event_Kind, RC.Token;
   Page_Bytes : constant Positive := RC.Page_Bytes;
   Maximum_Payload : constant Natural := (CC.Ring_Pages'Last - RC.Client_Ring_Pages) * Page_Bytes;
   Protocol_Version : constant CuBit.Protocols.Protocol_Version := 1;
   function Observe (IO : Session) return Report is (IO.View);
   function Address (Base : W.Word) return System.Address is
     (System.Storage_Elements.To_Address (System.Storage_Elements.Integer_Address (Base)));
   function Mapped (Base, Bytes : W.Word) return Boolean is
     (Base /= 0 and then Base mod W.Word (Page_Bytes) = 0
      and then Base <= W.Word'Last - Bytes);
   function Own_Mapping (IO : Session) return Boolean is
     (IO.Channel.Opener and then IO.Channel.This_Side = C.Producing
      and then IO.Channel.Item = IO.Contract and then IO.Channel.Has_Own
      and then IO.Channel.Own_Bytes = W.Word (IO.Contract.Pages * Page_Bytes)
      and then Mapped (IO.Channel.Own_Base, IO.Channel.Own_Bytes));
   function Live (IO : Session) return Boolean is
     (IO.Bound and then not IO.View.Poisoned and then IO.Owner = CuBit.Messages.Own_Process
      and then IO.Channel.Active and then Own_Mapping (IO)
      and then IO.Channel.Has_Peer and then IO.Channel.Own_Grant = IO.Own_Grant
      and then IO.Channel.Peer_Grant = IO.Peer_Grant);
   procedure Capture_Region (IO : in out Session) is
   begin
      if IO.Channel.Has_Own then IO.Region := Owned_Region; IO.Own_Grant := IO.Channel.Own_Grant;
      elsif IO.Channel.Closed_Own_Present then
         IO.Region := Owned_Region; IO.Own_Grant := IO.Channel.Closed_Own_Grant;
      else IO.Region := No_Region;
      end if;
   end Capture_Region;
   procedure Expire (IO : in out Session; Now : W.Word) is
   begin
      if IO.Attempted and then Now >= IO.View.Deadline then IO.View.Expired := True; end if;
   end Expire;
   procedure Begin_Open (IO : in out Session; Endpoint : CuBit.Handles.Endpoint;
      Peer : P.Process_ID; Schema : CuBit.Protocols.Schema_Contract;
      Connector : Interfaces.Unsigned_16; Open_Token, Data_Token, Deadline, Now : W.Word;
      Info : S.Header; Items : W.Rows; Source : S.Bytes; Submitted : out Boolean) is
      Encoded : S.Report;
      Packet_Bytes, Pages : Natural;
   begin
      Submitted := False;
      if IO.Working then IO.View.Poisoned := True; return; end if;
      if IO.Attempted then return; end if;
      IO.Attempted := True; IO.Working := True; IO.Region := No_Region;
      IO.Owner := CuBit.Messages.Own_Process; IO.Peer := Peer;
      IO.View := (Stage => Setup_Refused, Why => Invalid_Setup, Root_ID => Info.Root_ID,
        Sequence => Info.Sequence, Deadline => Deadline, Open_Token => Open_Token,
        Data_Token => Data_Token, others => <>);
      if not P.Is_Process (IO.Owner) or else not P.Is_Process (Peer)
        or else not CuBit.Handles.Valid (Endpoint) or else not CuBit.Protocols.Valid (Schema)
        or else Schema /= M.Element or else Schema.Version /= Protocol_Version or else Schema.Sizing /= CuBit.Protocols.Fixed_Size
        or else Schema.Wire_Size /= M.Message_Bytes or else Open_Token = 0 or else Data_Token = 0
        or else Open_Token = Data_Token or else Open_Token = CuBit.Messages.NO_COMPLETION_TOKEN
        or else Data_Token = CuBit.Messages.NO_COMPLETION_TOKEN
        or else Deadline = 0 or else Deadline = CuBit.Messages.Wait_Forever or else Now >= Deadline
      then IO.Working := False; return; end if;
      if Source'Length > Maximum_Payload - S.Header_Bytes
        or else Items'Length > (Maximum_Payload - S.Header_Bytes - Source'Length) / S.Row_Bytes
      then IO.View.Why := Payload_Too_Large; IO.Working := False; return; end if;
      Packet_Bytes := S.Header_Bytes + Items'Length * S.Row_Bytes + Source'Length;
      Pages := RC.Client_Ring_Pages + (Packet_Bytes + Page_Bytes - 1) / Page_Bytes;
      if RC.Client_Ring_Pages /= M.Client_Ring_Pages or else RC.Server_Pages /= M.Server_Pages
      then IO.Working := False; return; end if;
      IO.Contract := M.Measured_Contract (Pages);
      declare
         Prepared : S.Bytes (1 .. Packet_Bytes);
      begin
         S.Encode (Info, Items, Source, Prepared, Encoded);
         if Encoded.Status /= S.Done or else Encoded.Used /= Packet_Bytes then
            IO.View.Why := Invalid_Packet; IO.Working := False; return;
         end if;
         IO.View.Packet_Bytes := Packet_Bytes; IO.Region := Unknown_Region;
         C.Begin_Open (Endpoint, IO.Contract, C.Producing, Open_Token, IO.Channel, Submitted, Connector);
         Capture_Region (IO); IO.Pending_Open := Submitted;
         if not Submitted then IO.View.Why := Open_Not_Submitted; IO.Working := False; return; end if;
         IO.View.Stage := Opening; IO.View.Why := None;
         if not Own_Mapping (IO) then
            IO.View.Why := Mapping_Refused; IO.View.Poisoned := True; IO.Working := False; return;
         end if;
         declare
            --  SEC-025: own producer payload; write private encoding before publishing the request.
            Payload : S.Bytes (1 .. Packet_Bytes) with Import,
              Address => Address (IO.Channel.Own_Base + W.Word (RC.Client_Payload_At));
         begin Payload := Prepared; end;
      end;
      IO.Working := False;
   exception when others => IO.View.Why := Owner_Fault; IO.View.Poisoned := True; raise;
   end Begin_Open;
   procedure Complete_Open (IO : in out Session; Item : CuBit.Messages.CompletionEntry;
      Handled : out Boolean) is
      Status : C.Open_Result;
      Refusal : CuBit.Channel_Protocol.Open_Refusal;
      Peer_Bytes : constant W.Word := W.Word (RC.Server_Pages * Page_Bytes);
      Watched : CuBit.Channel_Bells.Watch_Result;
   begin
      Handled := False;
      if IO.Working then IO.View.Poisoned := True; return; end if;
      if not IO.Pending_Open or else IO.View.Poisoned then return; end if;
      if IO.Owner /= CuBit.Messages.Own_Process or else not Item.valid
        or else Item.token /= IO.View.Open_Token or else Item.from /= P.To_Word (IO.Peer)
      then return; end if;
      if Item.status not in CuBit.Messages.COMPLETION_OK
        | CuBit.Messages.COMPLETION_TARGET_DIED | CuBit.Messages.COMPLETION_CANCELLED
      then return; end if;
      IO.Working := True; Handled := True;
      IO.View.Completion_Status := Item.status;
      if Item.status = CuBit.Messages.COMPLETION_OK then
         C.Finish_Open (IO.Channel, Item.msg, Status, Refusal);
      else
         -- The kernel has removed this exact pending request. Its message
         -- is not a peer acceptance and must never reach Finish_Open.
         C.Close (IO.Channel);
         Status := C.Failed;
      end if;
      IO.Pending_Open := False;
      if (IO.Channel.Has_Own and then IO.Channel.Own_Grant /= IO.Own_Grant)
        or else (IO.Channel.Closed_Own_Present and then IO.Channel.Closed_Own_Grant /= IO.Own_Grant)
      then IO.View.Why := Mapping_Refused; IO.View.Poisoned := True; IO.Working := False; return; end if;
      if Status /= C.Opened then
         IO.View.Stage := Setup_Refused;
         IO.View.Why :=
           (if Item.status = CuBit.Messages.COMPLETION_TARGET_DIED then Open_Target_Died
            elsif Item.status = CuBit.Messages.COMPLETION_CANCELLED then Open_Cancelled
            elsif Status = C.Refused then Open_Refused else Open_Failed);
         IO.Working := False; return;
      end if;
      if not IO.Channel.Active or else not Own_Mapping (IO) or else not IO.Channel.Has_Peer
        or else not Mapped (IO.Channel.Peer_Base, Peer_Bytes)
        or else (if IO.Channel.Own_Base <= IO.Channel.Peer_Base then
          IO.Channel.Peer_Base - IO.Channel.Own_Base < IO.Channel.Own_Bytes
          else IO.Channel.Own_Base - IO.Channel.Peer_Base < Peer_Bytes)
      then IO.View.Why := Mapping_Refused; IO.View.Poisoned := True; IO.Working := False; return; end if;
      IO.Peer_Grant := IO.Channel.Peer_Grant;
      RC.Attach_Client (IO.Ring, Address (IO.Channel.Own_Base), Address (IO.Channel.Peer_Base));
      CuBit.Channel_Bells.Watch (IO.Peer_Grant, Watched);
      IO.Watching := True; IO.Routed := Watched = CuBit.Channel_Bells.Routed;
      IO.Bound := True; IO.View.Stage := Ready; IO.Working := False;
   exception when others => IO.View.Why := Owner_Fault; IO.View.Poisoned := True; raise;
   end Complete_Open;
   procedure Publish (IO : in out Session; Now : W.Word; Submitted : out Boolean) is
      Ring_Peer : Boolean;
      Signalled : C.Signal_Result;
   begin
      Submitted := False;
      if IO.Working then IO.View.Poisoned := True; return; end if;
      Expire (IO, Now);
      if not Live (IO) or else IO.View.Stage /= Ready or else IO.View.Expired then return; end if;
      IO.Working := True;
      RC.Submit (IO.Ring, RC.Token (IO.View.Data_Token),
        [M.Root_ID => IO.View.Root_ID, M.Sequence => IO.View.Sequence,
         M.Packet_Bytes => W.Word (IO.View.Packet_Bytes)], Submitted, Ring_Peer);
      if Submitted then
         IO.View.Published := True; IO.View.Stage := Awaiting_Receipt;
         if Ring_Peer then
            C.Signal (IO.Channel, Sync => False, Result => Signalled);
            if Signalled /= C.Signalled then IO.View.Why := Wake_Failed; end if;
         end if;
      end if;
      IO.Working := False;
   exception when others => IO.View.Why := Owner_Fault; IO.View.Poisoned := True; raise;
   end Publish;
   procedure Poll (IO : in out Session) is
      Item : RC.Queues.Completion;
      Got, Woke : Boolean;
      pragma Unreferenced (Woke);
   begin
      if IO.Working then IO.View.Poisoned := True; return; end if;
      if not Live (IO) or else IO.View.Stage /= Awaiting_Receipt then return; end if;
      IO.Working := True;
      Woke := CuBit.Channel_Bells.Take (IO.Peer_Grant, IO.Routed);
      RC.Reap (IO.Ring, Item, Got);
      if Got then
         if Item.Tag /= RC.Token (IO.View.Data_Token)
           or else Item.Answer (M.Root_ID) /= IO.View.Root_ID
           or else Item.Answer (M.Sequence) /= IO.View.Sequence
           or else not M.Valid_Acceptance_Code (Item.Answer (M.Disposition))
         then IO.View.Why := Receipt_Refused; IO.View.Poisoned := True;
         else
            IO.View.Acceptance := M.Acceptance_From_Code (Item.Answer (M.Disposition));
            IO.View.Acknowledged := True; IO.View.Peer_Accepted := IO.View.Acceptance = M.Accepted;
            IO.View.Stage := Receipt_Known;
         end if;
      end if;
      IO.Working := False;
   exception when others => IO.View.Why := Owner_Fault; IO.View.Poisoned := True; raise;
   end Poll;
   procedure Note_Time (IO : in out Session; Now : W.Word) is
   begin
      if IO.Working then IO.View.Poisoned := True; return; end if;
      Expire (IO, Now);
   end Note_Time;
   procedure Prepare_Wait (IO : in out Session; Armed : out Boolean) is
   begin
      Armed := False;
      if IO.Working then IO.View.Poisoned := True; return; end if;
      if Live (IO) and then IO.View.Stage = Awaiting_Receipt then
         Armed := RC.Arm_For_Completions (IO.Ring);
      end if;
   end Prepare_Wait;
   procedure Disarm (IO : in out Session) is
   begin
      if IO.Working then IO.View.Poisoned := True; return; end if;
      if Live (IO) then RC.Disarm_For_Completions (IO.Ring); end if;
   end Disarm;
   procedure Note_Event (IO : in out Session; Event : CuBit.Control_Events.Event; Ends : out Boolean) is
   begin
      Ends := False;
      if IO.Working then IO.View.Poisoned := True; return; end if;
      if IO.View.Stage = Closed then return; end if;
      if Event.Kind in CuBit.Control_Events.Grant_Revoked | CuBit.Control_Events.Grant_Returned
        and then Event.Peer /= IO.Peer then return; end if;
      Ends := C.Ended (IO.Channel, Event)
        or else (IO.Channel.Has_Own and then Event.Kind = CuBit.Control_Events.Grant_Returned
          and then Event.Slot = IO.Own_Grant.slot and then Event.Generation = IO.Own_Grant.generation);
      if Ends then
         if not IO.View.Poisoned then IO.View.Why := Peer_Ended; end if;
         IO.View.Poisoned := True; IO.Bound := False;
      end if;
   end Note_Event;
   procedure Close (IO : in out Session; Completed : out Boolean) is
      Settled_End : constant Boolean := IO.View.Poisoned and then IO.View.Why = Peer_Ended
        and then IO.View.Acknowledged and then not IO.Pending_Open;
   begin
      Completed := False;
      if IO.Working then IO.View.Poisoned := True; return; end if;
      if not IO.Attempted or else IO.Pending_Open or else (IO.View.Poisoned and then not Settled_End)
        or else (IO.View.Published and then not IO.View.Acknowledged)
        or else IO.Owner /= CuBit.Messages.Own_Process then return; end if;
      if IO.View.Stage = Closed then Completed := True; return; end if;
      IO.Working := True;
      if RC.Client_Attached (IO.Ring) then
         if RC.Outstanding (IO.Ring) /= 0 then IO.Working := False; return; end if;
         if IO.Watching then CuBit.Channel_Bells.Unwatch (IO.Peer_Grant); IO.Watching := False; end if;
         RC.Detach_Client (IO.Ring);
      end if;
      C.Close (IO.Channel); IO.Bound := False;
      if IO.Region = Owned_Region and then
        (not IO.Channel.Closed_Own_Present or else IO.Channel.Closed_Own_Grant /= IO.Own_Grant)
      then IO.View.Why := Mapping_Refused; IO.View.Poisoned := True; IO.Working := False; return; end if;
      IO.View.Stage := Closed;
      IO.Working := False; Completed := True;
   exception when others => IO.View.Why := Owner_Fault; IO.View.Poisoned := True; raise;
   end Close;
   function Retired (IO : Session) return Boolean is
     (not IO.Working and then IO.View.Stage = Closed and then IO.Owner = CuBit.Messages.Own_Process
      and then (IO.Region = No_Region or else
        (IO.Region = Owned_Region and then CuBit.Memory_Grants.Retirement_Confirmed (IO.Own_Grant))));
end ACPI_Host_Bridge_Channel;

pragma Ada_2022;
with CuBit.Handles.IPC;
with CuBit.Handles.Devices;
with CuBit.Monotonic;
with Capability_ABI;
with ACPI_PCI_Window_Client;
with ACPI_PCI_Bus_Geometry;
with ACPI_Target_Storage;
with ACPI_Firmware_Data_Arena;
with ACPI_Backend_Kernel_Handles;
with ACPI_Backend_Binding_Types;
with ACPI_Backend_Memory_Access;
package body ACPI_Native_PCI_Windows is
   package Q renames ACPI_PCI_Protocol;
   package P renames Q.P;
   package G renames ACPI_PCI_Bus_Geometry;
   package Client renames ACPI_PCI_Window_Client;
   package X renames Client.Q;
   package H renames CuBit.Handles;
   package IPC renames H.IPC;
   package IDs renames CuBit.Process_IDs;
   package B renames ACPI_Backend_Binding_Types;
   package Arena is new ACPI_Firmware_Data_Arena;
   use type Word, P.Bus_Key, X.Request_ID, X.Record_ID, X.Disposition;
   use type H.ABI.Kind, IPC.Carried_Kind, Client.Outcome, Q.Operation;
   use type G.Outcome, P.T.Access_Width, Arena.Outcome;
   use type Q.Outcome, B.Tick;
   use type Q.Wire.Time_Instant;
   use type Interfaces.Unsigned_32;
   use type X.Segment_Number, X.Bus_Number;
   type Row_Phase is (Reserved, Retained, Mapped, Refused);
   type Row is record
      Key : P.Bus_Key;
      State : Row_Phase := Reserved;
      Owned : IPC.Carried;
      Memory : H.Device_Memory := H.No_Device_Memory;
      Info : X.Reply;
      Virtual : Word := 0;
      Ticket : Arena.Reservation := Arena.No_Reservation;
      Problem : Q.Outcome := Q.Unavailable;
      Rejected_Cleanup : Boolean := False;
   end record;
   package Store is new ACPI_Target_Storage (Row);
   use type Store.Outcome;
   Rows : Store.Context;
   Reservations : Arena.Context;
   Service : H.Endpoint := H.No_Endpoint;
   Provider : IDs.Process_ID := IDs.No_Process;
   Owner_Scope, Life : Word := 0;
   Next_Request : X.Request_ID := 1;
   Stage : Phase := Fresh;
   Working : Boolean := False;
   Broker_Timeout_Milliseconds : constant Word := 1_000;
   Pages_Per_Bus : constant Word := X.Bus_Bytes / X.Page_Bytes;

   function Observe return Statistics is
      R : Statistics := (Stage => Stage, Working => Working, Rows => Store.Length (Rows), others => <>);
   begin
      for K in 1 .. Store.Length (Rows) loop
         declare Item : constant Row := Store.Read (Rows, K); begin
            if Item.State = Mapped then R.Mapped := R.Mapped + 1;
            elsif Item.State = Refused then
               R.Refused := R.Refused + 1;
               if Item.Rejected_Cleanup then R.Cleanup_Pending := R.Cleanup_Pending + 1; end if;
            end if;
         end;
      end loop;
      return R;
   end Observe;

   procedure Initialize
     (Endpoint : H.Endpoint; Server : IDs.Process_ID;
      Scope, Epoch, First, Limit : Word; Accepted : out Boolean)
   is
      Code : Arena.Outcome;
      D : constant IPC.Description := IPC.Describe (Endpoint);
   begin
      Accepted := False;
      if Stage /= Fresh or else Working then return; end if;
      Stage := Failed;
      if Scope = 0 or else Epoch = 0 or else not IDs.Is_Process (Server)
        or else not D.Current or else D.Kind /= H.ABI.Endpoint
        or else D.Object /= IDs.To_Word (Server) or else D.Authority_Tag /= X.Backend_Tag
        or else not D.Held.Read or else not D.Held.Write or else D.Held.Grant
        or else D.Held.Execute or else D.Held.Revoke then return;
      end if;
      Arena.Configure (Reservations, First, Limit, Code);
      if Code /= Arena.Configured then return; end if;
      Service := Endpoint; Provider := Server; Owner_Scope := Scope; Life := Epoch;
      Stage := Serving; Accepted := True;
   end Initialize;

   function Backing_Live (Item : Row) return Boolean is
      D : constant IPC.Description := IPC.Describe (Item.Memory);
   begin
      return D.Current and then D.Kind = H.ABI.Device_Memory
        and then D.Authority_Tag = Capability_ABI.Firmware_PCI_Config_Tag
        and then D.Held.Read and then not D.Held.Write and then not D.Held.Grant
        and then not D.Held.Execute and then not D.Held.Revoke
        and then D.Object = Item.Info.First and then D.Parameter = X.Bus_Bytes;
   end Backing_Live;

   procedure Ensure
     (Key : P.Bus_Key; Setup_Deadline : Q.Wire.Time_Instant;
      Allow_Setup : Boolean; Index : out Natural; Why : out Q.Outcome) is
      Item : Row;
      Code : Store.Outcome;
      Local : Client.Context;
      Reply : Client.Report;
      Saved_Request : X.Request;
      Accepted : Boolean;
      Due, Now, Pages : Word;
      Reservation_Code : Arena.Outcome;
   begin
      Index := 0; Why := Q.Unavailable;
      for K in 1 .. Store.Length (Rows) loop
         Item := Store.Read (Rows, K);
         if Item.Key = Key then
            if Item.State = Mapped and then Backing_Live (Item) then
               Index := K; Why := Q.Ready;
            else
               Why := (if Item.State = Mapped then Q.Backing_Unavailable else Item.Problem);
               if Item.State = Mapped then
                  Item.State := Refused; Item.Problem := Why;
                  Store.Replace (Rows, K, Item);
               end if;
            end if;
            return;
         end if;
      end loop;
      if not Allow_Setup then Why := Q.Not_Ready; return; end if;
      if Next_Request = X.No_Request then return; end if;
      Now := CuBit.Monotonic.Milliseconds;
      if Setup_Deadline = Q.Wire.No_Finite_Deadline or else Word (Setup_Deadline) <= Now then
         Why := Q.Deadline_Expired; return;
      end if;
      Due := Word (Setup_Deadline);
      if Now <= Word'Last - Broker_Timeout_Milliseconds - 1 then
         Due := Word'Min (Due, Now + Broker_Timeout_Milliseconds);
      end if;
      -- Reserve the metadata row before acquiring any handle. Replace cannot
      -- allocate, so even failed-close ownership can always be retained.
      Item := (Key => Key, others => <>);
      Store.Append (Rows, Item, Index, Code);
      if Code /= Store.Stored then Index := 0; return; end if;
      Client.Acquire (Local, Service, Provider,
        (ID => Next_Request, Key => (X.Segment_Number (Key.Segment), X.Bus_Number (Key.Bus)), Mode => X.Read_Only), Due, Reply);
      Next_Request := (if Next_Request = X.Request_ID'Last then X.No_Request else Next_Request + 1);
      Client.Move (Local, Item.Owned, Item.Memory, Item.Info, Saved_Request, Accepted);
      if not Accepted or else Reply.Status /= Client.Acquired then
         Item.State := Refused;
         Item.Rejected_Cleanup := Reply.Status = Client.Cleanup_Pending;
         Item.Problem := (if Reply.Status = Client.Deadline_Passed then Q.Deadline_Expired else Q.Unavailable);
         Store.Replace (Rows, Index, Item); Why := Item.Problem; return;
      end if;
      Item.State := Retained;
      for K in 1 .. Store.Length (Rows) loop
         if K /= Index and then Store.Read (Rows, K).Info.Record_Number = Item.Info.Record_Number then
            Item.State := Refused; Store.Replace (Rows, Index, Item); return;
         end if;
      end loop;
      if Saved_Request.Key.Segment /= X.Segment_Number (Key.Segment)
        or else Saved_Request.Key.Bus /= X.Bus_Number (Key.Bus)
        or else not Backing_Live (Item) then
         Item.State := Refused; Item.Problem := Q.Backing_Unavailable;
         Store.Replace (Rows, Index, Item); Why := Item.Problem; return;
      elsif CuBit.Monotonic.Milliseconds >= Due then
         Item.State := Refused; Item.Problem := Q.Deadline_Expired;
         Store.Replace (Rows, Index, Item); Why := Item.Problem; return;
      end if;
      Arena.Reserve (Reservations, Pages_Per_Bus, Item.Ticket, Reservation_Code);
      if Reservation_Code /= Arena.Reserved then
         Item.State := Refused; Store.Replace (Rows, Index, Item); return;
      end if;
      Arena.Describe (Reservations, Item.Ticket, Item.Virtual, Pages, Accepted);
      if not Accepted or else Pages /= Pages_Per_Bus then
         Item.State := Refused; Store.Replace (Rows, Index, Item); return;
      end if;
      Arena.Claim (Reservations, Item.Ticket, Accepted);
      if not Accepted then
         Item.State := Refused; Store.Replace (Rows, Index, Item); return;
      end if;
      -- A failed syscall may have mapped a prefix. The full reservation and
      -- retained handle remain quarantined forever; no second map is attempted.
      if H.Devices.Map_Device (Item.Memory, Item.Info.First, Item.Virtual, Pages, True) then
         Item.State := Mapped; Item.Problem := Q.Ready; Why := Q.Ready;
      else
         Item.State := Refused; Item.Problem := Q.Backing_Unavailable; Why := Item.Problem;
      end if;
      Store.Replace (Rows, Index, Item);
   end Ensure;

   procedure Dispatch
     (Scope : Word; Item : Q.Packet; Result : out Q.Packet; Allow_Setup : Boolean := True)
   is
      Request : constant Q.Request := Q.Decode_Request (Item);
      Operation : Q.Operation := Q.Read;
      Index : Natural := 0;
      Code : Q.Outcome;
      Selected : Row;
      Region : P.Region;
      Planned : G.View;
      Value : Word := 0;
      Before, Now : B.Tick;
      Clock_OK, Completed : Boolean;
   begin
      if Item.Label = Q.Header_Operation then Operation := Q.Header;
      elsif Item.Label = Q.Describe_Operation then Operation := Q.Describe; end if;
      Result := Q.Error_Reply (Operation, Q.Malformed);
      if not Request.Valid then return;
      elsif Working then Result := Q.Error_Reply (Operation, Q.Reentered); return;
      elsif Scope = 0 or else Scope /= Owner_Scope then Result := Q.Error_Reply (Operation, Q.Denied); return;
      elsif Stage /= Serving then Result := Q.Error_Reply (Operation, Q.Not_Ready); return;
      end if;
      Working := True;
      if Request.Kind = Q.Header then
         Ensure (Request.Key, Request.Setup_Deadline, Allow_Setup, Index, Code);
         Result := (if Code = Q.Ready then Q.Header_Result (Life, Word (Store.Read (Rows, Index).Info.Record_Number))
                    else Q.Error_Reply (Q.Header, Code));
      else
         if Request.Epoch /= Life then Result := Q.Error_Reply (Operation, Q.Stale);
         else
            for K in 1 .. Store.Length (Rows) loop
               Selected := Store.Read (Rows, K);
               if Selected.State = Mapped and then Word (Selected.Info.Record_Number) = Request.ID then Index := K; exit; end if;
            end loop;
            if Index = 0 then Result := Q.Error_Reply (Operation, Q.Denied);
            elsif not Backing_Live (Selected) then
               Selected.State := Refused; Selected.Problem := Q.Backing_Unavailable;
               Store.Replace (Rows, Index, Selected);
               Result := Q.Error_Reply (Operation, Q.Backing_Unavailable);
            elsif Request.Kind = Q.Describe then Result := Q.Describe_Result (Selected.Info.First, X.Bus_Bytes);
            else
               Region := Request.Region;
               Region.Device.Segment := Selected.Key.Segment; Region.Device.Bus := Selected.Key.Bus;
               Planned := G.Plan_Config ((Selected.Key.Segment, Selected.Key.Bus, Selected.Info.First, X.Bus_Bytes),
                 Region, Request.Offset, Request.Width, Request.For_Write);
               if Planned.Status /= G.Read_Ready then Result := Q.Error_Reply (Q.Read, Q.Denied);
               else
                  ACPI_Backend_Kernel_Handles.Read_Clock (Before, Clock_OK);
                  if not Clock_OK then Result := Q.Error_Reply (Q.Read, Q.Clock_Unavailable);
                  elsif Request.Deadline in 0 | Q.No_Finite_Deadline or else Word (Before) >= Word (Request.Deadline) then
                     Result := Q.Error_Reply (Q.Read, Q.Deadline_Expired);
                  else
                     ACPI_Backend_Kernel_Handles.Read_Clock (Now, Clock_OK);
                     if not Clock_OK then Result := Q.Error_Reply (Q.Read, Q.Clock_Unavailable);
                     elsif Now < Before then Result := Q.Error_Reply (Q.Read, Q.Clock_Reversed);
                     elsif Word (Now) >= Word (Request.Deadline) then Result := Q.Error_Reply (Q.Read, Q.Deadline_Expired);
                     else
                        ACPI_Backend_Memory_Access.Transfer (Selected.Virtual + Word (Planned.First), Request.Width, False, 0, Value, Completed);
                        if Completed and then Value <= (case Request.Width is when P.T.Byte_Access => 255,
                          when P.T.Word_Access => 65_535, when P.T.Dword_Access => 16#FFFF_FFFF#, when P.T.Qword_Access => 0)
                        then Result := Q.Read_Result (Value);
                        else
                           Selected.State := Refused; Selected.Problem := Q.Fault;
                           Store.Replace (Rows, Index, Selected);
                           Result := Q.Error_Reply (Q.Read, Q.Fault);
                        end if;
                     end if;
                  end if;
               end if;
            end if;
         end if;
      end if;
      Working := False;
   end Dispatch;
end ACPI_Native_PCI_Windows;

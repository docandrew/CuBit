pragma Ada_2022;
with System.Storage_Elements;
with ACPI_Fallible_Heap;
with CuBit.Monotonic;
with CuBit.Handles.IPC;
with CuBit.Grant_References;
with CuBit.Firmware_Data_Exchange;
package body ACPI_Native_HID_Offers is
   package M renames CuBit.Messages;
   package H renames CuBit.Handles;
   package IPC renames H.IPC;
   package GX renames CuBit.HID_Offer_Exchange;
   package P renames HID_Offer_Payload;
   package Grants renames CuBit.Memory_Grants;
   use type Interfaces.Unsigned_64, Interfaces.Unsigned_32, Interfaces.Unsigned_8, Interfaces.Unsigned_16;
   use type H.ABI.Kind, GX.Disposition, System.Address, System.Storage_Elements.Integer_Address;
   Page_Bytes : constant Interfaces.Unsigned_64 := Grants.PAGE_SIZE;
   Page_Count : constant Positive := 1;
   -- GETTIME continuity is process-wide across all offer sessions.
   Last_Clock : Interfaces.Unsigned_64 := 0;
   function Observe (Run : Session) return Report is (Run.View);
   function Quiet (Run : Session) return Boolean is
     (not Run.View.Working and then not Run.View.Grant_Pending and then not Run.View.Memory_Owned
      and then Run.View.State not in Completion_Unknown | Invalid_Reply);
   function New_Deadline (Run : in out Session) return Boolean is
      Now : constant Interfaces.Unsigned_64 := CuBit.Monotonic.Milliseconds;
   begin
      if Now<Last_Clock or else Now>=Run.Original_Deadline
        or else Run.Original_Deadline=Interfaces.Unsigned_64'Last then
         return False;
      end if;
      Last_Clock := Now;
      Run.View.Deadline_Milliseconds := Now+Interfaces.Unsigned_64'Min(GX.Offer_Budget_Milliseconds,Run.Original_Deadline-Now);
      return True;
   end New_Deadline;
   procedure Retire_Page (Run : in out Session) is
      Accepted : Boolean;
   begin
      -- Keep the same immutable page live for the one explicit confirmation.
      if Run.View.State in Completion_Unknown | Invalid_Reply and then not Run.View.Confirmation_Attempted then return; end if;
      if Run.View.Grant_Pending then
         Grants.Revoke(Run.Reference,Accepted);
         Run.View.Revoke_Accepted := Run.View.Revoke_Accepted or Accepted;
         if not Grants.Retirement_Confirmed(Run.Reference) then return; end if;
         Run.View.Grant_Pending := False;
      end if;
      if Run.View.Memory_Owned then
         ACPI_Fallible_Heap.Release(Run.Buffer_Address);
         Run.Buffer_Address := System.Null_Address; Run.View.Memory_Owned := False;
      end if;
   end Retire_Page;
   procedure Advance_Cleanup (Run : in out Session) is
   begin
      if Run.View.Working then return; end if;
      Run.View.Working := True; Retire_Page (Run); Run.View.Working := False;
   end Advance_Cleanup;
   procedure Abandon_Confirmation (Run : in out Session) is
   begin
      if Run.View.Working then return; end if;
      Run.View.Working:=True;
      if Run.View.State in Completion_Unknown | Invalid_Reply then Run.View.Confirmation_Attempted:=True; end if;
      Retire_Page (Run);
      Run.View.Working:=False;
   end Abandon_Confirmation;
   procedure Exchange (Run : in out Session) is
      Message : M.Message := Run.Original_Message;
   begin
      Run.View.Dispatched := True; Run.View.State := Completion_Unknown;
      Run.View.Calls := Run.View.Calls+1;
      Message.tag := IPC.Call(Run.Saved_Target,Message,Run.View.Deadline_Milliseconds);
      if Message.tag.label>=M.Kernel_Reply_First then return; end if;
      if Message.tag.label/=GX.Offer_Request or else Message.tag.length/=GX.Reply_Words
        or else M.Flag_Byte(Message.tag)/=0 or else Message.tag.reserved/=0
        or else Message.words(GX.Reply_Request_Word)/=Run.View.Request
        or else not GX.Valid_Disposition(Message.words(GX.Disposition_Word))
      then Run.View.State := Invalid_Reply; return; end if;
      Run.View.Answer := GX.Disposition'Enum_Val(Message.words(GX.Disposition_Word));
      Run.View.Detail := Message.words(GX.Detail_Word);
      Run.View.Queue_Position := Message.words(GX.Queue_Position_Word);
      if (Run.View.Answer=GX.Queued and then (Run.View.Detail/=0 or else Run.View.Queue_Position=0))
        or else (Run.View.Answer/=GX.Queued and then Run.View.Queue_Position/=0)
        or else (Run.View.Answer not in GX.Queued | GX.Malformed and then Run.View.Detail/=0)
      then Run.View.State := Invalid_Reply; return; end if;
      Run.View.Reply_Known := True; Run.View.State := Reply_Confirmed;
   end Exchange;
   procedure Confirm (Run : in out Session; Result : out Report) is
   begin
      if Run.View.Working or else Run.View.Confirmation_Attempted or else
        Run.View.State not in Completion_Unknown | Invalid_Reply
      then Result := Run.View; return; end if;
      Run.View.Working := True; Run.View.Confirmation_Attempted := True;
      if Handoff_Quiet and then New_Deadline (Run) then Exchange (Run); end if;
      Retire_Page (Run);
      Run.View.Working := False; Result := Run.View;
   end Confirm;
   procedure Submit (Run : in out Session; Target : H.Endpoint; Data : P.Request;
      Request, Deadline : Interfaces.Unsigned_64; Result : out Report) is
      Created : Boolean;
      Now : Interfaces.Unsigned_64;
      procedure Attempt is
      begin
         if not Handoff_Quiet then Run.View.State := Boundary_Refused; return; end if;
         if Request=0 or else not P.Valid(Data)
           or else P.Encoded_Length(Data)>Natural(Page_Bytes) or else Data.Epoch=0
           or else not M.Is_Process(M.Own_Process) or else Data.Epoch/=M.To_Word(M.Own_Process)
         then Run.View.State := Payload_Refused; return; end if;
         declare D : constant IPC.Description := IPC.Describe(Target); begin
            if not D.Current or else D.Kind/=H.ABI.Endpoint or else not D.Held.Read
              or else D.Authority_Tag/=CuBit.Firmware_Data_Exchange.Authority_Tag
              or else not M.Is_Process(M.From_Word(D.Object))
            then Run.View.State := Endpoint_Refused; return; end if;
         end;
         Run.Saved_Target := Target; Run.Original_Deadline := Deadline;
         if not New_Deadline (Run) then Run.View.State := Clock_Refused; return; end if;
         Run.Buffer_Address := ACPI_Fallible_Heap.Reserve(Page_Bytes,Page_Bytes);
         Run.View.Memory_Owned := Run.Buffer_Address/=System.Null_Address;
         if not Run.View.Memory_Owned or else
           System.Storage_Elements.To_Integer(Run.Buffer_Address) mod System.Storage_Elements.Integer_Address(Page_Bytes)/=0
         then Run.View.State := Allocation_Refused; return; end if;
         declare
            Address : constant System.Address := Run.Buffer_Address;
            Length : constant Positive := P.Encoded_Length(Data);
            Bytes : P.Byte_Array(0 .. Natural(Page_Bytes)-1) with Import, Address => Address;
         begin
            Bytes := [others=>0]; P.Encode(Data,Bytes(0 .. Length-1));
            Grants.Create_Via_Capability(Target,Address,Page_Count,False,Run.Reference,Created);
            if not Created then Run.View.State := Grant_Refused; return; end if;
            Run.View.Grant_Pending := True;
            -- Allocation/grant creation count against the same finite call
            -- budget. No dispatch after expiry or loss of the quiet boundary.
            Now := CuBit.Monotonic.Milliseconds;
            if Now<Last_Clock or else Now>=Run.View.Deadline_Milliseconds or else not Handoff_Quiet then
               Run.View.State := Not_Dispatched; return;
            end if;
            Last_Clock := Now;
            Run.Original_Message.tag := (label=>GX.Offer_Request,length=>GX.Request_Words,
               protocol=>0,ipc=><>,reserved=>0);
            Run.Original_Message.words := [GX.Request_Word=>Request,
               GX.Reference_Word=>CuBit.Grant_References.Encode(Run.Reference),
               GX.Length_Word=>Interfaces.Unsigned_64(Length),others=>0];
            Exchange (Run);
         end;
      end Attempt;
   begin
      if Run.View.Attempted or else Run.View.Working then Result := Run.View; return; end if;
      Run.View.Attempted := True; Run.View.Working := True; Run.View.Request := Request;
      Attempt;
      Retire_Page (Run);
      Run.View.Working := False; Result := Run.View;
   end Submit;
end ACPI_Native_HID_Offers;

pragma Ada_2022;
with System;
with System.Storage_Elements;
with ACPI_Fallible_Heap;
with CuBit.Messages;
with CuBit.Monotonic;
with CuBit.Handles.IPC;
with CuBit.Grant_References;
with CuBit.Memory_Grants;
with CuBit.Firmware_Data_Exchange;
package body ACPI_Native_GPIO_Admission is
   package M renames CuBit.Messages;
   package H renames CuBit.Handles;
   package IPC renames H.IPC;
   package GX renames CuBit.GPIO_Bootstrap_Exchange;
   package P renames GPIO_Bootstrap_Payload;
   package Grants renames CuBit.Memory_Grants;
   use type Interfaces.Unsigned_64, Interfaces.Unsigned_32, Interfaces.Unsigned_8, Interfaces.Unsigned_16;
   use type H.ABI.Kind, GX.Disposition, System.Address, System.Storage_Elements.Integer_Address;
   Page_Bytes : constant Interfaces.Unsigned_64 := Grants.PAGE_SIZE;
   Page_Count : constant Positive := 1;
   View : Report;
   Buffer_Address : System.Address := System.Null_Address;
   Reference : Grants.Grant_Reference;
   Saved_Target : H.Endpoint := H.No_Endpoint;
   Original_Message : M.Message := M.NULL_MESSAGE;
   Expected_Spans, Last_Clock : Interfaces.Unsigned_64 := 0;
   function Observe return Report is (View);
   function Quiet return Boolean is
     (not View.Working and then not View.Grant_Pending and then not View.Memory_Owned
      and then View.State not in Completion_Unknown | Invalid_Reply);
   function New_Deadline return Boolean is
      Now : constant Interfaces.Unsigned_64 := CuBit.Monotonic.Milliseconds;
   begin
      if Now<Last_Clock or else Now>=Interfaces.Unsigned_64'Last-GX.Admission_Budget_Milliseconds then
         return False;
      end if;
      Last_Clock := Now;
      View.Deadline_Milliseconds := Now+GX.Admission_Budget_Milliseconds;
      return True;
   end New_Deadline;
   procedure Retire_Page is
      Accepted : Boolean;
   begin
      -- Keep the same immutable page live for the one explicit confirmation.
      if View.State in Completion_Unknown | Invalid_Reply and then not View.Confirmation_Attempted then return; end if;
      if View.Grant_Pending then
         Grants.Revoke(Reference,Accepted);
         View.Revoke_Accepted := View.Revoke_Accepted or Accepted;
         if not Grants.Retirement_Confirmed(Reference) then return; end if;
         View.Grant_Pending := False;
      end if;
      if View.Memory_Owned then
         ACPI_Fallible_Heap.Release(Buffer_Address);
         Buffer_Address := System.Null_Address; View.Memory_Owned := False;
      end if;
   end Retire_Page;
   procedure Advance_Cleanup is
   begin
      if View.Working then return; end if;
      View.Working := True; Retire_Page; View.Working := False;
   end Advance_Cleanup;
   procedure Abandon_Confirmation is
   begin
      if View.Working then return; end if;
      View.Working:=True;
      if View.State in Completion_Unknown | Invalid_Reply then View.Confirmation_Attempted:=True; end if;
      Retire_Page;
      View.Working:=False;
   end Abandon_Confirmation;
   procedure Exchange is
      Message : M.Message := Original_Message;
   begin
      View.Dispatched := True; View.State := Completion_Unknown;
      View.Calls := View.Calls+1;
      Message.tag := IPC.Call(Saved_Target,Message,View.Deadline_Milliseconds);
      if Message.tag.label>=M.Kernel_Reply_First then return; end if;
      if Message.tag.label/=GX.Admission_Request or else Message.tag.length/=GX.Reply_Words
        or else M.Flag_Byte(Message.tag)/=0 or else Message.tag.reserved/=0
        or else Message.words(GX.Reply_Request_Word)/=View.Request
        or else not GX.Valid_Disposition(Message.words(GX.Disposition_Word))
      then View.State := Invalid_Reply; return; end if;
      View.Answer := GX.Disposition'Enum_Val(Message.words(GX.Disposition_Word));
      View.Detail := Message.words(GX.Detail_Word);
      View.Members := Message.words(GX.Members_Word);
      if View.Answer=GX.Admitted and then
        (View.Detail/=Expected_Spans or else View.Members<View.Detail)
      then View.State := Invalid_Reply; return; end if;
      View.Reply_Known := True; View.State := Reply_Confirmed;
   end Exchange;
   procedure Confirm (Result : out Report) is
   begin
      if View.Working or else View.Confirmation_Attempted or else
        View.State not in Completion_Unknown | Invalid_Reply
      then Result := View; return; end if;
      View.Working := True; View.Confirmation_Attempted := True;
      if Startup_Quiet and then New_Deadline then Exchange; end if;
      Retire_Page;
      View.Working := False; Result := View;
   end Confirm;
   procedure Submit (Target : H.Endpoint; Data : P.Request;
      Request : Interfaces.Unsigned_64; Result : out Report) is
      Created : Boolean;
      Now : Interfaces.Unsigned_64;
      procedure Attempt is
      begin
         if not Startup_Quiet then View.State := Boundary_Refused; return; end if;
         if Request=0 or else not P.Valid(Data) or else Data.Epoch=0
           or else not M.Is_Process(M.Own_Process) or else Data.Epoch/=M.To_Word(M.Own_Process)
         then View.State := Payload_Refused; return; end if;
         declare D : constant IPC.Description := IPC.Describe(Target); begin
            if not D.Current or else D.Kind/=H.ABI.Endpoint or else not D.Held.Read
              or else D.Authority_Tag/=CuBit.Firmware_Data_Exchange.Authority_Tag
              or else not M.Is_Process(M.From_Word(D.Object))
            then View.State := Endpoint_Refused; return; end if;
         end;
         Saved_Target := Target; Expected_Spans := Interfaces.Unsigned_64(Data.Spans);
         if not New_Deadline then View.State := Clock_Refused; return; end if;
         Buffer_Address := ACPI_Fallible_Heap.Reserve(Page_Bytes,Page_Bytes);
         View.Memory_Owned := Buffer_Address/=System.Null_Address;
         if not View.Memory_Owned or else
           System.Storage_Elements.To_Integer(Buffer_Address) mod System.Storage_Elements.Integer_Address(Page_Bytes)/=0
         then View.State := Allocation_Refused; return; end if;
         declare
            Address : constant System.Address := Buffer_Address;
            Length : constant Positive := P.Encoded_Length(Data.Spans);
            Bytes : P.Byte_Array(0 .. Natural(Page_Bytes)-1) with Import, Address => Address;
         begin
            Bytes := [others=>0]; P.Encode(Data,Bytes(0 .. Length-1));
            Grants.Create_Via_Capability(Target,Address,Page_Count,False,Reference,Created);
            if not Created then View.State := Grant_Refused; return; end if;
            View.Grant_Pending := True;
            -- Allocation/grant creation count against the same finite call
            -- budget. No dispatch after expiry or loss of the quiet boundary.
            Now := CuBit.Monotonic.Milliseconds;
            if Now<Last_Clock or else Now>=View.Deadline_Milliseconds or else not Startup_Quiet then
               View.State := Not_Dispatched; return;
            end if;
            Last_Clock := Now;
            Original_Message.tag := (label=>GX.Admission_Request,length=>GX.Request_Words,
               protocol=>0,ipc=><>,reserved=>0);
            Original_Message.words := [GX.Request_Word=>Request,
               GX.Reference_Word=>CuBit.Grant_References.Encode(Reference),
               GX.Length_Word=>Interfaces.Unsigned_64(Length),others=>0];
            Exchange;
         end;
      end Attempt;
   begin
      if View.Attempted or else View.Working then Result := View; return; end if;
      View.Attempted := True; View.Working := True; View.Request := Request;
      Attempt;
      Retire_Page;
      View.Working := False; Result := View;
   end Submit;
end ACPI_Native_GPIO_Admission;

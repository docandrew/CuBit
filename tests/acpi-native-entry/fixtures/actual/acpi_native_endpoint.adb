with ACPI_Observer_Endpoint;
with ACPI_Native_Power_Preparation;
with ACPI_Native_Power_Entry;
with ACPI_Native_Power_Buttons;
with ACPI_Native_Event_Domain;
with ACPI_Endpoint;
with ACPI_Protocol;
with CuBit.Refusal_Notes;
package body ACPI_Native_Endpoint is
   use type Interfaces.Unsigned_64,Interfaces.Unsigned_32,Interfaces.Unsigned_16,Interfaces.Unsigned_8;
   use type ACPI_Serving_State.Mode;
   Serving : ACPI_Serving_State.Session;
   Current_Reply_Now : Boolean := True;
   function Reply_Now return Boolean is (Current_Reply_Now);
   procedure Select_Runtime(Runtime_Admitted:Boolean;Accepted:out Boolean) is
   begin ACPI_Serving_State.Select_Runtime(Serving,Runtime_Admitted,Accepted);end;
   procedure Select_Observer(Facts:ACPI_Serving_State.Eligibility;
      Physical_Retirement_Confirmed:Boolean;Accepted:out Boolean) is
   begin ACPI_Serving_State.Select_Observer(Serving,Facts,Physical_Retirement_Confirmed,Accepted);
      if Accepted then ACPI_Native_Power_Buttons.Begin_Stop;ACPI_Native_Power_Entry.Begin_Stop;end if;end;
   function Serving_State return ACPI_Serving_State.Snapshot is(ACPI_Serving_State.Observe(Serving));
   procedure Dispatch
     (Server : in out ACPI_Requests.State;
      From : CuBit.Messages.Process_ID;
      Request : CuBit.Messages.Message; Reply : out CuBit.Messages.Message) is
      Result : ACPI_Requests.Packet;
      Power_Handled : Boolean;
      Stamp : constant ACPI_Protocol.Role_Tag :=
        ACPI_Protocol.Role_Tag (Request.authorityTag);
   begin
      Current_Reply_Now := True;
      ACPI_Native_Power_Buttons.Dispatch (Request,
        ACPI_Native_Event_Domain.Diagnostics_Quiet (Server), Reply, Power_Handled, Current_Reply_Now);
      if Power_Handled then return;end if;
      declare Mode:constant ACPI_Serving_State.Snapshot:=Serving_State;begin
         ACPI_Native_Power_Entry.Dispatch (Server,Request,
           Mode.Selected and then Mode.Current=ACPI_Serving_State.Local_Runtime,
           Reply,Power_Handled,Current_Reply_Now);
         if Power_Handled then
            if ACPI_Native_Power_Entry.Blocks_Runtime then ACPI_Native_Power_Buttons.Begin_Stop;end if;
            return;
         end if;
         ACPI_Native_Power_Preparation.Dispatch (Server,Request,
           Mode.Selected and then Mode.Current=ACPI_Serving_State.Local_Runtime
             and then not ACPI_Native_Power_Entry.Blocks_Runtime,Reply,Power_Handled);
      end;
      if Power_Handled then return;end if;
      if not ACPI_Protocol.Is_Observer (Stamp)
        and then not ACPI_Native_Power_Entry.Blocks_Runtime then
         CuBit.Refusal_Notes.Warn_Once
           ("acpi", From'Image & " observer",
            "process" & From'Image & " undeclared observer");
      end if;
      if ACPI_Protocol.Is_Observer(Stamp) and then Request.tag.label=ACPI_Requests.Read_Metrics
        and then Request.tag.length=4 and then CuBit.Messages.Flag_Byte(Request.tag)=0
        and then Request.tag.reserved=0 and then Request.words(0)=Metrics_Serving_State_Page
        and then Request.words(1)=0 and then Request.words(2)=0 and then Request.words(3)=0
      then
         declare S:constant ACPI_Serving_State.Snapshot:=Serving_State;begin
            Result:=ACPI_Endpoint.Encode((Status=>ACPI_Requests.OK,
              Data=>[ACPI_Requests.Revision(Server),
                Interfaces.Unsigned_64(ACPI_Serving_State.Mode'Enum_Rep(S.Current)),
                Interfaces.Unsigned_64(ACPI_Serving_State.Reason'Enum_Rep(S.Why)),
                Boolean'Pos(S.Selected and then S.Current=ACPI_Serving_State.Local_Runtime)]));
         end;
      else
      ACPI_Observer_Endpoint.Dispatch (Server, Stamp,
        (Label => Request.tag.label, Length => Request.tag.length,
         Flags => CuBit.Messages.Flag_Byte (Request.tag),
         Reserved => Request.tag.reserved,
         Data => ACPI_Requests.Words (Request.words)), Result);
      end if;
      Reply := CuBit.Messages.NULL_MESSAGE;
      Reply.tag := (label => Result.Label, length => Result.Length,
                    protocol => 0, ipc => <>, reserved => Result.Reserved);
      Reply.words := CuBit.Messages.MessageWords (Result.Data);
   end Dispatch;
end ACPI_Native_Endpoint;

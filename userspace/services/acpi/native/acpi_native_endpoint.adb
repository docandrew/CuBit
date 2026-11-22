with ACPI_Observer_Endpoint;
with ACPI_Native_Passive_Queries;
with ACPI_Native_Power_Preparation;
with ACPI_Native_Power_Entry;
with ACPI_Native_Power_Buttons;
with ACPI_Native_Power_Inventory;
with ACPI_Native_Battery_Reads;
with ACPI_Native_Power_Identities;
with ACPI_Native_Event_Domain;
with ACPI_Endpoint;
with ACPI_Protocol;
with CuBit.Refusal_Notes;
package body ACPI_Native_Endpoint is
   use type Interfaces.Unsigned_64,Interfaces.Unsigned_32,Interfaces.Unsigned_16,Interfaces.Unsigned_8;
   use type ACPI_Serving_State.Mode;
   pragma Compile_Time_Error (Metrics_Battery_Page <= ACPI_Requests.Metrics_Last_Page
     or Metrics_Battery_Page = Metrics_Serving_State_Page or Metrics_Battery_Page = Metrics_Lid_Page
     or Metrics_Battery_Page = Metrics_Source_Page, "battery observer page collides with existing metrics");
   Serving : ACPI_Serving_State.Session;
   Current_Reply_Now : Boolean := True;
   function Reply_Now return Boolean is (Current_Reply_Now);
   procedure Select_Runtime(Runtime_Admitted:Boolean;Accepted:out Boolean) is
   begin ACPI_Serving_State.Select_Runtime(Serving,Runtime_Admitted,Accepted);end;
   procedure Select_Observer(Facts:ACPI_Serving_State.Eligibility;
      Physical_Retirement_Confirmed:Boolean;Accepted:out Boolean) is
   begin ACPI_Serving_State.Select_Observer(Serving,Facts,Physical_Retirement_Confirmed,Accepted);
      if Accepted then ACPI_Native_Battery_Reads.Begin_Stop;ACPI_Native_Power_Inventory.Begin_Stop;ACPI_Native_Power_Buttons.Begin_Stop;ACPI_Native_Power_Entry.Begin_Stop;end if;end;
   function Serving_State return ACPI_Serving_State.Snapshot is(ACPI_Serving_State.Observe(Serving));
   procedure Dispatch_With_Admission
     (Server : in out ACPI_Requests.State;
      From : CuBit.Messages.Process_ID;
      Request : CuBit.Messages.Message; Reply : out CuBit.Messages.Message;
      Admission_Open : Boolean; Cached_Button_Watches_Open : Boolean := False;
      Cached_Shutdown_Open : Boolean := False) is
      Result : ACPI_Requests.Packet;
      Power_Handled : Boolean;
      Stamp : constant ACPI_Protocol.Role_Tag :=
        ACPI_Protocol.Role_Tag (Request.authorityTag);
   begin
      Current_Reply_Now := True;
      declare Mode : constant ACPI_Serving_State.Snapshot := Serving_State; begin
         ACPI_Native_Power_Identities.Dispatch(Server,Request,
           Admission_Open and then Mode.Selected and then Mode.Current=ACPI_Serving_State.Local_Runtime
             and then not ACPI_Native_Power_Entry.Blocks_Runtime,Reply,Power_Handled);
         if Power_Handled then return;end if;
         ACPI_Native_Battery_Reads.Dispatch (Server, Request,
           Admission_Open and then Mode.Selected and then Mode.Current = ACPI_Serving_State.Local_Runtime
             and then not ACPI_Native_Power_Entry.Blocks_Runtime, Reply, Power_Handled, Current_Reply_Now);
         if Power_Handled then return; end if;
         ACPI_Native_Cached_Power.Dispatch (Server, Request,
           Admission_Open and then Mode.Selected and then Mode.Current = ACPI_Serving_State.Local_Runtime
             and then not ACPI_Native_Power_Entry.Blocks_Runtime, Reply, Power_Handled);
         if Power_Handled then return; end if;
      end;
      ACPI_Native_Power_Inventory.Dispatch (From,Request,
        Admission_Open and then Serving_State.Selected
          and then Serving_State.Current=ACPI_Serving_State.Local_Runtime
          and then not ACPI_Native_Power_Entry.Blocks_Runtime
          and then ACPI_Native_Event_Domain.Diagnostics_Quiet(Server),
        Reply,Power_Handled,Current_Reply_Now);
      if Power_Handled then return;end if;
      ACPI_Native_Power_Buttons.Dispatch (Request,
        (Cached_Button_Watches_Open and then Serving_State.Selected
          and then Serving_State.Current = ACPI_Serving_State.Local_Runtime)
        or else (Admission_Open and then ACPI_Native_Event_Domain.Diagnostics_Quiet (Server)),
        Reply, Power_Handled, Current_Reply_Now);
      if Power_Handled then return;end if;
      declare Mode:constant ACPI_Serving_State.Snapshot:=Serving_State;begin
         ACPI_Native_Power_Entry.Dispatch (Server,Request,
           Admission_Open and then Mode.Selected and then Mode.Current=ACPI_Serving_State.Local_Runtime,
           Reply,Power_Handled,Current_Reply_Now,
           Cached_Shutdown_Selected => Cached_Shutdown_Open and then Mode.Selected
             and then Mode.Current=ACPI_Serving_State.Local_Runtime);
         if Power_Handled then
            if ACPI_Native_Power_Entry.Blocks_Runtime then ACPI_Native_Battery_Reads.Begin_Stop;ACPI_Native_Power_Inventory.Begin_Stop;ACPI_Native_Power_Buttons.Begin_Stop;end if;
            return;
         end if;
         ACPI_Native_Power_Preparation.Dispatch (Server,Request,
           Admission_Open and then Mode.Selected and then Mode.Current=ACPI_Serving_State.Local_Runtime
             and then not ACPI_Native_Power_Entry.Blocks_Runtime,Reply,Power_Handled,
           Cached_Shutdown_Selected => Cached_Shutdown_Open and then Mode.Selected
             and then Mode.Current=ACPI_Serving_State.Local_Runtime and then not ACPI_Native_Power_Entry.Blocks_Runtime);
      end;
      if Power_Handled then return;end if;
      declare Mode:constant ACPI_Serving_State.Snapshot:=Serving_State;begin
         ACPI_Native_Passive_Queries.Dispatch(Server,From,Request,
           Admission_Open and then Mode.Selected and then not ACPI_Native_Power_Entry.Blocks_Runtime
             and then ACPI_Native_Event_Domain.Diagnostics_Quiet(Server),
           Reply,Power_Handled,Current_Reply_Now);
      end;
      if Power_Handled then return;end if;
      if not ACPI_Protocol.Is_Observer (Stamp)
        and then Admission_Open and then not ACPI_Native_Power_Entry.Blocks_Runtime then
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
   end Dispatch_With_Admission;
   procedure Dispatch
     (Server : in out ACPI_Requests.State; From : CuBit.Messages.Process_ID;
      Request : CuBit.Messages.Message; Reply : out CuBit.Messages.Message) is
   begin Dispatch_With_Admission (Server, From, Request, Reply, True); end Dispatch;
end ACPI_Native_Endpoint;

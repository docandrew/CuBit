pragma Ada_2022;
with Ada.Text_IO;
with Ada.Strings.Fixed;
with ACPI_Firmware_Data_Startup;
with ACPI_Capacity_Diagnostics;
with ACPI_Global_Lock_Protocol;
with Interfaces; use Interfaces;
with ACPI_Firmware_Data_Publications;
with ACPI_Firmware_Data_History;
with ACPI_Firmware_Data_Scopes;
with ACPI_Firmware_Data_Record_Access;
with ACPI_Firmware_Data_Dispatch;
with ACPI_Firmware_Data_Transport;
with ACPI_Firmware_Data_Field_Client;
with ACPI_Firmware_Data_Provider;
with AML_Field_Access;
with AML_Field_Layout;
with AML_Identity;
with ACPI_Firmware_Data_Identities;
with ACPI_Firmware_Data_Framing;
with ACPI_Firmware_Data_Replies;
with ACPI_Firmware_Data_Frames;
with ACPI_Firmware_Data_Client;
with ACPI_Firmware_Data_Arena;
with ACPI_Firmware_Data_Mapping;
with ACPI_Firmware_Data_Records;
with ACPI_Firmware_Data_Requests;
with ACPI_Firmware_Data_Layouts;
with ACPI_Target_Heap_Test_Control;
with Capability_ABI;
with Firmware_Data_ABI;
procedure Publication_Tests is
   package R renames ACPI_Firmware_Data_Requests;
   package L renames ACPI_Firmware_Data_Layouts;
   package W renames R.Wire;
   package IPC renames R.IPC;
   Scope : constant Unsigned_64 := 16#ABCD_0000_0000_0001#;
   Second_Scope : constant Unsigned_64 := 16#BCDE_0000_0000_0001#;
   Logical : constant L.Region := (16#3EA5_B418#, 16#2EC#);
   Checks : Natural := 0;
   procedure Check (OK : Boolean; Why : String) is
   begin Checks := Checks + 1; if not OK then raise Program_Error with Why; end if; end Check;
   type Mode is (Normal, Lost, Late_Grant, Refusal, Close_Failure, Attach_Failure,
      Partial_Map, Journal_OOM, Row_OOM, Arena_OOM, No_Allocation_After_IPC, Reentry);
   procedure Run_Case (Choice : Mode) is
      package A is new ACPI_Firmware_Data_Arena;
      Reservations : A.Context;
      Arena_Status : A.Outcome;
      type Channel_State is limited record
         Calls, Handles : Natural := 0;
         First : IPC.Packet;
         Fault : Boolean := True;
      end record;
      type Facts is array (Positive range 1 .. 16) of R.P.Envelope;
      Cap_Extents : Facts := [others => R.P.Empty_Envelope];
      Channel : Channel_State;
      Now : IPC.Time_Instant := 10;
      Maps, Inspections, Closes, Transfers : Natural := 0;
      Close_OK : Boolean := True;
      function Present (Held : Unsigned_64) return Boolean is (Held /= 0);
      procedure Inspect (Held : Unsigned_64; Evidence : out R.Grant_Evidence);
      procedure Clock (Time : out IPC.Time_Instant; Available : out Boolean);
      procedure Exchange (Transport : in out Channel_State; Deadline : IPC.Time_Instant;
         Request : IPC.Packet; Reply : out IPC.Packet; Received : out Unsigned_64; Delivered : out IPC.Delivery);
      procedure Dispose (Held : in out Unsigned_64; Closed : out Boolean);
      package C is new ACPI_Firmware_Data_Client
        (Channel_State, Unsigned_64, 0, Present, Clock, Exchange, Inspect, Dispose);
      procedure Map_Chunk (Held, Physical, Virtual, Pages : Unsigned_64; Completed : out Boolean);
      package M is new ACPI_Firmware_Data_Mapping (A, Reservations, Unsigned_64, Inspect, Map_Chunk, Clock, 2);
      Mapper : M.Context;
      procedure Describe (Item : A.Reservation; Base, Pages : out Unsigned_64; Accepted : out Boolean) is
      begin A.Describe (Reservations, Item, Base, Pages, Accepted); end Describe;
      procedure Map (Held : Unsigned_64; Item : A.Reservation; Physical, Virtual, Pages : Unsigned_64;
         Deadline : IPC.Time_Instant; Completed : out Boolean) is
         Result : M.Report;
         use type M.Outcome;
      begin M.Map_Exact (Mapper, Held, Item, Physical, Virtual, Pages, Deadline, Result); Completed := Result.Status = M.Mapped; end Map;
      Scope_Open, Transfer_Succeeds : Boolean := True;
      New_Admission_Open : Boolean := True;
      Model_Write_Success : Boolean := False;
      Enforce_Data_Scope : Boolean := False;
      procedure Check_Data_Scope (Requester : Unsigned_64; For_Write : Boolean; Accepted : out Boolean);
      function Usable (Requester : Unsigned_64; For_Write : Boolean) return Boolean is
         Accepted : Boolean;
      begin
         if not Scope_Open or else Requester not in Scope | Second_Scope then return False; end if;
         if not Enforce_Data_Scope then return True; end if;
         Check_Data_Scope (Requester,For_Write,Accepted); return Accepted;
      end Usable;
      Try_Wire_Reentry : Boolean := False;
      procedure Reenter_Dispatch;
      procedure Transfer (Virtual : Unsigned_64; Width : L.T.Access_Width; For_Write : Boolean;
         Input : Unsigned_64; Output : out Unsigned_64; Completed : out Boolean) is
         pragma Unreferenced (Virtual, Width, Input);
      begin if Try_Wire_Reentry then Reenter_Dispatch; end if;
         Transfers := Transfers + 1; Output := (if For_Write and then Model_Write_Success then 0 else 16#5A#); Completed := Transfer_Succeeds; end Transfer;
      package D is new ACPI_Firmware_Data_Records
        (Unsigned_64, 0, Present, Inspect, A.Reservation, A.No_Reservation, Describe, Map,
         Clock, Usable, Transfer);
      Rows : D.Context;
      Frontend_Admitted : Boolean := True;
      function Allowed (Requester : Unsigned_64) return Boolean is
         (Frontend_Admitted and then Requester in Scope | Second_Scope);
      package P is new ACPI_Firmware_Data_Publications
        (Channel_State, Unsigned_64, 0, C, A, Reservations, D, Rows, Allowed);
      use type P.Outcome, D.Record_ID, D.Outcome, W.Disposition, IPC.Packet, IPC.Time_Instant;
      Owner : P.Context;
      Reply : P.Reply;
      Ready_Row : D.Record_ID;
      package F renames ACPI_Firmware_Data_Frames;
      use type F.Identifier, F.Operation;
      procedure Execute (Who : F.Word; Descriptor : F.Identifier;
         For_Write : Boolean; Input : F.Word; Output : out F.Word; Status : out IPC.Outcome) is
         Attempt : D.Operation;
         Code : D.Outcome;
      begin
         Output := 0; Status := IPC.Denied;
         if Descriptor /= 1 then return; end if;
         D.Access_Record (Rows, Attempt, Who, Ready_Row, 0, L.T.Byte_Access,
            For_Write, Input, 40, Output, Code);
         case Code is
            when D.Done => Status := IPC.Done;
            when D.Transfer_Indeterminate | D.Late_Completed => Status := IPC.Indeterminate;
            when others => Status := IPC.Denied;
         end case;
      end Execute;
      package H is new ACPI_Firmware_Data_History (Execute);
      use type H.Outcome, IPC.Outcome;
      History : H.Context;
      Saved : H.Report;
      package Accesses is new ACPI_Firmware_Data_Record_Access (D,Rows);
      Unexpected_Lease_Calls : Natural := 0;
      procedure No_Lease (Caller,Domain,Sequence : F.Word; Accepted : out Boolean) is
         pragma Unreferenced (Caller,Domain,Sequence);
      begin Unexpected_Lease_Calls := Unexpected_Lease_Calls + 1; Accepted := False; end No_Lease;
      function Data_Authorized (Caller : F.Word; For_Write : Boolean) return Boolean is
         pragma Unreferenced (For_Write);
      begin return Allowed (Caller) and then New_Admission_Open; end Data_Authorized;
      package S is new ACPI_Firmware_Data_Scopes
        (Clock,Data_Authorized,No_Lease,Accesses.Check_Selection,Accesses.Transfer_Record);
      Data_Scopes : S.Context;
      procedure Check_Data_Scope (Requester : Unsigned_64; For_Write : Boolean; Accepted : out Boolean) is
      begin S.Check_Active (Data_Scopes,Requester,For_Write,Accepted); end Check_Data_Scope;
      procedure Scoped_Execute (Caller : F.Word; Descriptor : F.Identifier;
         For_Write : Boolean; Input : F.Word; Output : out F.Word; Status : out IPC.Outcome) is
      begin S.Perform (Data_Scopes,Caller,Descriptor,For_Write,Input,Output,Status); end Scoped_Execute;
      package Scoped_History is new ACPI_Firmware_Data_History (Scoped_Execute);
      use type S.Outcome;
      Joined_History : Scoped_History.Context;
      Data_Reply : S.Reply;
      Data_Result : Scoped_History.Report;
      Scope_ID, Descriptor_ID : F.Identifier;
      package Wire renames ACPI_Firmware_Data_Replies;
      use type Wire.Outcome, Wire.Effect_Kind;
   function Publication_Status (Value : P.Outcome) return Wire.Outcome is
     (case Value is
       when P.Record_Ready => Wire.Ready,
       when P.Pending => Wire.Pending,
       when P.Backend_Refused => Wire.Refused,
       when P.Publication_Stopped => Wire.Stopped,
       when P.Capacity_Refused => Wire.Capacity_Refused,
       when P.Invalid_Request | P.Invalid_Deadline => Wire.Invalid_Request,
       when P.Request_Changed => Wire.Request_Changed,
       when P.Not_Found => Wire.Not_Found,
       when P.Busy | P.Reentered => Wire.Busy,
       when P.Denied => Wire.Denied);
   function Scope_Status (Value : S.Outcome) return Wire.Outcome is
     (case Value is
       when S.Ready => Wire.Ready,
       when S.Closed => Wire.Closed,
       when S.Stopped => Wire.Stopped,
       when S.Not_Found => Wire.Not_Found,
       when S.Invalid_Request => Wire.Invalid_Request,
       when S.Request_Changed => Wire.Request_Changed,
       when S.Denied => Wire.Denied,
       when S.Clock_Unavailable => Wire.Clock_Unavailable,
       when S.Clock_Reversed => Wire.Clock_Reversed,
       when S.Expired => Wire.Expired,
       when S.Lease_Refused => Wire.Lease_Refused,
       when S.Selection_Refused => Wire.Selection_Refused,
       when S.Allocation_Refused => Wire.Capacity_Refused,
       when S.Reentered => Wire.Busy);
   procedure Save_History (Item : Scoped_History.Report; Reply : in out Wire.Reply) is
   begin
      Reply.Effect := Item.Effect;
      case Item.State is
         when Scoped_History.No_Evidence => Reply.Status := Wire.No_Evidence;
         when Scoped_History.Running => Reply.Status := Wire.Running;
         when Scoped_History.Identity_Conflict => Reply.Status := Wire.Request_Changed;
         when Scoped_History.Allocation_Refused => Reply.Status := Wire.Capacity_Refused;
         when Scoped_History.Invalid_Identity => Reply.Status := Wire.Invalid_Request;
         when Scoped_History.Reentered => Reply.Status := Wire.Busy;
         when Scoped_History.Finished =>
            case Item.Status is
               when IPC.Done => Reply.Status := Wire.Done;
               when IPC.Denied => Reply.Status := Wire.Denied;
               when IPC.Indeterminate => Reply.Status := Wire.Indeterminate;
               when IPC.Malformed | IPC.Backend_Fault => Reply.Status := Wire.Backend_Fault;
            end case;
      end case;
      if Reply.Status = Wire.Done and then Reply.Effect = Wire.Read_Effect then Reply.Value := Item.Value; end if;
   end Save_History;

      procedure Handle (Caller : Wire.Word; Item : F.Request;
         Status : out Wire.Outcome; Effect : out Wire.Effect_Kind; Value : out Wire.Word) is
         Reply : Wire.Reply;
         Published : P.Reply;
         Scoped : S.Reply;
         Saved : Scoped_History.Report;
      begin
         if not New_Admission_Open and then not F.Allowed_While_Quiescing (Item.Kind) then
            Status := Wire.Denied; Effect := Wire.No_Effect; Value := 0; return;
         end if;
            case Item.Kind is
               when F.Publish =>
                  P.Publish (Owner,Channel,Caller,
                     P.Publication_ID (Item.ID),Item.Region,Item.Publication_Deadline,Published);
                  Reply.Status := Publication_Status (Published.Status);
                  if Reply.Status = Wire.Ready then Reply.Value := Wire.Word (Published.ID); end if;
               when F.Describe_Publication =>
                  P.Describe (Owner,Caller,P.Publication_ID (Item.ID),Published);
                  Reply.Status := Publication_Status (Published.Status);
                  if Reply.Status = Wire.Ready then Reply.Value := Wire.Word (Published.ID); end if;
               when F.Recover_Publication =>
                  P.Recover (Owner,Channel,Caller,
                     P.Publication_ID (Item.ID),Item.Recovery_Deadline,Published);
                  Reply.Status := Publication_Status (Published.Status);
                  if Reply.Status = Wire.Ready then Reply.Value := Wire.Word (Published.ID); end if;
               when F.Begin_Scope =>
                  S.Begin_Scope (Data_Scopes,Caller,Item.ID,Item.Scope_Deadline,
                     Item.Lock_Domain,Item.Lock_Sequence,Scoped);
                  Reply.Status := Scope_Status (Scoped.Status);
                  if Reply.Status = Wire.Ready then Reply.Value := Wire.Word (Scoped.ID); end if;
               when F.Select_Datum =>
                  S.Select_Datum (Data_Scopes,Caller,Item.ID,Item.Client_Descriptor_ID,
                     Item.Record_ID,Item.Offset,Item.Width,Scoped);
                  Reply.Status := Scope_Status (Scoped.Status);
                  if Reply.Status = Wire.Ready then Reply.Value := Wire.Word (Scoped.ID); end if;
               when F.Close_Scope =>
                  S.Close_Scope (Data_Scopes,Caller,Item.ID,Scoped);
                  Reply.Status := Scope_Status (Scoped.Status);
               when F.Describe_Scope =>
                  Scoped := S.Describe_Scope (Data_Scopes,Caller,Item.ID);
                  Reply.Status := Scope_Status (Scoped.Status);
                  if Reply.Status = Wire.Ready then Reply.Value := Wire.Word (Scoped.ID); end if;
               when F.Read_Datum =>
                  Scoped_History.Submit (Joined_History,Caller,Item.ID,Item.Operation_ID,False,0,Saved);
                  Save_History (Saved,Reply);
               when F.Write_Datum =>
                  Scoped_History.Submit (Joined_History,Caller,Item.ID,Item.Write_Operation_ID,True,Item.Value,Saved);
                  Save_History (Saved,Reply);
               when F.Query_Operation =>
                  Saved := Scoped_History.Query (Joined_History,Caller,Item.ID,Item.Operation_ID);
                  Save_History (Saved,Reply);
            end case;
         Status := Reply.Status; Effect := Reply.Effect; Value := Reply.Value;
      end Handle;
      package Dispatch is new ACPI_Firmware_Data_Dispatch (Allowed,Handle);
      Dispatcher : Dispatch.Context;
      Wire_Scope, Wire_Descriptor : F.Identifier := 0;
      procedure Reenter_Dispatch is
         Request : constant F.Request := (F.Query_Operation,Wire_Descriptor,1);
         Response : IPC.Packet;
         Handled : Boolean;
         Decoded : Wire.Decoded;
      begin
         Dispatch.Dispatch (Dispatcher,Scope,F.Encode (Request),False,Response,Handled);
         Decoded := Wire.Decode (Response,Request,Wire.Read_Effect,False);
         Check (Handled and then Decoded.Valid and then Decoded.Item.Status = Wire.Busy,
           "dispatcher reentry returns correlated busy without history lookup");
      end Reenter_Dispatch;
      procedure Send (Request : F.Request; Expected : Wire.Effect_Kind;
         Answer : out Wire.Reply; Caller : Unsigned_64 := Scope) is
         Response : IPC.Packet;
         Handled : Boolean;
         Decoded : Wire.Decoded;
      begin
         Dispatch.Dispatch (Dispatcher,Caller,F.Encode (Request),False,Response,Handled);
         Decoded := Wire.Decode (Response,Request,Expected,False);
         Check (Handled and then Decoded.Valid,"typed dispatcher response");
         if not Decoded.Valid then raise Program_Error with "invalid typed reply"; end if;
         Answer := Decoded.Item;
      end Send;
      type Front_Channel is limited record Calls : Natural := 0; end record;
      Front_Link : Front_Channel;
      All_Front_Calls : Natural := 0;
      Lose_Next_Reply : Boolean := False;
      Lose_Kind : F.Operation := F.Read_Datum;
      Expected_Front_Deadline : IPC.Time_Instant := 90;
      Last_Front_Request : IPC.Packet;
      procedure Front_Exchange (Connection : in out Front_Channel; Deadline : IPC.Time_Instant;
         Request : IPC.Packet; Reply : out IPC.Packet; Received : out Unsigned_64; Delivered : out IPC.Delivery) is
         Handled : Boolean;
         Accepting : constant IPC.Packet := (Request with delta
            Flags => ACPI_Firmware_Data_Framing.Acceptance_Option);
      begin
         Connection.Calls := Connection.Calls + 1; All_Front_Calls := All_Front_Calls + 1;
         Check (Deadline = Expected_Front_Deadline, "frontend captured finite deadline");
         Last_Front_Request := Request;
         Received := 0;
         Dispatch.Dispatch (Dispatcher,Scope,ACPI_Firmware_Data_Framing.Normalize_Request (Accepting),False,Reply,Handled);
         Check (Handled, "frontend acceptance IPC option reaches protocol dispatcher");
         Delivered := IPC.Reply_Received;
         if Lose_Next_Reply and then Request.Label = F.Label_For (Lose_Kind) then
            Delivered := IPC.Completion_Unknown; Lose_Next_Reply := False;
         end if;
      end Front_Exchange;
      procedure Front_Dispose (Item : in out Unsigned_64; Closed : out Boolean) is
      begin Check (Item = 0, "backend protocol returns no capability"); Closed := True; Item := 0; end Front_Dispose;
      package Front is new ACPI_Firmware_Data_Transport
         (Front_Channel,Unsigned_64,0,Present,Clock,Front_Exchange,Front_Dispose);
      Front_State : Front.Context;
      package Client is new ACPI_Firmware_Data_Field_Client (Front_Channel,Front,Front_Link);
      use type Client.Outcome, Client.Cache_Kind, Client.Capacity_Problem;
      package AA renames AML_Field_Access;
      package AL renames AML_Field_Layout;
      use type AA.Completion, AA.Target, AA.Operation_ID, AA.Endpoint_ID, AA.Register_ID;
      use type AML_Identity.Identity, AL.Register_Geometry;
      type Original is record
         Boot, Definition, Address_Literal, Length_Literal : Boolean := True;
         Region : F.L.Region := Logical;
      end record;
      function Original_Matches (Item : Original; Geometry : AL.Register_Geometry) return Boolean is
         (Item.Boot and then Item.Definition and then Item.Address_Literal and then Item.Length_Literal
          and then Item.Region.Address = Geometry.Region.Base and then Item.Region.Length = Geometry.Region.Length);
      function Session_Pending (IO : Client.Context) return Boolean is
         (Client.Observe (IO).Failed or else Client.Observe (IO).Working
          or else (Client.Observe (IO).Depth = 0 and then Client.Observe (IO).Cleanup_Due));
      procedure Publish_Region (IO : in out Client.Context; Region : F.L.Region;
         Deadline : IPC.Time_Instant; Record_ID : out F.Identifier; Status : out AA.Completion) is
         R : Client.Result;
      begin
         Client.Publish (IO,Region,Deadline,R); Record_ID := R.ID;
         Status := (case R.Status is when Client.Ready => AA.Completed,
            when Client.Pending => AA.Indeterminate, when others => AA.Denied);
      end Publish_Region;
      procedure Begin_Data (IO : in out Client.Context; Requires_Lock : Boolean;
         Deadline : IPC.Time_Instant; Domain, Sequence : F.Word; Accepted : out Boolean) is
         R : Client.Result;
      begin Client.Begin_Field (IO,Requires_Lock,Deadline,Domain,Sequence,R); Accepted := R.Status = Client.Ready; end Begin_Data;
      Confirm_Bad_Read : Boolean := False;
      procedure Datum (IO : in out Client.Context; Record_ID : F.Identifier; Offset : F.Word;
         Width : F.T.Access_Width; For_Write : Boolean; Input : F.Word;
         Value : out F.Word; Status : out AA.Completion) is
         R : Client.Result;
      begin
         Client.Access_Datum (IO,Record_ID,Offset,Width,For_Write,Input,R); Value := R.Value;
         Status := (case R.Status is when Client.Completed => AA.Completed,
            when Client.Pending => AA.Indeterminate, when others => AA.Denied);
         if Confirm_Bad_Read and then Status = AA.Completed then Value := 16#1A5#; end if;
      end Datum;
      procedure End_Data (IO : in out Client.Context; Deadline : IPC.Time_Instant; Accepted : out Boolean) is
         R : Client.Result;
      begin Client.End_Field (IO,Deadline,R); Accepted := R.Status = Client.Closed; end End_Data;
      procedure Stop_Data (IO : in out Client.Context; Deadline : IPC.Time_Instant; Accepted : out Boolean) is
         R : Client.Result;
      begin Client.Cleanup (IO,Deadline,R); Accepted := R.Status = Client.Closed; end Stop_Data;
      procedure Publish_Window (Deadline : out IPC.Time_Instant) is
      begin Deadline := 120; Expected_Front_Deadline := Deadline; end Publish_Window;
      procedure Field_Context (Requires_Lock, For_Write : Boolean; Deadline : out IPC.Time_Instant;
         Domain, Sequence : out F.Word; Accepted : out Boolean) is
         pragma Unreferenced (For_Write);
      begin Deadline := 100; Expected_Front_Deadline := Deadline; Domain := 0; Sequence := 0; Accepted := not Requires_Lock; end Field_Context;
      procedure Cleanup_Window (Deadline : out IPC.Time_Instant) is
      begin Deadline := 130; Expected_Front_Deadline := Deadline; end Cleanup_Window;
      package Provider is new ACPI_Firmware_Data_Provider
         (Client.Context,Original,Original_Matches,Session_Pending,Publish_Region,Begin_Data,Datum,
          End_Data,Stop_Data,Publish_Window,Field_Context,Cleanup_Window,True);
      package Read_Only is new ACPI_Firmware_Data_Provider
         (Client.Context,Original,Original_Matches,Session_Pending,Publish_Region,Begin_Data,Datum,
          End_Data,Stop_Data,Publish_Window,Field_Context,Cleanup_Window,False);
      use type Provider.Capacity_Problem;
      use type Front.Outcome;
      procedure Call_Front (Request : F.Request; Expected : Wire.Effect_Kind; Reply : out Wire.Reply) is
         Attempt : Front.Attempt;
         Result : Front.Result;
      begin
         Front.Call (Front_State,Front_Link,Attempt,Request,Expected,L.T.Byte_Access,90,Result);
         Check (Result.Status = Front.Completed and then Result.Reply.Valid, "frontend typed completion");
         if Result.Reply.Valid then Reply := Result.Reply.Item; else raise Program_Error; end if;
      end Call_Front;
      procedure Clock (Time : out IPC.Time_Instant; Available : out Boolean) is
      begin Time := Now; Available := True; end Clock;
      procedure Inspect (Held : Unsigned_64; Evidence : out R.Grant_Evidence) is
      begin
         Inspections := Inspections + 1;
         Check (Held > 1000 and then Held <= 1016, "fresh carried handle identity");
         Evidence := (Fresh_Current_Handle => not (Choice = Attach_Failure and then Inspections = 2),
           Device_Memory => True, Tag => Firmware_Data_ABI.Firmware_Data_Tag,
           Extent => Cap_Extents (Positive (Held - 1000)), Readable => True, Writable => True,
           Grantable => False, Executable => False, Revocable => False, Cache => R.Uncached);
      end Inspect;
      procedure Exchange (Transport : in out Channel_State; Deadline : IPC.Time_Instant;
         Request : IPC.Packet; Reply : out IPC.Packet; Received : out Unsigned_64; Delivered : out IPC.Delivery) is
         Nonce : Unsigned_64 := Request.Data (W.Request_Word);
         Nested : P.Reply;
      begin
         Transport.Calls := Transport.Calls + 1;
         if Transport.Calls = 1 then Transport.First := Request;
         elsif Choice in Lost | Late_Grant | Close_Failure then
            Check (Request = Transport.First, "continuation uses exact devmgr request");
         end if;
         if Choice = Reentry then
            P.Publish (Owner, Transport, Scope, 99, Logical, Deadline, Nested);
            Check (Nested.Status = P.Reentered, "reentry cannot allocate or retarget");
         end if;
         Reply := (Label => W.Data_Window_Request, Length => W.Reply_Words, others => <>);
         Received := 0; Delivered := IPC.Reply_Received;
         if Transport.Fault and then Choice = Lost then Delivered := IPC.Completion_Unknown; return; end if;
         if Transport.Fault and then Choice = Refusal then
            Reply.Data := [W.Reply_Request_Word => Nonce,
              W.Disposition_Word => W.Disposition'Enum_Rep (W.Capacity), others => 0]; return;
         end if;
         Transport.Handles := Transport.Handles + 1;
         Received := Unsigned_64 (1000 + Transport.Handles);
         Cap_Extents (Transport.Handles) := (Request.Data (W.First_Page_Word), Request.Data (W.Page_Count_Word));
         if Transport.Fault and then Choice = Close_Failure then Nonce := Nonce + 1; end if;
         Reply.Data := [W.Reply_Request_Word => Nonce,
           W.Disposition_Word => W.Disposition'Enum_Rep (W.Granted_Data),
           W.Reply_First_Page_Word => Request.Data (W.First_Page_Word),
           W.Reply_Page_Count_Word => Request.Data (W.Page_Count_Word)];
         Reply.Flags := Capability_ABI.Flag_Byte (0, (Capability_Transferred => True, others => False));
         if Transport.Fault and then Choice = Late_Grant then Now := Deadline; end if;
         if Choice = No_Allocation_After_IPC then ACPI_Target_Heap_Test_Control.Successful_Allocation_Budget := 0; end if;
      end Exchange;
      procedure Dispose (Held : in out Unsigned_64; Closed : out Boolean) is
      begin
         Check (Held > 1000, "dispose owned handle"); Closes := Closes + 1; Closed := Close_OK;
         if Closed then Held := 0; end if;
      end Dispose;
      procedure Map_Chunk (Held, Physical, Virtual, Pages : Unsigned_64; Completed : out Boolean) is
      begin
         Check (Held > 1000 and then Physical = L.Envelope (Logical).First_Page * L.Page_Bytes
           and then Virtual >= 16#6000_0010_0000# and then Pages = 1, "publication maps exact reserved page");
         Maps := Maps + 1; Completed := Choice /= Partial_Map;
      end Map_Chunk;
   begin
      ACPI_Target_Heap_Test_Control.Successful_Allocation_Budget := Natural'Last;
      A.Configure (Reservations, 16#6000_0010_0000#, 16#6000_0020_0000#, Arena_Status);
      if Choice = Close_Failure then Close_OK := False; end if;
      P.Publish (Owner, Channel, 0, 1, Logical, 20, Reply);
      Check (Reply.Status = P.Denied and then Channel.Calls = 0, "unauthorized caller cannot reserve");
      if Choice = Journal_OOM then ACPI_Target_Heap_Test_Control.Successful_Allocation_Budget := 0;
      elsif Choice = Row_OOM then ACPI_Target_Heap_Test_Control.Successful_Allocation_Budget := 1;
      elsif Choice = Arena_OOM then ACPI_Target_Heap_Test_Control.Successful_Allocation_Budget := 2;
      end if;
      P.Publish (Owner, Channel, Scope, 1, Logical, 20, Reply);
      Check (Transfers = 0, "publication never executes Field data access");
      if Choice in Journal_OOM | Row_OOM | Arena_OOM then
         Check (Reply.Status = P.Capacity_Refused and then Channel.Calls = 0 and then Maps = 0,
           "all storage reserved before external grant request");
         ACPI_Target_Heap_Test_Control.Successful_Allocation_Budget := Natural'Last;
         if Choice = Journal_OOM then P.Publish (Owner, Channel, Scope, 1, Logical, 30, Reply);
         else P.Recover (Owner, Channel, Scope, 1, 30, Reply); end if;
      elsif Choice in Lost | Late_Grant | Close_Failure then
         Check (Reply.Status = P.Pending and then Reply.ID = D.No_Record and then Maps = 0, "uncertainty exposes no record");
         P.Describe (Owner, Scope, 1, Reply);
         Check (Reply.Status = P.Pending and then Channel.Calls = 1 and then Maps = 0, "history query is passive");
         P.Publish (Owner, Channel, Scope, 1, (Logical.Address + 1, Logical.Length), 30, Reply);
         Check (Reply.Status = P.Request_Changed and then Channel.Calls = 1, "same publication cannot change declaration");
         P.Publish (Owner, Channel, Scope, 2, Logical, 30, Reply);
         Check (Reply.Status = P.Busy and then Channel.Calls = 1, "another ID cannot replace uncertain request");
         P.Describe (Owner, Second_Scope, 1, Reply); Check (Reply.Status = P.Not_Found, "full requester namespace");
         Channel.Fault := False; Now := 21;
         if Choice = Close_Failure then
            P.Recover (Owner, Channel, Scope, 1, 30, Reply);
            Check (Reply.Status = P.Pending and then Channel.Calls = 1 and then P.Observe (Owner).Grant_Cleanup_Due,
              "cleanup failure blocks another request");
            Close_OK := True;
         end if;
         P.Recover (Owner, Channel, Scope, 1, 30, Reply);
      elsif Choice = Refusal then
         Check (Reply.Status = P.Backend_Refused and then Reply.Remote = W.Capacity and then Maps = 0, "typed no-grant refusal");
         Channel.Fault := False;
         P.Publish (Owner, Channel, Scope, 2, Logical, 30, Reply);
         Check (Reply.Status = P.Record_Ready and then Channel.Calls = 2 and then D.Observe (Rows).Records = 1
           and then A.Observe (Reservations).Reservations = 1, "new intent after definitive refusal reuses unmapped reservation");
      elsif Choice in Attach_Failure | Partial_Map then
         Check (Reply.Status = P.Publication_Stopped and then Reply.ID = D.No_Record, "failed attachment/map never publishes");
         Check (P.Observe (Owner).Retained_Unattached = (if Choice = Attach_Failure then 1 else 0), "unattached capability remains owned");
         P.Recover (Owner, Channel, Scope, 1, 30, Reply);
         Check (Reply.Status = P.Publication_Stopped and then Channel.Calls = 1, "stopped publication cannot replay");
         P.Publish (Owner, Channel, Scope, 2, Logical, 30, Reply);
         Check (Reply.Status = P.Publication_Stopped and then Channel.Calls = 1, "new ID cannot remap stopped row");
         Check (P.Observe (Owner).Retained_Unattached = (if Choice = Attach_Failure then 1 else 0),
           "journal growth preserves retained unattached capability");
         Check (not D.Selection_Valid (Rows,Scope,1,0,L.T.Byte_Access), "stopped record cannot select");
         return;
      end if;
      Check (Reply.Status = P.Record_Ready and then Reply.ID /= D.No_Record and then Maps = 1,
        "single joined publication completed " & Choice'Image & Reply.Status'Image);
      Ready_Row := Reply.ID;
      declare Before_Inspections : constant Natural := Inspections;
         Before_Transfers : constant Natural := Transfers;
         Before_Allocations : constant Natural := ACPI_Target_Heap_Test_Control.Attempts;
      begin
         Check (not D.Selection_Valid (Rows,Scope,D.No_Record,0,L.T.Byte_Access), "missing record refused");
         Check (not D.Selection_Valid (Rows,Scope,D.Record_ID'Last,0,L.T.Byte_Access), "record ID never narrows");
         Check (not D.Selection_Valid (Rows,Second_Scope,Ready_Row,0,L.T.Byte_Access), "selection full caller ownership");
         for Width in L.T.Access_Width loop
            for Offset in Unsigned_64 range 0 .. Logical.Length + 8 loop
               Check (D.Selection_Valid (Rows,Scope,Ready_Row,Offset,Width) =
                 (Offset < Logical.Length and then Unsigned_64 (L.T.Octets (Width)) <= Logical.Length - Offset
                  and then L.T.Aligned (Logical.Address + Offset,Width)), "exact logical metadata geometry");
            end loop;
         end loop;
         Check (Inspections = Before_Inspections and then Transfers = Before_Transfers
           and then ACPI_Target_Heap_Test_Control.Attempts = Before_Allocations, "selection has no callbacks/allocation/effect");
      end;
      ACPI_Target_Heap_Test_Control.Successful_Allocation_Budget := Natural'Last;
      declare Before : constant Natural := Channel.Calls; begin
         P.Publish (Owner, Channel, Scope, 3, Logical, 30, Reply);
         Check (Reply.Status = P.Record_Ready and then Reply.ID = Ready_Row and then Reply.Remote = W.Granted_Data
           and then Channel.Calls = Before and then Maps = 1, "published region reused without a second grant/map");
      end;
      declare Attempt : D.Operation; Value : Unsigned_64; Code : D.Outcome; begin
         D.Access_Record (Rows, Attempt, Scope, Ready_Row, 0, L.T.Byte_Access, False, 0, 40, Value, Code);
         Check (Code = D.Done and then Value = 16#5A# and then Transfers = 1, "published ID reaches separate scoped data owner");
      end;
      if Choice = Refusal then
         P.Describe (Owner, Scope, 1, Reply);
         Check (Reply.Status = P.Backend_Refused and then Reply.ID = D.No_Record, "old refusal identity remains terminal");
      end if;
      if Choice = Normal then
         for ID in P.Publication_ID range 4 .. 300 loop
            P.Publish (Owner, Channel, Scope, ID, Logical, 30, Reply);
            Check (Reply.Status = P.Record_Ready and then Reply.ID = Ready_Row, "publication history grows without remapping");
         end loop;
         Check (Channel.Calls = 1 and then Maps = 1 and then P.Observe (Owner).Publications = 299,
           "no fixed publication cap or duplicate grant");
         ACPI_Target_Heap_Test_Control.Successful_Allocation_Budget := 0;
         P.Describe (Owner, Scope, 1, Reply);
         Check (Reply.Status = P.Record_Ready and then Reply.ID = Ready_Row, "history query requires no allocation");
         ACPI_Target_Heap_Test_Control.Successful_Allocation_Budget := Natural'Last;
         P.Publish (Owner, Channel, Second_Scope, 1, Logical, 40, Reply);
         Check (Reply.Status = P.Record_Ready and then Reply.ID /= Ready_Row and then Channel.Calls = 2 and then Maps = 2,
           "same low caller bits and publication id cannot alias another requester");
         declare Attempt : D.Operation; Value : Unsigned_64; Code : D.Outcome; begin
            D.Access_Record (Rows, Attempt, Scope, Reply.ID, 0, L.T.Byte_Access, False, 0, 40, Value, Code);
            Check (Code = D.Wrong_Requester and then Value = 0 and then Transfers = 1,
              "published records stay scoped to the full requester identity");
         end;
         H.Submit (History, Scope, 1, 1, False, 0, Saved);
         Check (Saved.State = H.Finished and then Saved.Status = IPC.Done
           and then Saved.Value = 16#5A# and then Transfers = 2, "history dispatch reaches actual record once");
         H.Submit (History, Scope, 1, 1, False, 0, Saved);
         Check (Transfers = 2 and then Saved.Value = 16#5A#, "lost read reply recovers without second transfer");
         Transfer_Succeeds := False;
         H.Submit (History, Scope, 1, 2, True, 1, Saved);
         Check (Transfers = 3 and then Saved.Status = IPC.Indeterminate and then Saved.Value = 0,
           "uncertain write is retained after actual record transfer callback");
         Transfer_Succeeds := True;
         H.Submit (History, Scope, 1, 2, True, 1, Saved);
         Check (Transfers = 3 and then Saved.Status = IPC.Indeterminate, "unknown write never replays");
         H.Submit (History, Scope, 1, 2, True, 2, Saved);
         Check (Transfers = 3 and then Saved.State = H.Identity_Conflict, "changed write cannot execute");
         Scope_Open := False;
         H.Submit (History, Scope, 1, 3, False, 0, Saved);
         Check (Transfers = 3 and then Saved.Status = IPC.Denied and then Saved.Value = 0,
           "closed modeled scope refuses through record owner");
         Saved := H.Query (History, Scope, 1, 1);
         Check (Transfers = 3 and then Saved.Value = 16#5A#, "closed scope history remains passive");
         Scope_Open := True;
         H.Submit (History, Scope, 1, 3, False, 0, Saved);
         Check (Transfers = 3 and then Saved.Status = IPC.Denied, "scope reopen cannot replay rejected operation");
         H.Submit (History, Scope, 1, 4, False, 0, Saved);
         Check (Transfers = 4 and then Saved.Status = IPC.Done, "explicit later operation gets fresh admission");
         Now := 40;
         H.Submit (History, Scope, 1, 5, False, 0, Saved);
         Check (Transfers = 4 and then Saved.Status = IPC.Denied, "record deadline precedes transfer");
         Now := 41;
         H.Submit (History, Scope, 1, 5, False, 0, Saved);
         Check (Transfers = 4 and then Saved.Status = IPC.Denied, "expired identity remains terminal");
         H.Submit (History, Second_Scope, 1, 1, False, 0, Saved);
         Check (Transfers = 4 and then Saved.Status = IPC.Denied and then Saved.Value = 0,
           "full caller retained history cannot borrow another record");
         declare Empty_History : H.Context; begin
            ACPI_Target_Heap_Test_Control.Successful_Allocation_Budget := 0;
            H.Submit (Empty_History, Scope, 1, 1, False, 0, Saved);
            Check (Saved.State = H.Allocation_Refused and then Transfers = 4,
              "journal allocation refusal before actual data owner");
         end;

         ACPI_Target_Heap_Test_Control.Successful_Allocation_Budget := Natural'Last;
         Enforce_Data_Scope := True;
         S.Begin_Scope (Data_Scopes,Scope,1,70,0,0,Data_Reply);
         Check (Data_Reply.Status = S.Ready, "joined scope begin"); Scope_ID := Data_Reply.ID;
         S.Select_Datum (Data_Scopes,Scope,Scope_ID,1,F.Identifier (Ready_Row),0,L.T.Byte_Access,Data_Reply);
         Check (Data_Reply.Status = S.Ready, "real metadata adapter selects ready record"); Descriptor_ID := Data_Reply.ID;
         Scoped_History.Submit (Joined_History,Scope,Descriptor_ID,1,False,0,Data_Result);
         Check (Data_Result.Status = IPC.Done and then Data_Result.Value = 16#5A# and then Transfers = 5,
           "history scope adapter actual record and transfer joined");
         Scoped_History.Submit (Joined_History,Scope,Descriptor_ID,1,False,0,Data_Result);
         Check (Data_Result.Status = IPC.Done and then Transfers = 5, "joined retry cannot repeat transfer");
         S.Select_Datum (Data_Scopes,Scope,Scope_ID,1,F.Identifier (Ready_Row),1,L.T.Byte_Access,Data_Reply);
         Check (Data_Reply.Status = S.Request_Changed, "joined immutable descriptor");
         S.Close_Scope (Data_Scopes,Scope,Scope_ID,Data_Reply);
         Check (Data_Reply.Status = S.Closed, "joined scope close");
         Scoped_History.Submit (Joined_History,Scope,Descriptor_ID,2,False,0,Data_Result);
         Check (Data_Result.Status = IPC.Denied and then Data_Result.Value = 0 and then Transfers = 5,
           "joined closed scope reaches no record transfer");
         Data_Result := Scoped_History.Query (Joined_History,Scope,Descriptor_ID,1);
         Check (Data_Result.Status = IPC.Done and then Data_Result.Value = 16#5A# and then Transfers = 5,
           "joined passive cleanup history");
         Check (Unexpected_Lease_Calls = 0, "NoLock uses no physical lock callback");
         declare
            Answer : Wire.Reply;
            Request : F.Request;
            Response, Malformed : IPC.Packet;
            Handled : Boolean;
            Before_Calls : constant Natural := Channel.Calls;
         begin
            Request := (F.Publish,1,Logical,80);
            Send (Request,Wire.No_Effect,Answer);
            Check (Answer.Status = Wire.Ready and then Answer.Value = Unsigned_64 (Ready_Row), "wire publication reused");
            Send ((F.Describe_Publication,1),Wire.No_Effect,Answer);
            Check (Answer.Status = Wire.Ready, "wire publication passive history");
            Send ((F.Recover_Publication,1,80),Wire.No_Effect,Answer);
            Check (Answer.Status = Wire.Ready and then Channel.Calls = Before_Calls, "wire setup recovery no replay");
            Send ((F.Begin_Scope,2,80,0,0),Wire.No_Effect,Answer);
            Check (Answer.Status = Wire.Ready, "wire begin scope"); Wire_Scope := F.Identifier (Answer.Value);
            Send ((F.Describe_Scope,2),Wire.No_Effect,Answer);
            Check (Answer.Status = Wire.Ready and then Answer.Value = Unsigned_64 (Wire_Scope), "wire scope history");
            Send ((F.Select_Datum,Wire_Scope,F.Identifier (Ready_Row),2,0,L.T.Byte_Access),Wire.No_Effect,Answer);
            Check (Answer.Status = Wire.Ready, "wire descriptor from actual selection"); Wire_Descriptor := F.Identifier (Answer.Value);
            Try_Wire_Reentry := True;
            Send ((F.Read_Datum,Wire_Descriptor,1),Wire.Read_Effect,Answer);
            Try_Wire_Reentry := False;
            Check (Answer.Status = Wire.Done and then Answer.Value = 16#5A# and then Transfers = 6, "wire scoped read exactly once");
            Send ((F.Read_Datum,Wire_Descriptor,1),Wire.Read_Effect,Answer);
            Check (Answer.Status = Wire.Done and then Transfers = 6, "wire duplicate read history");
            Send ((F.Query_Operation,Wire_Descriptor,1),Wire.Read_Effect,Answer);
            Check (Answer.Status = Wire.Done and then Answer.Effect = Wire.Read_Effect
              and then Transfers = 6, "wire query carries original read kind");
            Send ((F.Write_Datum,Wire_Descriptor,2,1),Wire.Write_Effect,Answer);
            Check (Answer.Status = Wire.Indeterminate and then Answer.Value = 0 and then Transfers = 7,
              "wire uncertain write from modeled record transfer retained");
            Send ((F.Write_Datum,Wire_Descriptor,2,1),Wire.Write_Effect,Answer);
            Check (Answer.Status = Wire.Indeterminate and then Transfers = 7, "wire write retry no replay");
            Send ((F.Write_Datum,Wire_Descriptor,2,2),Wire.Write_Effect,Answer);
            Check (Answer.Status = Wire.Request_Changed and then Transfers = 7, "wire changed write refused");
            Send ((F.Close_Scope,Wire_Scope),Wire.No_Effect,Answer);
            Check (Answer.Status = Wire.Closed and then Answer.Value = 0, "wire close admission");
            Send ((F.Read_Datum,Wire_Descriptor,3),Wire.Read_Effect,Answer);
            Check (Answer.Status = Wire.Denied and then Answer.Value = 0 and then Transfers = 7, "wire late queued data refused");
            Send ((F.Describe_Scope,2),Wire.No_Effect,Answer);
            Check (Answer.Status = Wire.Closed and then Answer.Value = 0, "wire closed scope exposes no usable ID");
            Send ((F.Close_Scope,Wire_Scope),Wire.No_Effect,Answer,0);
            Check (Answer.Status = Wire.Denied, "wire zero caller denied");
            Send ((F.Close_Scope,Wire_Scope),Wire.No_Effect,Answer,77);
            Check (Answer.Status = Wire.Denied, "wire unauthorized full caller denied");
            Request := (F.Read_Datum,Wire_Descriptor,4);
            Malformed := F.Encode (Request); Malformed.Flags := 1;
            Dispatch.Dispatch (Dispatcher,Scope,Malformed,False,Response,Handled);
            Check (Handled and then Response.Length = 0 and then Transfers = 7, "malformed frame has no effect");
            Dispatch.Dispatch (Dispatcher,Scope,F.Encode (Request),True,Response,Handled);
            Check (Handled and then Response.Length = 0 and then Transfers = 7, "unexpected carried capability has no effect");
            Malformed := (Label => F.Reserved_Last_Label,others => <>);
            Dispatch.Dispatch (Dispatcher,Scope,Malformed,False,Response,Handled);
            Check (not Handled and then Response.Length = 0, "unused reserved operation is not dispatched");
            Check (Dispatch.Observe (Dispatcher).Reentered > 0 and then
              Dispatch.Observe (Dispatcher).Malformed = 2 and then not Dispatch.Observe (Dispatcher).Working,
              "bounded dispatcher counters and guard");
            Call_Front ((F.Begin_Scope,3,90,0,0),Wire.No_Effect,Answer);
            Check (Answer.Status = Wire.Ready, "frontend wire begin"); Wire_Scope := F.Identifier (Answer.Value);
            Call_Front ((F.Select_Datum,Wire_Scope,F.Identifier (Ready_Row),3,0,L.T.Byte_Access),Wire.No_Effect,Answer);
            Check (Answer.Status = Wire.Ready, "frontend wire selection"); Wire_Descriptor := F.Identifier (Answer.Value);
            declare Attempt : Front.Attempt; Result : Front.Result;
               Request : constant F.Request := (F.Read_Datum,Wire_Descriptor,1);
            begin
               Lose_Next_Reply := True;
               Front.Call (Front_State,Front_Link,Attempt,Request,Wire.Read_Effect,L.T.Byte_Access,90,Result);
               Check (Result.Status = Front.Completion_Unknown and then not Result.Reply.Valid
                 and then Transfers = 8 and then Front_Link.Calls = 3,
                 "frontend loses reply after one actual record transfer");
               Front.Call (Front_State,Front_Link,Attempt,Request,Wire.Read_Effect,L.T.Byte_Access,90,Result);
               Check (Result.Status = Front.Already_Attempted and then Front_Link.Calls = 3 and then Transfers = 8,
                 "frontend never re-dispatches original uncertain attempt");
            end;
            Call_Front ((F.Query_Operation,Wire_Descriptor,1),Wire.Read_Effect,Answer);
            Check (Answer.Status = Wire.Done and then Answer.Value = 16#5A# and then Transfers = 8
              and then Front_Link.Calls = 4, "explicit cleanup query is passive end to end");
            Call_Front ((F.Close_Scope,Wire_Scope),Wire.No_Effect,Answer);
            Check (Answer.Status = Wire.Closed and then Transfers = 8, "frontend closes data scope after unknown completion");
            Check (Unexpected_Lease_Calls = 0, "frontend NoLock path never creates a physical lease");
            declare
               IO : Client.Context;
               R : Client.Result;
               Row_ID : F.Identifier;
               Issued : F.Identifier;
               Good : Boolean;
               Before : Natural;
            begin
               -- Earlier fixture sections intentionally use manual protocol
               -- IDs below512. Reserve that test namespace before the sole
               -- production issuer begins lifecycle-client traffic.
               for I in 1 .. 512 loop ACPI_Firmware_Data_Identities.Issue (Issued,Good);
                  Check (Good and then Issued /= 0,"global client identity reservation"); end loop;
               Expected_Front_Deadline := 120;
               Client.Publish (IO,Logical,120,R); Check (R.Status = Client.Ready,"client publication"); Row_ID := R.ID;
               Before := Front_Link.Calls;
               Client.Publish (IO,Logical,120,R);
               Check (R.Status = Client.Ready and then R.ID = Row_ID and then Front_Link.Calls = Before,"client publication cache no RPC");
               Expected_Front_Deadline := 100;
               Client.Begin_Field (IO,False,100,0,0,R); Check (R.Status = Client.Ready,"client root begin");
               Client.Begin_Field (IO,False,200,0,0,R);
               Check (R.Status = Client.Ready and then Last_Front_Request.Data (1) = 100
                 and then Client.Observe (IO).Depth = 2,"nested deadline never extends parent");
               Client.Access_Datum (IO,Row_ID,0,L.T.Byte_Access,False,0,R);
               Check (R.Status = Client.Completed and then R.Value = 16#5A#,"client nested datum through record");
               Expected_Front_Deadline := 130;
               Client.End_Field (IO,130,R); Check (R.Status = Client.Closed and then Client.Observe (IO).Depth = 1,"client child unwind");
               Expected_Front_Deadline := 100;
               Client.Access_Datum (IO,Row_ID,0,L.T.Byte_Access,False,0,R); Check (R.Status = Client.Completed,"parent scope restored");
               Before := Front_Link.Calls;
               Client.Access_Datum (IO,Row_ID,0,L.T.Byte_Access,False,0,R);
               Check (R.Status = Client.Completed and then Front_Link.Calls = Before + 1,"descriptor cached only within same field");
               Before := Front_Link.Calls;
               Client.Access_Datum (IO,Row_ID,Logical.Length,L.T.Byte_Access,False,0,R);
               Check (R.Status = Client.Invalid_Request and then Front_Link.Calls = Before,"client logical padding refused locally");
               Client.Access_Datum (IO,Row_ID,0,L.T.Byte_Access,True,256,R);
               Check (R.Status = Client.Invalid_Request and then Front_Link.Calls = Before,"client width mask before RPC");
               Client.Access_Datum (IO,F.Identifier'Last,0,L.T.Byte_Access,False,0,R);
               Check (R.Status = Client.Invalid_Request and then Front_Link.Calls = Before,"foreign record not selected");
               Expected_Front_Deadline := 130;
               Client.End_Field (IO,130,R);
               Check (R.Status = Client.Closed and then Client.Observe (IO).Depth = 0
                 and then not Client.Observe (IO).Failed and then not Client.Observe (IO).Cleanup_Due,"client root closes cleanly");
               for I in 1 .. 301 loop
                  Expected_Front_Deadline := 100; Client.Begin_Field (IO,False,100,0,0,R);
                  Check (R.Status = Client.Ready,"growable client frame");
                  Client.Access_Datum (IO,Row_ID,0,L.T.Byte_Access,False,0,R);
                  Check (R.Status = Client.Completed,"growable descriptor");
                  Expected_Front_Deadline := 130; Client.End_Field (IO,130,R);
                  Check (R.Status = Client.Closed,"growable field close");
               end loop;
               Check (Client.Observe (IO).Frames > 300 and then Client.Observe (IO).Descriptors > 300,
                 "client metadata has no fixed sixteen or thirty-two cap");
            end;
            for Lost in F.Operation loop
               if Lost in F.Publish | F.Begin_Scope | F.Select_Datum | F.Read_Datum | F.Write_Datum | F.Close_Scope then
                  declare IO : Client.Context; R : Client.Result; Row_ID : F.Identifier := 0;
                     Before : constant Natural := Transfers;
                     Before_Retry : Natural;
                  begin
                     Lose_Kind := Lost; Lose_Next_Reply := True;
                     Expected_Front_Deadline := 120;
                     Client.Publish (IO,Logical,120,R);
                     if Lost /= F.Publish then
                        Check (R.Status = Client.Ready,"fault client publication"); Row_ID := R.ID;
                        Expected_Front_Deadline := 100;
                        Client.Begin_Field (IO,False,100,0,0,R);
                        if Lost not in F.Begin_Scope then
                           Check (R.Status = Client.Ready,"fault client scope");
                           if Lost /= F.Close_Scope then
                              Client.Access_Datum (IO,Row_ID,0,L.T.Byte_Access,Lost = F.Write_Datum,(if Lost = F.Write_Datum then 1 else 0),R);
                              Check (R.Status = Client.Pending,"lost selector/data stays pending");
                           end if;
                           Expected_Front_Deadline := 130;
                           Client.End_Field (IO,130,R);
                        end if;
                     end if;
                     Check (Client.Observe (IO).Failed and then Client.Observe (IO).Cleanup_Due,"uncertainty retains stopped client");
                     Before_Retry := Front_Link.Calls;
                     Client.Access_Datum (IO,Row_ID,0,L.T.Byte_Access,False,0,R);
                     Check (R.Status = Client.Failed and then Front_Link.Calls = Before_Retry,"failed client never replays datum");
                     Expected_Front_Deadline := 130;
                     if Lost in F.Begin_Scope | F.Read_Datum | F.Write_Datum then
                        Frontend_Admitted := False;
                        Client.Cleanup (IO,130,R);
                        Check (R.Status = Client.Pending and then Client.Observe (IO).Cleanup_Due,
                          "admission denial is not evidence of old scope or data completion");
                        Frontend_Admitted := True;
                     end if;
                     Client.Cleanup (IO,130,R);
                     Check (R.Status = Client.Closed and then R.Value = 0 and then Client.Observe (IO).Depth = 0
                       and then Client.Observe (IO).Failed and then not Client.Observe (IO).Cleanup_Due,"explicit cleanup closes without rearming");
                     Check (Transfers = Before + (if Lost in F.Read_Datum | F.Write_Datum then 1 else 0),"fault cleanup never adds a data effect");
                     Before_Retry := Front_Link.Calls;
                     Client.Begin_Field (IO,False,100,0,0,R);
                     Check (R.Status = Client.Failed and then Front_Link.Calls = Before_Retry,"cleanup cannot resume failed AML");
                  end;
               end if;
            end loop;
            declare IO : Client.Context; R : Client.Result; Row_ID : F.Identifier;
               Before : constant Natural := Transfers;
            begin
               Expected_Front_Deadline := 120; Client.Publish (IO,Logical,120,R); Row_ID := R.ID;
               Expected_Front_Deadline := 100; Client.Begin_Field (IO,False,100,0,0,R);
               Client.Access_Datum (IO,Row_ID,0,L.T.Byte_Access,True,1,R);
               Check (R.Status = Client.Pending and then R.Value = 0 and then Transfers = Before + 1,
                 "received indeterminate write is not a no-effect denial");
               Expected_Front_Deadline := 130; Client.End_Field (IO,130,R); Client.Cleanup (IO,130,R);
               Check (R.Status = Client.Closed and then Client.Observe (IO).Failed and then Transfers = Before + 1,
                 "terminal unknown write cleanup never replays or rearms");
            end;
            declare IO : Client.Context; R : Client.Result; Row_ID : F.Identifier;
               Before : Natural := Front_Link.Calls;
            begin
               ACPI_Target_Heap_Test_Control.Successful_Allocation_Budget := 0;
               Client.Publish (IO,Logical,120,R);
               Check (R.Status = Client.Allocation_Refused and then Front_Link.Calls = Before
                 and then Client.Observe_Capacity (IO).Cache = Client.Region_Cache and then Client.Observe_Capacity (IO).Problem = Client.Process_Heap_Limit and then Client.Observe_Capacity (IO).Demand = 1,"region metadata preallocated before IPC");
               ACPI_Target_Heap_Test_Control.Successful_Allocation_Budget := Natural'Last;
               Expected_Front_Deadline := 120; Client.Publish (IO,Logical,120,R); Row_ID := R.ID;
               Before := Front_Link.Calls; ACPI_Target_Heap_Test_Control.Successful_Allocation_Budget := 0;
               Client.Begin_Field (IO,False,100,0,0,R);
               Check (R.Status = Client.Allocation_Refused and then Front_Link.Calls = Before
                 and then Client.Observe_Capacity (IO).Cache = Client.Frame_Cache and then Client.Observe_Capacity (IO).Problem = Client.Process_Heap_Limit and then Client.Observe_Capacity (IO).Demand = 1,"frame metadata preallocated before IPC");
               ACPI_Target_Heap_Test_Control.Successful_Allocation_Budget := Natural'Last;
               Expected_Front_Deadline := 100; Client.Begin_Field (IO,False,100,0,0,R);
               Before := Front_Link.Calls; ACPI_Target_Heap_Test_Control.Successful_Allocation_Budget := 0;
               Client.Access_Datum (IO,Row_ID,0,L.T.Byte_Access,False,0,R);
               Check (R.Status = Client.Allocation_Refused and then Front_Link.Calls = Before
                 and then Client.Observe_Capacity (IO).Cache = Client.Descriptor_Cache and then Client.Observe_Capacity (IO).Problem = Client.Process_Heap_Limit and then Client.Observe_Capacity (IO).Demand = 1,"descriptor metadata preallocated before IPC");
               Expected_Front_Deadline := 130; Client.End_Field (IO,130,R);
               Check (R.Status = Client.Closed,"close is allocation-free after metadata failure");
               ACPI_Target_Heap_Test_Control.Successful_Allocation_Budget := Natural'Last;
            end;
            declare
               IO : Provider.Context;
               Other_IO : Read_Only.Context;
               Target, Other_Target, Forged, Exported : AA.Target := AA.No_Target;
               Geometry : AL.Register_Geometry :=
                  (Region => (Space => AL.Memory_Space,Base => Logical.Address,Length => Logical.Length,others => <>),
                   Byte_Offset => 0,Width => L.T.Byte_Access);
               Observed : AL.Register_Geometry;
               Declaration : Original;
               Good : Boolean;
               Reply : AA.Response;
               Before : Natural;
            begin
               Provider.Initialize (IO,17,Good); Check (Good,"provider identity initialized");
               Read_Only.Initialize (Other_IO,17,Good); Check (Good,"other context identity initialized");
               Before := Front_Link.Calls;
               Declaration.Boot := False;
               Provider.Resolve_Startup_Firmware_Target (IO,Declaration,Geometry,Target,Good);
               Check (not Good and then Target = AA.No_Target and then Front_Link.Calls = Before,"non-boot declaration never publishes");
               Declaration.Boot := True; Declaration.Address_Literal := False;
               Provider.Resolve_Startup_Firmware_Target (IO,Declaration,Geometry,Target,Good);
               Check (not Good and then Front_Link.Calls = Before,"name-derived base never publishes");
               Declaration.Address_Literal := True; Declaration.Length_Literal := False;
               Provider.Resolve_Startup_Firmware_Target (IO,Declaration,Geometry,Target,Good);
               Check (not Good and then Front_Link.Calls = Before,"nonliteral length never publishes");
               Declaration.Length_Literal := True; Declaration.Definition := False;
               Provider.Resolve_Startup_Firmware_Target (IO,Declaration,Geometry,Target,Good);
               Check (not Good and then Front_Link.Calls = Before,"missing definition never publishes");
               Declaration.Definition := True;
               Provider.Resolve_Startup_Firmware_Target (IO,Declaration,Geometry,Target,Good);
               Check (Good and then Target /= AA.No_Target and then Provider.Register_Count (IO) = 1,"original declaration target registered");
               Before := Front_Link.Calls; Declaration.Boot := False;
               Forged := Target;
               Provider.Resolve_Startup_Firmware_Target (IO,Declaration,Geometry,Forged,Good);
               Check (not Good and then Forged = AA.No_Target and then Front_Link.Calls = Before,"cached geometry cannot skip declaration predicate");
               Declaration.Boot := True;
               Provider.Describe (IO,Target,Observed,Good); Check (Good and then Observed = Geometry,"target metadata describes exact datum");
               Provider.Export_Target (IO,1,Exported,Good); Check (Good and then Exported = Target,"stable exported target");
               Provider.Dispatch (IO,(Target,False,0),Reply);
               Check (Reply.Status = AA.Denied and then Front_Link.Calls = Before,"no datum before Field begin");
               Read_Only.Resolve_Startup_Firmware_Target (Other_IO,Declaration,Geometry,Other_Target,Good);
               Check (Good and then Other_Target.Provider /= Target.Provider,"global provider identities differ");
               Provider.Authorize (IO,Other_Target,False,Good); Check (not Good,"other context target rejected");
               Forged := Target; Forged.Endpoint := Forged.Endpoint + 1;
               Provider.Authorize (IO,Forged,False,Good); Check (not Good,"wrong endpoint rejected");
               Forged := Target; Forged.Epoch := Forged.Epoch + 1;
               Provider.Authorize (IO,Forged,False,Good); Check (not Good,"wrong epoch rejected");
               Forged := Target; Forged.Register := AA.Register_ID'Last;
               Provider.Authorize (IO,Forged,False,Good); Check (not Good,"register ID not narrowed");
               Read_Only.Authorize (Other_IO,Other_Target,True,Good); Check (not Good,"explicit read-only policy");
               Before := Front_Link.Calls; Read_Only.Begin_Field (Other_IO,False,True,Good);
               Check (not Good and then Front_Link.Calls = Before,"write refusal has no scope effect");
               Provider.Begin_Field (IO,True,True,Good);
               Check (not Good and then Front_Link.Calls = Before,"locked write restriction preserved");
               Provider.Begin_Field (IO,False,False,Good); Check (Good,"provider whole Field begins");
               Before := Transfers;
               Provider.Dispatch (IO,(Target,False,0),Reply);
               Check (Reply.Status = AA.Completed and then Reply.Provider = Provider.Identity (IO)
                 and then Reply.Value = 16#5A# and then Reply.Operation = AA.No_Operation and then Transfers = Before + 1,
                 "AML response reaches existing scoped record");
               Provider.End_Field (IO,Good); Check (Good and then Provider.Ready (IO),"successful End keeps provider live");
               Provider.Begin_Field (IO,False,True,Good); Check (Good,"NoLock write admitted by explicit policy");
               Model_Write_Success := True;
               Provider.Dispatch (IO,(Target,True,1),Reply); Model_Write_Success := False;
               Check (Reply.Status = AA.Completed and then Reply.Value = 0,"known write response has no read data");
               Provider.End_Field (IO,Good); Check (Good,"write Field ends");
               for I in Unsigned_64 range 1 .. 301 loop
                  Geometry.Byte_Offset := I;
                  Provider.Resolve_Startup_Firmware_Target (IO,Declaration,Geometry,Forged,Good);
                  Check (Good,"provider metadata grows");
               end loop;
               Check (Provider.Register_Count (IO) = 302,"no fixed target catalogue cap");
               ACPI_Target_Heap_Test_Control.Successful_Allocation_Budget := 0;
               Provider.Describe (IO,Target,Observed,Good);
               Check (Good and then Observed.Byte_Offset = 0,"old target stable after growth without allocation");
               ACPI_Target_Heap_Test_Control.Successful_Allocation_Budget := Natural'Last;
            end;
            for Case_ID in 1 .. 3 loop
               declare IO : Provider.Context; Target : AA.Target := AA.No_Target;
                  Geometry : constant AL.Register_Geometry :=
                     (Region => (Space => AL.Memory_Space,Base => Logical.Address,Length => Logical.Length,others => <>),others => <>);
                  Good : Boolean; Reply : AA.Response; Captured : AA.Observation; Before : Natural;
               begin
                  Provider.Initialize (IO,17,Good); Check (Good,"fault provider init");
                  Provider.Resolve_Startup_Firmware_Target (IO,(others => <>),Geometry,Target,Good); Check (Good,"fault provider target");
                  Provider.Begin_Field (IO,False,False,Good); Check (Good,"fault provider begins"); Before := Transfers;
                  if Case_ID = 1 then Lose_Kind := F.Read_Datum; Lose_Next_Reply := True;
                  elsif Case_ID = 2 then Confirm_Bad_Read := True; end if;
                  Provider.Dispatch (IO,(Target,Case_ID = 3,(if Case_ID = 3 then 1 else 0)),Reply);
                  Confirm_Bad_Read := False;
                  if Case_ID = 2 then
                     Check (Reply.Status = AA.Failed_Completed and then not Provider.Uncertain (IO).Present
                       and then Provider.Completed_Fault (IO).Present,"completed unusable result is not an unknown receipt");
                  else
                     Captured := Provider.Uncertain (IO);
                     Check (Reply.Status = AA.Indeterminate and then Reply.Operation /= AA.No_Operation
                       and then Captured.Present and then Captured.Item.Destination = Target
                       and then Captured.Operation = Reply.Operation,"exact AML uncertainty observation retained");
                  end if;
                  Check (Reply.Value = 0 and then Transfers = Before + 1,"fault does not expose data");
                  Before := Front_Link.Calls; Provider.Dispatch (IO,(Target,False,0),Reply);
                  Check (Reply.Status = AA.Denied and then Front_Link.Calls = Before,"failed provider never replays");
                  Provider.End_Field (IO,Good); Check (not Good,"failed Field cleanup cannot report AML success");
                  Provider.Cleanup (IO,Good); Check (Good and then Provider.Pending (IO),"closed admission does not reconcile AML");
                  Captured := Provider.Uncertain (IO); Provider.Reconciled (IO,Captured,Good); Check (not Good,"history never resumes expired AML");
               end;
            end loop;
            declare IO : Provider.Context; Target : AA.Target := AA.No_Target;
               Geometry : constant AL.Register_Geometry :=
                  (Region => (Space => AL.Memory_Space,Base => Logical.Address,Length => Logical.Length,others => <>),others => <>);
               Good : Boolean; Before : constant Natural := Front_Link.Calls;
            begin
               Provider.Initialize (IO,17,Good); Check (Good,"OOM provider init");
               ACPI_Target_Heap_Test_Control.Successful_Allocation_Budget := 0;
               Provider.Resolve_Startup_Firmware_Target (IO,(others => <>),Geometry,Target,Good);
               Check (not Good and then Target = AA.No_Target and then Front_Link.Calls = Before
                 and then Provider.Observe_Capacity (IO).Problem = Provider.Process_Heap_Limit,
                 "AML target metadata reserved before publication");
               ACPI_Target_Heap_Test_Control.Successful_Allocation_Budget := Natural'Last;
            end;
            for Adapter_Case in 1 .. 14 loop
               declare
                  Connections : Natural := 0;
                  procedure Initialize_Link (Connection : in out Front_Channel; Target : Unsigned_64; Accepted : out Boolean) is
                  begin
                     Check (Target = 101 and then Connection.Calls = 0,"adapter binds original endpoint once");
                     Connections := Connections + 1; Accepted := True;
                  end Initialize_Link;
                  package Startup is new ACPI_Firmware_Data_Startup
                     (Unsigned_64,0,Front_Channel,Front,Initialize_Link,Clock,Original,Original_Matches);
                  Target : AA.Target := AA.No_Target;
                  Geometry : constant AL.Register_Geometry :=
                     (Region => (Space => AL.Memory_Space,Base => Logical.Address,Length => Logical.Length,others => <>),others => <>);
                  Declaration : Original;
                  Good, Is_Ready, Is_Pending : Boolean := False;
                  Observation : AA.Observation;
                  Capacity : ACPI_Capacity_Diagnostics.Report;
                  Response : AA.Response;
                  Before, Before_Transfers : Natural;
                  function Capacity_Cache return ACPI_Capacity_Diagnostics.Cache_Kind is
                  begin
                     Startup.Inspect (Is_Ready,Is_Pending,Observation,Capacity);
                     return Capacity.Cache;
                  end Capacity_Cache;
                  use type ACPI_Capacity_Diagnostics.Cache_Kind, ACPI_Capacity_Diagnostics.Entry_Count;
               begin
                  Before := All_Front_Calls;
                  Startup.Inspect (Is_Ready,Is_Pending,Observation,Capacity);
                  Check (not Is_Ready and then not Is_Pending,"fresh adapter passive");
                  if Adapter_Case = 12 then
                     Startup.Cleanup (Good); Check (Good,"fresh adapter close has no RPC");
                     Startup.Initialize (101,17,120,Good);
                     Check (not Good and then Connections = 0 and then All_Front_Calls = Before,"fresh close cannot rearm initialization");
                  elsif Adapter_Case in 8 | 13 then
                     Startup.Initialize ((if Adapter_Case = 8 then 0 else 101),17,
                        (if Adapter_Case = 13 then Now else 120),Good);
                     Check (not Good and then Connections = 0 and then All_Front_Calls = Before,"bad endpoint/expired initialization is effect free");
                     Startup.Initialize (101,17,120,Good); Check (not Good,"initialization refusal consumes lifetime");
                  else
                     Startup.Initialize (101,17,120,Good); Check (Good and then Connections = 1,"adapter initialized");
                     Startup.Initialize (102,18,130,Good); Check (not Good and then Connections = 1,"no endpoint retarget");
                     Declaration.Address_Literal := False;
                     Startup.Resolve (Declaration,Geometry,Target,Good);
                     Check (not Good and then All_Front_Calls = Before,"adapter cannot publish nonliteral source");
                     Declaration.Address_Literal := True; Expected_Front_Deadline := 120;
                     if Adapter_Case = 3 then ACPI_Target_Heap_Test_Control.Successful_Allocation_Budget := 0;
                     elsif Adapter_Case = 4 then ACPI_Target_Heap_Test_Control.Successful_Allocation_Budget := 1;
                     end if;
                     Startup.Resolve (Declaration,Geometry,Target,Good);
                     ACPI_Target_Heap_Test_Control.Successful_Allocation_Budget := Natural'Last;
                     if Adapter_Case in 3 | 4 then
                        Check (not Good and then All_Front_Calls = Before,"metadata allocation refusal precedes publication RPC");
                        Check (Capacity_Cache = (if Adapter_Case = 3 then ACPI_Capacity_Diagnostics.Firmware_Targets
                           else ACPI_Capacity_Diagnostics.Firmware_Regions),"typed adapter target/region capacity");
                        Check (ACPI_Capacity_Diagnostics.Present (Capacity) and then Capacity.Demand = 1,"copied capacity demand present");
                        Check (Ada.Strings.Fixed.Index (ACPI_Capacity_Diagnostics.Format (Capacity),"FIRMWARE_") > 0,"capacity formatted by cache");
                     else
                        Check (Good,"adapter target published through real lifecycle client");
                        Before := All_Front_Calls; Expected_Front_Deadline := 100;
                        if Adapter_Case = 5 then ACPI_Target_Heap_Test_Control.Successful_Allocation_Budget := 0; end if;
                        Startup.Begin_Field (Adapter_Case in 7 | 9,Adapter_Case = 11,100,
                           (if Adapter_Case = 9 then (33,44) else ACPI_Global_Lock_Protocol.No_Ticket),Good);
                        ACPI_Target_Heap_Test_Control.Successful_Allocation_Budget := Natural'Last;
                        if Adapter_Case in 5 | 7 | 9 | 11 then
                           Check (not Good,"refused frame/lock/write admission");
                           if Adapter_Case /= 9 then Check (All_Front_Calls = Before,"pre-RPC admission refusal"); end if;
                           if Adapter_Case = 5 then Check (Capacity_Cache = ACPI_Capacity_Diagnostics.Firmware_Frames,"typed frame capacity"); end if;
                        else
                           Check (Good,"NoLock adapter Field begins");
                           Startup.Inspect (Is_Ready,Is_Pending,Observation,Capacity);
                           Check (Is_Ready and then not Is_Pending,"ordinary active frames are not pending uncertainty");
                           if Adapter_Case = 1 then
                              Startup.Begin_Field (False,False,200,ACPI_Global_Lock_Protocol.No_Ticket,Good);
                              Check (Good,"nested scope clamps original parent deadline");
                              Expected_Front_Deadline := Now + 25; Startup.End_Field (Good); Check (Good,"child closes without refreshing parent");
                              Expected_Front_Deadline := 100;
                           end if;
                           Before := All_Front_Calls; Before_Transfers := Transfers;
                           if Adapter_Case = 2 then Lose_Kind := F.Read_Datum; Lose_Next_Reply := True;
                           elsif Adapter_Case = 6 then ACPI_Target_Heap_Test_Control.Successful_Allocation_Budget := 0; end if;
                           Startup.Dispatch ((Target,False,0),Response);
                           ACPI_Target_Heap_Test_Control.Successful_Allocation_Budget := Natural'Last;
                           if Adapter_Case = 2 then
                              Check (Response.Status = AA.Indeterminate and then Transfers = Before_Transfers + 1,"lost read effect not repeated");
                              Startup.Inspect (Is_Ready,Is_Pending,Observation,Capacity);
                              Check (not Is_Ready and then Is_Pending and then Observation.Present
                                 and then Observation.Operation = Response.Operation,"adapter preserves exact uncertainty observation");
                              Before := All_Front_Calls; Startup.Dispatch ((Target,False,0),Response);
                              Check (Response.Status = AA.Denied and then All_Front_Calls = Before,"stopped adapter never replays read");
                           elsif Adapter_Case = 6 then
                              Check (Response.Status = AA.Denied and then Transfers = Before_Transfers and then All_Front_Calls = Before,"descriptor OOM precedes select/data");
                              Check (Capacity_Cache = ACPI_Capacity_Diagnostics.Firmware_Descriptors,"typed descriptor capacity");
                           else
                              Check (Response.Status = AA.Completed and then Response.Value = 16#5A#
                                 and then Transfers = Before_Transfers + 1,"adapter dispatch reaches real records exactly once");
                           end if;
                           Expected_Front_Deadline := Now + 25;
                           if Adapter_Case = 14 then Now := Now - 1; end if;
                           Startup.End_Field (Good);
                           Check (Good = (Adapter_Case not in 2 | 6 | 14),"Field end preserves terminal failure while closing scope");
                           if Adapter_Case = 14 then Now := Now + 1; end if;
                        end if;
                     end if;
                  end if;
                  ACPI_Target_Heap_Test_Control.Successful_Allocation_Budget := Natural'Last;
                  Expected_Front_Deadline := Now + 25; Startup.Cleanup (Good);
                  Check (Good = (Adapter_Case /= 14),"adapter explicit cleanup preserves clock-broken scope evidence");
                  Startup.Inspect (Is_Ready,Is_Pending,Observation,Capacity);
                  Check (not Is_Ready and then Is_Pending,"cleanup never rearms adapter");
                  Before := All_Front_Calls; Startup.Dispatch ((Target,False,0),Response);
                  Check (Response.Status = AA.Denied and then All_Front_Calls = Before,"no data after close");
               end;
            end loop;
            declare
               Client_Scope, Client_Descriptor, Original_Operation, Refused_Operation : F.Identifier;
               Server_Scope, Server_Descriptor : F.Identifier;
               Good : Boolean;
               Answer : Wire.Reply;
               Before_Transfers : Natural;
               Before_History, Before_Scopes, Before_Descriptors, Before_Publications, Before_Broker : Natural;
               procedure Submit (Item : F.Request; Expected : Wire.Effect_Kind := Wire.No_Effect) is
               begin Send (Item,Expected,Answer); end Submit;
               procedure Expect_Refused (Item : F.Request; Expected : Wire.Effect_Kind := Wire.No_Effect) is
               begin
                  Submit (Item,Expected);
                  Check (Answer.Status = Wire.Denied and then Answer.Effect = Wire.No_Effect and then Answer.Value = 0,
                     "quiescing refuses new metadata/data effects without a historical receipt");
               end Expect_Refused;
            begin
               for Kind in F.Operation loop
                  Check (F.Allowed_While_Quiescing (Kind) =
                     (case Kind is when F.Describe_Publication | F.Describe_Scope | F.Query_Operation | F.Close_Scope => True,
                      when F.Publish | F.Recover_Publication | F.Begin_Scope | F.Select_Datum | F.Read_Datum | F.Write_Datum => False),
                     "explicit quiescing operation policy");
               end loop;
               ACPI_Firmware_Data_Identities.Issue (Client_Scope,Good); Check (Good,"quiesce scope ID");
               ACPI_Firmware_Data_Identities.Issue (Client_Descriptor,Good); Check (Good,"quiesce descriptor ID");
               ACPI_Firmware_Data_Identities.Issue (Original_Operation,Good); Check (Good,"quiesce original operation ID");
               ACPI_Firmware_Data_Identities.Issue (Refused_Operation,Good); Check (Good,"quiesce new operation ID");
               Submit ((F.Begin_Scope,Client_Scope,120,0,0)); Check (Answer.Status = Wire.Ready,"quiesce live scope"); Server_Scope := F.Identifier (Answer.Value);
               Submit ((F.Select_Datum,Server_Scope,F.Identifier (Ready_Row),Client_Descriptor,0,L.T.Byte_Access));
               Check (Answer.Status = Wire.Ready,"quiesce live descriptor"); Server_Descriptor := F.Identifier (Answer.Value);
               Submit ((F.Read_Datum,Server_Descriptor,Original_Operation),Wire.Read_Effect);
               Check (Answer.Status = Wire.Done and then Answer.Value = 16#5A#,"quiesce original effect completed");
               Before_Transfers := Transfers;
               Before_History := Scoped_History.Observe (Joined_History).Operations;
               Before_Scopes := S.Observe (Data_Scopes).Scopes;
               Before_Descriptors := S.Observe (Data_Scopes).Descriptors;
               Before_Publications := P.Observe (Owner).Publications;
               Before_Broker := Channel.Calls;
               New_Admission_Open := False;
               Expect_Refused ((F.Publish,Client_Scope,Logical,120));
               Expect_Refused ((F.Recover_Publication,1,120));
               Expect_Refused ((F.Begin_Scope,Refused_Operation,120,0,0));
               Expect_Refused ((F.Select_Datum,Server_Scope,F.Identifier (Ready_Row),Refused_Operation,0,L.T.Byte_Access));
               Expect_Refused ((F.Read_Datum,Server_Descriptor,Refused_Operation),Wire.Read_Effect);
               Expect_Refused ((F.Write_Datum,Server_Descriptor,Refused_Operation,1),Wire.Write_Effect);
               Submit ((F.Query_Operation,Server_Descriptor,Original_Operation),Wire.Read_Effect);
               Check (Answer.Status = Wire.Done and then Answer.Effect = Wire.Read_Effect and then Answer.Value = 16#5A#,
                  "old history remains passive during quiescing");
               Submit ((F.Describe_Scope,Client_Scope)); Check (Answer.Status = Wire.Ready,"refused effects did not mutate live scope");
               Submit ((F.Describe_Publication,1)); Check (Answer.Status = Wire.Ready,"publication description remains passive");
               Submit ((F.Close_Scope,Server_Scope)); Check (Answer.Status = Wire.Closed,"data close remains possible while quiescing");
               Submit ((F.Describe_Scope,Client_Scope)); Check (Answer.Status = Wire.Closed,"scope terminal evidence visible");
               Submit ((F.Query_Operation,Server_Descriptor,Original_Operation),Wire.Read_Effect);
               Check (Answer.Status = Wire.Done and then Answer.Value = 16#5A#,"closed scope history is still passive");
               Check (Transfers = Before_Transfers and then Scoped_History.Observe (Joined_History).Operations = Before_History
                  and then S.Observe (Data_Scopes).Scopes = Before_Scopes
                  and then S.Observe (Data_Scopes).Descriptors = Before_Descriptors
                  and then P.Observe (Owner).Publications = Before_Publications and then Channel.Calls = Before_Broker,
                  "quiescing causes no new data, history, scope, descriptor, publication or grant effects");
            end;
         end;
      end if;
   end Run_Case;
begin
   for Choice in Mode loop Run_Case (Choice); end loop;
   Ada.Text_IO.Put_Line ("Firmware data AML-provider-to-record checks" & Checks'Image);
end Publication_Tests;

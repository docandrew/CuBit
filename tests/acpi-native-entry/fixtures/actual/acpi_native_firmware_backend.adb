pragma Ada_2022;
package body ACPI_Native_Firmware_Backend is
   use type Word, Wire.Outcome, Wire.Effect_Kind, Arena.Outcome;
   function Authorized (Caller : Word) return Boolean is
      (Stage = Ready and then not Initializing and then Caller /= 0 and then Caller_Allowed (Caller));
   function Effects_Authorized (Caller : Word; For_Write : Boolean) return Boolean is
      (Authorized (Caller) and then New_Effects_Allowed and then (not For_Write or else Allow_Writes));
   function May_Publish (Requester : Word) return Boolean is
      (Authorized (Requester) and then New_Effects_Allowed);
   function Scope_Usable (Requester : Word; For_Write : Boolean) return Boolean is
      Accepted : Boolean;
   begin Scopes.Check_Active (Data_State,Requester,For_Write,Accepted); return Accepted; end Scope_Usable;
   procedure Execute_Effect (Caller : Word; Descriptor : F.Identifier; For_Write : Boolean;
      Input : Word; Value : out Word; Status : out F.IPC.Outcome) is
   begin Scopes.Perform (Data_State,Caller,Descriptor,For_Write,Input,Value,Status); end Execute_Effect;
   procedure Initialize (Approved_Endpoint : CuBit.Handles.Endpoint; First, Limit : Word; Accepted : out Boolean) is
      Result : Arena.Outcome;
   begin
      Accepted := False; if Stage /= Fresh or else Initializing then return; end if;
      Stage := Failed; Initializing := True;
      Startup_Result := Endpoint_Rejected;
      Native.Initialize (Broker,Approved_Endpoint,Accepted);
      if Accepted then
         Startup_Result := Arena_Rejected;
         Arena.Configure (Reservations,First,Limit,Result); Accepted := Result = Arena.Configured;
      end if;
      if Accepted then Stage := Ready; Startup_Result := Initialized; end if;
      Initializing := False;
   end Initialize;
   function Observe return Report is
      P : constant Publications.Statistics := Publications.Observe (Publication_State);
      R : constant Records.Statistics := Records.Observe (Rows);
      S : constant Scopes.Statistics := Scopes.Observe (Data_State);
      H : constant History.Statistics := History.Observe (Operations);
      D : constant Dispatch.Statistics := Dispatch.Observe (Dispatcher);
   begin
      return (Stage => Stage,Initialization => Startup_Result,Working => Initializing or else P.Working or else R.Working
         or else S.Working or else H.Working or else D.Working,
         Pending_Setup => P.Pending_Ordinal /= 0 or else P.Grant_Cleanup_Due,
         Retained_Unattached => P.Retained_Unattached /= 0,
         Publications => P.Publications,Records => R.Records,Scopes => S.Scopes,
         Descriptors => S.Descriptors,Operations => H.Operations,
         Accepted => D.Accepted,Malformed => D.Malformed,Denied => D.Denied,Reentered => D.Reentered);
   end Observe;
   function Publication_Status (Value : Publications.Outcome) return Wire.Outcome is
      (case Value is when Publications.Record_Ready => Wire.Ready,
       when Publications.Pending => Wire.Pending,
       when Publications.Backend_Refused => Wire.Refused,
       when Publications.Publication_Stopped => Wire.Stopped,
       when Publications.Capacity_Refused => Wire.Capacity_Refused,
       when Publications.Invalid_Request | Publications.Invalid_Deadline => Wire.Invalid_Request,
       when Publications.Request_Changed => Wire.Request_Changed,
       when Publications.Not_Found => Wire.Not_Found,
       when Publications.Busy | Publications.Reentered => Wire.Busy,
       when Publications.Denied => Wire.Denied);
   function Scope_Status (Value : Scopes.Outcome) return Wire.Outcome is
      (case Value is when Scopes.Ready => Wire.Ready,
       when Scopes.Closed => Wire.Closed,when Scopes.Stopped => Wire.Stopped,
       when Scopes.Not_Found => Wire.Not_Found,
       when Scopes.Invalid_Request => Wire.Invalid_Request,
       when Scopes.Request_Changed => Wire.Request_Changed,
       when Scopes.Denied => Wire.Denied,
       when Scopes.Clock_Unavailable => Wire.Clock_Unavailable,
       when Scopes.Clock_Reversed => Wire.Clock_Reversed,
       when Scopes.Expired => Wire.Expired,
       when Scopes.Lease_Refused => Wire.Lease_Refused,
       when Scopes.Selection_Refused => Wire.Selection_Refused,
       when Scopes.Allocation_Refused => Wire.Capacity_Refused,
       when Scopes.Reentered => Wire.Busy);
   procedure Save_History (Item : History.Report; Reply : in out Wire.Reply) is
   begin
      Reply.Effect := Item.Effect;
      case Item.State is
         when History.No_Evidence => Reply.Status := Wire.No_Evidence;
         when History.Running => Reply.Status := Wire.Running;
         when History.Identity_Conflict => Reply.Status := Wire.Request_Changed;
         when History.Allocation_Refused => Reply.Status := Wire.Capacity_Refused;
         when History.Invalid_Identity => Reply.Status := Wire.Invalid_Request;
         when History.Reentered => Reply.Status := Wire.Busy;
         when History.Finished =>
            Reply.Status := (case Item.Status is
               when F.IPC.Done => Wire.Done,when F.IPC.Denied => Wire.Denied,
               when F.IPC.Indeterminate => Wire.Indeterminate,
               when F.IPC.Malformed | F.IPC.Backend_Fault => Wire.Backend_Fault);
      end case;
      if Reply.Status = Wire.Done and then Reply.Effect = Wire.Read_Effect then Reply.Value := Item.Value; end if;
   end Save_History;
   procedure Handle (Caller : Word; Item : F.Request; Status : out Wire.Outcome;
      Effect : out Wire.Effect_Kind; Value : out Word) is
      Reply : Wire.Reply;
      Published : Publications.Reply;
      Scoped : Scopes.Reply;
      Saved : History.Report;
   begin
      Status := Wire.Denied; Effect := Wire.No_Effect; Value := 0;
      if not New_Effects_Allowed and then not F.Allowed_While_Quiescing (Item.Kind) then return; end if;
      case Item.Kind is
         when F.Publish =>
            Publications.Publish (Publication_State,Broker,Caller,
               Publications.Publication_ID (Item.ID),Item.Region,Item.Publication_Deadline,Published);
            Reply.Status := Publication_Status (Published.Status);
            if Reply.Status = Wire.Ready then Reply.Value := Word (Published.ID); end if;
         when F.Describe_Publication =>
            Publications.Describe (Publication_State,Caller,Publications.Publication_ID (Item.ID),Published);
            Reply.Status := Publication_Status (Published.Status);
            if Reply.Status = Wire.Ready then Reply.Value := Word (Published.ID); end if;
         when F.Recover_Publication =>
            Publications.Recover (Publication_State,Broker,Caller,
               Publications.Publication_ID (Item.ID),Item.Recovery_Deadline,Published);
            Reply.Status := Publication_Status (Published.Status);
            if Reply.Status = Wire.Ready then Reply.Value := Word (Published.ID); end if;
         when F.Begin_Scope =>
            Scopes.Begin_Scope (Data_State,Caller,Item.ID,Item.Scope_Deadline,Item.Lock_Domain,Item.Lock_Sequence,Scoped);
            Reply.Status := Scope_Status (Scoped.Status);
            if Reply.Status = Wire.Ready then Reply.Value := Word (Scoped.ID); end if;
         when F.Select_Datum =>
            Scopes.Select_Datum (Data_State,Caller,Item.ID,Item.Client_Descriptor_ID,Item.Record_ID,Item.Offset,Item.Width,Scoped);
            Reply.Status := Scope_Status (Scoped.Status);
            if Reply.Status = Wire.Ready then Reply.Value := Word (Scoped.ID); end if;
         when F.Close_Scope => Scopes.Close_Scope (Data_State,Caller,Item.ID,Scoped); Reply.Status := Scope_Status (Scoped.Status);
         when F.Describe_Scope =>
            Scoped := Scopes.Describe_Scope (Data_State,Caller,Item.ID);
            Reply.Status := Scope_Status (Scoped.Status);
            if Reply.Status = Wire.Ready then Reply.Value := Word (Scoped.ID); end if;
         when F.Read_Datum =>
            History.Submit (Operations,Caller,Item.ID,Item.Operation_ID,False,0,Saved); Save_History (Saved,Reply);
         when F.Write_Datum =>
            History.Submit (Operations,Caller,Item.ID,Item.Write_Operation_ID,True,Item.Value,Saved); Save_History (Saved,Reply);
         when F.Query_Operation => Saved := History.Query (Operations,Caller,Item.ID,Item.Operation_ID); Save_History (Saved,Reply);
      end case;
      Status := Reply.Status; Effect := Reply.Effect; Value := Reply.Value;
   end Handle;
   procedure Dispatch_Frame (Caller : Word; Request : F.IPC.Packet; Has_Capability : Boolean;
      Response : out F.IPC.Packet; Handled : out Boolean) is
   begin Dispatch.Dispatch (Dispatcher,Caller,Request,Has_Capability,Response,Handled); end Dispatch_Frame;
   procedure Additional_Request (From : CuBit.Messages.Process_ID;
      Request : CuBit.Messages.Message; Response : in out CuBit.Messages.Message; Handled : in out Boolean) is
   begin Receiver.Handle (From,Request,Response,Handled); end Additional_Request;
end ACPI_Native_Firmware_Backend;

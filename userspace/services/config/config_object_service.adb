with Config_Object_Messages;
with Config_Worker_Protocol;
with Config_Schema_Protocol;
with CCL.Objects.Schemas;

package body Config_Object_Service is
   package Channel renames Config_Worker_Channel;
   package Store renames Config_Typed_Store;
   package P renames Config_Worker_Protocol;
   package T renames Config_Schema_Protocol;
   package W renames Config_Object_Messages;
   use type Interfaces.Unsigned_64;
   use type Interfaces.Unsigned_32;
   use type Config_Objects.Outcome;
   use type Channel.Submission;
   use type Channel.Completion_Result;
   use type Channel.Phase;
   use type Config_Collections.Result;
   use type CCL.Objects.Schema_Key;
   --  Package/process lifetime, NOT reset when a retired service State is
   --  replaced. Never share this completion queue with an independent token
   --  allocator. Other async roles would need this same allocator/domain.
   Last_Token : Interfaces.Unsigned_64 := 0;
   function Status (Object : State) return Worker_Status is (Object.Current);
   function Waiting (Object : State) return Boolean is (Receiver.Waiting (Object.Requests));

   procedure Attach
     (Object : in out State; Endpoint : CuBit.Messages.CapabilitySlot;
      Session : Interfaces.Unsigned_64; Success : out Boolean) is
   begin
      Success := False;
      if Object.Current /= Unattached or else Session = 0 then return; end if;
      Object.Current := Recovery_Required;
      Object.Session := Session;
      Channel.Initialize (Object.Worker, Endpoint, Success);
      if Success then Object.Current := Online; end if;
   end Attach;

   procedure Token (Value : out Interfaces.Unsigned_64) is
   begin
      Value := 0;
      if Last_Token < Interfaces.Unsigned_64'Last - 1 then
         Last_Token := Last_Token + 1;
         Value := Last_Token;
      end if;
   end Token;

   procedure Retire (Object : in out State; Confirmed : out Boolean) is
   begin
      Object.Current := Recovery_Required;
      Object.Provisioning := False;
      Object.Metadata_Pending := False;
      Receiver.Lost (Object.Requests, Object.Store, Object.Session);
      Channel.Retire (Object.Worker, Confirmed);
   end Retire;

   procedure Submit (Object : in out State; Submitted : out Boolean) is
      Request : P.Frame;
      Contract : CCL.Objects.Binding;
      Available, Retired : Boolean;
      Result : Channel.Submission;
   begin
      Submitted := False;
      Store.Pending (Object.Store, Request, Contract, Available);
      if Object.Current = Online and then Available then
         Channel.Submit (Object.Worker, Request, Contract, Result);
         Submitted := Result = Channel.Submitted;
      end if;
      --  Even a definite queue rejection needs to unwind the staged operation
      --  and reply slot. Recovery, not a hidden retry, is the initial policy.
      if not Submitted then Retire (Object, Retired); end if;
   end Submit;

   procedure Restore
     (Object : in out State; ID : Config_Collections.Registered_ID;
      Result : out Config_Objects.Outcome)
   is
      Schema_Token, Request_Token : Interfaces.Unsigned_64;
      Request : P.Frame;
      Contract : CCL.Objects.Binding;
      Available, Retired : Boolean;
      Submitted : Channel.Submission;
   begin
      Result := Config_Objects.Needs_Recovery;
      if Object.Current /= Online then return; end if;
      if Object.Metadata_Pending then Result := Config_Objects.Busy; return; end if;
      --  The provisioning token precedes the retained load's token. Sharing
      --  one channel cannot reorder its monotonic request identity sequence.
      Token (Schema_Token);
      Token (Request_Token);
      Store.Restore (Object.Store, ID, Object.Session, Request_Token, Result);
      if Result = Config_Objects.Accepted then
         Store.Pending (Object.Store, Request, Contract, Available);
         Submitted := Channel.Unavailable;
         if Available then Channel.Provision (Object.Worker, Contract, Schema_Token, Submitted); end if;
         if Submitted = Channel.Submitted then
            Object.Provisioning := True;
         else
            Retire (Object, Retired); Result := Config_Objects.Needs_Recovery;
         end if;
      end if;
   end Restore;

   procedure Handle
     (Object : in out State; Authority : Config_Authority.Authority_State;
      Sender : CuBit.Messages.ProcessID; Request : CuBit.Messages.Message)
   is
      Request_Token : Interfaces.Unsigned_64 := 0;
      Staged, Submitted : Boolean;
      Retired, Valid : Boolean;
      Control : W.Open_Descriptor;
      Contract : CCL.Objects.Binding;
      Definition : T.Frame;
      Sent : Channel.Submission;
   begin
      if Request.tag.label in W.Operation'Enum_Rep (W.Create_Collection) | W.Operation'Enum_Rep (W.Open_Collection) then
         Receiver.Begin_Definition (Object.Requests, Object.Store,
           (if Request.tag.label = W.Operation'Enum_Rep (W.Create_Collection) then W.Create_Collection else W.Open_Collection),
           Authority, Sender, Request,
           Object.Current = Online and then Channel.Status (Object.Worker) = Channel.Ready and then
           Store.Pending_Token (Object.Store) = 0, Staged);
         if Staged then
            Receiver.Pending_Definition (Object.Requests, Control, Contract);
            Token (Request_Token);
            T.Make_Request
              ((if Request.tag.label = W.Operation'Enum_Rep (W.Create_Collection) then T.Create else T.Recover),
               Object.Session, Request_Token,
               Control.Name (1 .. Natural (Control.Name_Length)), "machine", Contract, Definition, Valid);
            Sent := Channel.Invalid_Request;
            if Valid then Channel.Submit_Type (Object.Worker, Definition, Sent); end if;
            if Sent = Channel.Submitted then Object.Metadata_Pending := True;
            else Retire (Object, Retired); end if;
         end if;
         if Receiver.Needs_Recovery (Object.Requests) then Retire (Object, Retired); end if;
         return;
      end if;
      if Request.tag.label = Config_Object_Messages.Operation'Enum_Rep (Config_Object_Messages.Set_Object) then
         Token (Request_Token);
      end if;
      Receiver.Handle (Object.Requests, Object.Store, Authority, Sender, Request, Request_Token, Staged);
      if Staged then Submit (Object, Submitted); end if;
      if Receiver.Needs_Recovery (Object.Requests) then Retire (Object, Retired); end if;
   end Handle;

   procedure Complete
     (Object : in out State; Authority : Config_Authority.Authority_State;
      Completion : CuBit.Messages.CompletionEntry)
   is
      Result : Channel.Completion_Result;
      Response : P.Frame;
      Valid, Taken, Retired, Submitted : Boolean;
      Type_Response : T.Frame;
      Control : W.Open_Descriptor;
      Contract : CCL.Objects.Binding;
      ID : Config_Collections.Collection_ID;
      Registered : Config_Collections.Result;
      Restored : Config_Objects.Outcome;
   begin
      if Object.Current /= Online then return; end if;
      Channel.Complete (Object.Worker, Completion, Result);
      if Result /= Channel.Completed then return; end if;
      if Object.Metadata_Pending then
         Channel.Take_Type_Result (Object.Worker, Type_Response, Valid, Taken);
         Object.Metadata_Pending := False;
         if not Taken or else not Valid or else Channel.Status (Object.Worker) /= Channel.Ready then
            Retire (Object, Retired); return;
         end if;
         if Type_Response.Reply = T.Reply_Kind'Enum_Rep (T.Definition_Conflict) then
            Receiver.Finish_Definition (Object.Requests, Object.Store, Authority, W.Schema_Mismatch); return;
         elsif Type_Response.Reply = T.Reply_Kind'Enum_Rep (T.Rejected) then
            Receiver.Finish_Definition (Object.Requests, Object.Store, Authority, W.Invalid_Request); return;
         elsif Type_Response.Reply = T.Reply_Kind'Enum_Rep (T.Absent) then
            Receiver.Finish_Definition (Object.Requests, Object.Store, Authority, W.Missing); return;
         end if;
         Receiver.Pending_Definition (Object.Requests, Control, Contract);
         if Type_Response.Action = T.Operation'Enum_Rep (T.Recover) then
            CCL.Objects.Schemas.Read (Type_Response.Metadata, Contract, Valid);
            if not Valid then Retire (Object, Retired); return; end if;
            if CCL.Objects.Identity (Contract) /= Control.Schema then
               Receiver.Finish_Definition (Object.Requests, Object.Store, Authority, W.Schema_Mismatch); return;
            end if;
         end if;
         Store.Register (Object.Store, Control.Name (1 .. Natural (Control.Name_Length)), Contract, ID, Registered);
         if Registered not in Config_Collections.Registered | Config_Collections.Already_Registered then
            if Registered = Config_Collections.Capacity_Exceeded then
               Receiver.Finish_Definition (Object.Requests, Object.Store, Authority, W.Capacity_Exceeded);
            elsif Registered = Config_Collections.Schema_Conflict then
               Receiver.Finish_Definition (Object.Requests, Object.Store, Authority, W.Schema_Mismatch);
            else Retire (Object, Retired); end if;
            return;
         end if;
         if Registered = Config_Collections.Already_Registered and then
           Store.Ready_In_Session (Object.Store, ID, Object.Session)
         then
            Receiver.Finish_Definition (Object.Requests, Object.Store, Authority, W.Success); return;
         end if;
         Restore (Object, ID, Restored);
         if Restored /= Config_Objects.Accepted then Retire (Object, Retired); end if;
         return;
      end if;
      if Object.Provisioning then
         Channel.Take_Provision_Result (Object.Worker, Valid, Taken);
         Object.Provisioning := False;
         if Taken and then Valid and then Channel.Status (Object.Worker) = Channel.Ready then
            Submit (Object, Submitted);
         else
            Retire (Object, Retired);
         end if;
         return;
      end if;
      Channel.Take_Result (Object.Worker, Response, Valid, Taken);
      if Taken and then Valid then
         if Receiver.Definition_Pending (Object.Requests) then
            Store.Complete (Object.Store, Response, Restored);
            if Restored in Config_Objects.Accepted | Config_Objects.Published then
               Receiver.Finish_Definition (Object.Requests, Object.Store, Authority, W.Success);
            else
               Retire (Object, Retired);
            end if;
         else
            Receiver.Finish (Object.Requests, Object.Store, Response);
         end if;
      end if;
      if not Taken or else not Valid or else Channel.Status (Object.Worker) /= Channel.Ready then
         Retire (Object, Retired);
      end if;
   end Complete;

   procedure Revoke_Subject (Object : in out State; Subject : Config_Authority.Subject_ID) is
   begin
      Store.Revoke_Subject (Object.Store, Subject);
   end Revoke_Subject;
end Config_Object_Service;

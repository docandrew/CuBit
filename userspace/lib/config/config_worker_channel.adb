with Config_Worker_Messages;

package body Config_Worker_Channel is
   use CuBit.Messages;
   use type Interfaces.Unsigned_64;
   use type Interfaces.Unsigned_32;
   package P renames Config_Worker_Protocol;
   package T renames Config_Schema_Protocol;
   package Grants renames CuBit.Memory_Grants;
   function Status (Object : Channel) return Phase is (Object.Current);
   function Pending_Token (Object : Channel) return Interfaces.Unsigned_64 is
     (Object.Active_Token);

   procedure Initialize
     (Object : in out Channel; Endpoint : CapabilitySlot; Success : out Boolean) is
   begin
      Success := False;
      if Object.Current /= Fresh then return; end if;
      Object.Current := Failed;
      Object.Endpoint := Endpoint;
      Grants.Create_Via_Capability
        (Endpoint, Object.Loan'Address, Loan_Bytes_Count / 4096,
         True, Object.Grant, Success);
      if Success then Object.Has_Grant := True; Object.Current := Ready; end if;
   end Initialize;

   procedure Submit
     (Object : in out Channel; Request : P.Frame;
      Contract : CCL.Objects.Binding; Result : out Submission) is
   begin
      Result := Unavailable;
      if Object.Current in Waiting | Result_Ready then Result := Busy; return; end if;
      if Object.Current /= Ready then return; end if;
      --  Original is NEVER exposed to the worker. A writable response grant
      --  cannot change which request/schema we later validate its reply against.
      Object.Original := Request;
      Result := Invalid_Request;
      if not P.Valid_Request (Object.Original, Contract) or else
        Object.Original.Token <= Object.Last_Token
      then return; end if;
      Object.Contract := Contract;
      declare
         Shared : P.Frame with Import, Volatile, Address => Object.Loan'Address;
      begin
         Shared := Object.Original;
      end;
      Object.Last_Token := Object.Original.Token;
      if not capSubmit (Object.Endpoint, Config_Worker_Messages.Request (Object.Grant),
                        Object.Original.Token)
      then Result := Not_Submitted; return; end if;
      Object.Active_Token := Object.Original.Token;
      Object.Active_Operation := Data_Exchange;
      Object.Valid_Response := False;
      Object.Current := Waiting;
      Result := Submitted;
   end Submit;

   procedure Provision
     (Object : in out Channel; Contract : CCL.Objects.Binding;
      Token : Interfaces.Unsigned_64; Result : out Submission)
   is
      Data : CCL.Objects.Schemas.Image;
      Valid : Boolean;
   begin
      Result := Unavailable;
      if Object.Current in Waiting | Result_Ready then Result := Busy; return; end if;
      if Object.Current /= Ready then return; end if;
      Result := Invalid_Request;
      if Token <= Object.Last_Token or else Token = NO_COMPLETION_TOKEN then return; end if;
      CCL.Objects.Schemas.Write (Contract, Data, Valid);
      if not Valid then return; end if;
      declare
         Shared : CCL.Objects.Schemas.Image with Import, Volatile, Address => Object.Loan'Address;
      begin
         Shared := Data;
      end;
      Object.Last_Token := Token;
      if not capSubmit (Object.Endpoint, Config_Worker_Messages.Schema_Request (Object.Grant), Token) then
         Result := Not_Submitted; return;
      end if;
      Object.Contract := Contract;
      Object.Active_Token := Token; Object.Active_Operation := Schema_Provision;
      Object.Valid_Response := False; Object.Current := Waiting;
      Result := Submitted;
   end Provision;

   procedure Submit_Type
     (Object : in out Channel; Request : T.Frame; Result : out Submission)
   is
   begin
      Result := Unavailable;
      if Object.Current in Waiting | Result_Ready then Result := Busy; return; end if;
      if Object.Current /= Ready then return; end if;
      Object.Type_Original := Request;
      Result := Invalid_Request;
      if not T.Valid_Request (Object.Type_Original) or else Object.Type_Original.Token <= Object.Last_Token then return; end if;
      declare
         Shared : T.Frame with Import, Volatile, Address => Object.Loan'Address;
      begin
         Shared := Object.Type_Original;
      end;
      Object.Last_Token := Object.Type_Original.Token;
      if not capSubmit (Object.Endpoint, Config_Worker_Messages.Type_Request (Object.Grant), Object.Type_Original.Token) then
         Result := Not_Submitted; return;
      end if;
      Object.Active_Token := Object.Type_Original.Token; Object.Active_Operation := Type_Exchange;
      Object.Valid_Response := False; Object.Current := Waiting;
      Result := Submitted;
   end Submit_Type;

   procedure Complete
     (Object : in out Channel; Completion : CompletionEntry;
      Result : out Completion_Result) is
   begin
      Result := Ignored;
      if Object.Current /= Waiting or else not Completion.valid or else
        Completion.token /= Object.Active_Token
      then return; end if;
      Result := Completed;
      Object.Current := Result_Ready;
      Object.Valid_Response := False;
      Object.Poisoned := True;
      if Completion.requestId = 0 or else Completion.status /= COMPLETION_OK then return; end if;
      if Object.Active_Operation = Type_Exchange then
         if not Config_Worker_Messages.Valid_Type_Acknowledgment (Completion.msg) then return; end if;
         declare
            Shared : T.Frame with Import, Volatile, Address => Object.Loan'Address;
         begin
            Object.Type_Response := Shared;
         end;
         if not T.Valid_Reply (Object.Type_Response, Object.Type_Original) then return; end if;
         Object.Valid_Response := True;
         Object.Poisoned := Object.Type_Response.Reply in
           T.Reply_Kind'Enum_Rep (T.Uncertain) | T.Reply_Kind'Enum_Rep (T.Load_Failed);
         return;
      end if;
      if Object.Active_Operation = Schema_Provision then
         Object.Valid_Response := Config_Worker_Messages.Valid_Schema_Acknowledgment (Completion.msg);
         Object.Poisoned := not Object.Valid_Response;
         return;
      end if;
      if not Config_Worker_Messages.Valid_Acknowledgment (Completion.msg) then return; end if;
      declare
         Shared : P.Frame with Import, Volatile, Address => Object.Loan'Address;
      begin
         Object.Response := Shared;
      end;
      if not P.Valid_Reply (Object.Response, Object.Original, Object.Contract) then return; end if;
      Object.Valid_Response := True;
      Object.Poisoned := Object.Response.Reply in
        P.Reply_Kind'Enum_Rep (P.Uncertain) | P.Reply_Kind'Enum_Rep (P.Load_Failed);
   end Complete;

   procedure Take_Result
     (Object : in out Channel; Response : out P.Frame; Valid, Taken : out Boolean) is
   begin
      Response := (others => <>);
      Valid := False;
      Taken := Object.Current = Result_Ready and Object.Active_Operation = Data_Exchange;
      if not Taken then return; end if;
      Valid := Object.Valid_Response;
      if Valid then Response := Object.Response; end if;
      Object.Active_Token := 0;
      Object.Current := (if Object.Poisoned then Failed else Ready);
   end Take_Result;

   procedure Take_Provision_Result (Object : in out Channel; Valid, Taken : out Boolean) is
   begin
      Valid := False;
      Taken := Object.Current = Result_Ready and Object.Active_Operation = Schema_Provision;
      if not Taken then return; end if;
      Valid := Object.Valid_Response;
      Object.Active_Token := 0;
      Object.Current := (if Object.Poisoned then Failed else Ready);
   end Take_Provision_Result;

   procedure Take_Type_Result
     (Object : in out Channel; Response : out T.Frame; Valid, Taken : out Boolean)
   is
   begin
      Response := (others => <>); Valid := False;
      Taken := Object.Current = Result_Ready and Object.Active_Operation = Type_Exchange;
      if not Taken then return; end if;
      Valid := Object.Valid_Response;
      if Valid then Response := Object.Type_Response; end if;
      Object.Active_Token := 0;
      Object.Current := (if Object.Poisoned then Failed else Ready);
   end Take_Type_Result;

   procedure Retire (Object : in out Channel; Confirmed : out Boolean) is
   begin
      Object.Current := Retired;
      Object.Active_Token := 0;
      Object.Valid_Response := False;
      Confirmed := not Object.Has_Grant;
      if Object.Has_Grant then
         if not Object.Revocation_Requested then
            Grants.Revoke (Object.Grant, Object.Revocation_Requested);
         end if;
         Confirmed := Grants.Retirement_Confirmed (Object.Grant);
         if Confirmed then Object.Has_Grant := False; end if;
      end if;
   end Retire;
end Config_Worker_Channel;

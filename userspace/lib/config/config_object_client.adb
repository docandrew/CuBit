with CCL.Objects.Schemas;
package body Config_Object_Client is
   package Wire renames Config_Object_Messages;
   package Grants renames CuBit.Memory_Grants;
   package IPC renames CuBit.Messages;
   package Requests renames CuBit.Async_Requests;
   use type Number;
   use type Wire.Operation;
   use type Wire.Status;
   use type Interfaces.Unsigned_32;
   function Status (Object : Client) return Phase is (Object.Current);
   function Admission (Object : Client; Token : Number) return Submission is
     (if Object.Current in Waiting | Result_Ready then Busy
      elsif Object.Current /= Ready then Unavailable
      elsif not Requests.Can_Reserve (Object.Request, Token) then Invalid_Request
      else Submitted);

   procedure Initialize
     (Object : in out Client; Endpoint : IPC.CapabilitySlot; Success : out Boolean) is
   begin
      Success := False;
      if Object.Current /= Fresh then return; end if;
      Object.Current := Failed; Object.Endpoint := Endpoint;
      Grants.Create_Via_Capability (Endpoint, Object.Loan'Address, Wire.Creation_Bytes / 4096,
                                    True, Object.Grant, Success);
      if Success then Object.Has_Grant := True; Object.Current := Ready; end if;
   end Initialize;

   procedure Start
     (Object : in out Client; Action : Wire.Operation; Token, Revision : Number; Result : out Submission) is
      Reserved, Accepted : Boolean;
   begin
      Requests.Reserve (Object.Request, Token, Reserved);
      if not Reserved then Result := Invalid_Request; return; end if;
      Object.Action := Action; Object.Expected_Revision := Revision;
      Accepted := IPC.capSubmit (Object.Endpoint, Wire.Request (Action, Object.Grant, Object.Handle, Revision), Token);
      Requests.Submitted (Object.Request, Accepted);
      if not Accepted then
         Result := Not_Submitted; return;
      end if;
      Object.Output := (others => <>);
      Object.Current := Waiting;
      Result := Submitted;
   end Start;

   procedure Open
     (Object : in out Client; Name : String; Contract : CCL.Objects.Binding;
      Access_Rights : Wire.Access_Mode; Context, Token : Number; Result : out Submission)
   is
      Valid : Boolean;
   begin
      Result := Admission (Object, Token); if Result /= Submitted then return; end if;
      Result := Invalid_Request;
      if Object.Handle /= 0 or not CCL.Objects.Is_Bound (Contract) then return; end if;
      Wire.Describe (Name, Access_Rights, Context, CCL.Objects.Identity (Contract), Object.Loan.Control, Valid);
      if not Valid then return; end if;
      Object.Contract := Contract;
      Object.Loan.Metadata := (others => <>);
      Start (Object, Wire.Open_Collection, Token, 0, Result);
   end Open;

   procedure Create
     (Object : in out Client; Name : String; Contract : CCL.Objects.Binding;
      Access_Rights : Wire.Access_Mode; Context, Token : Number; Result : out Submission)
   is
      Valid : Boolean;
   begin
      Result := Admission (Object, Token); if Result /= Submitted then return; end if;
      Result := Invalid_Request;
      if Object.Handle /= 0 or not CCL.Objects.Is_Bound (Contract) then return; end if;
      Wire.Describe (Name, Access_Rights, Context, CCL.Objects.Identity (Contract), Object.Loan.Control, Valid);
      if not Valid then return; end if;
      CCL.Objects.Schemas.Write (Contract, Object.Loan.Metadata, Valid);
      if not Valid then return; end if;
      Object.Contract := Contract;
      Start (Object, Wire.Create_Collection, Token, 0, Result);
   end Create;

   procedure Get (Object : in out Client; Token : Number; Result : out Submission) is
   begin
      Result := Admission (Object, Token); if Result /= Submitted then return; end if;
      if Object.Handle = 0 then Result := Invalid_Request; return; end if;
      declare
         Frame : Wire.Frame with Import, Address => Object.Loan'Address;
      begin
         Frame.Value := (others => <>);
      end;
      Start (Object, Wire.Get_Object, Token, 0, Result);
   end Get;

   procedure Set
     (Object : in out Client; Value : CCL.Objects.Image;
      Expected_Revision, Token : Number; Result : out Submission) is
   begin
      Result := Admission (Object, Token); if Result /= Submitted then return; end if;
      Result := Invalid_Request;
      if Object.Handle = 0 or Expected_Revision >= Wire.Maximum_Revision then return; end if;
      declare
         Owned : constant CCL.Objects.Image := Value;
      begin
         if not CCL.Objects.Validate (Owned, Object.Contract) then return; end if;
         declare
            Frame : Wire.Frame with Import, Address => Object.Loan'Address;
         begin
            Frame.Value := Owned;
         end;
      end;
      Start (Object, Wire.Set_Object, Token, Expected_Revision, Result);
   end Set;

   procedure Close (Object : in out Client; Token : Number; Result : out Submission) is
   begin
      Result := Admission (Object, Token); if Result /= Submitted then return; end if;
      if Object.Handle = 0 then Result := Invalid_Request; return; end if;
      Start (Object, Wire.Close_Collection, Token, 0, Result);
   end Close;

   procedure Complete
     (Object : in out Client; Completion : IPC.CompletionEntry; Result : out Completion_Result) is
      Accepted : Boolean;
   begin
      Result := Ignored;
      if Object.Current /= Waiting then return; end if;
      Requests.Capture (Object.Request, Completion.token, Completion.valid, Accepted);
      if not Accepted then return; end if;
      Result := Completed; Object.Current := Result_Ready;
      Object.Output := (others => <>); Object.Poisoned := True;
      if Completion.requestId = 0 or else Completion.status /= IPC.COMPLETION_OK or else
        not Wire.Valid_Reply (Completion.msg, Object.Action, Object.Expected_Revision)
      then return; end if;
      for Code in Wire.Status loop
         if Completion.msg.tag.label = Wire.Status'Enum_Rep (Code) then Object.Output.Code := Code; exit; end if;
      end loop;
      if Object.Action = Wire.Get_Object and Object.Output.Code in Wire.Success | Wire.Stale then
         declare
            Shared : Wire.Frame with Import, Volatile, Address => Object.Loan'Address;
            Owned : constant CCL.Objects.Image := Shared.Value;
         begin
            if not CCL.Objects.Validate (Owned, Object.Contract) then
               Object.Output := (others => <>); return;
            end if;
            Object.Output.Value := Owned;
         end;
         Object.Output.Revision := Completion.msg.words (0);
      elsif Object.Output.Code = Wire.Success then
         case Object.Action is
            when Wire.Open_Collection | Wire.Create_Collection => Object.Handle := Completion.msg.words (0);
            when Wire.Close_Collection => Object.Handle := 0;
            when Wire.Set_Object => Object.Output.Revision := Completion.msg.words (0);
            when Wire.Get_Object => null;
         end case;
      end if;
      Object.Output.Valid := True;
      Object.Poisoned := Object.Output.Code = Wire.Uncertain;
   end Complete;

   procedure Consume_Result (Object : in out Client) is
   begin
      Requests.Release (Object.Request);
      Object.Current := (if Object.Poisoned then Failed else Ready);
   end Consume_Result;

   procedure Take_Result (Object : in out Client; Item : out Response; Taken : out Boolean) is
   begin
      Item := (others => <>); Taken := Object.Current = Result_Ready;
      if not Taken then return; end if;
      Item := Object.Output;
      Consume_Result (Object);
   end Take_Result;

   procedure Retire (Object : in out Client; Confirmed : out Boolean) is
   begin
      Object.Current := Retired; Object.Output := (others => <>);
      Confirmed := not Object.Has_Grant;
      if Object.Has_Grant then
         if not Object.Revocation_Requested then Grants.Revoke (Object.Grant, Object.Revocation_Requested); end if;
         Confirmed := Grants.Retirement_Confirmed (Object.Grant);
         if Confirmed then Object.Has_Grant := False; end if;
      end if;
   end Retire;
end Config_Object_Client;

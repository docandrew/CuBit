with CuBit.Filesystems; use CuBit.Filesystems;

package body Storage_Channel is
   use CuBit.Messages;
   package Requests renames CuBit.Async_Requests;

   function Status (Object : Channel) return Phase is (Object.Current);
   function Pending_Token (Object : Channel) return Unsigned_64 is
     (if Object.Current = Retired then 0 else Requests.Pending_Token (Object.Request_Lifetime));

   procedure Initialize
     (Object : in out Channel; Endpoint : CapabilitySlot;
      Success : out Boolean)
   is
   begin
      Success := False;
      if Object.Current /= Fresh then return; end if;
      Object.Current := Failed;
      Object.Endpoint := Endpoint;
      CuBit.Memory_Grants.Create_Via_Capability
        (Endpoint, Object.Buffer'Address, Transfer_Bytes / 4096, True, Object.Grant, Success);
      if Success then
         Object.Has_Grant := True;
         Object.Current := Ready;
      end if;
   end Initialize;

   procedure Submit_With_Payload
     (Object : in out Channel; Op : Operation; Handle, Position : Unsigned_64;
      Input_Length, Read_Length : Natural; Token : Unsigned_64;
      Result : out Submission)
   is
      Request : Message := NULL_MESSAGE;
      Reserved, Accepted : Boolean;
   begin
      Result := Unavailable;
      if Object.Current in Waiting | Result_Ready then Result := Busy; return; end if;
      if Object.Current /= Ready and then
        not (Object.Current = Failed and Object.Has_Grant and Op = Close)
      then return; end if;
      Result := Invalid_Request;
      if not Requests.Can_Reserve (Object.Request_Lifetime, Token) then return; end if;
      if (Op not in Open_Existing | Open_Create and Handle = 0) or else
        Input_Length > Object.Buffer'Length or else Read_Length > Object.Buffer'Length
      then return; end if;
      case Op is
         when Open_Existing | Open_Create =>
            if Input_Length = 0 or else Input_Length > MAXIMUM_PATH_BYTES or else
              Read_Length /= 0 then return; end if;
            Request := Open_Request
              (Object.Grant, Input_Length, OPEN_READ_WRITE or OPEN_DENY_SHARING or
               (if Op = Open_Create then CuBit.Filesystems.OPEN_CREATE else 0));
         when Write_Data =>
            if Input_Length = 0 or else Read_Length /= 0 then return; end if;
            Request := Write_At_Request
              (File_Handle (Handle), Object.Grant, Unsigned_64 (Input_Length), Position);
         when Read_Data =>
            if Input_Length /= 0 or else Read_Length = 0 then return; end if;
            Request := Read_At_Request
              (File_Handle (Handle), Object.Grant, Unsigned_64 (Read_Length), Position);
         when others =>
            if Input_Length /= 0 or else Read_Length /= 0 then return; end if;
            case Op is
               when Close => Request := Close_Request (File_Handle (Handle));
               when Size => Request := Seek_Request (File_Handle (Handle), 0, From_End);
               when Resize => Request := Resize_Request (File_Handle (Handle), Position);
               when Flush => Request := Flush_Request (File_Handle (Handle));
               when others => null;
            end case;
      end case;
      if Input_Length > 0 then
         Fill (Object.Buffer (1 .. Input_Length));
      end if;
      --  Burn the correlation token even on definite queue rejection.
      Requests.Reserve (Object.Request_Lifetime, Token, Reserved);
      if not Reserved then Result := Invalid_Request; return; end if;
      Accepted := capSubmit (Object.Endpoint, Request, Token);
      Requests.Submitted (Object.Request_Lifetime, Accepted);
      if not Accepted then
         Result := Not_Submitted;
         return;
      end if;
      Object.Pending_Operation := Op;
      Object.Length := (if Op = Read_Data then Read_Length else Input_Length);
      Object.Current := Waiting;
      Result := Submitted;
   end Submit_With_Payload;

   procedure Submit
     (Object : in out Channel; Op : Operation; Handle, Position : Unsigned_64;
      Input : String; Read_Length : Natural; Token : Unsigned_64;
      Result : out Submission)
   is
      procedure Copy (Buffer : out Transfer_Buffer) is
      begin
         Buffer := Transfer_Buffer (Input);
      end Copy;
      procedure Send is new Submit_With_Payload (Copy);
   begin
      Send (Object, Op, Handle, Position, Input'Length, Read_Length, Token, Result);
   end Submit;

   procedure Complete
     (Object : in out Channel; Completion : CompletionEntry;
      Result : out Completion_Result)
   is
      Op : constant Operation := Object.Pending_Operation;
      Reply : constant Message := Completion.msg;
      Accepted : Boolean;
   begin
      Result := Ignored;
      if Object.Current /= Waiting then return; end if;
      Requests.Capture (Object.Request_Lifetime, Completion.token, Completion.valid, Accepted);
      if not Accepted then return; end if;
      Result := Completed;
      Object.Current := Result_Ready;
      Object.Reply_Value := 0;
      Object.Reply_Code := REPLY_IO_ERROR;
      if Completion.requestId = 0 or else Completion.status /= COMPLETION_OK or else
        Reply.tag.length /=
          (if Op in Open_Existing | Open_Create and Reply.tag.label = REPLY_OK then 2 else 1)
        or else Reply.tag.flags /= 0 or else Reply.tag.reserved /= 0
      then
         Object.Poisoned := True;
         return;
      end if;
      if Reply.tag.label /= REPLY_OK then
         Object.Reply_Code := Reply.tag.label;
         if Op not in Open_Existing | Open_Create or else
           Reply.tag.label not in REPLY_NOT_FOUND | REPLY_ACCESS_DENIED |
             REPLY_SHARING_VIOLATION | REPLY_NO_SPACE | REPLY_READ_ONLY
         then Object.Poisoned := True; end if;
         return;
      end if;
      if (Op in Read_Data | Write_Data and Reply.words (0) > Unsigned_64 (Object.Length)) or else
        (Op in Close | Resize | Flush and Reply.words (0) /= 0) or else
        (Op in Open_Existing | Open_Create and Reply.words (0) = 0)
      then Object.Poisoned := True; return; end if;
      Object.Reply_Code := REPLY_OK;
      Object.Reply_Value := Reply.words (0);
   end Complete;

   procedure Take_Result
     (Object : in out Channel; Output : in out String;
      Code : out Unsigned_32; Value : out Unsigned_64; Taken : out Boolean)
   is
   begin
      Code := REPLY_RECOVERY_REQUIRED;
      Value := 0;
      Taken := False;
      if Object.Current /= Result_Ready then return; end if;
      if Object.Pending_Operation = Read_Data and Object.Reply_Code = REPLY_OK then
         if Object.Reply_Value > Unsigned_64 (Output'Length) then
            Code := REPLY_ERR;
            return;
         end if;
         --  Slice assignment slides indices; no assumption that Output starts
         --  at one, and no borrowed pointer survives this call.
         if Object.Reply_Value > 0 then
            Output (Output'First .. Output'First + (Natural (Object.Reply_Value) - 1)) :=
              String (Object.Buffer (1 .. Natural (Object.Reply_Value)));
         end if;
      end if;
      Code := Object.Reply_Code;
      Value := Object.Reply_Value;
      Taken := True;
      Requests.Release (Object.Request_Lifetime);
      Object.Current := (if Object.Poisoned then Failed else Ready);
   end Take_Result;

   procedure Retire (Object : in out Channel; Retirement_Confirmed : out Boolean) is
   begin
      Object.Current := Retired;
      Requests.Stop (Object.Request_Lifetime);
      Retirement_Confirmed := not Object.Has_Grant;
      if Object.Has_Grant then
         if not Object.Revocation_Requested then
            CuBit.Memory_Grants.Revoke (Object.Grant, Object.Revocation_Requested);
         end if;
         --  Repeated revoke may reject an already inactive reference. Only
         --  the owned-generation query establishes completed retirement.
         Retirement_Confirmed :=
           CuBit.Memory_Grants.Retirement_Confirmed (Object.Grant);
         if Retirement_Confirmed then Object.Has_Grant := False; end if;
      end if;
   end Retire;
end Storage_Channel;

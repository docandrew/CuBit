pragma Ada_2022;
------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Userspace Config Store Service
--
--  Hierarchical key-value store with capability-gated, per-app isolated
--  access. Follows the same IPC loop + ACL pattern as the FS server.
--
--  Capability slots:
--    7 = CAP_NOTIFICATION for DRIVER_CONFIG registration
--   15 = CAP_ENDPOINT to devmgr (for OP_READY signal)
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System; use System;

with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;
with CuBit.Config_Inspection;
with Config_Authority;
with Config_Store;
with CuBit.Config_Protocol;

procedure main is
   use ASCII;

   --  IPC operation labels
   OP_SET_ACL       : constant Unsigned_32 := 16#0080#;
   OP_REVOKE_ACL    : constant Unsigned_32 := 16#0081#;
   REPLY_OK            : constant Unsigned_32 := 16#F000#;
   REPLY_ERR           : constant Unsigned_32 := 16#F001#;
   REPLY_ACCESS_DENIED : constant Unsigned_32 := 16#F007#;

   PAGE_SIZE : constant := 4096;

   MAX_KEY_LEN : constant := Config_Store.Maximum_Key;
   store : Config_Store.State;

   ---------------------------------------------------------------------------
   --  Per-process ACL infrastructure (same pattern as FS server)
   ---------------------------------------------------------------------------

   ACL_READ : constant Config_Authority.Operation := Config_Authority.Read_Config;
   ACL_WRITE : constant Config_Authority.Operation := Config_Authority.Write_Config;
   Authorities : Config_Authority.Authority_State;

   --  Resolve administrative roles on each check. These operations are
   --  control-plane traffic, and a live registry lookup avoids turning a
   --  cached raw PID into authority if its original process dies.
   function isAdmin (sender : ProcessID) return Boolean is
      devmgrAdmin : constant Unsigned_64 :=
        getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_DEVMGR);
      procmgrAdmin : constant Unsigned_64 :=
        getInfo (SYSINFO_REGISTERED_DRIVER, DRIVER_PROCMGR);
   begin
      return
        (devmgrAdmin /= 0 and then devmgrAdmin /= Unsigned_64'Last and then
         sender = devmgrAdmin) or else
        (procmgrAdmin /= 0 and then procmgrAdmin /= Unsigned_64'Last and then
         sender = procmgrAdmin);
   end isAdmin;

   --  Check if sender has access rights for the given key
   function checkAccess
     (sender : ProcessID;
      key    : String;
      rights : Config_Authority.Operation) return Boolean
   is
      use type Config_Authority.Operation;
   begin
      if isAdmin (sender) then
         return True;
      end if;

      if key'Length >= 11 and then key (key'First .. key'First + 10) = "clock.boot-"
        and then rights = ACL_WRITE
      then
         return False;
      end if;

      return Config_Authority.Allows (Authorities, sender, key, rights);
   end checkAccess;

   --  Send a reply with the given label and word0 value
   procedure sendReply
     (dest   : ProcessID;
      label  : Unsigned_32;
      word0  : Unsigned_64)
   is
      replyMsg : Message;
      ignore   : Unsigned_64;
   begin
      replyMsg.tag := (label  => label,
                       length => 1,
                       flags  => 0,
                       reserved  => 0);
      replyMsg.words := (0 => word0, others => 0);
      ignore := reply (dest, replyMsg);
   end sendReply;

   ---------------------------------------------------------------------------
   --  Handle OP_SET_ACL using a checked Config grant reference
   --  words(0) = target PID
   --  words(1) = entry count (0 = wildcard full access)
   --  words(2..3) = grant slot and generation (zero when count = 0)
   ---------------------------------------------------------------------------
   procedure handleSetACL (sender : ProcessID; msg : Message) is
      use type Config_Authority.Install_Result;
      Candidate : Config_Authority.Rule_Set;
      Result : Config_Authority.Install_Result;
      Accepted, Acquired, Returned : Boolean;
      Reference : CuBit.Memory_Grants.Grant_Reference;
      Address : System.Address;
      Count : Natural;
   begin
      if not isAdmin (sender) then
         sendReply (sender, REPLY_ACCESS_DENIED, 0); return;
      end if;
      if msg.tag.length /= 4 or else msg.words (0) = NO_PROCESS or else
        msg.words (0) = Unsigned_64'Last or else
        msg.words (1) > Config_Authority.Maximum_Rules
      then sendReply (sender, REPLY_ERR, 0); return; end if;
      Count := Natural (msg.words (1));
      if Count = 0 then
         if msg.words (2) /= 0 or msg.words (3) /= 0 then
            sendReply (sender, REPLY_ERR, 0); return;
         end if;
         Config_Authority.Append (Candidate, "", Config_Authority.Read_Write, Accepted);
      else
         if msg.words (2) > CuBit.Memory_Grants.MAXIMUM_GLOBAL_SLOT or else
           msg.words (3) not in CuBit.Memory_Grants.Grant_Generation
         then sendReply (sender, REPLY_ERR, 0); return; end if;
         Reference := (slot => msg.words (2), generation => msg.words (3));
         CuBit.Memory_Grants.Acquire
           (Reference, sender, 0, Unsigned_64 (Count * 72),
            CuBit.Memory_Grants.Read_Access, Address, Acquired);
         if not Acquired then sendReply (sender, REPLY_ERR, 0); return; end if;
         declare
            Shared : String (1 .. Count * 72) with Import, Address => Address;
            -- Snapshot once; subsequent authorization never rereads client bytes.
            Data : constant String := Shared;
            Base, Length, Mask : Natural;
         begin
            CuBit.Memory_Grants.Return_Acquisition (Reference, Returned);
            if not Returned then sendReply (sender, REPLY_ERR, 0); return; end if;
            for I in 0 .. Count - 1 loop
               Base := I * 72;
               Mask := Character'Pos (Data (Base + 1));
               Length := Character'Pos (Data (Base + 2));
               if Length > Config_Authority.Maximum_Scope or else Mask > 3 or else
                 (for some J in Base + 3 .. Base + 8 => Data (J) /= ASCII.NUL)
               then sendReply (sender, REPLY_ERR, 0); return; end if;
               Config_Authority.Append
                 (Candidate, Data (Base + 9 .. Base + 8 + Length),
                  (Config_Authority.Read_Config => Mask mod 2 = 1,
                   Config_Authority.Write_Config => Mask >= 2), Accepted);
               if not Accepted then sendReply (sender, REPLY_ERR, 0); return; end if;
            end loop;
         end;
      end if;
      Config_Authority.Install (Authorities, msg.words (0), Candidate, Result);
      if Result = Config_Authority.Installed then
         debugPrint ("Config: ACL set for PID" & LF);
         sendReply (sender, REPLY_OK, 0);
      else sendReply (sender, REPLY_ERR, 0);
      end if;
   end handleSetACL;

   --  Handle OP_REVOKE_ACL
   --  words(0) = target PID
   procedure handleRevokeACL (sender : ProcessID; msg : Message) is
      targetPID : constant ProcessID := msg.words (0);
   begin
      if not isAdmin (sender) then
         sendReply (sender, REPLY_ACCESS_DENIED, 0);
         return;
      end if;

      if msg.tag.length /= 1 then sendReply (sender, REPLY_ERR, 0); return; end if;
      Config_Authority.Revoke (Authorities, targetPID);

      sendReply (sender, REPLY_OK, 0);
   end handleRevokeACL;

   ---------------------------------------------------------------------------
   --  KV store helpers
   ---------------------------------------------------------------------------

   --  Check if key starts with "system/" prefix (7 chars)
   function isSystemKey (key : String) return Boolean is
   begin
      if key'Length < 7 then
         return False;
      end if;
      return key (key'First)     = 's' and then
             key (key'First + 1) = 'y' and then
             key (key'First + 2) = 's' and then
             key (key'First + 3) = 't' and then
             key (key'First + 4) = 'e' and then
             key (key'First + 5) = 'm' and then
             key (key'First + 6) = '/';
   end isSystemKey;

   --  Inspector protocol: bounded owned text, no raw slot-to-address math.
   --  words = (slot, generation, key length, context).
   procedure handleInspection (sender : ProcessID; msg : Message) is
      use CuBit.Config_Inspection;
      Reference : CuBit.Memory_Grants.Grant_Reference;
      Address : System.Address;
      Acquired, Returned : Boolean;
      Result : Status := OK;
      Output : Text;
      Key_Length : Natural;
      Stored_Value : Config_Store.Value_Text;
      Found : Boolean;
      procedure Respond (Value : Status; Length : Natural := 0) is
      begin
         sendReply (sender, Status'Enum_Rep (Value), Unsigned_64 (Length));
      end Respond;
   begin
      if msg.tag.length /= 4 or else
        msg.words (3) /= Unsigned_64 (Machine_Context)
      then Respond (Invalid_Request); return; end if;
      if msg.tag.label = Operation'Enum_Rep (Probe) then
         -- A global inspector must have an explicit wildcard read grant.
         Respond ((if checkAccess (sender, "", ACL_READ) then OK else Denied));
         return;
      end if;
      if msg.words (0) > CuBit.Memory_Grants.MAXIMUM_GLOBAL_SLOT or else
        msg.words (1) not in CuBit.Memory_Grants.Grant_Generation or else
        msg.words (2) > MAX_KEY_LEN
      then Respond (Invalid_Request); return; end if;
      Key_Length := Natural (msg.words (2));
      if msg.tag.label = Operation'Enum_Rep (Read_Value) and Key_Length = 0 then
         Respond (Invalid_Request); return;
      end if;
      Reference := (slot => msg.words (0), generation => msg.words (1));
      CuBit.Memory_Grants.Acquire
        (Reference, sender, 0, PAGE_SIZE, CuBit.Memory_Grants.Write_Access,
         Address, Acquired);
      if not Acquired then Respond (Invalid_Request); return; end if;
      declare
         Buffer : String (1 .. PAGE_SIZE) with Import, Address => Address;
         Key : constant String := Buffer (1 .. Key_Length);
      begin
         if not checkAccess (sender, Key, ACL_READ) then
            Result := Denied;
         elsif msg.tag.label = Operation'Enum_Rep (Read_Value) then
            Config_Store.Read (store, Key, Stored_Value, Found);
            if not Found then Result := Missing;
            elsif Stored_Value.Length > Maximum_Text then Result := Too_Large;
            else
               Output.Length := Stored_Value.Length;
               Output.Data (1 .. Output.Length) :=
                 Stored_Value.Data (1 .. Output.Length);
            end if;
         else
            for I in Config_Store.Slot loop
               declare
                  Item : constant Config_Store.Key_Text := Config_Store.Key_At (store, I);
               begin
                  if Item.Length > 0 and then Contains (Key, Item.Data (1 .. Item.Length))
                    and then checkAccess (sender, Item.Data (1 .. Item.Length), ACL_READ)
                  then
                     if Item.Length + 1 > Maximum_Text - Output.Length then
                        Result := Too_Large; exit;
                     end if;
                     Output.Data (Output.Length + 1 .. Output.Length + Item.Length) :=
                       Item.Data (1 .. Item.Length);
                     Output.Length := Output.Length + Item.Length + 1;
                     Output.Data (Output.Length) := ASCII.LF;
                  end if;
               end;
            end loop;
         end if;
         if Result = OK then Buffer (1 .. Output.Length) := Output.Data (1 .. Output.Length); end if;
      end;
      CuBit.Memory_Grants.Return_Acquisition (Reference, Returned);
      if not Returned then Result := Unavailable; end if;
      Respond (Result, (if Result = OK then Output.Length else 0));
   end handleInspection;

   -- All data messages carry an owned, generation-bearing grant reference.
   procedure Handle_Data
     (Sender : ProcessID; Msg : Message; Op : CuBit.Config_Protocol.Operation)
   is
      use CuBit.Config_Protocol;
      use type Config_Store.Update_Result;
      Bounds : Request_Bounds;
      Valid, Acquired, Returned : Boolean;
      Reference : CuBit.Memory_Grants.Grant_Reference;
      Address : System.Address;
      Input : String (1 .. Maximum_Key + Maximum_Value);
      Output : String (1 .. Maximum_Value) := [others => ASCII.NUL];
      Output_Length, Count : Natural := 0;
      Stored_Value : Config_Store.Value_Text;
      Found : Boolean;
      Update : Config_Store.Update_Result;
      Status : Unsigned_32 := REPLY_OK;
      Writes_Output : constant Boolean := Op in Get_Value | List_Keys;
   begin
      Decode (Op, Natural (Msg.tag.length), Msg.words (2), Msg.words (3), Bounds, Valid);
      if not Valid or else Msg.words (0) > CuBit.Memory_Grants.MAXIMUM_GLOBAL_SLOT or else
        Msg.words (1) not in CuBit.Memory_Grants.Grant_Generation
      then sendReply (Sender, REPLY_ERR, 0); return; end if;
      Reference := (slot => Msg.words (0), generation => Msg.words (1));
      CuBit.Memory_Grants.Acquire
        (Reference, Sender, 0, Unsigned_64 (Bounds.Mapping_Bytes),
         (if Writes_Output then CuBit.Memory_Grants.Write_Access else CuBit.Memory_Grants.Read_Access),
         Address, Acquired);
      if not Acquired then sendReply (Sender, REPLY_ERR, 0); return; end if;
      declare
         Shared : String (1 .. Bounds.Mapping_Bytes) with Import, Address => Address;
      begin
         Input (1 .. Bounds.Input_Bytes) := Shared (1 .. Bounds.Input_Bytes);
         if not Writes_Output then
            CuBit.Memory_Grants.Return_Acquisition (Reference, Returned);
            if not Returned then sendReply (Sender, REPLY_ERR, 0); return; end if;
         end if;
         declare
            Key : constant String := Input (1 .. Bounds.Key);
         begin
            if not checkAccess (Sender, Key, (if Writes_Output then ACL_READ else ACL_WRITE)) or else
              (not Writes_Output and then isSystemKey (Key) and then not isAdmin (Sender))
            then Status := REPLY_ACCESS_DENIED;
            else
               case Op is
                  when Get_Value =>
                     Config_Store.Read (store, Key, Stored_Value, Found);
                     if not Found then Status := 16#F060#;
                     else
                        Output_Length := Stored_Value.Length;
                        Output (1 .. Output_Length) := Stored_Value.Data (1 .. Output_Length);
                     end if;
                  when Set_Value =>
                     Config_Store.Put
                       (store, Key, Input (Bounds.Key + 1 .. Bounds.Input_Bytes), Update);
                     if Update /= Config_Store.Stored then Status := REPLY_ERR; end if;
                  when Delete_Value =>
                     Config_Store.Remove (store, Key, Found);
                     if not Found then Status := 16#F060#; end if;
                  when List_Keys =>
                     for I in Config_Store.Slot loop
                        declare
                           Item : constant Config_Store.Key_Text := Config_Store.Key_At (store, I);
                        begin
                           if Item.Length > 0 and then CuBit.Config_Inspection.Contains
                             (Key, Item.Data (1 .. Item.Length)) and then
                             checkAccess (Sender, Item.Data (1 .. Item.Length), ACL_READ)
                           then
                              if Item.Length + 1 > Maximum_Value - Output_Length then
                                 Status := 16#F061#; exit;
                              end if;
                              Output (Output_Length + 1 .. Output_Length + Item.Length) :=
                                Item.Data (1 .. Item.Length);
                              Output_Length := Output_Length + Item.Length + 1;
                              Output (Output_Length) := ASCII.NUL;
                              Count := Count + 1;
                           end if;
                        end;
                     end loop;
               end case;
            end if;
         end;
         if Writes_Output then
            if Status = REPLY_OK then Shared (1 .. Output_Length) := Output (1 .. Output_Length); end if;
            CuBit.Memory_Grants.Return_Acquisition (Reference, Returned);
            if not Returned then Status := REPLY_ERR; end if;
         end if;
      end;
      sendReply (Sender, Status,
        (if Status /= REPLY_OK then 0 elsif Op = List_Keys then Unsigned_64 (Count)
         else Unsigned_64 (Output_Length)));
   end Handle_Data;

   ---------------------------------------------------------------------------
   --  Main message loop variables
   ---------------------------------------------------------------------------
   sender : ProcessID;
   msg    : Message;
begin
   debugPrint ("Config: starting..." & LF);

   --  Register as DRIVER_CONFIG so other services can discover us
   declare
      ignore : Unsigned_64;
   begin
      ignore := registerDriver (DRIVER_CONFIG);
   end;

   --  Signal devmgr that we are ready
   declare
      CAP_SLOT_READY : constant Unsigned_64 := 15;
      OP_READY       : constant Unsigned_32 := 16#FF00#;
      ignore : MessageTag;
   begin
      ignore := capSend (CAP_SLOT_READY,
         (tag      => (label => OP_READY, length => 0,
                       flags => 0, reserved => 0),
          authorityTag => 0,
          words    => (others => 0)));
   end;

   debugPrint ("Config: entering message loop" & LF);

   --  Main IPC message loop
   loop
      receive (sender, msg);

      case msg.tag.label is
         when CuBit.Config_Inspection.Operation'Enum_Rep (CuBit.Config_Inspection.Read_Value) |
              CuBit.Config_Inspection.Operation'Enum_Rep (CuBit.Config_Inspection.List_Keys) |
              CuBit.Config_Inspection.Operation'Enum_Rep (CuBit.Config_Inspection.Probe) =>
            handleInspection (sender, msg);
         when CuBit.Config_Protocol.Operation'Enum_Rep (CuBit.Config_Protocol.Get_Value) =>
            Handle_Data (sender, msg, CuBit.Config_Protocol.Get_Value);
         when CuBit.Config_Protocol.Operation'Enum_Rep (CuBit.Config_Protocol.Set_Value) =>
            Handle_Data (sender, msg, CuBit.Config_Protocol.Set_Value);
         when CuBit.Config_Protocol.Operation'Enum_Rep (CuBit.Config_Protocol.Delete_Value) =>
            Handle_Data (sender, msg, CuBit.Config_Protocol.Delete_Value);
         when CuBit.Config_Protocol.Operation'Enum_Rep (CuBit.Config_Protocol.List_Keys) =>
            Handle_Data (sender, msg, CuBit.Config_Protocol.List_Keys);
         when OP_SET_ACL =>
            handleSetACL (sender, msg);
         when OP_REVOKE_ACL =>
            handleRevokeACL (sender, msg);
         when others =>
            sendReply (sender, REPLY_ERR, 0);
      end case;
   end loop;
end main;

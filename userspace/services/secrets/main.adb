------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  secrets.svc v0 (SEC-022): the secret store. Named secrets, used through
--  grants instead of environment variables, .env files, argv or settings
--  (CuBit.Secrets_Protocol).
--
--  @description
--  Values live in this process's memory only; a reboot forgets them, and
--  encryption at rest (TPM-sealed on the laptop) is a later slice. A value
--  is stored by a holder of the "secrets" endpoint (the console's
--  non-echoing (secrets.set ...)) and is never read back by it. Use hands
--  the value to the holder of the secret's use grant: a registered service
--  role, checked against the kernel's service registry, granted by this
--  program's typed startup parameters (the trusted plan) and revocable
--  until the next boot. Values are never logged; names and uses are.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System;
with CuBit.Log;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;
with CuBit.Canonical_Readers;
with CuBit.Llm_Protocol;
with CuBit.Secrets_Protocol;
with Parameters;
with Startup;

procedure Main is
   package S_P renames CuBit.Secrets_Protocol;

   --  How many secrets v0 keeps (a named ceiling: the store refuses Full).
   Maximum_Secrets : constant := 16;
   subtype Secret_Index is Natural range 1 .. Maximum_Secrets;

   type Secret is record
      Used : Boolean := False;
      Name : String (1 .. S_P.Maximum_Name_Bytes) := [others => ' '];
      Name_Length : Natural range 0 .. S_P.Maximum_Name_Bytes := 0;
      Stored : Boolean := False;
      Value : String (1 .. S_P.Maximum_Value_Bytes) := [others => ' '];
      Value_Length : Natural range 0 .. S_P.Maximum_Value_Bytes := 0;
      --  The registered service role that may Use it (0: none).
      Holder : Unsigned_64 := 0;
   end record;
   Secrets : array (Secret_Index) of Secret;

   --  The holder roles a startup parameter can name.
   function Role_Of (Holder : Parameters.Secret_Holder_Value) return Unsigned_64 is
     (case Holder is
         when Parameters.Llm_Service => CuBit.Llm_Protocol.Service_Role);

   function Find (Name : String) return Natural is
   begin
      for I in Secret_Index loop
         if Secrets (I).Used and then Secrets (I).Name (1 .. Secrets (I).Name_Length) = Name then
            return I;
         end if;
      end loop;
      return 0;
   end Find;

   --  The entry for Name, made when absent (0: the table is full).
   function Entry_For (Name : String) return Natural is
      Found : constant Natural := Find (Name);
   begin
      if Found /= 0 then
         return Found;
      end if;
      for I in Secret_Index loop
         if not Secrets (I).Used then
            Secrets (I) := (Used => True, Name_Length => Name'Length, others => <>);
            Secrets (I).Name (1 .. Name'Length) := Name;
            return I;
         end if;
      end loop;
      return 0;
   end Entry_For;

   procedure Wipe (S : in out Secret) is
   begin
      S.Value := [others => ' '];
      S.Value_Length := 0;
      S.Stored := False;
   end Wipe;

   Client_Now : Process_ID := No_Process;

   procedure Send_Current (Label : Unsigned_32; Length : Unsigned_8;
                           W0 : Unsigned_64 := 0; W1 : Unsigned_64 := 0) is
      Ignore : Unsigned_64;
   begin
      Ignore := reply
        (Client_Now,
         (tag => (label => Label, length => Length, protocol => 0, ipc => <>, reserved => 0),
          authorityTag => 0, words => [W0, W1, 0, 0]));
   end Send_Current;

   procedure Refuse (Why : S_P.Refusal) is
   begin
      Send_Current (S_P.Reply_Error, 1, Unsigned_64 (S_P.Refusal'Enum_Rep (Why)));
   end Refuse;

   function Image (Value : Unsigned_64) return String is
      Text : constant String := Value'Image;
   begin
      return Text (Text'First + 1 .. Text'Last);
   end Image;

   --  Every call: the client's page with the name (then a value for Store),
   --  lent writable so the value can be wiped (Store) or written (Use).
   procedure Serve (From : Process_ID; Request : Message) is
      Reference : constant CuBit.Memory_Grants.Grant_Reference :=
        (slot => Request.words (0), generation => Request.words (1));
      Name_Length : constant Unsigned_64 := Request.words (2);
      Value_Length : constant Unsigned_64 :=
        (if Request.tag.label = S_P.Store_Operation then Request.words (3) else 0);
      Span : constant Unsigned_64 :=
        (if Request.tag.label = S_P.Use_Operation then S_P.Maximum_Value_Bytes
         else Name_Length + Value_Length);
      Address : System.Address;
      Acquired, Returned : Boolean;
   begin
      if Name_Length not in 1 .. S_P.Maximum_Name_Bytes
        or else Value_Length > S_P.Maximum_Value_Bytes
        or else Span < Name_Length
      then
         Refuse (S_P.Malformed_Request);
         return;
      end if;
      CuBit.Memory_Grants.Acquire
        (Reference, From, 0, Span, CuBit.Memory_Grants.Write_Access, Address, Acquired);
      if not Acquired then
         Refuse (S_P.Malformed_Request);
         return;
      end if;
      declare
         Page : String (1 .. Natural (Span)) with Import, Address => Address;
         Name : constant String := Page (1 .. Natural (Name_Length));
         Index : Natural;
      begin
         if not S_P.Valid_Name (Name) then
            Page := [others => ' '];
            CuBit.Memory_Grants.Return_Acquisition (Reference, Returned);
            Refuse (S_P.Bad_Name);
            return;
         end if;
         case Request.tag.label is
            when S_P.Store_Operation =>
               declare
                  Value : constant String := Page (Natural (Name_Length) + 1 .. Page'Last);
               begin
                  if not S_P.Valid_Value (Value) then
                     Page := [others => ' '];
                     CuBit.Memory_Grants.Return_Acquisition (Reference, Returned);
                     Refuse (S_P.Bad_Value);
                     return;
                  end if;
                  Index := Entry_For (Name);
                  if Index = 0 then
                     Page := [others => ' '];
                     CuBit.Memory_Grants.Return_Acquisition (Reference, Returned);
                     Refuse (S_P.Full);
                     return;
                  end if;
                  Wipe (Secrets (Index));
                  Secrets (Index).Value (1 .. Value'Length) := Value;
                  Secrets (Index).Value_Length := Value'Length;
                  Secrets (Index).Stored := True;
               end;
               --  The storer's copy goes at once.
               Page := [others => ' '];
               CuBit.Memory_Grants.Return_Acquisition (Reference, Returned);
               CuBit.Log.Info ("secrets: stored " & Name & " for process " & Image (To_Word (From)));
               Send_Current (S_P.Reply_OK, 0);
            when S_P.Forget_Operation =>
               CuBit.Memory_Grants.Return_Acquisition (Reference, Returned);
               Index := Find (Name);
               if Index /= 0 then
                  Wipe (Secrets (Index));
                  CuBit.Log.Info ("secrets: forgot " & Name);
               end if;
               Send_Current (S_P.Reply_OK, 0);
            when S_P.Status_Operation =>
               CuBit.Memory_Grants.Return_Acquisition (Reference, Returned);
               Index := Find (Name);
               Send_Current (S_P.Reply_OK, 2,
                             (if Index /= 0 and then Secrets (Index).Stored then 1 else 0),
                             (if Index /= 0 then Secrets (Index).Holder else 0));
            when S_P.Revoke_Use_Operation =>
               CuBit.Memory_Grants.Return_Acquisition (Reference, Returned);
               Index := Find (Name);
               if Index /= 0 and then Secrets (Index).Holder /= 0 then
                  CuBit.Log.Info ("secrets: use grant for " & Name & " revoked from role " &
                                  Image (Secrets (Index).Holder));
                  Secrets (Index).Holder := 0;
               end if;
               Send_Current (S_P.Reply_OK, 0);
            when S_P.Use_Operation =>
               Index := Find (Name);
               Page := [others => ' '];
               if Index = 0 or else Secrets (Index).Holder = 0
                 or else Registered_Driver (Secrets (Index).Holder) /= From
               then
                  CuBit.Memory_Grants.Return_Acquisition (Reference, Returned);
                  CuBit.Log.Warning ("secrets: use of " & Name & " refused to process " &
                                     Image (To_Word (From)));
                  Refuse (S_P.Not_Granted);
               elsif not Secrets (Index).Stored then
                  CuBit.Memory_Grants.Return_Acquisition (Reference, Returned);
                  Refuse (S_P.Not_Stored);
               else
                  declare
                     S : Secret renames Secrets (Index);
                  begin
                     Page (1 .. S.Value_Length) := S.Value (1 .. S.Value_Length);
                     CuBit.Memory_Grants.Return_Acquisition (Reference, Returned);
                     CuBit.Log.Info ("secrets: " & Name & " used by role " & Image (S.Holder) &
                                     " process " & Image (To_Word (From)));
                     Send_Current (S_P.Reply_OK, 1, Unsigned_64 (S.Value_Length));
                  end;
               end if;
            when others =>
               CuBit.Memory_Grants.Return_Acquisition (Reference, Returned);
               Refuse (S_P.Malformed_Request);
         end case;
      end;
   end Serve;

   From : Process_ID;
   Request : Message;
   Found : Boolean;
   Ignore : Unsigned_64;
begin
   CuBit.Log.Info ("secrets: starting (v0: in memory only, not encrypted at rest)");
   declare
      Granted : constant String := CuBit.Canonical_Readers.Image (Parameters.Value.grant.secret);
      Index : Natural;
   begin
      if not S_P.Valid_Name (Granted) then
         CuBit.Log.Warning ("secrets: the granted secret's name is invalid; no grant");
      else
         Index := Entry_For (Granted);
         if Index /= 0 then
            Secrets (Index).Holder := Role_Of (Parameters.Value.grant.holder);
            CuBit.Log.Info ("secrets: " & Granted & " may be used by role " &
                            Image (Secrets (Index).Holder));
         end if;
      end if;
   end;
   Ignore := registerDriver (S_P.Service_Role, Startup.Registration);
   if Ignore = Unsigned_64'Last then
      CuBit.Log.Warning ("secrets: registration failed");
      return;
   end if;
   CuBit.Log.Info ("secrets: ready");
   loop
      Poll_Service_Request (From, Request, Found);
      Client_Now := From;
      if Found then
         case Request.tag.label is
            when S_P.Store_Operation | S_P.Forget_Operation | S_P.Status_Operation |
                 S_P.Use_Operation | S_P.Revoke_Use_Operation =>
               Serve (From, Request);
            when others =>
               Refuse (S_P.Malformed_Request);
         end case;
      elsif Wait_For_Activity_Until (Unsigned_64'Last) = Unavailable then
         CuBit.Log.Warning ("secrets: activity wait unavailable");
         return;
      end if;
   end loop;
end Main;

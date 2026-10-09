with Ada.Command_Line;
with Ada.Text_IO;
with GNAT.Source_Info;
with Config_Worker_Channel;
with System;
with Interfaces; use Interfaces;
with CCL.Types;
with CCL.Objects.Persistence;
with CCL.Objects.Schemas;
with Config_Authority;
with Config_Collections;
with Config_Objects;
with Config_Object_Service;
with Config_Object_Receiver;
with Config_Object_Messages;
with Config_Database;
with Config_Database.Schemas;
with Config_Schema_Protocol;
with Config_Worker_Protocol;
with Config_Worker_Storage;
with Config_Worker_Messages;
with Config_Worker_Receiver;
with CuBit.Messages;
with CuBit.Memory_Grants;

--  Linux-hosted real Turso; native production store/channel/receiver, but
--  modeled IPC/grants. Not a live Config service or a power-cut simulation.
procedure Typed_Store_Turso is
   package W renames Config_Object_Messages;
   package A renames Config_Authority;
   package C renames Config_Collections;
   package V renames Config_Objects;
   package P renames Config_Worker_Protocol;
   package Codec renames CCL.Objects.Persistence;
   package Grants renames CuBit.Memory_Grants;
   package IPC renames CuBit.Messages;
   use type System.Address;
   use type IPC.Message;
   use type A.Install_Result;
   use type C.Result;
   use type V.Outcome;
   use type V.Read_Result;
   use type CCL.Objects.Image;
   use type CCL.Objects.Build_Result;
   App_Input : W.Frame;
   App_Metadata : CCL.Objects.Schemas.Image;
   App_Reply : IPC.Message;
   App_Current, App_Saved, App_Borrowed, Staged : Boolean := False;
   App_Sender : IPC.Process_ID := 0;
   App_Replies : Natural := 0;
   Allow_Delivery : Boolean := True;
   Authority : A.Authority_State;
   Rules, Read_Rules : A.Rule_Set;
   Types : CCL.Types.Registry;
   Contract : CCL.Objects.Binding;
   Schema : constant CCL.Objects.Schema_Key := [1, 2, 3, 4];
   First, Second : CCL.Objects.Image;
   ID : C.Collection_ID;
   Handle : C.Handle;
   Access_Result : C.Result;
   Installed : A.Install_Result;
   Built : CCL.Objects.Build_Result;
   Result : V.Outcome;
   Good : Boolean;
   Response, Old_Response : IPC.CompletionEntry;
   Checks, Calls : Natural := 0;
   Database : System.Address := System.Null_Address;
   function Open_Database (Path : System.Address; Length : Unsigned_64) return System.Address
     with Import, Convention => C, External_Name => "cubit_config_test_open";
   function Close_Database (Handle : System.Address) return Unsigned_32
     with Import, Convention => C, External_Name => "cubit_config_test_close";
   function Seed_Managed (Path : System.Address; Length : Unsigned_64) return Unsigned_32
     with Import, Convention => C, External_Name => "cubit_config_test_seed_managed";
   procedure Check (OK : Boolean; Site : String := GNAT.Source_Info.Source_Location) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with "typed Turso check" & Checks'Image & " at " & Site; end if;
   end Check;
   procedure App_Acquire
     (Reference : Grants.Grant_Reference; Expected_Owner : IPC.Process_ID;
      Byte_Offset, Byte_Length : Unsigned_64; Required_Access : Grants.Required_Access;
      Mapped_Address : out System.Address; Success : out Boolean) is
      use type Grants.Required_Access;
      use type Grants.Grant_Reference;
   begin
      Check (App_Current and not App_Borrowed and Expected_Owner = App_Sender);
      Check (Reference = Grants.Grant_Reference'(7, 9));
      Check ((Byte_Offset = 0 and Byte_Length = W.Control_Bytes and Required_Access = Grants.Read_Access)
        or (Byte_Offset = W.Value_Offset and Byte_Length = CCL.Objects.Native_Image_Bytes)
        or (Byte_Offset = W.Control_Bytes and Byte_Length = CCL.Objects.Schemas.Native_Schema_Bytes
            and Required_Access = Grants.Read_Access));
      App_Borrowed := True; Success := True;
      Mapped_Address := (if Byte_Offset = 0 then App_Input.Control'Address
        elsif Byte_Length = CCL.Objects.Schemas.Native_Schema_Bytes then App_Metadata'Address
        else App_Input.Value'Address);
   end App_Acquire;
   procedure App_Return (Reference : Grants.Grant_Reference; Success : out Boolean) is
      use type Grants.Grant_Reference;
   begin
      Check (App_Borrowed and Reference = Grants.Grant_Reference'(7, 9));
      App_Borrowed := False; Success := True;
   end App_Return;
   function App_Save (Slot : Unsigned_64) return Unsigned_64 is
   begin
      Check (Slot = 62 and App_Current and not App_Saved and not App_Borrowed);
      App_Current := False; App_Saved := True; return 1;
   end App_Save;
   function App_Send (Slot : IPC.CapabilitySlot; Message : IPC.Message) return Unsigned_64 is
   begin
      Check (not App_Borrowed);
      if Slot = 62 then Check (App_Saved); App_Saved := False;
      else Check (Slot = 63 and App_Current); App_Current := False; end if;
      App_Reply := Message; App_Replies := App_Replies + 1;
      return (if Allow_Delivery then 1 else 0);
   end App_Send;
   package App is new Config_Object_Receiver (62, App_Acquire, App_Return, App_Save, App_Send);
   package Service is new Config_Object_Service (App);
   use type Service.Worker_Status;
   -- Four complete service lifetimes coexist only in this hosted fixture.
   -- Keep their grant addresses stable without exhausting Linux's test stack.
   type Service_Access is access all Service.State;
   First_Service : constant Service_Access := new Service.State;
   Replacement_Service : constant Service_Access := new Service.State;
   Interrupted_Service : constant Service_Access := new Service.State;
   Recovered_Service : constant Service_Access := new Service.State;
   Object : Service_Access := First_Service;
   procedure App_Call
     (Sender : IPC.Process_ID; Action : W.Operation; Revision : Unsigned_64 := 0) is
      use type W.Operation;
   begin
      Check (not App_Current); App_Current := True; App_Sender := Sender;
      App_Reply := IPC.NULL_MESSAGE;
      Service.Handle (Object.all, Authority, Sender,
        W.Request (Action, (7, 9), (if Action in W.Open_Collection | W.Create_Collection then 0 else Handle), Revision));
      Staged := App_Reply = IPC.NULL_MESSAGE;
      Check (not App_Current and not App_Borrowed);
   end App_Call;
   procedure Finish (Item : IPC.CompletionEntry; Expect_Reply : Boolean) is
      Before : constant Natural := App_Replies;
   begin
      Service.Complete (Object.all, Authority, Item);
      Check (App_Replies = Before + (if Expect_Reply then 1 else 0));
   end Finish;
   procedure Read_Check (Expected : V.Read_Result; Rev : Unsigned_64; Value : CCL.Objects.Image) is
      Code : constant W.Status :=
        (case Expected is when V.Found => W.Success, when V.Stale => W.Stale,
          when V.Missing => W.Missing, when V.Unavailable => W.Unavailable,
          when V.Schema_Mismatch => W.Schema_Mismatch);
   begin
      App_Input.Value := (others => <>);
      App_Call (42, W.Get_Object);
      Check (not Staged and W.Valid_Reply (App_Reply, W.Get_Object));
      Check (App_Reply.tag.label = W.Status'Enum_Rep (Code) and App_Reply.words (0) = Rev);
      Check (App_Input.Value = (if Expected in V.Found | V.Stale then Value else (others => <>)));
   end Read_Check;
   procedure Write_Request (Sender : IPC.Process_ID; Value : CCL.Objects.Image; Revision : Unsigned_64) is
   begin
      App_Input.Value := Value;
      App_Call (Sender, W.Set_Object, Revision);
      if Staged then Check (App_Reply = IPC.NULL_MESSAGE and App_Saved and Service.Waiting (Object.all));
      else Check (W.Valid_Reply (App_Reply, W.Set_Object, Revision)); end if;
   end Write_Request;
   procedure Open is
      Path : constant String := Ada.Command_Line.Argument (1);
   begin
      Database := Open_Database (Path'Address, Path'Length);
      Check (Database /= System.Null_Address);
   end Open;
   procedure Close is
      Closed : constant Unsigned_32 := Close_Database (Database);
   begin
      Database := System.Null_Address;
      Check (Closed = 1);
   end Close;
   procedure Invoke
     (Action : P.Operation; Name, Context : String; Expected_Revision : Unsigned_64;
      Schema : CCL.Objects.Schema_Key; Input : Codec.Packet;
      Output : out Config_Worker_Storage.Reply) is
   begin
      Check (Grants.Active_Acquisitions = 0 and not App_Borrowed);
      Calls := Calls + 1;
      Config_Database.Invoke (Database, Action, Name, Context, Expected_Revision, Schema, Input, Output);
   end Invoke;
   function Trusted (Sender : IPC.Process_ID; Tag : Unsigned_64) return Boolean is
     (Sender = 42 and Tag = 77);
   procedure Invoke_Type
     (Action : Config_Schema_Protocol.Operation; Name, Context : String;
      Contract : CCL.Objects.Binding; Recovered : out CCL.Objects.Binding;
      Result : out Config_Schema_Protocol.Reply_Kind) is
   begin
      Check (Grants.Active_Acquisitions = 0 and not App_Borrowed);
      Config_Database.Schemas.Invoke (Database, Action, Name, Context, Contract, Recovered, Result);
   end Invoke_Type;
   package Receiver is new Config_Worker_Receiver (12, Trusted, Invoke, Invoke_Type);
   First_Server, Replacement_Server, Interrupted_Server, Recovered_Server : Receiver.State;
   procedure Exchange
     (Server : in out Receiver.State; Lose_Response : Boolean := False)
   is
      Envelope, Reply : IPC.Message;
      Token : constant Unsigned_64 := IPC.Last_Token;
   begin
      Grants.Acquisitions := 0; Grants.Returns := 0;
      Grants.Deny_Acquisition := (if Lose_Response then 2 else 0);
      Envelope := IPC.Last_Request; Envelope.authorityTag := 77;
      Grants.Expected_Transfer_Bytes := Envelope.words (2);
      Receiver.Handle (Server, 42, Envelope, Reply);
      Check (Receiver.Needs_Recovery (Server) = Lose_Response);
      Response := (requestId => 100, token => Token, msg => Reply,
        from => 42, status => IPC.COMPLETION_OK, valid => True);
   end Exchange;
   procedure Begin_Create is
   begin
      W.Describe ("org.cubit.publication", W.Read_Write, 0, Schema, App_Input.Control, Good); Check (Good);
      CCL.Objects.Schemas.Write (Contract, App_Metadata, Good); Check (Good);
      App_Call (666, W.Create_Collection);
      Check (not Staged and App_Reply.tag.label = W.Status'Enum_Rep (W.Denied));
      App_Call (42, W.Create_Collection);
      Check (Staged and App_Saved and Service.Waiting (Object.all));
      Check (Config_Worker_Messages.Valid_Type_Request (IPC.Last_Request));
   end Begin_Create;
   procedure Begin_Read_Open
     (Name : String := "org.cubit.publication"; Key : CCL.Objects.Schema_Key := Schema) is
   begin
      W.Describe (Name, W.Read_Only, 0, Key, App_Input.Control, Good); Check (Good);
      -- Deliberately invalid client metadata: Open must never read it.
      App_Metadata := (others => <>);
      App_Call (42, W.Open_Collection);
      Check (Staged and App_Saved and Service.Waiting (Object.all));
      Check (Config_Worker_Messages.Valid_Type_Request (IPC.Last_Request));
   end Begin_Read_Open;
begin
   Check (Ada.Command_Line.Argument_Count = 1);
   CCL.Objects.Bind (Types, CCL.Types.Integer_Type, Schema, Contract, Good); Check (Good);
   First := CCL.Objects.Empty (Contract); Second := First;
   CCL.Objects.Append (First, CCL.Objects.Integer_Cell (41), Built); Check (Built = CCL.Objects.Added);
   CCL.Objects.Append (Second, CCL.Objects.Integer_Cell (42), Built); Check (Built = CCL.Objects.Added);
   A.Append (Rules, "org.cubit.publication", A.Read_Write, Good); Check (Good);
   A.Append (Read_Rules, "org.cubit.publication", A.Read_Only, Good); Check (Good);
   A.Install (Authority, 42, Rules, Installed); Check (Installed = A.Installed);
   Open;
   Grants.Expected_Pages := Config_Worker_Channel.Loan_Bytes_Count / 4096;
   Service.Attach (Object.all, 4, 0, Good); Check (not Good and Service.Status (Object.all) = Service.Unattached);
   Service.Attach (Object.all, 4, 10, Good); Check (Good);
   Service.Attach (Object.all, 4, 11, Good); Check (not Good and Service.Status (Object.all) = Service.Online);
   Begin_Create;
   Exchange (First_Server); Finish (Response, False);
   Check (Config_Worker_Messages.Valid_Schema_Request (IPC.Last_Request));
   Exchange (First_Server); Finish (Response, False);
   Check (Calls = 0 and Config_Worker_Messages.Valid_Request (IPC.Last_Request));
   Exchange (First_Server);
   Finish (Response, True);
   Check (App_Reply.tag.label = W.Status'Enum_Rep (W.Success) and App_Reply.words (0) /= 0);
   Handle := App_Reply.words (0);
   Read_Check (V.Missing, 0, First);
   Write_Request (666, First, 0);
   Check (not Staged and App_Reply.tag.label = W.Status'Enum_Rep (W.Denied) and Calls = 1);
   Write_Request (42, First, 0); Check (Staged);
   Exchange (First_Server);
   declare
      Invalid_Completion : IPC.CompletionEntry := Response;
   begin
      Invalid_Completion.valid := False;
      Finish (Invalid_Completion, False); Check (App_Saved and Service.Waiting (Object.all));
      Invalid_Completion := Response; Invalid_Completion.token := Response.token + 1;
      Finish (Invalid_Completion, False); Check (App_Saved and Service.Waiting (Object.all));
   end;
   Read_Check (V.Missing, 0, First);
   Finish (Response, True);
   Check (not App_Saved and W.Valid_Reply (App_Reply, W.Set_Object, 0) and
          App_Reply.tag.label = W.Status'Enum_Rep (W.Success));
   Old_Response := Response;
   Read_Check (V.Found, 1, First);
   App_Input.Value := (others => <>); App_Call (666, W.Get_Object);
   Check (App_Reply.tag.label = W.Status'Enum_Rep (W.Denied) and
          App_Reply.words (0) = 0 and App_Input.Value = CCL.Objects.Image'(others => <>));
   Write_Request (42, Second, 1); Check (Staged);
   Exchange (First_Server, Lose_Response => True);
   Read_Check (V.Found, 1, First);
   Finish (Response, True);
   -- The database committed, but its receipt could not be returned. This is
   -- not a definitely failed write: the caller must receive Uncertain and
   -- must not retry. The replacement below recovers revision 2 from disk.
   Check (not App_Saved and W.Valid_Reply (App_Reply, W.Set_Object, 1) and
          App_Reply.tag.label = W.Status'Enum_Rep (W.Uncertain) and App_Reply.words (0) = 0);
   Check (Service.Status (Object.all) = Service.Recovery_Required);
   Close;
   Service.Retire (Object.all, Good); Check (Good);
   Read_Check (V.Stale, 1, First);
   Write_Request (42, Second, 1);
   Check (not Staged and App_Reply.tag.label = W.Status'Enum_Rep (W.Unavailable) and Calls = 3);
   Open;
   Object := Replacement_Service;
   Service.Attach (Object.all, 4, 11, Good); Check (Good);
   -- Fresh Config has no catalogue. Read authority alone must recover it.
   A.Install (Authority, 42, Read_Rules, Installed); Check (Installed = A.Installed);
   Begin_Read_Open ("org.cubit.publication.absent");
   Exchange (Replacement_Server);
   A.Revoke (Authority, 42);
   A.Install (Authority, 42, Read_Rules, Installed); Check (Installed = A.Installed);
   Finish (Response, True);
   Check (App_Reply.tag.label = W.Status'Enum_Rep (W.Denied) and App_Reply.words (0) = 0);
   -- A fresh request may learn absence; the previous authority lifetime may not.
   Begin_Read_Open ("org.cubit.publication.absent");
   Exchange (Replacement_Server); Finish (Response, True);
   Check (App_Reply.tag.label = W.Status'Enum_Rep (W.Missing) and Calls = 3);
   Begin_Read_Open (Key => [others => 9]);
   Exchange (Replacement_Server);
   A.Revoke (Authority, 42);
   A.Install (Authority, 42, Read_Rules, Installed); Check (Installed = A.Installed);
   Finish (Response, True);
   Check (App_Reply.tag.label = W.Status'Enum_Rep (W.Denied) and App_Reply.words (0) = 0);
   Begin_Read_Open (Key => [others => 9]);
   Exchange (Replacement_Server); Finish (Response, True);
   Check (App_Reply.tag.label = W.Status'Enum_Rep (W.Schema_Mismatch) and Calls = 3);
   Begin_Read_Open;
   Check (IPC.Last_Token > Old_Response.token);
   Exchange (Replacement_Server); Finish (Response, False);
   Check (Config_Worker_Messages.Valid_Schema_Request (IPC.Last_Request));
   Exchange (Replacement_Server); Finish (Response, False);
   Exchange (Replacement_Server);
   Finish (Old_Response, False);
   -- Revocation after the final value load still prevents minting a handle.
   -- Regranting identical rights is a new authority lifetime, not a revival.
   A.Revoke (Authority, 42);
   A.Install (Authority, 42, Read_Rules, Installed); Check (Installed = A.Installed);
   Finish (Response, True);
   Check (App_Reply.tag.label = W.Status'Enum_Rep (W.Denied) and App_Reply.words (0) = 0);
   Check (not App_Saved and not Service.Waiting (Object.all));
   -- Recovering the object did not recover any historical application grant.
   -- A new, currently authorized Open obtains a new handle from the cache.
   App_Call (42, W.Open_Collection);
   Check (App_Reply.tag.label = W.Status'Enum_Rep (W.Success) and App_Reply.words (0) /= 0);
   Handle := App_Reply.words (0);
   Read_Check (V.Found, 2, Second);
   Check (Calls = 4);
   Write_Request (42, First, 2);
   Check (not Staged and App_Reply.tag.label = W.Status'Enum_Rep (W.Denied) and Calls = 4);
   App_Call (42, W.Close_Collection);
   Check (App_Reply.tag.label = W.Status'Enum_Rep (W.Success));
   A.Install (Authority, 42, Rules, Installed); Check (Installed = A.Installed);
   -- An idempotent Create must not reattach/reload an already-ready cache.
   Begin_Create;
   Exchange (Replacement_Server); Finish (Response, True);
   Check (App_Reply.tag.label = W.Status'Enum_Rep (W.Success) and Calls = 4);
   Handle := App_Reply.words (0);
   Read_Check (V.Found, 2, Second);
   App_Call (42, W.Close_Collection);
   Check (App_Reply.tag.label = W.Status'Enum_Rep (W.Success));
   Begin_Create;
   Exchange (Replacement_Server);
   -- The durable result may arrive after the caller's authority lifetime
   -- ended. It remains durable, but grants NO handle under the new lifetime.
   A.Revoke (Authority, 42);
   A.Install (Authority, 42, Rules, Installed); Check (Installed = A.Installed);
   Finish (Response, True);
   Check (App_Reply.tag.label = W.Status'Enum_Rep (W.Denied) and App_Reply.words (0) = 0);
   App_Call (42, W.Open_Collection);
   Check (App_Reply.tag.label = W.Status'Enum_Rep (W.Success));
   Handle := App_Reply.words (0);
   Read_Check (V.Found, 2, Second);
   Check (Calls = 4);
   --  Backpressure after saving the reply must unwind immediately, without
   --  leaving a caller blocked or silently retrying the requested write.
   IPC.Accept_Submission := False;
   Grants.Allow_Revoke := False; Grants.Is_Retired := False;
   Write_Request (42, First, 2);
   Check (not Staged and not App_Saved and not Service.Waiting (Object.all));
   -- A staged write is conservatively uncertain when its channel retires,
   -- including queue rejection; the dispatch path does not invent a retry.
   Check (App_Reply.tag.label = W.Status'Enum_Rep (W.Uncertain) and Calls = 4);
   Check (Service.Status (Object.all) = Service.Recovery_Required);
   IPC.Accept_Submission := True;
   Read_Check (V.Stale, 2, Second);
   Service.Retire (Object.all, Good); Check (not Good);
   Service.Attach (Object.all, 4, 12, Good); Check (not Good);
   Grants.Allow_Revoke := True; Grants.Is_Retired := True;
   Close;
   Service.Retire (Object.all, Good); Check (Good);
   -- Lost metadata reply during read-only recovery must unwind the saved reply,
   -- poison this transport, and never retry or convert uncertainty to Missing.
   Open;
   Object := Interrupted_Service;
   Service.Attach (Object.all, 4, 12, Good); Check (Good);
   A.Install (Authority, 42, Read_Rules, Installed); Check (Installed = A.Installed);
   Begin_Read_Open;
   Exchange (Interrupted_Server, Lose_Response => True);
   Finish (Response, True);
   Check (App_Reply.tag.label = W.Status'Enum_Rep (W.Unavailable) and App_Reply.words (0) = 0);
   Check (not App_Saved and not Service.Waiting (Object.all));
   Check (Service.Status (Object.all) = Service.Recovery_Required and Calls = 4);
   Old_Response := Response;
   Finish (Old_Response, False);
   App_Call (42, W.Open_Collection);
   Check (not Staged and App_Reply.tag.label = W.Status'Enum_Rep (W.Unavailable));
   Close;
   Service.Retire (Object.all, Good); Check (Good);
   -- An explicitly replaced owner, with a new session and nonreused tokens,
   -- can recover unchanged data; a late old completion cannot finish its Open.
   Open;
   Object := Recovered_Service;
   Service.Attach (Object.all, 4, 13, Good); Check (Good);
   Begin_Read_Open;
   Finish (Old_Response, False);
   Exchange (Recovered_Server); Finish (Response, False);
   Exchange (Recovered_Server); Finish (Response, False);
   Exchange (Recovered_Server); Finish (Response, True);
   Check (App_Reply.tag.label = W.Status'Enum_Rep (W.Success) and App_Reply.words (0) /= 0);
   Handle := App_Reply.words (0);
   Read_Check (V.Found, 2, Second);
   Check (Calls = 5);
   App_Call (42, W.Close_Collection);
   Check (App_Reply.tag.label = W.Status'Enum_Rep (W.Success));
   -- Repeated lost acquisition replies cannot fill the handle table or undo
   -- an existing durable collection. Real Turso executes each idempotent
   -- declaration check; the independent SQLite oracle still requires exactly
   -- the original two revisions, with no retry-created value.
   A.Install (Authority, 42, Rules, Installed); Check (Installed = A.Installed);
   Allow_Delivery := False;
   for Attempt in 1 .. 2 * C.Maximum_Handles loop
      Begin_Create;
      Exchange (Recovered_Server); Finish (Response, True);
      Check (App_Reply.tag.label = W.Status'Enum_Rep (W.Success) and App_Reply.words (0) /= 0);
      Handle := App_Reply.words (0);
      App_Call (42, W.Get_Object);
      Check (App_Reply.tag.label = W.Status'Enum_Rep (W.Denied) and not Staged);
   end loop;
   Allow_Delivery := True;
   Begin_Create;
   Exchange (Recovered_Server); Finish (Response, True);
   Check (App_Reply.tag.label = W.Status'Enum_Rep (W.Success));
   Handle := App_Reply.words (0);
   Read_Check (V.Found, 2, Second);
   Check (Calls = 5);
   App_Call (42, W.Close_Collection);
   Check (App_Reply.tag.label = W.Status'Enum_Rep (W.Success));
   Close;
   Service.Retire (Object.all, Good); Check (Good);
   declare
      Managed_Path : constant String := Ada.Command_Line.Argument (1) & ".managed";
      Managed_Rules : A.Rule_Set;
   begin
      Check (Seed_Managed (Managed_Path'Address, Managed_Path'Length) = 1);
      A.Append (Managed_Rules, "org.cubit.managed", A.Read_Write, Good); Check (Good);
      A.Install (Authority, 42, Managed_Rules, Installed); Check (Installed = A.Installed);
      for Phase in 1 .. 2 loop
         declare
            Server : Receiver.State;
         begin
            -- Empty catalog each time: protection must come from persisted
            -- registration, not a surviving handle or startup-only flag.
            Object := new Service.State;
            Database := Open_Database (Managed_Path'Address, Managed_Path'Length);
            Check (Database /= System.Null_Address);
            Service.Attach (Object.all, 4, Unsigned_64 (20 + Phase), Good); Check (Good);
            W.Describe ("org.cubit.managed", W.Read_Write, 0, Schema, App_Input.Control, Good); Check (Good);
            CCL.Objects.Schemas.Write (Contract, App_Metadata, Good); Check (Good);
            App_Call (42, W.Create_Collection); Check (Staged);
            Exchange (Server); Finish (Response, True);
            Check (App_Reply.tag.label = W.Status'Enum_Rep (W.Denied));
            Check (Service.Status (Object.all) = Service.Online);
            Begin_Read_Open ("org.cubit.managed");
            for Step in 1 .. 4 loop
               Exchange (Server);
               Service.Complete (Object.all, Authority, Response);
               exit when not App_Saved;
            end loop;
            Check (not App_Saved and App_Reply.tag.label = W.Status'Enum_Rep (W.Success));
            Handle := App_Reply.words (0); Check (Handle /= 0);
            Read_Check (V.Missing, 0, First);
            Write_Request (42, First, 0);
            Check (not Staged and App_Reply.tag.label = W.Status'Enum_Rep (W.Denied));
            W.Describe ("org.cubit.managed", W.Read_Write, 0, Schema, App_Input.Control, Good); Check (Good);
            App_Call (42, W.Open_Collection);
            Check (not Staged and App_Reply.tag.label = W.Status'Enum_Rep (W.Denied));
            CCL.Objects.Schemas.Write (Contract, App_Metadata, Good); Check (Good);
            App_Call (42, W.Create_Collection);
            Check (not Staged and App_Reply.tag.label = W.Status'Enum_Rep (W.Denied));
            Check (Service.Status (Object.all) = Service.Online and not Service.Waiting (Object.all));
            Close;
            Service.Retire (Object.all, Good); Check (Good);
         end;
      end loop;
   end;
   Ada.Text_IO.Put_Line ("Authorized typed Config -> modeled IPC -> real Turso recovery: PASS" & Checks'Image & " checks");
end Typed_Store_Turso;

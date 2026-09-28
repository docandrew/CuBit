with Ada.Text_IO;
with Interfaces; use Interfaces;
with CBOR;
with CCL.Types;
with CCL.Objects.Persistence;
with CCL.Objects.Schemas;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;
with Config_Worker_Channel; use Config_Worker_Channel;
with Config_Worker_Receiver;
with Config_Worker_Protocol;
with Config_Worker_Storage;
with Config_Worker_Messages;
with Config_Schema_Protocol;

procedure Receiver_Tests is
   package P renames Config_Worker_Protocol;
   package Codec renames CCL.Objects.Persistence;
   package Grants renames CuBit.Memory_Grants;
   package Wire renames Config_Worker_Messages;
   use type P.Operation;
   use type CCL.Objects.Image;
   use type CCL.Objects.Schema_Key;
   use type CCL.Objects.Build_Result;
   use type Codec.Outcome;
   type Fault is
     (None, Wrong_Source, Wrong_Tag, Wrong_Operation, Short_Envelope,
      Flagged_Envelope, Reserved_Envelope, Invalid_Slot, Invalid_Generation,
      Wrong_Size, Wrong_Version, Initial_Acquire_Denied, Initial_Return_Fails,
      Response_Acquire_Denied, Response_Return_Fails, Invalid_Native_Frame,
      Backend_Uncertain, Backend_Malformed, Mutated_Request_During_Storage, Unprovisioned_Schema);
   Current : Fault := None;
   Types : CCL.Types.Registry;
   Contract : CCL.Objects.Binding;
   Value : CCL.Objects.Image;
   Built : CCL.Objects.Build_Result;
   Good : Boolean;
   Calls, Cases : Natural := 0;

   function Authorized (Sender : ProcessID; Authority_Tag : Unsigned_64) return Boolean is
     (Sender = 42 and Authority_Tag = 77);

   procedure Invoke
     (Action : P.Operation; Name, Context : String; Expected_Revision : P.Number;
      Schema : CCL.Objects.Schema_Key; Input : Codec.Packet;
      Output : out Config_Worker_Storage.Reply)
   is
      Decoded : CCL.Objects.Image;
      Result : Codec.Outcome;
   begin
      --  Storage may block. It must neither borrow nor reread a client mapping.
      pragma Assert (Grants.Active_Acquisitions = 0);
      pragma Assert (Action = P.Commit and Expected_Revision = 0);
      pragma Assert (Name = "org.cubit.settings" and Context = "test");
      pragma Assert (Schema = CCL.Objects.Identity (Contract));
      Codec.Decode (Input.Data (1 .. CBOR.SE_Offset (Input.Length)), Contract, Decoded, Result);
      pragma Assert (Result = Codec.Success and Decoded = Value);
      Calls := Calls + 1;
      Output := (Code => P.Reply_Kind'Enum_Rep (P.Committed), Revision => 1, others => <>);
      case Current is
         when Backend_Uncertain =>
            Output.Code := P.Reply_Kind'Enum_Rep (P.Uncertain); Output.Revision := 0;
         when Backend_Malformed => Output.Length := Unsigned_32'Last;
         when Mutated_Request_During_Storage =>
            declare
               Loan : P.Frame with Import, Address => Grants.Mapping;
            begin
               Loan := (others => <>);
            end;
         when others => null;
      end case;
   end Invoke;
   procedure Invoke_Type
     (Action : Config_Schema_Protocol.Operation; Name, Context : String;
      Contract : CCL.Objects.Binding; Recovered : out CCL.Objects.Binding;
      Result : out Config_Schema_Protocol.Reply_Kind)
   is
      package T renames Config_Schema_Protocol;
      use type T.Operation;
   begin
      pragma Assert (Grants.Active_Acquisitions = 0);
      pragma Assert (Name = "org.cubit.settings" and Context = "test");
      pragma Assert (CCL.Objects.Is_Bound (Contract) = (Action = T.Create));
      Calls := Calls + 1;
      Recovered := Receiver_Tests.Contract;
      Result := (if Action = T.Create then T.Created else T.Loaded);
      case Current is
         when Backend_Uncertain => Result := (if Action = T.Create then T.Uncertain else T.Load_Failed);
         when Backend_Malformed => Result := (if Action = T.Create then T.Loaded else T.Created);
         when Mutated_Request_During_Storage =>
            declare
               Loan : T.Frame with Import, Address => Grants.Mapping;
            begin
               Loan := (others => <>);
            end;
         when others => null;
      end case;
   end Invoke_Type;
   package Receiver is new Config_Worker_Receiver (12, Authorized, Invoke, Invoke_Type);

   procedure Run_Type (Scenario : Fault; Action : Config_Schema_Protocol.Operation) is
      package T renames Config_Schema_Protocol;
      Client : Channel;
      Server : Receiver.State;
      Input, Output : T.Frame;
      Envelope, Reply : Message;
      Source : ProcessID := 42;
      Ok, Valid, Taken : Boolean;
      Sent : Submission;
      Done : Completion_Result;
      Recovery : constant Boolean := Scenario in Initial_Return_Fails |
        Response_Acquire_Denied | Response_Return_Fails | Backend_Uncertain | Backend_Malformed;
      Delivered : constant Boolean := Scenario in None | Backend_Uncertain |
        Backend_Malformed | Mutated_Request_During_Storage;
      Invoked : constant Boolean := Delivered or Scenario in Response_Acquire_Denied | Response_Return_Fails;
   begin
      Current := Scenario; Calls := 0;
      Grants.Acquisitions := 0; Grants.Returns := 0; Grants.Active_Acquisitions := 0;
      Grants.Deny_Acquisition := 0; Grants.Fail_Return := 0;
      Grants.Expected_Pages := Loan_Bytes_Count / 4096;
      Grants.Expected_Transfer_Bytes := T.Frame_Bytes;
      Initialize (Client, 4, Ok); pragma Assert (Ok);
      T.Make_Request (Action, 10, 1, "org.cubit.settings", "test", Contract, Input, Ok);
      pragma Assert (Ok);
      Submit_Type (Client, Input, Sent); pragma Assert (Sent = Submitted);
      Envelope := Last_Request; Envelope.authorityTag := 77;
      case Scenario is
         when Wrong_Source => Source := 43;
         when Wrong_Tag => Envelope.authorityTag := 78;
         when Wrong_Operation => Envelope.tag.label := 0;
         when Short_Envelope => Envelope.tag.length := 3;
         when Flagged_Envelope => Envelope.tag.flags := 1;
         when Reserved_Envelope => Envelope.tag.reserved := 1;
         when Invalid_Slot => Envelope.words (0) := Unsigned_64'Last;
         when Invalid_Generation => Envelope.words (1) := 0;
         when Wrong_Size => Envelope.words (2) := T.Frame_Bytes - 1;
         when Wrong_Version => Envelope.words (3) := 2;
         when Initial_Acquire_Denied => Grants.Deny_Acquisition := 1;
         when Initial_Return_Fails => Grants.Fail_Return := 1;
         when Response_Acquire_Denied => Grants.Deny_Acquisition := 2;
         when Response_Return_Fails => Grants.Fail_Return := 2;
         when Invalid_Native_Frame =>
            declare
               Loan : T.Frame with Import, Address => Grants.Mapping;
            begin
               Loan.Reserved := 1;
            end;
         when others => null;
      end case;
      Receiver.Handle (Server, Source, Envelope, Reply);
      pragma Assert (Calls = (if Invoked then 1 else 0));
      pragma Assert (Receiver.Needs_Recovery (Server) = Recovery);
      if Scenario in Wrong_Source | Wrong_Tag then pragma Assert (Grants.Acquisitions = 0); end if;
      Complete (Client, (requestId => 100, token => 1, msg => Reply,
        from => 42, status => COMPLETION_OK, valid => True), Done);
      pragma Assert (Done = Completed);
      Take_Type_Result (Client, Output, Valid, Taken);
      pragma Assert (Taken and (Valid = Delivered));
      if Valid then pragma Assert (T.Valid_Reply (Output, Input)); end if;
      if Recovery then
         Receiver.Handle (Server, 42, Envelope, Reply);
         pragma Assert (Reply.tag.label = Wire.Status'Enum_Rep (Wire.Unavailable));
         pragma Assert (Calls = (if Invoked then 1 else 0));
      end if;
      Grants.Is_Retired := True;
      Retire (Client, Ok); pragma Assert (Ok);
      Cases := Cases + 1;
   end Run_Type;

   procedure Run (Scenario : Fault) is
      Client : Channel;
      Server : Receiver.State;
      Input, Output : P.Frame;
      Schema : CCL.Objects.Schemas.Image;
      Envelope, Reply : Message;
      Sender : ProcessID := 42;
      Ok, Valid, Taken : Boolean;
      Sent : Submission;
      Done : Completion_Result;
      Recovery : constant Boolean := Scenario in Initial_Return_Fails |
        Response_Acquire_Denied | Response_Return_Fails | Backend_Uncertain | Backend_Malformed;
      Delivered : constant Boolean := Scenario in None | Backend_Uncertain |
        Backend_Malformed | Mutated_Request_During_Storage;
      Invoked : constant Boolean := Delivered or Scenario in Response_Acquire_Denied | Response_Return_Fails;
   begin
      Current := Scenario;
      Calls := 0;
      Grants.Acquisitions := 0; Grants.Returns := 0; Grants.Active_Acquisitions := 0;
      Grants.Deny_Acquisition := 0; Grants.Fail_Return := 0;
      Grants.Expected_Transfer_Bytes := CCL.Objects.Schemas.Native_Schema_Bytes;
      if Scenario /= Unprovisioned_Schema then
         CCL.Objects.Schemas.Write (Contract, Schema, Ok); pragma Assert (Ok);
         Grants.Mapping := Schema'Address;
         Grants.Expected_Pages := CCL.Objects.Schemas.Native_Schema_Bytes / 4096;
         Envelope := Wire.Schema_Request ((7, 9)); Envelope.authorityTag := 77;
         Receiver.Handle (Server, 42, Envelope, Reply);
         pragma Assert (Wire.Valid_Schema_Acknowledgment (Reply) and Calls = 0);
      end if;
      Grants.Acquisitions := 0; Grants.Returns := 0;
      Grants.Expected_Pages := Loan_Bytes_Count / 4096;
      Initialize (Client, 4, Ok); pragma Assert (Ok);
      Grants.Expected_Transfer_Bytes := P.Frame_Bytes;
      P.Make_Request (P.Commit, 10, 1, 0, "org.cubit.settings", "test", Contract, Value, Input, Ok);
      pragma Assert (Ok);
      Submit (Client, Input, Contract, Sent); pragma Assert (Sent = Submitted);
      Envelope := Last_Request;
      Envelope.authorityTag := 77;
      case Scenario is
         when Wrong_Source => Sender := 43;
         when Wrong_Tag => Envelope.authorityTag := 78;
         when Wrong_Operation => Envelope.tag.label := 0;
         when Short_Envelope => Envelope.tag.length := 3;
         when Flagged_Envelope => Envelope.tag.flags := 1;
         when Reserved_Envelope => Envelope.tag.reserved := 1;
         when Invalid_Slot => Envelope.words (0) := Unsigned_64'Last;
         when Invalid_Generation => Envelope.words (1) := 0;
         when Wrong_Size => Envelope.words (2) := P.Frame_Bytes - 1;
         when Wrong_Version => Envelope.words (3) := 0;
         when Initial_Acquire_Denied => Grants.Deny_Acquisition := 1;
         when Initial_Return_Fails => Grants.Fail_Return := 1;
         when Response_Acquire_Denied => Grants.Deny_Acquisition := 2;
         when Response_Return_Fails => Grants.Fail_Return := 2;
         when Invalid_Native_Frame =>
            declare
               Loan : P.Frame with Import, Address => Grants.Mapping;
            begin
               Loan.Reserved := 1;
            end;
         when others => null;
      end case;
      Receiver.Handle (Server, Sender, Envelope, Reply);
      pragma Assert (Receiver.Needs_Recovery (Server) = Recovery);
      pragma Assert (Calls = (if Invoked then 1 else 0));
      pragma Assert (Wire.Valid_Acknowledgment (Reply) = Delivered);
      if Scenario in Wrong_Source .. Wrong_Version then
         pragma Assert (Grants.Acquisitions = 0 and Grants.Returns = 0);
      end if;
      if Scenario not in Initial_Return_Fails | Response_Return_Fails then
         pragma Assert (Grants.Active_Acquisitions = 0);
      end if;
      if Recovery then
         declare
            Previous_Acquisitions : constant Natural := Grants.Acquisitions;
            Previous_Calls : constant Natural := Calls;
            Ignored_Reply : Message;
         begin
            Receiver.Handle (Server, 42, Envelope, Ignored_Reply);
            pragma Assert (Ignored_Reply.tag.label = Wire.Status'Enum_Rep (Wire.Unavailable));
            pragma Assert (Grants.Acquisitions = Previous_Acquisitions and Calls = Previous_Calls);
         end;
      end if;
      Complete (Client, (requestId => 100, token => 1, msg => Reply,
                         from => 42, status => COMPLETION_OK, valid => True), Done);
      pragma Assert (Done = Completed);
      Take_Result (Client, Output, Valid, Taken);
      pragma Assert (Taken and (Valid = Delivered));
      pragma Assert (Status (Client) = (if Delivered and not Recovery then Ready else Failed));
      if Delivered then
         pragma Assert (P.Valid_Reply (Item => Output, Request => Input, Contract => Contract));
         pragma Assert (Output.Reply = P.Reply_Kind'Enum_Rep
           (if Recovery then P.Uncertain else P.Committed));
      end if;
      --  Each scenario models a separate process lifetime. This does not prove
      --  kernel cleanup after a failed Return_Acquisition.
      Retire (Client, Ok); pragma Assert (Ok);
      Cases := Cases + 1;
   end Run;

   procedure Provisioning_Checks is
      Server, Broken : Receiver.State;
      Schema : CCL.Objects.Schemas.Image;
      Envelope, Reply : Message;
      Previous_Calls : constant Natural := Calls;
      procedure Send (Sender : ProcessID; Expected : Wire.Status) is
      begin
         Grants.Acquisitions := 0; Grants.Returns := 0;
         Receiver.Handle (Server, Sender, Envelope, Reply);
         pragma Assert (Reply.tag.label = Wire.Status'Enum_Rep (Expected));
         pragma Assert (Calls = Previous_Calls and Grants.Active_Acquisitions = 0);
         Cases := Cases + 1;
      end Send;
   begin
      Grants.Active_Acquisitions := 0; Grants.Deny_Acquisition := 0; Grants.Fail_Return := 0;
      Grants.Expected_Pages := CCL.Objects.Schemas.Native_Schema_Bytes / 4096;
      Grants.Expected_Transfer_Bytes := CCL.Objects.Schemas.Native_Schema_Bytes;
      CCL.Objects.Schemas.Write (Contract, Schema, Good); pragma Assert (Good);
      Grants.Mapping := Schema'Address;
      Envelope := Wire.Schema_Request ((7, 9)); Envelope.authorityTag := 77;
      pragma Assert (Wire.Valid_Schema_Request (Envelope) and not Wire.Valid_Request (Envelope));
      pragma Assert (not Wire.Valid_Acknowledgment (Wire.Schema_Acknowledgment));
      pragma Assert (not Wire.Valid_Schema_Acknowledgment (Wire.Acknowledgment));
      Send (99, Wire.Denied); pragma Assert (Grants.Acquisitions = 0);
      Envelope.authorityTag := 78;
      Send (42, Wire.Denied); pragma Assert (Grants.Acquisitions = 0);
      Envelope.authorityTag := 77;
      Envelope.tag.flags := 1;
      Send (42, Wire.Invalid_Request); pragma Assert (Grants.Acquisitions = 0);
      Envelope.tag.flags := 0;
      Envelope.words (2) := P.Frame_Bytes;
      Send (42, Wire.Invalid_Request); pragma Assert (Grants.Acquisitions = 0);
      Envelope.words (2) := CCL.Objects.Schemas.Native_Schema_Bytes;
      Grants.Deny_Acquisition := 1;
      Send (42, Wire.Invalid_Request); pragma Assert (Grants.Returns = 0);
      Grants.Deny_Acquisition := 0;
      Schema.Root := CCL.Objects.Schemas.Handler_ID;
      Send (42, Wire.Invalid_Request);
      Schema.Root := CCL.Objects.Schemas.Integer_ID;
      Send (42, Wire.Schema_Ready);
      pragma Assert (Wire.Valid_Schema_Acknowledgment (Reply));
      Send (42, Wire.Schema_Ready); -- exact duplicate, no second slot
      declare
         Extra_Types : CCL.Types.Registry;
         Extra : CCL.Objects.Binding;
         Ref : CCL.Types.Type_Reference;
         Defined_As : CCL.Types.Definition_Result;
         use type CCL.Types.Definition_Result;
      begin
         CCL.Types.Define (Extra_Types, (Identifier => CCL.Types.Named ("Unrelated"),
           Form => CCL.Types.Product, others => <>), Ref, Defined_As);
         pragma Assert (Defined_As = CCL.Types.Defined);
         CCL.Objects.Bind (Extra_Types, CCL.Types.Integer_Type,
           CCL.Objects.Identity (Contract), Extra, Good); pragma Assert (Good);
         CCL.Objects.Schemas.Write (Extra, Schema, Good); pragma Assert (Good);
         Send (42, Wire.Schema_Ready); -- equivalent root, different registry
         CCL.Objects.Schemas.Write (Contract, Schema, Good); pragma Assert (Good);
      end;
      Schema.Root := CCL.Objects.Schemas.Boolean_ID;
      Send (42, Wire.Invalid_Request); -- same key, incompatible layout
      Schema.Root := CCL.Objects.Schemas.Integer_ID;
      for Index in 2 .. Receiver.Maximum_Schemas loop
         Schema.Key (0) := Unsigned_64 (Index);
         Send (42, Wire.Schema_Ready);
      end loop;
      Schema.Key (0) := Receiver.Maximum_Schemas + 1;
      Send (42, Wire.Unavailable);
      Schema.Key (0) := 1;
      Send (42, Wire.Schema_Ready); -- duplicate remains usable at capacity
      -- Failure to release even a valid provisioning grant poisons this
      -- receiver before import. No later call can reuse that uncertain mapping.
      Grants.Acquisitions := 0; Grants.Returns := 0; Grants.Fail_Return := 1;
      Receiver.Handle (Broken, 42, Envelope, Reply);
      pragma Assert (Receiver.Needs_Recovery (Broken) and Grants.Active_Acquisitions = 1);
      pragma Assert (Reply.tag.label = Wire.Status'Enum_Rep (Wire.Unavailable));
      Receiver.Handle (Broken, 42, Envelope, Reply);
      pragma Assert (Grants.Acquisitions = 1 and Calls = Previous_Calls);
      Cases := Cases + 1;
      -- Hosted model teardown only; native mapping cleanup is a kernel duty.
      Grants.Active_Acquisitions := 0; Grants.Fail_Return := 0;
   end Provisioning_Checks;
begin
   CCL.Objects.Bind (Types, CCL.Types.Integer_Type, [1, 2, 3, 4], Contract, Good);
   pragma Assert (Good);
   Value := CCL.Objects.Empty (Contract);
   CCL.Objects.Append (Value, CCL.Objects.Integer_Cell (42), Built);
   pragma Assert (Built = CCL.Objects.Added);
   Grants.Expected_Pages := CCL.Objects.Schemas.Native_Schema_Bytes / 4096;
   for Scenario in Fault loop Run (Scenario); end loop;
   Provisioning_Checks;
   for Action in Config_Schema_Protocol.Operation loop
      for Scenario in None .. Mutated_Request_During_Storage loop Run_Type (Scenario, Action); end loop;
   end loop;
   Ada.Text_IO.Put_Line ("Config worker receiver/channel: PASS" & Cases'Image & " scenarios");
end Receiver_Tests;

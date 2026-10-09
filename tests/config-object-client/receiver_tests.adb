with Ada.Text_IO;
with System;
with Interfaces; use Interfaces;
with CCL.Types;
with CCL.Objects;
with CCL.Objects.Schemas;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;
with Config_Authority;
with Config_Collections;
with Config_Objects;
with Config_Typed_Store;
with Config_Worker_Protocol;
with Config_Object_Messages;
with Config_Object_Receiver;

procedure Receiver_Tests is
   package A renames Config_Authority;
   package C renames Config_Collections;
   package V renames Config_Objects;
   package T renames Config_Typed_Store;
   package P renames Config_Worker_Protocol;
   package W renames Config_Object_Messages;
   package G renames CuBit.Memory_Grants;
   use type A.Install_Result;
   use type C.Result;
   use type V.Outcome;
   use type CCL.Objects.Image;
   use type CCL.Objects.Build_Result;
   use type G.Grant_Reference;
   use type G.Required_Access;
   use type W.Operation;
   use type W.Open_Descriptor;
   use type CCL.Objects.Schema_Key;
   Input : W.Frame;
   Metadata : CCL.Objects.Schemas.Image;
   Creating_Request : Boolean := False;
   Checks, Acquires, Returns, Saves, Replies : Natural := 0;
   Active, Saved, Current, Allow_Acquire, Allow_Return, Allow_Save, Allow_Delivery : Boolean := False;
   Mutate_On_Return : Boolean := False;
   Saved_Caller, Current_Caller, Last_Caller : Process_ID := 0;
   Last_Slot : CapabilitySlot := 0;
   Last_Reply : Message := NULL_MESSAGE;
   Expected_Offset, Expected_Length : Unsigned_64 := 0;
   Expected_Access : G.Required_Access := G.Read_Access;
   procedure Check (OK : Boolean) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with "Config receiver check" & Checks'Image; end if;
   end Check;
   procedure Acquire
     (Reference : G.Grant_Reference; Expected_Owner : Process_ID;
      Byte_Offset, Byte_Length : Unsigned_64; Required_Access : G.Required_Access;
      Mapped_Address : out System.Address; Success : out Boolean) is
   begin
      Acquires := Acquires + 1;
      Check (not Active and Current and Expected_Owner = Current_Caller);
      Check (Reference = G.Grant_Reference'(7, 9));
      if Creating_Request then
         Check (Required_Access = G.Read_Access and then
           ((Byte_Offset = 0 and Byte_Length = W.Control_Bytes) or else
            (Byte_Offset = W.Control_Bytes and Byte_Length = CCL.Objects.Schemas.Native_Schema_Bytes)));
      else
         Check (Byte_Offset = Expected_Offset and Byte_Length = Expected_Length and Required_Access = Expected_Access);
      end if;
      Success := Allow_Acquire; Active := Success;
      Mapped_Address := (if Byte_Offset = 0 then Input.Control'Address
        elsif Creating_Request then Metadata'Address else Input.Value'Address);
   end Acquire;
   procedure Return_Acquisition (Reference : G.Grant_Reference; Success : out Boolean) is
   begin
      Returns := Returns + 1;
      Check (Active and Reference = G.Grant_Reference'(7, 9));
      Success := Allow_Return;
      if Success then Active := False; end if;
      if Mutate_On_Return then Input.Value := (others => <>); end if;
   end Return_Acquisition;
   function Save_Reply (Slot : Unsigned_64) return Unsigned_64 is
   begin
      Saves := Saves + 1;
      Check (Slot = 62 and not Saved and Current and not Active);
      if not Allow_Save then return 0; end if;
      Saved := True; Current := False; Saved_Caller := Current_Caller;
      return 1;
   end Save_Reply;
   function Send_Reply (Slot : CapabilitySlot; Message : CuBit.Messages.Message) return Unsigned_64 is
   begin
      Replies := Replies + 1; Last_Reply := Message; Last_Slot := Slot;
      if Slot = 62 then
         Check (Saved); Saved := False; Last_Caller := Saved_Caller;
      else
         Check (Slot = 63 and Current); Current := False; Last_Caller := Current_Caller;
      end if;
      return (if Allow_Delivery then 1 else 0);
   end Send_Reply;
   package R is new Config_Object_Receiver (62, Acquire, Return_Acquisition, Save_Reply, Send_Reply);
   Object : R.State;
   Store : T.State;
   Authority : A.Authority_State;
   Rules : A.Rule_Set;
   Types : CCL.Types.Registry;
   Contract, Binding : CCL.Objects.Binding;
   Schema : constant CCL.Objects.Schema_Key := [1, 2, 3, 4];
   First : CCL.Objects.Image;
   ID : C.Collection_ID;
   Handle : Unsigned_64;
   Access_Result : C.Result;
   Installed : A.Install_Result;
   Result : V.Outcome;
   Built : CCL.Objects.Build_Result;
   Request : Message;
   Receipt, Job, Old_Receipt : P.Frame;
   Good, Staged, Available : Boolean;
   Before_Acquires, Before_Saves, Before_Replies : Natural;
   procedure Call
     (Caller : Process_ID; Op : W.Operation; Token : Unsigned_64 := 0;
      Revision : Unsigned_64 := 0) is
   begin
      Check (not Current);
      Current := True; Current_Caller := Caller;
      Request := W.Request (Op, (7, 9), (if Op = W.Open_Collection then 0 else Handle), Revision);
      Expected_Offset := (if Op = W.Open_Collection then 0 else W.Value_Offset);
      Expected_Length := (if Op = W.Open_Collection then W.Control_Bytes else CCL.Objects.Native_Image_Bytes);
      Expected_Access := (if Op = W.Get_Object then G.Write_Access else G.Read_Access);
      R.Handle (Object, Store, Authority, Caller, Request, Token, Staged);
      Check (not Current);
   end Call;
   procedure Is_Reply (Code : W.Status; Caller : Process_ID := 42; Slot : CapabilitySlot := 63) is
   begin
      Check (Last_Reply.tag.label = W.Status'Enum_Rep (Code) and Last_Caller = Caller and Last_Slot = Slot);
   end Is_Reply;
   procedure Receipt_For (Code : P.Reply_Kind; Revision : Unsigned_64) is
   begin
      T.Pending (Store, Job, Binding, Available); Check (Available);
      P.Make_Reply (Job, Code, Revision, Binding, First, Receipt, Good); Check (Good);
   end Receipt_For;
begin
   Allow_Acquire := True; Allow_Return := True; Allow_Save := True; Allow_Delivery := True;
   CCL.Objects.Bind (Types, CCL.Types.Integer_Type, Schema, Contract, Good); Check (Good);
   First := CCL.Objects.Empty (Contract);
   CCL.Objects.Append (First, CCL.Objects.Integer_Cell (41), Built); Check (Built = CCL.Objects.Added);
   T.Register (Store, "org.cubit.settings", Contract, ID, Access_Result); Check (Access_Result = C.Registered);
   A.Append (Rules, "org.cubit", A.Read_Write, Good); Check (Good);
   A.Install (Authority, 42, Rules, Installed); Check (Installed = A.Installed);
   W.Describe ("org.cubit.settings", W.Read_Write, 0, Schema, Input.Control, Good); Check (Good);
   Call (42, W.Open_Collection); Is_Reply (W.Success); Handle := Last_Reply.words (0);
   Check (Acquires = 1 and Returns = 1 and not Active and not Saved);
   -- More failures than the entire handle table: immediate Open must reclaim
   -- its fresh undelivered handle, without closing the earlier delivered one.
   Allow_Delivery := False;
   for Attempt in 1 .. 2 * C.Maximum_Handles loop
      Call (42, W.Open_Collection); Is_Reply (W.Success);
      Check (Last_Reply.words (0) /= Handle);
      Check (not T.Check_Access (Store, Authority, 42, Last_Reply.words (0), A.Read_Config));
      Check (T.Check_Access (Store, Authority, 42, Handle, A.Read_Config));
   end loop;
   Allow_Delivery := True;
   T.Restore (Store, ID, 10, 1, Result); Check (Result = V.Accepted);
   Receipt_For (P.Absent, 0); Before_Replies := Replies;
   R.Finish (Object, Store, Receipt); Check (Replies = Before_Replies);
   Before_Acquires := Acquires;
   Call (99, W.Get_Object); Is_Reply (W.Denied, 99); Check (Acquires = Before_Acquires);
   Call (99, W.Set_Object, 2); Is_Reply (W.Denied, 99); Check (Acquires = Before_Acquires and Saves = 0);
   Input.Value := First; Allow_Save := False;
   Call (42, W.Set_Object, 2); Is_Reply (W.Unavailable);
   Check (not Staged and not Saved and not Active and T.Pending_Token (Store) = 0);
   Allow_Save := True; Mutate_On_Return := True; Input.Value := First;
   Before_Replies := Replies;
   Call (42, W.Set_Object, 2);
   Check (Staged and Saved and R.Waiting (Object) and Replies = Before_Replies and not Active);
   T.Pending (Store, Job, Binding, Available); Check (Available and Job.Value = First);
   Check (Input.Value = CCL.Objects.Image'(others => <>)); Mutate_On_Return := False;
   Before_Acquires := Acquires; Before_Saves := Saves;
   Call (42, W.Set_Object, 3); Is_Reply (W.Busy);
   Check (Acquires = Before_Acquires and Saves = Before_Saves and Saved);
   Call (99, W.Set_Object, 3); Is_Reply (W.Denied, 99);
   Check (Acquires = Before_Acquires and Saves = Before_Saves and Saved);
   Call (42, W.Get_Object); Is_Reply (W.Missing); Check (Saved and Acquires = Before_Acquires);
   Receipt_For (P.Committed, 1); Old_Receipt := Receipt;
   Receipt.Session := 9; Before_Replies := Replies;
   R.Finish (Object, Store, Receipt); Check (Replies = Before_Replies and Saved);
   -- Revision 1 numerically equals the first delivered handle. A dropped
   -- Set receipt must never be mistaken for an acquisition and close it.
   Check (Handle = 1); Allow_Delivery := False;
   R.Finish (Object, Store, Old_Receipt); Is_Reply (W.Success, 42, 62);
   Check (not Saved and not R.Waiting (Object) and Last_Reply.words (0) = 1);
   Allow_Delivery := True;
   Check (T.Check_Access (Store, Authority, 42, Handle, A.Read_Config));
   Before_Replies := Replies; R.Finish (Object, Store, Old_Receipt); Check (Replies = Before_Replies);
   Call (42, W.Get_Object); Is_Reply (W.Success); Check (Input.Value = First and not Active);
   Allow_Acquire := False; Input.Value := (others => <>);
   Call (42, W.Get_Object); Is_Reply (W.Unavailable); Check (Input.Value = CCL.Objects.Image'(others => <>));
   Call (42, W.Set_Object, 3, 1); Is_Reply (W.Unavailable); Check (not Staged and not Saved);
   Allow_Acquire := True; Input.Value := First;
   Call (42, W.Set_Object, 3, 0); Is_Reply (W.Conflict, 42, 62);
   Check (not Saved and not Staged and not R.Waiting (Object));
   Call (42, W.Set_Object, 4, 1); Check (Staged and Saved);
   Receipt_For (P.Committed, 2); Before_Replies := Replies;
   R.Lost (Object, Store, 9); Check (Saved and Replies = Before_Replies);
   R.Lost (Object, Store, 10); Is_Reply (W.Uncertain, 42, 62); Check (not Saved);
   Before_Replies := Replies;
   R.Finish (Object, Store, Receipt); R.Lost (Object, Store, 10); Check (Replies = Before_Replies);
   Call (42, W.Get_Object); Is_Reply (W.Stale); Check (Input.Value = First);
   T.Restore (Store, ID, 11, 5, Result); Check (Result = V.Accepted);
   Receipt_For (P.Loaded, 2); R.Finish (Object, Store, Receipt);
   Input.Value := First; Call (42, W.Set_Object, 6, 2); Check (Staged);
   Receipt_For (P.Committed, 3); Allow_Delivery := False;
   R.Finish (Object, Store, Receipt); Check (not Saved and not R.Waiting (Object));
   Allow_Delivery := True;
   Call (42, W.Get_Object); Is_Reply (W.Success); Check (Last_Reply.words (0) = 3);
   --  Invalid tag/grant never reaches acquire, including hostile enum values.
   Before_Acquires := Acquires; Current := True; Current_Caller := 42;
   Request := NULL_MESSAGE; Request.tag.label := Unsigned_32'Last;
   R.Handle (Object, Store, Authority, 42, Request, 7, Staged); Is_Reply (W.Invalid_Request);
   Current := True; Request := W.Request (W.Set_Object, (7, 9), Handle, 3); Request.words (1) := 0;
   R.Handle (Object, Store, Authority, 42, Request, 7, Staged); Is_Reply (W.Invalid_Request);
   Check (Acquires = Before_Acquires and not Staged);
   -- Creation borrows control before metadata, admits the namespace first,
   -- and retains only a snapshot while the durable worker runs.
   declare
      Creator : R.State;
      Read_Rules, Other_Rules : A.Rule_Set;
      Control_Snapshot : W.Open_Descriptor;
      Binding_Snapshot : CCL.Objects.Binding;
      procedure Create_Call (Caller : Process_ID := 42; Online : Boolean := True) is
      begin
         Current := True; Current_Caller := Caller; Creating_Request := True;
         R.Begin_Definition (Creator, Store, W.Create_Collection, Authority, Caller,
           W.Request (W.Create_Collection, (7, 9), 0, 0), Online, Staged);
         Creating_Request := False;
         Check (not Current and not Active);
      end Create_Call;
      procedure Open_Call is
      begin
         Current := True; Current_Caller := 43;
         Expected_Offset := 0; Expected_Length := W.Control_Bytes;
         Expected_Access := G.Read_Access;
         R.Begin_Definition (Creator, Store, W.Open_Collection, Authority, 43,
           W.Request (W.Open_Collection, (7, 9), 0, 0), True, Staged);
         Check (not Current and not Active);
      end Open_Call;
   begin
      W.Describe ("org.cubit.settings", W.Read_Write, 0, Schema, Input.Control, Good); Check (Good);
      CCL.Objects.Schemas.Write (Contract, Metadata, Good); Check (Good);
      -- Create cannot reclassify a managed collection, even with a valid schema
      -- and namespace write authority. Deny before reserving a deferred reply.
      T.Register (Store, "org.cubit.managed", Contract, ID, Access_Result, C.Declaration_Managed);
      Check (Access_Result = C.Registered);
      W.Describe ("org.cubit.managed", W.Read_Write, 0, Schema, Input.Control, Good); Check (Good);
      Before_Saves := Saves;
      Create_Call; Is_Reply (W.Denied);
      Check (not Staged and not Saved and Saves = Before_Saves);
      W.Describe ("org.cubit.settings", W.Read_Write, 0, Schema, Input.Control, Good); Check (Good);
      Before_Acquires := Acquires; Create_Call (99); Is_Reply (W.Denied, 99);
      Check (Acquires = Before_Acquires and not Staged);
      A.Append (Read_Rules, "org.cubit", A.Read_Only, Good); Check (Good);
      A.Install (Authority, 43, Read_Rules, Installed); Check (Installed = A.Installed);
      Before_Acquires := Acquires; Create_Call (43); Is_Reply (W.Denied, 43);
      Check (Acquires = Before_Acquires + 1 and not Staged);
      A.Append (Other_Rules, "org.other", A.Read_Write, Good); Check (Good);
      A.Install (Authority, 44, Other_Rules, Installed); Check (Installed = A.Installed);
      Before_Acquires := Acquires; Create_Call (44); Is_Reply (W.Denied, 44);
      Check (Acquires = Before_Acquires + 1 and not Staged);
      Before_Acquires := Acquires; Create_Call (Online => False); Is_Reply (W.Unavailable);
      Check (Acquires = Before_Acquires + 1 and not Staged);
      Metadata := (others => <>); Before_Saves := Saves;
      Create_Call; Is_Reply (W.Schema_Mismatch); Check (Saves = Before_Saves and not Staged);
      CCL.Objects.Schemas.Write (Contract, Metadata, Good); Check (Good);
      Allow_Save := False; Create_Call; Is_Reply (W.Unavailable);
      Check (not Saved and not R.Waiting (Creator)); Allow_Save := True;
      Create_Call; Check (Saved and Staged and R.Definition_Pending (Creator));
      R.Pending_Definition (Creator, Control_Snapshot, Binding_Snapshot);
      Check (Control_Snapshot = Input.Control);
      Input.Control := (others => <>); Metadata := (others => <>);
      R.Pending_Definition (Creator, Input.Control, Binding_Snapshot);
      Check (Input.Control = Control_Snapshot and CCL.Objects.Identity (Binding_Snapshot) = Schema);
      Before_Acquires := Acquires; Before_Saves := Saves;
      Create_Call; Is_Reply (W.Busy);
      Check (Saved and Saves = Before_Saves and Acquires = Before_Acquires + 1);
      -- Same rights reinstalled are still a DIFFERENT authorization lifetime.
      A.Install (Authority, 42, Rules, Installed); Check (Installed = A.Installed);
      R.Finish_Definition (Creator, Store, Authority, W.Success); Is_Reply (W.Denied, 42, 62);
      Check (Last_Reply.words (0) = 0 and not Saved and not R.Waiting (Creator));
      Before_Replies := Replies; R.Finish_Definition (Creator, Store, Authority, W.Success);
      Check (Replies = Before_Replies);
      CCL.Objects.Schemas.Write (Contract, Metadata, Good); Check (Good);
      Create_Call; Check (Staged);
      R.Finish_Definition (Creator, Store, Authority, W.Success); Is_Reply (W.Success, 42, 62);
      Handle := Last_Reply.words (0); Check (Handle /= 0 and not Saved);
      Allow_Delivery := False;
      for Attempt in 1 .. 2 * C.Maximum_Handles loop
         Create_Call; Check (Staged and Saved);
         R.Finish_Definition (Creator, Store, Authority, W.Success); Is_Reply (W.Success, 42, 62);
         Check (not Saved and not R.Waiting (Creator));
         Check (not T.Check_Access (Store, Authority, 42, Last_Reply.words (0), A.Read_Config));
         Check (T.Check_Access (Store, Authority, 42, Handle, A.Read_Config));
      end loop;
      Allow_Delivery := True;
      Create_Call; Check (Staged);
      R.Lost (Creator, Store, 999); Is_Reply (W.Uncertain, 42, 62);
      Check (not Saved and not R.Waiting (Creator));
      Check (W.Valid_Reply (Last_Reply, W.Create_Collection));
      Before_Replies := Replies;
      R.Lost (Creator, Store, 999);
      R.Finish_Definition (Creator, Store, Authority, W.Success);
      Check (Replies = Before_Replies);
      W.Describe ("org.cubit.unknown", W.Read_Write, 0, Schema, Input.Control, Good); Check (Good);
      -- Even a reported storage success cannot become a usable handle if
      -- local registration is missing. It must not claim pre-stage failure.
      Create_Call; Check (Staged and Saved);
      R.Finish_Definition (Creator, Store, Authority, W.Success);
      Is_Reply (W.Uncertain, 42, 62);
      Check (not Saved and not R.Waiting (Creator) and Last_Reply.words (0) = 0);
      Before_Acquires := Acquires; Open_Call; Is_Reply (W.Denied, 43);
      Check (Acquires = Before_Acquires + 1 and not Staged and not Saved);
      W.Describe ("org.cubit.unknown", W.Read_Only, 0, Schema, Input.Control, Good); Check (Good);
      Metadata := (others => <>); Before_Acquires := Acquires;
      Open_Call; Check (Staged and Saved and Acquires = Before_Acquires + 1);
      A.Revoke (Authority, 43);
      R.Finish_Definition (Creator, Store, Authority, W.Missing); Is_Reply (W.Denied, 43, 62);
      Check (not Saved and not R.Waiting (Creator) and Last_Reply.words (0) = 0);
      A.Install (Authority, 43, Read_Rules, Installed); Check (Installed = A.Installed);
      Open_Call; Check (Staged and Saved);
      R.Finish_Definition (Creator, Store, Authority, W.Missing); Is_Reply (W.Missing, 43, 62);
      Check (not Saved and not R.Waiting (Creator));
      -- Losing a read-only definition lookup cannot create durable state.
      Open_Call; Check (Staged and Saved);
      R.Lost (Creator, Store, 999); Is_Reply (W.Unavailable, 43, 62);
      Check (not Saved and not R.Waiting (Creator));
      -- Cached opens retain their fast path, even during another definition
      -- request. They must not overwrite that caller's saved reply.
      Open_Call; Check (Staged and Saved);
      W.Describe ("org.cubit.settings", W.Read_Only, 0, Schema, Input.Control, Good); Check (Good);
      Before_Saves := Saves; Open_Call; Is_Reply (W.Success, 43);
      Check (not Staged and Saved and Saves = Before_Saves and Last_Reply.words (0) /= 0);
      R.Finish_Definition (Creator, Store, Authority, W.Missing); Is_Reply (W.Missing, 43, 62);
      -- Cached Open takes the other acquisition-delivery path.
      Allow_Delivery := False;
      for Attempt in 1 .. 2 * C.Maximum_Handles loop
         Open_Call; Is_Reply (W.Success, 43);
         Check (not Staged and not Saved);
         Check (not T.Check_Access (Store, Authority, 43, Last_Reply.words (0), A.Read_Config));
         Check (T.Check_Access (Store, Authority, 42, Handle, A.Read_Config));
      end loop;
      -- Deferred Open after schema recovery likewise cannot strand a handle.
      W.Describe ("org.cubit.recovered", W.Read_Only, 0, Schema, Input.Control, Good); Check (Good);
      Open_Call; Check (Staged and Saved);
      T.Register (Store, "org.cubit.recovered", Contract, ID, Access_Result); Check (Access_Result = C.Registered);
      R.Finish_Definition (Creator, Store, Authority, W.Success); Is_Reply (W.Success, 43, 62);
      Check (not Saved and not T.Check_Access (Store, Authority, 43, Last_Reply.words (0), A.Read_Config));
      Allow_Delivery := True;
   end;
   --  Failed grant return: never stage or reuse this receiver. Kernel teardown
   --  must retire the leftover mapping; no fake local success cleanup here.
   Input.Value := First; Allow_Return := False; Before_Saves := Saves;
   Call (42, W.Set_Object, 7, 3); Is_Reply (W.Unavailable);
   Check (R.Needs_Recovery (Object) and Active and not Staged and Saves = Before_Saves);
   Before_Acquires := Acquires;
   Call (42, W.Get_Object); Is_Reply (W.Unavailable); Check (Acquires = Before_Acquires);
   --  Model teardown of the failed receiver and its mapping. A separate
   --  receiver can still use the store; there is no in-place reset API.
   Active := False; Allow_Return := True;
   declare
      Replacement : R.State;
      procedure Replacement_Call (Op : W.Operation; Token : Unsigned_64 := 0) is
      begin
         Current := True; Current_Caller := 42;
         Expected_Offset := W.Value_Offset; Expected_Length := CCL.Objects.Native_Image_Bytes;
         Expected_Access := (if Op = W.Get_Object then G.Write_Access else G.Read_Access);
         R.Handle (Replacement, Store, Authority, 42, W.Request (Op, (7, 9), Handle, 3), Token, Staged);
         Check (not Current);
      end Replacement_Call;
   begin
      Input.Value := First; Replacement_Call (W.Set_Object, 8); Check (Staged and Saved);
      Receipt_For (P.Committed, 4);
      --  An unrelated Get transfer failure must not discard the accepted
      --  Set's saved reply. Its eventual authenticated receipt still finishes.
      Allow_Return := False; Replacement_Call (W.Get_Object);
      Is_Reply (W.Unavailable); Check (Active and Saved and R.Needs_Recovery (Replacement));
      R.Finish (Replacement, Store, Receipt); Is_Reply (W.Success, 42, 62);
      Check (not Saved and not R.Waiting (Replacement) and Last_Reply.words (0) = 4);
      Before_Replies := Replies; R.Finish (Replacement, Store, Receipt); Check (Replies = Before_Replies);
   end;
   Ada.Text_IO.Put_Line ("Config object grant/reply receiver: PASS" & Checks'Image & " checks");
end Receiver_Tests;

pragma Ada_2022;
with Ada.Text_IO;
with Ada.Exceptions;
with Interfaces;
with AML_Objects;
with ACPI_Requests;
with ACPI_Native_Server;
with CuBit.Messages;
procedure Completion_Tests is
   package Q renames ACPI_Requests;
   package M renames CuBit.Messages;
   use type Interfaces.Unsigned_64, ACPI_Native_Server.Stop_Reason, M.MessageWords;
   Server : Q.State (2, 4_096, 4_096, 2, 0,
      Value_Object_Capacity => AML_Objects.Max_Objects,
      Value_Byte_Capacity => AML_Objects.Max_Bytes,
      Value_Element_Capacity => AML_Objects.Max_Elements);
   Peer : constant Interfaces.Unsigned_64 := 16#FEED_BEEF_1234_0001#;
   First_Token : constant Interfaces.Unsigned_64 := 16#FEDC_BA98_7654_3210#;
   First_Request : constant Interfaces.Unsigned_64 := 16#ABCD_1234_5678_0001#;
   Reply_Words : constant M.MessageWords := [16#F123_4567_89AB_CDEF#, 0, 42, Interfaces.Unsigned_64'Last];
   Checks, Accepted, Callbacks, Owners, Events, Dispatches, Maintenance : Natural := 0;
   Raise_Callback, Refuse_Owner, Stop_On_Event : Boolean := False;
   Last_Status : Interfaces.Unsigned_64 := Interfaces.Unsigned_64'Last;
   Reason : ACPI_Native_Server.Stop_Reason;
   procedure Check (OK : Boolean; Why : String) is
   begin
      Checks := Checks + 1;
      if not OK then raise Program_Error with Why; end if;
   end Check;
   procedure Handle (State : in out Q.State; Item : M.CompletionEntry; Handled : out Boolean) is
   begin
      Check (Q.Valid (State), "completion receives valid owner state");
      Check (Owners = M.Test.Poll_Count, "completion precedes new owner and request turn");
      Callbacks := Callbacks + 1;
      if Raise_Callback then raise Program_Error with "modeled completion owner fault"; end if;
      Handled := Item.valid and then Item.from = Peer
        and then Item.token = First_Token + Interfaces.Unsigned_64 (Accepted)
        and then Item.requestId = First_Request + Interfaces.Unsigned_64 (Accepted)
        and then Item.msg.words = Reply_Words;
      if Handled then Accepted := Accepted + 1; Last_Status := Item.status; end if;
   end Handle;
   procedure Advance (State : in out Q.State; Result : out ACPI_Native_Server.Owner_Outcome) is
   begin
      Check (Q.Valid (State), "advance receives valid state");
      Check (M.Test.Completions_Taken <= Owners + 1, "at most one completion per owner turn");
      Owners := Owners + 1;
      Result := (if Refuse_Owner then ACPI_Native_Server.Owner_Refused else ACPI_Native_Server.Owner_Idle);
   end Advance;
   procedure Event (Publisher : M.Process_ID; Item : M.Message; Stop, Handled : out Boolean) is
      pragma Unreferenced (Item);
   begin
      Check (M.To_Word (Publisher) = Peer, "event keeps full publisher identity");
      Check (Accepted > 0 and Owners = Events, "completion then event then owner order");
      Events := Events + 1; Stop := Stop_On_Event; Handled := True;
   end Event;
   procedure Dispatch (State : in out Q.State; From : M.Process_ID;
      Request : M.Message; Reply : out M.Message) is
   begin
      Check (Q.Valid (State) and M.To_Word (From) = Peer, "request receives current state and full process");
      Check (Owners = Dispatches + 1 and Accepted >= Owners, "request opportunity after each completion-owner turn");
      Dispatches := Dispatches + 1; Reply := Request;
   end Dispatch;
   procedure Maintain (Deadline : out Interfaces.Unsigned_64; May_Continue : out Boolean) is
   begin
      Check (Owners = Maintenance + 1, "maintenance follows owner once per turn");
      Maintenance := Maintenance + 1; Deadline := 120; May_Continue := True;
   end Maintain;
   package Loop_Owner is new ACPI_Native_Server.With_Dispatch
     (Dispatch, Handle_Event => Event, Advance_Owner => Advance,
      Maintain_Owner => Maintain, Handle_Completion => Handle);
   procedure Reset is
   begin
      M.Reset; Accepted := 0; Callbacks := 0; Owners := 0; Events := 0;
      Dispatches := 0; Maintenance := 0; Raise_Callback := False;
      Refuse_Owner := False; Stop_On_Event := False;
      Last_Status := Interfaces.Unsigned_64'Last;
   end Reset;
   function Completion (Offset : Natural := 0) return M.CompletionEntry is
      Item : M.CompletionEntry := M.NULL_COMPLETION;
   begin
      Item.valid := True; Item.from := Peer;
      Item.token := First_Token + Interfaces.Unsigned_64 (Offset);
      Item.requestId := First_Request + Interfaces.Unsigned_64 (Offset);
      Item.msg.words := Reply_Words;
      return Item;
   end Completion;
begin
   -- No callback for an empty lane; the existing finite maintenance deadline
   -- is used without replacement or an implicit infinite wait.
   Reset; Loop_Owner.Serve (Server, Reason);
   Check (Reason = ACPI_Native_Server.Wait_Unavailable and Callbacks = 0 and Owners = 1,
      "empty lane preserves ordinary service turn");
   Check (M.Test.Wait_Count = 1 and M.Test.Deadlines (1) = 120, "original deadline retained");
   -- A completion-only turn must repoll before the first idle wait.
   Reset; M.Enqueue_Completion (Completion); Loop_Owner.Serve (Server, Reason);
   Check (Reason = ACPI_Native_Server.Wait_Unavailable and Accepted = 1 and Callbacks = 1,
      "recognized completion does not stop serving");
   Check (Owners = 2 and Maintenance = 2 and M.Test.Poll_Count = 2 and M.Test.Wait_Count = 1,
      "completion activity prevents premature idle wait");
   Check (M.Test.Completion_Count = 2 and Last_Status = M.COMPLETION_OK, "completion copied once then lane repolled");
   -- Full lanes preserve one completion/event/request opportunity per turn.
   Reset;
   for I in 0 .. 3 loop
      M.Enqueue_Completion (Completion (I));
      M.Enqueue_Event (M.From_Word (Peer), M.NULL_MESSAGE);
      M.Enqueue (M.From_Word (Peer), M.NULL_MESSAGE);
   end loop;
   Loop_Owner.Serve (Server, Reason);
   Check (Accepted = 4 and Callbacks = 4 and Events = 4 and Dispatches = 4 and Owners = 5,
      "bounded completion processing preserves event and IPC fairness");
   Check (M.Test.Reply_Count = 4 and M.Test.Wait_Count = 1 and M.Test.Completion_Count = 5,
      "all replies once and no wait between ready completion turns");
   -- Unknown, duplicate, malformed and wrong-peer completions never reach
   -- another owner step, request dispatch or wait.
   for Bad in 1 .. 5 loop
      declare Item : M.CompletionEntry := Completion; begin
         case Bad is
            when 1 => Item.token := Item.token + 1;
            when 2 => Item.from := Item.from xor 16#0100_0000_0000_0000#;
            when 3 => Item.valid := False;
            when 4 => Item.requestId := Item.requestId + 1;
            when others => Item.msg.words (3) := 0;
         end case;
         Reset; M.Enqueue_Completion (Item); M.Enqueue (M.From_Word (Peer), M.NULL_MESSAGE);
         Loop_Owner.Serve (Server, Reason);
         Check (Reason = ACPI_Native_Server.Unexpected_Completion and Accepted = 0 and Callbacks = 1,
            "unrecognized completion remains terminal");
         Check (Owners = 0 and Dispatches = 0 and Maintenance = 0 and M.Test.Wait_Count = 0,
            "refusal precedes all fresh work");
      end;
   end loop;
   Reset; M.Enqueue_Completion (Completion); M.Enqueue_Completion (Completion);
   Loop_Owner.Serve (Server, Reason);
   Check (Reason = ACPI_Native_Server.Unexpected_Completion and Accepted = 1 and Callbacks = 2 and Owners = 1,
      "duplicate token cannot consume the original request twice");
   -- Failure status is delivered unchanged to its owner, not normalized by
   -- the generic loop into success or an unrelated completion.
   declare Item : M.CompletionEntry := Completion; begin
      Reset; Item.status := M.COMPLETION_TARGET_DIED; M.Enqueue_Completion (Item);
      Loop_Owner.Serve (Server, Reason);
      Check (Accepted = 1 and Last_Status = M.COMPLETION_TARGET_DIED, "native failure status preserved");
   end;
   Reset; Stop_On_Event := True; M.Enqueue_Completion (Completion);
   M.Enqueue_Event (M.From_Word (Peer), M.NULL_MESSAGE);
   Loop_Owner.Serve (Server, Reason);
   Check (Reason = ACPI_Native_Server.Stop_Requested and Accepted = 1 and Events = 1 and Owners = 0,
      "stop still precedes new owner work after completion routing");
   Reset; Refuse_Owner := True; M.Enqueue_Completion (Completion);
   Loop_Owner.Serve (Server, Reason);
   Check (Reason = ACPI_Native_Server.Owner_Work_Refused and Accepted = 1 and Owners = 1 and Maintenance = 0,
      "owner refusal still stops before maintenance or IPC");
   Reset; Raise_Callback := True; M.Enqueue_Completion (Completion);
   declare Raised : Boolean := False; begin
      begin Loop_Owner.Serve (Server, Reason);
      exception when Error : Program_Error =>
         Raised := Ada.Exceptions.Exception_Message (Error) = "modeled completion owner fault";
      end;
      Check (Raised and Callbacks = 1 and Accepted = 0 and Owners = 0 and M.Test.Wait_Count = 0,
         "callback fault propagates without replay or new work");
   end;
   Ada.Text_IO.Put_Line ("Native completion routing checks" & Checks'Image);
end Completion_Tests;

with Interfaces; use Interfaces;
with CuBit.Handles;
with CuBit.Messages;
with CuBit.Power_Requests;
with CCL_Native_Bindings;

--  machine.* on CuBit: one Request to power.svc through the console's
--  power-console endpoint (CuBit.Power_Requests), answered at once.
package body CCL_Machine is
   package PR renames CuBit.Power_Requests;
   package M renames CCL.Interfaces.Machine;
   use type M.Operation;
   use type CuBit.Messages.Call_Ending;

   Next_Id : Unsigned_64 := 1;

   function Available return Boolean is (CuBit.Handles.Valid (CCL_Native_Bindings.Power));

   procedure Request
     (Op : M.Operation; Result : out M.Answer; Success : out Boolean; Why : out CuBit.Failures.Failure)
   is
      Msg : CuBit.Messages.Message := CuBit.Messages.NULL_MESSAGE;
      Tag : CuBit.Messages.MessageTag;
      Id : constant Unsigned_64 := Next_Id;
   begin
      Result := M.Unavailable;
      Success := False;
      Why := (others => <>);
      if not Available then
         Why := CuBit.Failures.Failed
           (CuBit.Failures.Unavailable, "machine: this program has no endpoint to power.svc",
            "the program's manifest must request (service ""power-console"" ""power"") from the startup plan");
         return;
      end if;
      Next_Id := Next_Id + 1;
      Msg.tag := (label => PR.Request_Label, length => PR.Request_Words, protocol => 0, ipc => <>, reserved => 0);
      Msg.words (PR.Request_Word) := Id;
      Msg.words (PR.Action_Word) := PR.Action'Enum_Rep (if Op = M.Halt then PR.Halt else PR.Power_Off);
      Tag := CuBit.Messages.capCall
        (CCL_Native_Bindings.Power, Msg, CuBit.Messages.Deadline_After (PR.Answer_Budget_Milliseconds));
      --  Any answer is a typed result; no answer is Unavailable.
      Success := True;
      if CuBit.Messages.Ending_Of (Tag) = CuBit.Messages.Call_Answered
        and then Tag.label = PR.Request_Label and then Msg.words (PR.Reply_Request_Word) = Id
        and then PR.Valid_Answer (Msg.words (PR.Answer_Word))
      then
         Result :=
           (case PR.To_Answer (Msg.words (PR.Answer_Word)) is
              when PR.Accepted     => M.Accepted,
              when PR.Busy         => M.Busy,
              when PR.Not_Approved => M.Not_Approved,
              when PR.Malformed    => M.Malformed,
              when PR.Not_Ready    => M.Not_Ready);
      end if;
   end Request;
end CCL_Machine;

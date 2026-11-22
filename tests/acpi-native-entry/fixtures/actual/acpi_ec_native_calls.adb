pragma Ada_2022;
with ACPI_Region_Protocol;
package body ACPI_EC_Native_Calls is
   package M renames CuBit.Messages;
   package H renames CuBit.Handles;
   package W renames ACPI_Region_Protocol;
   use type Interfaces.Unsigned_64;
   use type Registers.Operation_ID, Registers.Owner_ID;
   use type Registers.Register_ID, Registers.Epoch_ID;
   Slot : H.IPC.Saved_Reply;
   Token : Registers.Operation_ID := 0;
   Owner : Registers.Owner_ID := 0;
   Register : Registers.Register_ID := 0;
   Epoch : Registers.Epoch_ID := 0;
   Broken : Boolean := False;
   function Pending return Boolean is (Slot.Saved);
   function Faulted return Boolean is (Broken);
   function Message (Packet : W.Packet) return M.Message is
      Reply : M.Message := M.NULL_MESSAGE;
   begin
      Reply.tag := (label => Packet.Label, length => Packet.Length,
                    protocol => 0, ipc => <>, reserved => Packet.Reserved);
      Reply.words := M.MessageWords (Packet.Data);
      return Reply;
   end Message;
   function Denied return M.Message is
     (Message ((Label => W.Operation_Reply,
                Data => [Interfaces.Unsigned_64 (W.Outcome'Pos (W.Denied)), 0, 0, 0],
                others => <>)));
   procedure Answer (Reply : M.Message; Delivered : out Boolean) is
      Saved : constant H.Reply := Slot.Handle;
   begin
      --  The runtime consumes this handle even if the caller died. Forget it
      --  before the callback so a lost answer never becomes a replay attempt.
      Slot := (Saved => False, Why => H.ABI.Invalid_Argument);
      Token := 0; Owner := 0; Register := 0; Epoch := 0;
      Delivered := Answer_Saved (Saved, Reply);
   end Answer;
   procedure Begin_Call
     (Sender : M.Process_ID; Request : M.Message;
      Deadline : ACPI_EC_Types.Microseconds; Budget : ACPI_EC_Types.Work_Budget;
      Result : out Report)
   is
      Deferred : Boolean;
      Reply : W.Packet;
   begin
      Result := (others => <>);
      Result.Recovery_Retained := Registers.Retained_For_Recovery;
      if Broken or else Slot.Saved then
         Result.Status := (if Broken then Owner_Fault else Immediate_Reply);
         Result.Delivered := Answer_Current (Sender, Denied) /= 0;
         return;
      end if;
      Slot := Save;
      if not Slot.Saved then
         Result.Status := Save_Refused;
         Result.Delivered := Answer_Current (Sender, Denied) /= 0;
         return;
      elsif not H.Valid (Slot.Handle) then
         Broken := True; Result.Status := Owner_Fault; return;
      end if;
      Owner := Registers.Owner_ID (M.To_Word (Sender));
      Register := Registers.Register_ID (Request.words (1));
      Epoch := Registers.Epoch_ID (Request.words (0));
      Calls.Begin_Call
        (Owner, (Label => Request.tag.label, Length => Request.tag.length,
                 Flags => M.Flag_Byte (Request.tag), Reserved => Request.tag.reserved,
                 Data => W.Words (Request.words)),
         Deadline, Budget, Deferred, Token, Reply);
      if Deferred then
         if Token = 0 then
            Broken := True; Result.Status := Owner_Fault; return;
         end if;
         Result.Status := Awaiting_Transaction; Result.Token := Token;
      else
         Result.Status := Immediate_Reply;
         Answer (Message (Reply), Result.Delivered);
      end if;
   end Begin_Call;
   procedure Advance (Result : out Report) is
      Fact : Registers.Completion;
      Reply : W.Packet;
   begin
      Result := (others => <>);
      Result.Recovery_Retained := Registers.Retained_For_Recovery;
      if Broken then Result.Status := Owner_Fault; return;
      elsif not Slot.Saved then return;
      end if;
      Result.Token := Token;
      Registers.Step;
      Calls.Collect (Fact, Reply);
      if not Fact.Present then Result.Status := Awaiting_Transaction; return; end if;
      if Fact.Token /= Token or else Fact.Owner /= Owner
        or else Fact.Register /= Register or else Fact.Epoch /= Epoch
      then
         Broken := True; Result.Status := Correlation_Fault; return;
      end if;
      Result.Status := Completion_Reply;
      Result.Hardware := Fact.Status;
      Result.Effects_Possible := Fact.Effects_Possible;
      Result.Recovery_Retained := Fact.Needs_Recovery;
      Answer (Message (Reply), Result.Delivered);
   end Advance;
end ACPI_EC_Native_Calls;

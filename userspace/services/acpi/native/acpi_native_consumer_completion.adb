pragma Ada_2022;
with CuBit.Messages;
with CuBit.Monotonic;
with CuBit.Handles.IPC;
with CuBit.Firmware_Data_Exchange;
package body ACPI_Native_Consumer_Completion is
   package M renames CuBit.Messages;
   package H renames CuBit.Handles;
   package IPC renames H.IPC;
   package P renames Protocol;
   use type Interfaces.Unsigned_64, Interfaces.Unsigned_32, Interfaces.Unsigned_8, Interfaces.Unsigned_16;
   use type H.ABI.Kind, P.Receipt;
   View : Report;
   Saved_Target : H.Endpoint := H.No_Endpoint;
   Original_Message : M.Message := M.NULL_MESSAGE;
   Original_Deadline, Last_Clock : Interfaces.Unsigned_64 := 0;
   function Observe return Report is (View);
   function Quiet return Boolean is
     (not View.Working and then View.State not in Completion_Unknown | Invalid_Reply);
   function Completed_Received return Boolean is
     (not View.Working and then View.State = Receipt_Confirmed and then View.Receipt_Known
      and then P.Ran_To_End (View.Declared) and then View.Answer = P.Received);
   function Set_Deadline return Boolean is
      Now : constant Interfaces.Unsigned_64 := CuBit.Monotonic.Milliseconds;
   begin
      if Now < Last_Clock or else Now >= Original_Deadline
        or else Original_Deadline = Interfaces.Unsigned_64'Last then return False; end if;
      Last_Clock := Now;
      View.Deadline_Milliseconds := Now + Interfaces.Unsigned_64'Min
        (P.Completion_Budget_Milliseconds, Original_Deadline - Now);
      return True;
   end Set_Deadline;
   procedure Exchange is
      Message : M.Message := Original_Message;
   begin
      View.State := Completion_Unknown; View.Dispatched := True; View.Calls := View.Calls + 1;
      Message.tag := IPC.Call (Saved_Target, Message, View.Deadline_Milliseconds);
      if Message.tag.label >= M.Kernel_Reply_First then return; end if;
      if Message.tag.label /= P.Consumer_Initialization_Complete or else Message.tag.length /= P.Reply_Words
        or else M.Flag_Byte (Message.tag) /= 0 or else Message.tag.reserved /= 0
        or else Message.words (P.Reply_Request_Word) /= View.Request
        or else not P.Valid_Receipt (Message.words (P.Receipt_Word))
      then View.State := Invalid_Reply; return; end if;
      for I in Message.words'Range loop
         if I >= P.Reply_Words and then Message.words (I) /= 0 then View.State := Invalid_Reply; return; end if;
      end loop;
      -- Only a ran-to-end/Received declaration can release driver launches.
      -- Inconsistent counts are malformed, not a zero-child observation.
      if Message.words (P.Released_Word) /= 0
        and then (not P.Ran_To_End (View.Declared)
          or else Message.words (P.Receipt_Word) /= P.Receipt'Enum_Rep (P.Received))
      then View.State := Invalid_Reply; return; end if;
      View.Released := Message.words (P.Released_Word);
      View.Answer := P.Receipt'Enum_Val (Message.words (P.Receipt_Word));
      View.Receipt_Known := True; View.State := Receipt_Confirmed;
   end Exchange;
   procedure Submit (Target : H.Endpoint; Request, Epoch, Deadline : Interfaces.Unsigned_64;
      Status : P.Phase_Status; Result : out Report) is
      procedure Attempt is
      begin
         if not Completion_Quiet then View.State := Boundary_Refused; return; end if;
         if Request = 0 or else Epoch = 0 or else not M.Is_Process (M.Own_Process)
           or else Epoch /= M.To_Word (M.Own_Process)
         then View.State := Request_Refused; return; end if;
         declare D : constant IPC.Description := IPC.Describe (Target); begin
            if not D.Current or else D.Kind /= H.ABI.Endpoint or else not D.Held.Read
              or else D.Authority_Tag /= CuBit.Firmware_Data_Exchange.Authority_Tag
              or else not M.Is_Process (M.From_Word (D.Object))
            then View.State := Endpoint_Refused; return; end if;
         end;
         Saved_Target := Target; Original_Deadline := Deadline;
         Original_Message.tag := (label => P.Consumer_Initialization_Complete, length => P.Request_Words,
            protocol => 0, ipc => <>, reserved => 0);
         Original_Message.words := [P.Request_Word => Request, P.Epoch_Word => Epoch,
            P.Status_Word => P.Phase_Status'Enum_Rep (Status), others => 0];
         if not Completion_Quiet then View.State := Boundary_Refused; return; end if;
         if not Set_Deadline then View.State := Clock_Refused; return; end if;
         Exchange;
      end Attempt;
   begin
      if View.Attempted or else View.Working then Result := View; return; end if;
      View.Attempted := True; View.Working := True;
      View.Request := Request; View.Epoch := Epoch; View.Declared := Status;
      Attempt;
      View.Working := False; Result := View;
   end Submit;
   procedure Confirm (Result : out Report) is
   begin
      if View.Working or else View.Confirmation_Attempted
        or else View.State not in Completion_Unknown | Invalid_Reply then Result := View; return; end if;
      View.Working := True; View.Confirmation_Attempted := True;
      if Completion_Quiet and then Set_Deadline then Exchange; end if;
      View.Working := False; Result := View;
   end Confirm;
   procedure Abandon_Confirmation is
   begin
      if View.Working then return; end if;
      if View.State in Completion_Unknown | Invalid_Reply then View.Confirmation_Attempted := True; end if;
   end Abandon_Confirmation;
end ACPI_Native_Consumer_Completion;

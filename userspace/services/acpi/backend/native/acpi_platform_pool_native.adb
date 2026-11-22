pragma Ada_2022;
with CuBit.Firmware_Data_Exchange;
with CuBit.Monotonic;
with Platform_Pool_ABI;
with Capability_ABI;
package body ACPI_Platform_Pool_Native is
   package M renames CuBit.Messages;
   package Wire renames Owner.Wire;
   use type Word, H.ABI.Kind, H.Rights, IPC.Carried_Kind, M.Call_Ending;
   function Inspect_Endpoint (Target : H.Endpoint; Expected_Owner : Word) return Boolean is
      D : constant IPC.Description := IPC.Describe (Target);
   begin
      return M.Is_Process (M.From_Word (Expected_Owner))
        and then D.Current and then D.Kind = H.ABI.Endpoint and then D.Held.Read
        and then D.Authority_Tag = CuBit.Firmware_Data_Exchange.Authority_Tag
        and then D.Object = Expected_Owner;
   end Inspect_Endpoint;
   function Is_Present (Item : IPC.Carried) return Boolean is (Item.Kind = IPC.Present);
   function Is_Refused (Item : IPC.Carried) return Boolean is (Item.Kind = IPC.Refused);
   procedure Inspect_Pool (Item : IPC.Carried; Value : out H.Device_Memory; Accepted : out Boolean) is
      Taken : constant IPC.Accepted_Device_Memory := IPC.Accept_Device_Memory (Item);
   begin
      Value := H.No_Device_Memory; Accepted := False;
      if not Taken.OK then return; end if;
      declare D : constant IPC.Description := IPC.Describe (Taken.Value); begin
         if not D.Current or else D.Kind /= H.ABI.Device_Memory
           or else D.Authority_Tag /= Platform_Pool_ABI.Platform_Pool_Tag
           or else D.Held /= H.Read_Write then return; end if;
      end;
      Value := Taken.Value; Accepted := True;
   end Inspect_Pool;
   procedure Dispose (Item : in out IPC.Carried; Closed : out Boolean) is
      Result : constant IPC.Close_Result := IPC.Close (Item);
   begin
      Closed := Result.Closed;
      if Closed then Item := (Kind => IPC.Absent); end if;
   end Dispose;
   procedure Read_Clock (Milliseconds : out Word; Available : out Boolean) is
   begin
      -- capCall deadlines use GETTIME's epoch. Do not divide a fallback
      -- microsecond reading: the HPET fallback may have a different epoch.
      Milliseconds := CuBit.Monotonic.Milliseconds;
      Available := True;
   end Read_Clock;
   procedure Exchange (Target : H.Endpoint; Request, Deadline_Milliseconds : Word;
      Reply : out Owner.Response; Received : out IPC.Carried)
   is
      Msg : M.Message := M.NULL_MESSAGE;
   begin
      Reply := (others => <>); Received := (Kind => IPC.Absent);
      if Request = 0 or else Deadline_Milliseconds = 0
        or else Deadline_Milliseconds = M.Wait_Forever then return; end if;
      Msg.tag.label := Wire.Pool_Request;
      Msg.tag.length := Wire.Request_Words;
      Msg.words (Wire.Request_Word) := Request;
      Msg.tag := IPC.Call_Accepting (Target,Msg,Deadline_Milliseconds,Received);
      Reply := (Ending => Owner.Completion_Unknown, Label => Msg.tag.label,
         Length => Msg.tag.length, Protocol => Capability_ABI.Protocol_Flags (Msg.tag.protocol),
         IPC => Capability_ABI.IPC_Flags (Msg.tag.ipc),
         Reserved => Msg.tag.reserved, Data => Owner.Words (Msg.words));
      if M.Ending_Of (Msg.tag) = M.Call_Answered then Reply.Ending := Owner.Answered;
      elsif M.Ending_Of (Msg.tag) = M.Call_Not_Delivered then Reply.Ending := Owner.Not_Dispatched;
      end if;
   end Exchange;
   procedure Get is new Owner.Acquire (Exchange);
   procedure Acquire (State : in out Owner.Context; Target : H.Endpoint;
      Expected_Owner : M.Process_ID; Request, Deadline_Milliseconds : Word; Accepted : out Boolean) is
   begin
      Get (State,Target,M.To_Word (Expected_Owner),Request,Deadline_Milliseconds,Accepted);
   end Acquire;
end ACPI_Platform_Pool_Native;

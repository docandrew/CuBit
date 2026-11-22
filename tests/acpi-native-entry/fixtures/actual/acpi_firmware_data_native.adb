pragma Ada_2022;
with Interfaces;
with CuBit.Messages;
with CuBit.Monotonic;
with Firmware_Data_ABI;
with Capability_ABI;
package body ACPI_Firmware_Data_Native is
   package H renames CuBit.Handles;
   package Calls renames H.IPC;
   package W renames R.Wire;
   use type H.Endpoint, H.ABI.Kind, Calls.Carried_Kind;
   use type Interfaces.Unsigned_64, Interfaces.Unsigned_32, Interfaces.Unsigned_8, Interfaces.Unsigned_16;
   use type IPC.Time_Instant;
   procedure Initialize (Context : in out State; Destination : H.Endpoint; Accepted : out Boolean) is
   begin
      Accepted := False;
      if Context.Consumed then return; end if;
      Context.Consumed := True;
      if Destination = H.No_Endpoint then return; end if;
      declare D : constant Calls.Description := Calls.Describe (Destination); begin
         if not D.Current or else D.Kind /= H.ABI.Endpoint or else not D.Held.Read
           or else D.Authority_Tag /= W.Authority_Tag then return; end if;
      end;
      Context.Destination := Destination; Context.Ready := True; Accepted := True;
   end Initialize;
   procedure Clock (Now : out IPC.Time_Instant; Available : out Boolean) is
      Sample : constant CuBit.Monotonic.Reading := CuBit.Monotonic.Read;
      Microseconds_Per_Millisecond : constant := 1000;
   begin
      Now := 0; Available := Sample.Available;
      if Available then Now := IPC.Time_Instant (Sample.Microseconds / Microseconds_Per_Millisecond); end if;
   end Clock;
   procedure Exchange (Channel : in out State; Deadline : IPC.Time_Instant;
      Request : IPC.Packet; Reply : out IPC.Packet;
      Received : out Calls.Carried; Delivered : out IPC.Delivery) is
      Message : CuBit.Messages.Message := CuBit.Messages.NULL_MESSAGE;
      Now : IPC.Time_Instant; Available : Boolean;
   begin
      Reply := (others => <>); Received := (Kind => Calls.Absent); Delivered := IPC.Not_Dispatched;
      if not Channel.Ready or else Channel.Working or else Deadline = 0
        or else Deadline = IPC.No_Finite_Deadline or else Request.Label /= W.Data_Window_Request
        or else Request.Length /= W.Request_Words or else Request.Flags /= 0 or else Request.Reserved /= 0
        or else Request.Data (3) /= 0
        or else not W.Well_Formed (Request.Data (W.First_Page_Word), Request.Data (W.Page_Count_Word),
          Request.Data (W.Request_Word)) then return; end if;
      Clock (Now, Available); if not Available or else Now >= Deadline then return; end if;
      Message.tag := (label => Request.Label, length => Request.Length, protocol => 0, ipc => <>, reserved => 0);
      Message.words := CuBit.Messages.MessageWords (Request.Data);
      Channel.Working := True; Delivered := IPC.Completion_Unknown;
      Message.tag := Calls.Call_Accepting (Channel.Destination, Message, Interfaces.Unsigned_64 (Deadline), Received);
      Channel.Working := False;
      -- Preserve Received even on a kernel error; the owner must dispose of
      -- any returned capability before allowing another request.
      if Message.tag.label >= CuBit.Messages.Kernel_Reply_First then return; end if;
      Reply := (Message.tag.label, Message.tag.length, CuBit.Messages.Flag_Byte (Message.tag),
        Message.tag.reserved, IPC.Words (Message.words));
      Delivered := IPC.Reply_Received;
   end Exchange;
   function Present (Item : Calls.Carried) return Boolean is (Item.Kind = Calls.Present);
   procedure Inspect (Item : Calls.Carried; Evidence : out R.Grant_Evidence) is
      Taken : constant Calls.Accepted_Device_Memory := Calls.Accept_Device_Memory (Item);
   begin
      Evidence := (others => <>);
      if not Taken.OK then return; end if;
      declare D : constant Calls.Description := Calls.Describe (Taken.Value); begin
         if not D.Current or else D.Kind /= H.ABI.Device_Memory
           or else D.Object mod W.Page_Bytes /= 0 or else D.Parameter mod W.Page_Bytes /= 0
           or else D.Parameter = 0 then return; end if;
         Evidence := (Fresh_Current_Handle => True, Device_Memory => True,
           Tag => D.Authority_Tag, Extent => (D.Object / W.Page_Bytes, D.Parameter / W.Page_Bytes),
           Readable => D.Held.Read, Writable => D.Held.Write, Grantable => D.Held.Grant,
           Executable => D.Held.Execute, Revocable => D.Held.Revoke,
           Cache => (if D.Authority_Tag = Firmware_Data_ABI.Firmware_Data_Tag then R.Uncached
             elsif D.Authority_Tag = Capability_ABI.Firmware_NVS_Tag then R.Write_Back else R.Other_Cache));
      end;
   end Inspect;
   procedure Dispose (Item : in out Calls.Carried; Closed : out Boolean) is
   begin
      Closed := True;
      if Present (Item) then
         declare Result : constant Calls.Close_Result := Close_Carried (Item); begin Closed := Result.Closed; end;
      end if;
      if Closed then Item := (Kind => Calls.Absent); end if;
   end Dispose;
end ACPI_Firmware_Data_Native;

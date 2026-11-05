pragma Ada_2022;
with Interfaces;
with CuBit.Messages;
with CuBit.Monotonic;
with ACPI_Register_Roles;
package body ACPI_Firmware_Data_Frontend_Native is
   package H renames CuBit.Handles;
   package Calls renames H.IPC;
   package M renames CuBit.Messages;
   package F renames P.F;
   use type H.Endpoint, H.ABI.Kind, Calls.Carried_Kind, IPC.Time_Instant;
   use type Interfaces.Unsigned_64, Interfaces.Unsigned_32;
   procedure Initialize (Connection : in out Channel; Target : H.Endpoint; Accepted : out Boolean) is
   begin
      Accepted := False;
      if Connection.Consumed then return; end if;
      Connection.Consumed := True;
      if Target = H.No_Endpoint then return; end if;
      declare D : constant Calls.Description := Calls.Describe (Target); begin
         if not D.Current or else D.Kind /= H.ABI.Endpoint or else not D.Held.Read
           or else D.Authority_Tag /= Interfaces.Unsigned_64 (ACPI_Register_Roles.Frontend_Tag)
         then return; end if;
      end;
      Connection.Endpoint := Target; Connection.Ready := True; Accepted := True;
   end Initialize;
   procedure Clock (Now : out IPC.Time_Instant; Available : out Boolean) is
      Sample : constant CuBit.Monotonic.Reading := CuBit.Monotonic.Read;
   begin
      Now := 0; Available := Sample.Available;
      if Available then Now := IPC.Time_Instant (Sample.Microseconds / F.Microseconds_Per_Millisecond); end if;
   end Clock;
   procedure Exchange (Connection : in out Channel; Deadline : IPC.Time_Instant;
      Request : IPC.Packet; Reply : out IPC.Packet;
      Received : out Calls.Carried; Delivered : out IPC.Delivery) is
      Message : M.Message := M.NULL_MESSAGE;
      Now : IPC.Time_Instant;
      Available : Boolean;
   begin
      Reply := (Length => 0,others => <>); Received := (Kind => Calls.Absent);
      Delivered := IPC.Not_Dispatched;
      if not Connection.Ready or else Connection.Working or else Deadline = 0
        or else Deadline = IPC.No_Finite_Deadline or else not F.Decode (Request,False).Valid then return; end if;
      Clock (Now,Available); if not Available or else Now >= Deadline then return; end if;
      Message.tag := (label => Request.Label,length => Request.Length,protocol => 0,ipc => <>,reserved => 0);
      Message.words := M.MessageWords (Request.Data);
      Connection.Working := True; Delivered := IPC.Completion_Unknown;
      Message.tag := Calls.Call_Accepting (Connection.Endpoint,Message,Interfaces.Unsigned_64 (Deadline),Received);
      Connection.Working := False;
      -- Preserve any received handle even when the kernel reports an error.
      if Message.tag.label >= M.Kernel_Reply_First then return; end if;
      Reply := (Message.tag.label,Message.tag.length,M.Flag_Byte (Message.tag),
         Message.tag.reserved,IPC.Words (Message.words));
      Delivered := IPC.Reply_Received;
   end Exchange;
   function Present (Item : Calls.Carried) return Boolean is (Item.Kind = Calls.Present);
   procedure Dispose (Item : in out Calls.Carried; Closed : out Boolean) is
   begin
      Closed := True;
      if Present (Item) then
         declare Result : constant Calls.Close_Result := Close_Carried (Item); begin Closed := Result.Closed; end;
      end if;
      if Closed then Item := (Kind => Calls.Absent); end if;
   end Dispose;
end ACPI_Firmware_Data_Frontend_Native;

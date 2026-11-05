pragma Ada_2022;
with Interfaces;
with CuBit.Messages;
with CuBit.Monotonic;
with CuBit.Handles.IPC;
package body ACPI_Register_Transport is
   use Interfaces;
   use ACPI_Region_Protocol;
   use type CuBit.Handles.Endpoint;
   procedure Exchange
     (Transport : in out State; Destination : CuBit.Handles.Endpoint;
      Deadline : Time_Instant; Item : Packet; Reply : out Packet;
      Delivered : out Delivery) is
      pragma Unreferenced (Transport);
      Message : CuBit.Messages.Message := CuBit.Messages.NULL_MESSAGE;
      Candidate : Packet;
   begin
      Reply := (others => <>); Delivered := Not_Dispatched;
      if Destination = CuBit.Handles.No_Endpoint or else Deadline = No_Finite_Deadline
        or else Item.Label not in Read_Operation | Write_Operation
        or else Item.Length /= 4 or else Item.Flags /= 0 or else Item.Reserved /= 0
        or else Item.Data (3) /= 0
        or else not Valid_Request
          ((Epoch => Epoch_Number (Item.Data (0)), Register => Register_ID (Item.Data (1)),
            For_Write => Item.Label = Write_Operation, Value => Item.Data (2)))
        or else Unsigned_64 (Deadline) <= CuBit.Monotonic.Milliseconds
      then return; end if;
      Message.tag := (label => Item.Label, length => Item.Length,
        protocol => 0, ipc => <>, reserved => 0);
      Message.words := CuBit.Messages.MessageWords (Item.Data);
      -- Once IPC starts, no error is assumed to imply no hardware effects.
      Delivered := Completion_Unknown;
      Message.tag := CuBit.Handles.IPC.Call (Destination, Message, Unsigned_64 (Deadline));
      if Message.tag.label >= CuBit.Messages.Kernel_Reply_First then return; end if;
      Candidate := (Label => Message.tag.label, Length => Message.tag.length,
        Flags => CuBit.Messages.Flag_Byte (Message.tag), Reserved => Message.tag.reserved,
        Data => Words (Message.words));
      if not Valid_Reply (Candidate, Item.Label = Write_Operation) then return; end if;
      Reply := Candidate; Delivered := Reply_Received;
   end Exchange;
   procedure Dispatch
     (IO : in out Client.Context; Deadline : Time_Instant;
      Item : Register_Request; Reply : out Client.Result) is
      Transport : State;
   begin
      Client.Dispatch (IO, Transport, Time_Instant (CuBit.Monotonic.Milliseconds),
        Deadline, Item, Reply);
   end Dispatch;
end ACPI_Register_Transport;

pragma Ada_2022;
with Interfaces;
with CuBit.Messages;
with CuBit.Monotonic;
with CuBit.Handles.IPC;
with ACPI_Field_Protocol_Frames;
package body ACPI_Field_Protocol_Transport is
   package W renames ACPI_Region_Protocol;
   use type CuBit.Handles.Endpoint, W.Time_Instant, Interfaces.Unsigned_64, Interfaces.Unsigned_32;
   procedure Exchange
     (Destination : CuBit.Handles.Endpoint; Deadline : W.Time_Instant;
      Item : W.Packet; Reply : out W.Packet; Delivered : out W.Delivery) is
      Message : CuBit.Messages.Message := CuBit.Messages.NULL_MESSAGE;
      Candidate : W.Packet;
   begin
      Reply := (others => <>); Delivered := W.Not_Dispatched;
      if Destination = CuBit.Handles.No_Endpoint or else Deadline = W.No_Finite_Deadline
        or else not ACPI_Field_Protocol_Frames.Request_Valid (Item)
        or else Interfaces.Unsigned_64 (Deadline) <= CuBit.Monotonic.Milliseconds then return; end if;
      Message.tag := (label => Item.Label, length => Item.Length, protocol => 0, ipc => <>, reserved => 0);
      Message.words := CuBit.Messages.MessageWords (Item.Data);
      Delivered := W.Completion_Unknown;
      Message.tag := CuBit.Handles.IPC.Call (Destination, Message, Interfaces.Unsigned_64 (Deadline));
      if Message.tag.label >= CuBit.Messages.Kernel_Reply_First then return; end if;
      Candidate := (Message.tag.label, Message.tag.length, CuBit.Messages.Flag_Byte (Message.tag),
        Message.tag.reserved, W.Words (Message.words));
      if not ACPI_Field_Protocol_Frames.Reply_Valid (Item, Candidate) then return; end if;
      Reply := Candidate; Delivered := W.Reply_Received;
   end Exchange;
end ACPI_Field_Protocol_Transport;

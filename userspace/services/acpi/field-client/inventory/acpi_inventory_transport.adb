pragma Ada_2022;
with Interfaces;
with CuBit.Messages;
with CuBit.Monotonic;
with CuBit.Handles.IPC;
package body ACPI_Inventory_Transport is
   package Proto renames ACPI_Inventory_Protocol;
   package Wire renames Proto.Wire;
   use Interfaces;
   use type CuBit.Handles.Endpoint, Wire.Time_Instant, Wire.Words;
   procedure Read_Clock (Now : out Wire.Time_Instant; Available : out Boolean) is
   begin Now := Wire.Time_Instant (CuBit.Monotonic.Milliseconds); Available := True; end Read_Clock;
   procedure Exchange
     (Transport : in out State; Destination : CuBit.Handles.Endpoint;
      Deadline : Wire.Time_Instant; Item : Wire.Packet;
      Reply : out Wire.Packet; Delivered : out Wire.Delivery)
   is
      pragma Unreferenced (Transport);
      Message : CuBit.Messages.Message := CuBit.Messages.NULL_MESSAGE;
      Candidate : Wire.Packet;
   begin
      Reply := (others => <>); Delivered := Wire.Not_Dispatched;
      if Destination = CuBit.Handles.No_Endpoint or else Deadline = Wire.No_Finite_Deadline
        or else not Proto.Envelope_Valid (Item)
        or else Unsigned_64 (Deadline) <= CuBit.Monotonic.Milliseconds then return; end if;
      if Item.Label = Proto.Header_Operation then
         if Item.Data /= Wire.Words'[0, 0, 0, 0] then return; end if;
      elsif Item.Label = Proto.Part_Operation then
         if Item.Data (0) = 0 or else Item.Data (1) = 0 or else Item.Data (3) /= 0
           or else Item.Data (2) > Proto.Part_Kind'Pos (Proto.Part_Kind'Last) then return; end if;
      else return; end if;
      Message.tag := (label => Item.Label, length => Item.Length,
                      protocol => 0, ipc => <>, reserved => 0);
      Message.words := CuBit.Messages.MessageWords (Item.Data);
      Delivered := Wire.Completion_Unknown;
      Message.tag := CuBit.Handles.IPC.Call (Destination, Message, Unsigned_64 (Deadline));
      if Message.tag.label >= CuBit.Messages.Kernel_Reply_First then return; end if;
      Candidate := (Label => Message.tag.label, Length => Message.tag.length,
        Flags => CuBit.Messages.Flag_Byte (Message.tag), Reserved => Message.tag.reserved,
        Data => Wire.Words (Message.words));
      if (if Item.Label = Proto.Header_Operation then not Proto.Decode_Header (Candidate).Valid
          else not Proto.Decode_Part (Candidate).Valid) then return; end if;
      Reply := Candidate; Delivered := Wire.Reply_Received;
   end Exchange;
   procedure Load
     (Destination : CuBit.Handles.Endpoint;
      Deadline : Client.Wire.Time_Instant; Result : out Client.Snapshot)
   is
      Transport : State;
   begin Client.Load (Transport, Destination, Deadline, Result); end Load;
end ACPI_Inventory_Transport;

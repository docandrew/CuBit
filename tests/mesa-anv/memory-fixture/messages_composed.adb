with System.Storage_Elements;
with Intel_GPU_Buffer_Reply;
with Intel_GPU_Buffer_Requests;
with Intel_GPU_Buffer_Requests.Sharing;
with Intel_GPU_Buffer_Backing;
package body CuBit.Messages is
   function Session_Of (Sender, Stamp : Unsigned_64) return Unsigned_64 is
     (if Sender = 42 and Stamp = 99 then 99 else 0);
   function Ready return Boolean is (True);
   procedure Recipient_Of
     (Sender, Stamp : Unsigned_64; Slot : out CapabilitySlot; Identity : out Unsigned_64) is
   begin
      pragma Assert (Sender = 42 and Stamp = 99);
      Slot := 7;
      Identity := 7 * 2 ** 32 + 42;
   end Recipient_Of;
   package Server is new Intel_GPU_Buffer_Requests (Session_Of, Ready);
   package Maps is new Server.Sharing (Recipient_Of);
   Object : Server.Service;
   Table : Maps.Mapping_Table;
   function Syscall (Number : Unsigned_64;
     A, B, C, D, E, F : Unsigned_64 := 0) return Unsigned_64 is
      pragma Unreferenced (D, E, F);
      type Inspection is array (0 .. 5) of Unsigned_64;
   begin
      case Number is
         when SYSCALL_GETPID => return 10;
         when SYSCALL_INSPECT_CAPABILITY =>
            declare
               Output : Inspection with Import, Address =>
                 System.Storage_Elements.To_Address
                   (System.Storage_Elements.Integer_Address (C));
            begin
               pragma Assert (A = 10 and B = 7);
               Output := [1, 1, 0, 42, 0, Recipient_Generation];
            end;
            return 1;
         when others => raise Program_Error;
      end case;
   end Syscall;
   function capCall (Slot : CapabilitySlot; Msg : in out Message) return MessageTag is
      Stamp : constant Unsigned_64 := (if Slot = 63 then 99 else 0);
      Result : Server.Words;
      Ticket : Server.Ticket;
      Created : Maps.Mapping_ID := 0;
      Consumed : Boolean;
   begin
      pragma Assert (Msg.authorityTag = 0);
      if Msg.tag.label = Maps.Map_Label then
         Maps.Handle (Object, Table, 42, Stamp, Msg.tag.label, Msg.tag.length,
           Msg.tag.flags, Msg.tag.reserved, Server.Words (Msg.words), Result, Created);
      else
         Server.Handle (Object, 42, Stamp, Msg.tag.label, Msg.tag.length,
           Msg.tag.flags, Msg.tag.reserved, Server.Words (Msg.words), Result, Ticket);
         if Ticket /= 0 then
            pragma Assert (Ticket = 1 and Msg.words (2) = 8192);
            Server.Complete (Object, Ticket,
              Intel_GPU_Buffer_Reply.From_Linear (16#1000_0000#, Intel_GPU_Buffer_Backing.CPU_Base, 8192,
               16#1000_0000#), Result, Consumed);
            pragma Assert (Consumed);
         end if;
      end if;
      Msg.words := MessageWords (Result);
      if Fail_Delivery then
         Maps.Reject_Delivery (Table, Created);
         return (0, 0, 0, 0);
      end if;
      return Msg.tag;
   end capCall;
end CuBit.Messages;

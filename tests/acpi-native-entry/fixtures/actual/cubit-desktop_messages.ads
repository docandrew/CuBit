with CuBit.Desktop_Protocol;
with CuBit.Messages;
--  Transport adapter only. Sender identity and authority tags are deliberately
--  absent from the portable wire schema; callers obtain them from kernel IPC.
package CuBit.Desktop_Messages is
   function To_Wire (Item : CuBit.Messages.Message) return
     CuBit.Desktop_Protocol.Wire_Message;
   function From_Wire (Item : CuBit.Desktop_Protocol.Wire_Message) return
     CuBit.Messages.Message;
end CuBit.Desktop_Messages;

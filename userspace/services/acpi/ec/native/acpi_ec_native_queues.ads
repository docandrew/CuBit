pragma Ada_2022;
with CuBit.Messages;
package ACPI_EC_Native_Queues is
   procedure No_Local_Work (Runnable, Stop, Receive_Request : out Boolean);
   function Unexpected_Completion return Boolean;
   function Poll_Event
     (Item : out CuBit.Messages.Message;
      Publisher : out CuBit.Messages.Process_ID) return Boolean;
   procedure Poll_Request
     (Sender : out CuBit.Messages.Process_ID;
      Item : out CuBit.Messages.Message; Found : out Boolean);
   function Wait_Idle return Boolean;
   -- No source or control request is admitted by the default hook.
   procedure Ignore_Message
     (Sender : CuBit.Messages.Process_ID; Item : CuBit.Messages.Message;
      Stop, Handled : out Boolean);
end ACPI_EC_Native_Queues;

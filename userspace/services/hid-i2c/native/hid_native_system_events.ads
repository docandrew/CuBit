pragma Ada_2022;
with CuBit.Control_Events;
with CuBit.Messages;
with CuBit.Process_IDs;
generic
   with procedure Grant_Notice (Item : CuBit.Control_Events.Event);
   -- Nonblocking owner-specific logging/channel lifetime handling only.
   -- Never release HID buffers from a notice; their authoritative retirement
   -- remains in the I2C state machine. No independent event-queue consumer.
package HID_Native_System_Events is
   procedure Handle (Publisher : CuBit.Process_IDs.Process_ID;
      Item : CuBit.Messages.Message; Stop : out Boolean; Handled : out Boolean);
end HID_Native_System_Events;

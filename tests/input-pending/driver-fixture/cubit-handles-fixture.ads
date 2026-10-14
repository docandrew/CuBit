with Interfaces; use Interfaces;
--  Hosted tests only: handles from numbers. Never part of a CuBit program.
package CuBit.Handles.Fixture is
   function IO_Ports_Of (Word : Unsigned_64) return IO_Ports;
   function Notification_Of (Word : Unsigned_64) return Notification;
   function Interrupt_Of (Word : Unsigned_64) return Interrupt;
   function Endpoint_Of (Word : Unsigned_64) return Endpoint;
   function Word_Of (Item : IO_Ports) return Unsigned_64;
   function Word_Of (Item : Notification) return Unsigned_64;
end CuBit.Handles.Fixture;

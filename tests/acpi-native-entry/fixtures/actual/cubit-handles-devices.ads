------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Device and service authority by handle (CAP-002): mapping device
--  memory, port I/O, registering a service role, publishing events,
--  real-time reservations and boot-timeline milestones.
------------------------------------------------------------------------------
pragma Ada_2022;
with CuBit.Messages;

package CuBit.Handles.Devices is

   --  Map Pages device pages at Physical to Virtual in this process; Region
   --  must cover them (read-write unless Read_Only). True on success.
   function Map_Device
     (Region : Device_Memory; Physical, Virtual, Pages : Unsigned_64;
      Read_Only : Boolean) return Boolean;

   --  Map the boot framebuffer, which Region must cover: the address it
   --  is mapped at, or Unsigned_64'Last when refused.
   function Map_Framebuffer (Region : Device_Memory) return Unsigned_64;

   --  Port I/O through Ports (which must cover the access). Reads return the
   --  value, or Unsigned_64'Last when refused; writes return True on success.
   function In_8 (Ports : IO_Ports; Port : Unsigned_16) return Unsigned_64;
   function In_16 (Ports : IO_Ports; Port : Unsigned_16) return Unsigned_64;
   function In_32 (Ports : IO_Ports; Port : Unsigned_16) return Unsigned_64;
   function Out_8 (Ports : IO_Ports; Port : Unsigned_16; Value : Unsigned_8) return Boolean;
   function Out_16 (Ports : IO_Ports; Port : Unsigned_16; Value : Unsigned_16) return Boolean;
   function Out_32 (Ports : IO_Ports; Port : Unsigned_16; Value : Unsigned_32) return Boolean;

   --  Register this process for a service role; Authority (Write) covers it.
   --  This process's identity word, or Unsigned_64'Last.
   function Register (Role : Unsigned_64; Authority : Notification) return Unsigned_64;

   --  Publish an event to Destination: through a notification (Read)
   --  covering a role Destination is registered for, or an endpoint (Write)
   --  to it. False when its queue is full or the authority does not reach it.
   function Send_Event
     (Destination : CuBit.Messages.Process_ID; Msg : CuBit.Messages.Message;
      Authority : Notification) return Boolean;
   function Send_Event
     (Destination : CuBit.Messages.Process_ID; Msg : CuBit.Messages.Message;
      Authority : Endpoint) return Boolean;

   --  SET_LATENCY_CONTRACT for the real-time class: the reservation Authority
   --  covers. 0, or Unsigned_64'Last.
   function Reserve_Realtime
     (Period_Us, Budget_Us, Flags : Unsigned_64; Authority : Scheduling) return Unsigned_64;

   --  The kernel's boot timeline (docs/boot-timeline.md): Read needs a
   --  process handle with Read, Mark the milestone owner's notification.
   function Read_Timeline
     (Authority : Process; Buffer, Offset, Bytes : Unsigned_64) return Unsigned_64;
   function Mark_Milestone (Milestone : Unsigned_64; Authority : Notification)
     return Boolean;

end CuBit.Handles.Devices;

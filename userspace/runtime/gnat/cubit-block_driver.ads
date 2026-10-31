------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The block driver contract (DEV-002, docs/device-classes.md). Every
--  Block.Device.V1 provider instantiates this with its hardware hooks; the
--  generic owns everything else: decoding, write authority, grant
--  admission and return, OP_IDENTIFY_DEVICE, slots, deadlines and replies.
--
--  Requests are asynchronous: Post starts a command on a free slot and the
--  reply is sent when Finished reports it done. A command already finished
--  when Post returns is answered at once, without keeping the reply, so a
--  synchronous device (one that completes inside Post) costs a receive and
--  a reply per request.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Block_Requests; use CuBit.Block_Requests;
with CuBit.Block_Work; use CuBit.Block_Work;
with CuBit.Device_Identities;

generic
   --  The device's command slots, one request in flight per slot.
   type Slot is (<>);

   --  Whether writes need the write grant (no default: each driver says).
   Writes : Write_Admission;

   --  A posted command not finished within this many microseconds is
   --  cancelled and answered as failed.
   Deadline_Microseconds : Unsigned_64;

   --  Starts Work on Item. A write's data is read from Work.Granted.Buffer
   --  here; the grant stays held until the command is collected.
   with procedure Post (Item : Slot; Work : Work_Order);

   --  The command on Item has finished.
   with function Finished (Item : Slot) return Boolean;

   --  After Finished (Item): the outcome. A read's data is copied into
   --  Work.Granted.Buffer here, and only for a whole successful read.
   with procedure Collect
     (Item : Slot; Work : Work_Order; Outcome : out Work_Outcome);

   --  Abandons the command on Item; the slot is reused afterwards.
   with procedure Cancel (Item : Slot);

   --  A counted event (the first, then every doubling).
   with procedure Note (Event : Driver_Event; Count : Unsigned_64);
package CuBit.Block_Driver is

   --  Serves State and reports Identity to OP_IDENTIFY_DEVICE. With
   --  Write_Grant_Required, State is what a holder of the write grant sees;
   --  everyone else sees it read only.
   procedure Start
     (State    : Device_State;
      Identity : CuBit.Device_Identities.Identity)
     with Pre =>
       CuBit.Device_Identities.Well_Formed (Identity.Model)
       and then CuBit.Device_Identities.Well_Formed (Identity.Serial);

   --  Collects finished commands, cancels expired ones, then takes new
   --  requests while a slot is free. Progressed: any of these happened.
   procedure Poll (Progressed : out Boolean);

   --  Some command is in flight (the caller should not sleep past the
   --  device's completion).
   function Outstanding return Boolean;

   --  Blocks until a request arrives and takes it (call only with nothing
   --  in flight: Poll's deadlines are not watched while blocked).
   procedure Wait_And_Serve
     with Pre => not Outstanding;

   function Expired_Commands return Unsigned_64;
   function Refused_Writes return Unsigned_64;
end CuBit.Block_Driver;

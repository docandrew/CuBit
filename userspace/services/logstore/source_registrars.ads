pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Channels;
with CuBit.Control_Events;
with CuBit.Messages;
with Source_Identities;

--  logstore's side of the launchers' registrar channels (LOG-003,
--  CuBit.Log_Sources): each launcher (devmgr, procmgr) opens one, producing,
--  under the registrar tag only it can stamp, and puts a notice as it
--  starts each process and as each exits. Drain applies them to the table
--  of who each source is. The opener's identity and tag come from the
--  kernel, never from the channel; each notice is decoded as untrusted.
package Source_Registrars is
   --  Registrar channels are numbered from here: apart from publishers'
   --  (Publisher_Rings, from 1) and readers' (Stream_Writers.First_Number).
   First_Number : constant := 16#8000#;
   --  One channel per launcher at once; a launcher opening again replaces
   --  its channel.
   Maximum_Registrars : constant := 4;
   --  Notices taken from one channel per pass, so one launcher cannot hold
   --  the loop.
   Batch_Notices : constant := 64;

   type Table is limited private;

   --  Answer From's request to open a registrar channel (Authority: its
   --  kernel-stamped tag, which the caller checked is a registrar's).
   procedure Open
     (Item : in out Table; Identities : in out Source_Identities.Table;
      From : CuBit.Messages.Process_ID; Authority : Unsigned_64;
      Request : CuBit.Messages.Message; Now_Ms : Unsigned_64; Reply : out CuBit.Messages.Message);

   --  Whether Number (an OP_CLOSE's or OP_KICK's channel number) is a
   --  registrar channel's.
   function Owns (Number : Unsigned_64) return Boolean is
     (Number > First_Number and then Number <= First_Number + Maximum_Registrars);

   --  Drain and end the channel Request (From's OP_CLOSE) names, if From
   --  opened it.
   procedure Close
     (Item : in out Table; Identities : in out Source_Identities.Table;
      From : CuBit.Messages.Process_ID; Request : CuBit.Messages.Message; Now_Ms : Unsigned_64);

   --  A grant event: drain and end the channel it belongs to, if any.
   procedure Ended
     (Item : in out Table; Identities : in out Source_Identities.Table;
      Event : CuBit.Control_Events.Event; Now_Ms : Unsigned_64);

   --  Apply every channel's notices to Identities. Pending: some channel
   --  still holds notices.
   procedure Drain
     (Item : in out Table; Identities : in out Source_Identities.Table;
      Now_Ms : Unsigned_64; Pending : out Boolean);

   --  Before sleeping: ask every launcher for a kick, then look again.
   --  Pending: notices arrived meanwhile, so do not sleep.
   procedure Arm (Item : in out Table; Pending : out Boolean);
   procedure Disarm (Item : in out Table);
private
   type Registrar is record
      Owner : CuBit.Messages.Process_ID := CuBit.Messages.No_Process;
      Authority : Unsigned_64 := 0;
      Link : CuBit.Channels.Channel;
   end record;
   type Table is array (1 .. Maximum_Registrars) of Registrar;
end Source_Registrars;

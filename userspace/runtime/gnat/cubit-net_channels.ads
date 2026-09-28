------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Stream network channels for Ada programs (docs/netstack-redesign.md,
--  "Async channels"). The program lends netstack one grant per channel: a
--  header page, a send ring and a receive ring (CuBit.Net_Channel_Layout).
--  Reading and writing move bytes through the rings with no IPC; the ring
--  indices go through the proved CuBit.Channel_Rings, so netstack's values
--  are checked too. IPC happens only to open and close a channel, to kick
--  netstack when it asked for it, and to WAIT for readiness.
--
--  Everything here is asynchronous: OPEN, ACCEPT and WAIT are submitted
--  with a token and complete in the process's completion queue; the
--  program's own loop collects them. Wait_For is a small blocking helper
--  for programs with nothing else to do.
--
--  The C mirror for the libc is userspace/libc/overlay/src/cubit/net.c.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with System;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Memory_Grants;
with CuBit.Channel_Rings;
with CuBit.Net_Channel_Layout;
with CuBit.Net_Address;

package CuBit.Net_Channels is

   package Layout renames CuBit.Net_Channel_Layout;
   package Rings renames CuBit.Channel_Rings;

   subtype Wait_Bit is Natural range 0 .. Layout.Maximum_Wait_Bit;

   --  The memory a channel with these ring sizes needs (whole pages).
   function Buffer_Bytes (Tx_Size, Rx_Size : Rings.Ring_Size)
     return Natural is (Layout.Header_Bytes + Tx_Size + Rx_Size);

   --  Memory lent to netstack once, cut into Count channel buffers of
   --  Buffer_Bytes (Tx_Size, Rx_Size) each. Channels (outbound or accepted)
   --  each occupy one buffer; the process decides which, so many channels
   --  cost one grant.
   type Arena is record
      Base    : System.Address := System.Null_Address;
      Grant   : CuBit.Memory_Grants.Grant_Reference;
      Handle  : Unsigned_64 := 0;
      Tx_Size : Rings.Ring_Size := Rings.Minimum_Size;
      Rx_Size : Rings.Ring_Size := Rings.Minimum_Size;
      Count   : Natural := 0;
   end record;

   type Stream is record
      Base   : System.Address := System.Null_Address;
      Arena  : Unsigned_64 := 0;
      Buffer : Natural := 0;
      Tx     : Rings.Producer;
      Rx     : Rings.Consumer;
      Bit    : Wait_Bit := 0;
      Handle : Unsigned_64 := 0;
      --  netstack broke the ring rules: the channel is unusable.
      Broken : Boolean := False;
   end record;

   --  Lend Memory (Count * Buffer_Bytes (Tx_Size, Rx_Size), page aligned,
   --  at most one grant) to netstack through the endpoint in Slot. A call:
   --  arenas are set up once, not per channel.
   procedure Create_Arena
     (A       : out Arena;
      Slot    : CapabilitySlot;
      Memory  : System.Address;
      Tx_Size : Rings.Ring_Size;
      Rx_Size : Rings.Ring_Size;
      Count   : Positive;
      OK      : out Boolean)
   with Pre => Count <= Layout.Maximum_Arena_Buffers;

   --  Take the arena back (no channel may hold a buffer) and revoke it.
   procedure Release_Arena
     (A : in out Arena; Slot : CapabilitySlot; OK : out Boolean);

   --  Use buffer Buffer of A for a stream and lay out its header. Bit is
   --  this channel's bit in WAIT and KICK masks; the program keeps them
   --  distinct.
   procedure Prepare
     (S      : in out Stream;
      A      : Arena;
      Buffer : Natural;
      Bit    : Wait_Bit;
      Slot   : CapabilitySlot)
   with Pre => Buffer < A.Count;

   --  Reuse a prepared stream's buffer for a new channel: reset its header
   --  and rings (after the previous channel was closed).
   procedure Reset (S : in out Stream);

   --  Submit OPEN for Target ("@net:tcp:host:port"); its completion
   --  carries the handle (Opened).
   procedure Submit_Open
     (S : in out Stream; Slot : CapabilitySlot; Target : String;
      Token : Unsigned_64; Submitted : out Boolean)
   with Pre => Target'Length in 1 .. Layout.Target_Maximum;

   --  The completion of an OPEN: True if the channel is open.
   function Opened (S : in out Stream; Reply : Message) return Boolean;

   --  Listeners (Layout, "Listeners"): Submit_Open on
   --  "@net:tcp-listen:<address>:<port>" makes S a listener, whose rings
   --  carry offers and arrivals instead of bytes; Close closes it.

   --  Offer buffer Buffer of A for Listener's next arriving connection.
   --  Prepare it first: its header (ring sizes, wait bit) is read when a
   --  connection takes it.
   procedure Offer
     (Listener : in out Stream; A : Arena; Buffer : Natural;
      Offered  : out Boolean);

   type Arrival is record
      Channel : Unsigned_64 := 0;   --  the new channel's handle
      Arena   : Unsigned_64 := 0;
      Buffer  : Natural := 0;       --  the offered buffer it lives in
      --  the peer (IPv4 mapped)
      Address : CuBit.Net_Address.Address := CuBit.Net_Address.Unspecified;
      Port    : Unsigned_16 := 0;
   end record;

   --  The next connection that arrived on Listener, if any.
   procedure Take_Arrival
     (Listener : in out Stream; Item : out Arrival; Found : out Boolean);

   --  The channel's status (Layout.Status_*).
   function Status (S : Stream) return Unsigned_32;
   function Final (S : Stream) return Boolean is
     (S.Broken or else Status (S) >= Layout.Status_Peer_Finished);
   function Failed (S : Stream) return Boolean is
     (S.Broken or else Status (S) >= Layout.Status_Reset);

   --  Receiving: the first contiguous readable bytes (Length 0 if none),
   --  then Consume what was used.
   procedure Readable
     (S : in out Stream; First : out System.Address; Length : out Natural);
   procedure Consume (S : in out Stream; Count : Natural);
   --  Copy up to Length received bytes to Into.
   procedure Read
     (S : in out Stream; Into : System.Address; Length : Natural;
      Got : out Natural);

   --  Sending: the first contiguous free bytes, then Commit what was
   --  written there.
   procedure Writable
     (S : in out Stream; First : out System.Address; Length : out Natural);
   procedure Commit (S : in out Stream; Count : Natural);
   --  Copy up to Length bytes from From into the send ring.
   procedure Write
     (S : in out Stream; From : System.Address; Length : Natural;
      Put : out Natural);

   --  Connected UDP: one datagram of Length bytes (at most
   --  Layout.Datagram_Maximum) into the send ring; Sent is False if it is
   --  too long or the ring is full.
   procedure Send_Datagram
     (S : in out Stream; From : System.Address; Length : Natural;
      Sent : out Boolean);
   --  The oldest received datagram into Into (at most Length bytes;
   --  Truncated if it was longer). Found is False if none is waiting.
   procedure Receive_Datagram
     (S : in out Stream; Into : System.Address; Length : Natural;
      Got : out Natural; Truncated : out Boolean; Found : out Boolean);

   --  Ask netstack to report this channel to the process's WAIT when it
   --  has what Flags (Layout.Want_* flags, replacing earlier ones) name.
   --  Look at the rings again afterwards: netstack may have moved before
   --  it could see the flags.
   procedure Want (S : Stream; Flags : Unsigned_32);

   --  FIN once netstack has sent what the send ring holds.
   procedure Shut_Write (S : in out Stream; Slot : CapabilitySlot);

   --  Release the channel (netstack takes what is left in the send ring,
   --  then sends FIN) and its grant.
   procedure Close (S : in out Stream; Slot : CapabilitySlot);

   --  Submit this process's WAIT: it completes with the wait bits of the
   --  ready channels among Interest, or 0 at Deadline (monotonic
   --  milliseconds). netstack services the channels in Kicks first.
   procedure Submit_Wait
     (Slot : CapabilitySlot; Kicks, Interest : Unsigned_64;
      Deadline : Unsigned_64; Token : Unsigned_64; Submitted : out Boolean);

   --  This stream's bit in WAIT and KICK masks.
   function Mask (S : Stream) return Unsigned_64 is
     (Shift_Left (Unsigned_64'(1), S.Bit));

   --  Received bytes wait, or the stream has ended.
   function Has_Input (S : in out Stream) return Boolean;
   --  The send ring has room, or the stream failed.
   function Has_Room (S : in out Stream) return Boolean;

   --  Block until the stream has input (Flags has Want_Readable) or room
   --  (Want_Writable), or Deadline (monotonic milliseconds) passes, using
   --  WAIT with Token. Uses Wait_For: only for programs whose only
   --  asynchronous work is this.
   procedure Await
     (S : in out Stream; Slot : CapabilitySlot; Flags : Unsigned_32;
      Deadline : Unsigned_64; Token : Unsigned_64; Ready : out Boolean);

   --  Block until the completion with Token arrives. Completions for other
   --  tokens that arrive first are dropped: only for programs whose only
   --  asynchronous work is this.
   procedure Wait_For (Token : Unsigned_64; Reply : out Message;
                       OK : out Boolean);

end CuBit.Net_Channels;

------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The shared memory layout and operations of a stream network channel
--  (docs/netstack-redesign.md, "Async channels"). The client lends netstack
--  one grant: a header page, then the send ring, then the receive ring.
--  The C mirror is userspace/libc/overlay/src/cubit/net_channel.h.
--
--  Each 64-byte header line has one writer. The client writes its line and
--  the open-time fields; netstack writes its own line. Netstack reads the
--  sizes and the wait bit once, when the channel opens.
------------------------------------------------------------------------------
package CuBit.Net_Channel_Layout with Pure, SPARK_Mode is

   Header_Bytes : constant := 4_096;

   --  The client's line: 32-bit values at these byte offsets.
   Tx_Produced_At : constant := 0;    --  bytes written into the send ring
   Rx_Consumed_At : constant := 4;    --  bytes read from the receive ring
   Want_At        : constant := 8;    --  Want_* flags: notify me
   Shut_Write_At  : constant := 12;   --  nonzero: FIN once the ring drains
   --  Read by netstack once, at open.
   Tx_Size_At     : constant := 16;   --  send ring bytes (a ring size)
   Rx_Size_At     : constant := 20;   --  receive ring bytes
   --  0 .. 63: this channel's bit in WAIT and KICK masks.
   Wait_Bit_At    : constant := 24;

   --  Netstack's line.
   Tx_Consumed_At : constant := 64;   --  send ring bytes taken
   Rx_Produced_At : constant := 68;   --  bytes written into the receive ring
   Kick_Wanted_At : constant := 72;   --  Kick_On_* flags
   Status_At      : constant := 76;   --  a Status_* value

   --  The target ("@net:tcp:host:port"), written before OPEN.
   Target_At      : constant := 256;
   Target_Maximum : constant := 255;

   Want_Readable : constant := 1;   --  received bytes, or a final status
   Want_Writable : constant := 2;   --  send ring space, or a final status

   Kick_On_Send    : constant := 1;   --  netstack drained the send ring
   Kick_On_Receive : constant := 2;   --  netstack waits for receive ring space

   Status_Opening        : constant := 0;
   Status_Open           : constant := 1;
   Status_Peer_Finished  : constant := 2;   --  the last byte is in the ring
   Status_Reset          : constant := 3;
   Status_Timed_Out      : constant := 4;
   Status_Unreachable    : constant := 5;
   Status_Protocol_Error : constant := 6;   --  the client broke the ring rules

   Maximum_Wait_Bit : constant := 63;

   --  Connected UDP: datagram records in both rings (CuBit.Datagram_Rings),
   --  each at most an unfragmented datagram's payload (1500-byte MTU less
   --  IPv4 and UDP headers).
   Datagram_Maximum : constant := 1472;

   --  Operations beyond OPEN, ACCEPT and SHUT.
   --  WAIT (async submit): words 0 = kick mask, 1 = absolute deadline in
   --  monotonic milliseconds (or none: 2 ** 64 - 1), 2 = interest mask:
   --  the channels whose readiness should complete it. Completes with
   --  REPLY_OK and words 0 = the ready channels of interest (0 at the
   --  deadline, or when a KICK ends it). One WAIT per process may be
   --  outstanding.
   OP_NET_WAIT : constant := 16#0428#;
   --  KICK (one-way submit): words 0 = kick mask; words 1 = End_Wait to
   --  also complete the process's WAIT now (something outside netstack
   --  became ready). No reply.
   OP_NET_KICK : constant := 16#0429#;
   End_Wait    : constant := 1;
   --  ARENA (call): lend netstack one grant cut into channel buffers, each
   --  Header_Bytes and both rings, laid out back to back. Words 0 = grant
   --  slot, 1 = grant generation, 2 = send ring size (low 32 bits) and
   --  receive ring size (high 32 bits), 3 = buffer count (at most
   --  Maximum_Arena_Buffers, and all within one grant). Replies REPLY_OK
   --  with words 0 = the arena handle. OPEN and ACCEPT then name an arena
   --  handle and a buffer index instead of a grant.
   OP_NET_ARENA : constant := 16#042A#;
   --  ARENA_RELEASE (call): words 0 = arena handle. Refused while a channel
   --  holds one of its buffers; on success netstack no longer maps it.
   OP_NET_ARENA_RELEASE : constant := 16#042B#;
   Maximum_Arena_Buffers : constant := 1024;
   --  SCOPE (call, on any network endpoint): what that endpoint's scope
   --  allows, so a program with several can route each connection to the
   --  one that permits it. Replies REPLY_OK with words 0 and 1 = the
   --  network (CuBit.Net_Address, IPv4 mapped), its 16 bytes as they lie in
   --  memory (byte 0 of the address is the low byte of word 0), and 2 = its
   --  descriptor (CuBit.Network_Authority.Descriptor: ports, prefix,
   --  operation, DNS, connections) whose prefix is over the 128 bits (an
   --  IPv4 /24 is 120).
   OP_NET_SCOPE : constant := 16#042C#;

   --  Listeners: OPEN of "@net:tcp-listen:<address>:<port>" makes the
   --  buffer a listener, checked against the scope's tcp-listen grant; SHUT
   --  closes it and resets connections nobody took. Its rings carry
   --  datagram records (CuBit.Datagram_Rings):
   --  - send ring, offers: buffers for arriving connections, each already
   --    laid out as a channel (ring sizes, wait bit): arena handle (64
   --    bits), buffer index (32 bits);
   --  - receive ring, arrivals: a connection now open in an offered buffer:
   --    channel handle (64), arena handle (64), buffer index (32), peer
   --    port (16), zero (16), peer address (16 bytes, CuBit.Net_Address:
   --    IPv4 mapped).
   --  An arrival is readable like stream data, so one WAIT covers
   --  listeners and streams. netstack asks for a kick (Kick_On_Send) when
   --  a connection waits for an offer.
   Offer_Bytes          : constant := 12;
   Offer_Arena_At       : constant := 0;
   Offer_Buffer_At      : constant := 8;
   Arrival_Bytes        : constant := 40;
   Arrival_Channel_At   : constant := 0;
   Arrival_Arena_At     : constant := 8;
   Arrival_Buffer_At    : constant := 16;
   Arrival_Port_At      : constant := 20;
   Arrival_Address_At   : constant := 24;
   Address_Bytes        : constant := 16;

   --  Control queues (docs/async-rings.md): OPEN and SHUT without a
   --  message each. QUEUE (call) lends netstack a grant of Queue_Bytes:
   --  words 0 = grant slot, 1 = grant generation. Replies REPLY_OK. The
   --  queue acts with the authority of the endpoint QUEUE came through,
   --  and one per endpoint (scope) is allowed.
   --  Layout: a header page (submission header at Queue_Submissions_At,
   --  completion header at Queue_Completions_At, each with the words
   --  below), then Queue_Slots submission entries at Queue_Requests_At
   --  and Queue_Slots completion entries at Queue_Answers_At, each
   --  Queue_Entry_Bytes. Indices are free-running entry counts
   --  (CuBit.Slot_Rings); the client produces requests and consumes
   --  answers.
   OP_NET_QUEUE : constant := 16#042D#;
   Queue_Bytes          : constant := 8_192;
   Queue_Slot_Bits      : constant := 6;
   Queue_Slots          : constant := 64;
   Queue_Entry_Bytes    : constant := 32;
   Queue_Submissions_At : constant := 0;
   Queue_Completions_At : constant := 2_048;
   Queue_Requests_At    : constant := 4_096;
   Queue_Answers_At     : constant := 6_144;
   --  Header words (as CuBit.Frame_Rings): the producer's count; the
   --  consumer's count, on its own line; and the consumer's wake word, a
   --  new nonzero epoch each time it is about to sleep. After producing,
   --  a client KICKs with Kick_Queue only if the submission wake word
   --  shows an epoch it has not kicked yet. netstack's answers complete
   --  the process's WAIT (words 1 = Wait_Answers).
   Queue_Produced_At : constant := 0;
   Queue_Consumed_At : constant := 64;
   Queue_Wake_At     : constant := 68;
   Kick_Queue        : constant := 2;
   Wait_Answers      : constant := 1;
   --  A request: token (64 bits, returned with the answer), operation
   --  (32), then OPEN: target length (32), arena handle (64), buffer
   --  index (32); SHUT: channel handle in the arena's place.
   Request_Token_At     : constant := 0;
   Request_Operation_At : constant := 8;
   Request_Length_At    : constant := 12;
   Request_Object_At    : constant := 16;
   Request_Buffer_At    : constant := 24;
   Queue_Open : constant := 1;
   Queue_Shut : constant := 2;
   --  An answer: token (64), status (32: Answer_OK or Answer_Refused),
   --  value (64: the channel handle of an OPEN).
   Answer_Token_At  : constant := 0;
   Answer_Status_At : constant := 8;
   Answer_Value_At  : constant := 16;
   Answer_OK      : constant := 0;
   Answer_Refused : constant := 1;

end CuBit.Net_Channel_Layout;

------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The libc's output streams (CuBit.Streams' layout and protocol;
--  docs/c-removal.md): stdout and stderr as producer-owned rings that
--  subscribers read through a read-only grant. Subscription requests reach
--  the process's mailbox, whose owner (the libc's dispatcher thread,
--  CuBit.Libc_Descriptors) passes them to Handle_Message.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with Interfaces.C;
with System;

package CuBit.Libc_Streams is

   procedure Create (Stream : Unsigned_16; Pages : Interfaces.C.unsigned;
                     Type_Tag : Unsigned_16)
   with Export, Convention => C, External_Name => "cubit_stream_create";

   --  Produce into a ring the program's launcher lent it (its header is
   --  the launcher's; docs/ccl-launch-parameters.md, "Launcher-owned port
   --  rings"), mapped at Base, Pages long.
   procedure Adopt (Stream : Unsigned_16; Pages : Interfaces.C.unsigned; Base : System.Address)
   with Export, Convention => C, External_Name => "cubit_stream_adopt";

   function Write (Stream : Unsigned_16; Data : System.Address; Length : Unsigned_32;
                   Type_Tag : Unsigned_16) return Unsigned_32
   with Export, Convention => C, External_Name => "cubit_stream_write";

   --  1 if the message was a stream request (it has been replied to).
   function Handle_Message (From : Interfaces.C.long; Message : System.Address)
     return Interfaces.C.int
   with Export, Convention => C, External_Name => "cubit_stream_handle_message";

   --  Whether writes poll the mailbox for subscription requests (a program
   --  without a mailbox owner); the libc's descriptors clear it.
   Poll_On_Write : Interfaces.C.int := 1
   with Export, Convention => C, External_Name => "cubit_stream_poll_on_write";

end CuBit.Libc_Streams;

with System;
with Interfaces; use Interfaces;

--  Hosted only: the request queue's regions, memory this process shares
--  with the mock filesystem service (Files_Mock_Service). The hosted
--  Files_Queue body runs the client side of the rings over them; on CuBit
--  Files_Queue's body is CuBit.Filesystem_Sessions (userspace/apps/files/
--  native/files_queue.adb).
package Files_Link is
   --  Open the queue with a transfer arena of Arena_Pages pages.
   procedure Open (Arena_Pages : Positive; Success : out Boolean);
   --  This client's region (it writes), the service's (read-only here) and
   --  the transfer arena.
   function Client_Region return System.Address;
   function Server_Region return System.Address;
   function Arena return System.Address;
   function Arena_Bytes return Unsigned_64;
   --  The queue channel's kick: new requests wait.
   procedure Kick;
   --  Ask to be woken when answers wait (OP_FS_WAKE, submitted without
   --  waiting): the platform's event loop then returns from its wait, so
   --  nothing polls while requests are out. One is held at a time; arming
   --  again supersedes it. Answers already waiting wake at once.
   procedure Arm_Wake;
   procedure Close;
end Files_Link;

with System;
with CuBit.Filesystem_Events;
with Interfaces; use Interfaces;

--  A stand-in for CuBit's filesystem service on Linux (tests/files-app):
--  it serves the real request-queue layout (CuBit.Filesystem_Queues) and
--  Directory.Page.V2 pages from a task of its own, so the app's side is
--  exactly what runs on CuBit. Places:
--    @host:0/       a host directory, read-only
--    @scratch:0/    the scratch directory, read-write
--    @synthetic:N/  N generated entries (no disk), for benchmarks
--  It also serves change watches (with an event ring), the granted scopes,
--  made-up volume descriptions and server-side copies (512 KiB slices, up
--  to four at once, progress in the service region).
--  Paths are checked as the real service does: at most 4096 bytes, no "."
--  or ".." components, a known place.
package Files_Mock_Service is
   procedure Configure (Host_Root, Scratch_Root : String);
   procedure Start (Client, Server, Arena : System.Address; Arena_Bytes : Unsigned_64);
   procedure Kick;
   --  OP_FS_WAKE: call the wake hook once answers wait in the client's
   --  queue (at once if they do already). The hook runs on the service's
   --  task; the hosted window posts an event to its own loop from it.
   type Wake_Hook is access procedure;
   procedure Set_Wake_Hook (Hook : Wake_Hook);
   procedure Arm_Wake;
   --  Rename Old_Path to New_Path (Queue_Rename; also callable directly).
   procedure Rename (Old_Path, New_Path : String; Status : out Unsigned_32);
   --  Change watches: the client opened its event ring (Queue_Watch then
   --  works); Take_Record takes the oldest record, as the ring holds it.
   procedure Enable_Events;
   procedure Take_Record
     (Into : out CuBit.Filesystem_Events.Record_Bytes; Used : out Natural; Got : out Boolean);
   procedure Stop;
   --  A delay before each answer, to show streaming as slow storage would.
   procedure Set_Latency (Microseconds : Natural);
   --  Requests answered so far.
   function Served return Natural;
end Files_Mock_Service;

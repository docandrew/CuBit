------------------------------------------------------------------------------
--  ipctest-client's channel clients at library level (CAP-002 step 3), so a
--  second thread can share them: the per-thread completion routing test.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with CuBit.Channel_Clients;
with IPCtest_Channel;

package IPCtest_Channel_Clients is
   package IC renames IPCtest_Channel;
   package Clients is new CuBit.Channel_Clients (IC.RC, IC.Element);

   --  Channels within the per-client limit (the first two are A and B).
   Pool : array (1 .. IC.Per_Client) of Clients.Client;

   --  The second thread: Calls on Pool (1) with values Worker_Base + K, at
   --  the same time as the main thread's; counts what it got right.
   Worker_Calls : constant := 5_000;
   Worker_Base  : constant Unsigned_64 := 1_000_000;
   type Word is mod 2 ** 32 with Atomic, Size => 32;
   Worker_Right : aliased Word := 0;
   Worker_Exit  : aliased Word := 1;   --  cleared by the kernel when it ends
   procedure Worker (Argument : Unsigned_64) with Convention => C;

   --  Start Worker on its own stack; False if the kernel refused.
   function Start_Worker return Boolean;
   --  Wait for Worker to end.
   procedure Join_Worker;
end IPCtest_Channel_Clients;

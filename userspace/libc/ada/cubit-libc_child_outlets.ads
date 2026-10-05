------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  A C launcher's children's diagnostics (docs/self-hosting.md, decision
--  D3): posix_spawn lends each child a ring for its unix.stderr outlet, in
--  this process's memory (docs/ccl-launch-parameters.md, "Launcher-owned
--  outlet rings"), and while the launcher waits, what arrives there is
--  copied to its own descriptor 2. The gcc driver's cc1, as and ld report
--  through the driver this way, and so to whoever launched it.
--
--  @description
--  A ring is lent only when the program's description declares a
--  unix.stderr outlet and a slot is free; otherwise the child is started
--  without one, as before. Its pages are kept for the next child once the
--  kernel confirms the grant retired. Text is copied as written, while the
--  launcher waits for any child (waitpid) and once more when the child
--  ends; a child writing faster than that loses its oldest output (the
--  ring drops oldest), and is never blocked by it.
--
--  All calls are made under the libc's child lock (CuBit.Libc_Process).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces.C;
with CuBit.Outlet_Rings;

package CuBit.Libc_Child_Outlets is

   subtype int is Interfaces.C.int;

   --  A ring slot, or none.
   Maximum_Forwarded : constant := 4;
   subtype Slot_Choice is Natural range 0 .. Maximum_Forwarded;
   No_Slot : constant Slot_Choice := 0;

   --  Lend a ring for Program's unix.stderr, if it has one: Rings is the
   --  table for OP_LAUNCH (no entries when Slot is No_Slot).
   procedure Prepare (Program : String; Rings : out CuBit.Outlet_Rings.Table;
                      Slot : out Slot_Choice);

   --  The launch succeeded (Process) or failed: the ring is then watched,
   --  or given back.
   procedure Started (Slot : Slot_Choice; Process : int);
   procedure Abandon (Slot : Slot_Choice);

   --  Copy what every watched ring holds to descriptor 2.
   procedure Forward_All;

   --  Process ended: copy the rest of its ring and give it back.
   procedure Ended (Process : int);

   --  Whether any child's ring is watched (the launcher then waits with a
   --  deadline, to copy as it goes).
   function Any_Watched return Boolean;

end CuBit.Libc_Child_Outlets;

with Boot_Framebuffer;
with Boot_Panel;
with System;
--  Kernel bootstrap graphics only. No terminal, allocator, IPC or mode setting.
--  Setup runs after per-CPU interrupt state and the direct mapping are ready.
package Boot_Diagnostics with SPARK_Mode => Off is
   procedure Setup (Item : Boot_Framebuffer.Description);
   procedure Begin_Step (Text : String);
   procedure Complete_Step (Text : String);
   procedure Set_Evidence (R : Boot_Panel.Evidence_Row; Text : String);
   procedure Append (C : Character);
   procedure Panic (Message : System.Address);
   --  Authorized takeover calls this before handing out the boot mapping or
   --  letting a native boot-adapter driver change its mode. Returns only once
   --  admitted paints have drained; no Setup/Panic operation can reopen it.
   procedure Retire;

   --  Screen diagnostics that outlive Retire, for hardware without a serial
   --  console. Both write the firmware scanout through the mapping Setup
   --  admitted (the Intel driver keeps that scanout protected), take no lock
   --  and touch only their own pixels. Without Setup they do nothing.
   --
   --  Heartbeat: called on CPU 0's timer interrupt. A small square in the
   --  bottom-right corner changes about twice a second while the kernel takes
   --  interrupts, so a frozen screen with a still-blinking square means a
   --  userspace stall, and a stopped square means the kernel stopped.
   procedure Heartbeat;
   --  Emergency: a fatal stop's message across the top of the screen. Never
   --  waits for another CPU's painter: the system is stopping. Message is a
   --  NUL-terminated string; Detail is printed after it (for example an
   --  exception vector and RIP), empty when not needed.
   procedure Emergency (Message : System.Address; Detail : String);
   --  Stuck_Calls: up to Stuck_Rows lines naming threads blocked in a
   --  synchronous call for a long time, drawn above the heartbeat (CPU 0's
   --  timer, every few seconds). Count = 0 clears a previous report.
   Stuck_Rows : constant := 12;
   subtype Stuck_Line is String (1 .. 112);
   type Stuck_Lines is array (1 .. Stuck_Rows) of Stuck_Line;
   procedure Stuck_Calls (Lines : Stuck_Lines; Count : Natural);
end Boot_Diagnostics;

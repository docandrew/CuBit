with Boot_Framebuffer;
with System;
--  Kernel bootstrap graphics only. No terminal, allocator, IPC or mode setting.
--  Setup runs after per-CPU interrupt state and the direct mapping are ready.
package Boot_Diagnostics with SPARK_Mode => Off is
   procedure Setup (Item : Boot_Framebuffer.Description);
   procedure Begin_Step (Text : String);
   procedure Complete_Step (Text : String);
   procedure Append (C : Character);
   procedure Panic (Message : System.Address);
   --  Authorized takeover calls this before handing out the boot mapping or
   --  letting a native boot-adapter driver change its mode. Returns only once
   --  admitted paints have drained; no Setup/Panic operation can reopen it.
   procedure Retire;
end Boot_Diagnostics;

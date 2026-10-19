with Interfaces;
with CuBit.Messages;
with Intel_GPU_Buffer_Reply;
with Intel_GPU_Firmware_Output;
with Intel_GPU_GGTT_Reservations;
--  intel-gpu's display executor role (docs/display-modesetting.md 5): it
--  answers only display.svc, on the GPU plane protocol (0x0A10..0x0A15)
--  and the scanout protocol (0x0A03..0x0A09), for the one pipe that scans
--  out the firmware framebuffer.
--
--  DISP-003: the hardware cursor plane on that pipe.
--  DISP-000a: tear-free flips of that pipe's primary plane between three
--  GGTT scanout buffers (Intel_GPU_Primary_Flip), on the firmware mode.
--  display.svc copies each frame's damage into a buffer the display engine
--  is not reading and presents it; the reply comes when the flip has
--  latched. When the takeover flip fails, Get_Status reports no scanout
--  and display.svc keeps the firmware framebuffer (reason logged here).
--
--  The formals are intel-gpu's retained state (trusted, never IPC input).
generic
   with function Display_Owner return Boolean;
   with function Pipe_A_Held return Boolean;
   with function Pipe_B_Held return Boolean;
   with function Pipe_C_Held return Boolean;
   with function Pipe_D_Held return Boolean;
   --  GGTT takeover held, publication owner ready, address layout valid.
   with function Publication_Ready return Boolean;
   --  The runtime GGTT range (exclusive limit).
   with function Runtime_First return Interfaces.Unsigned_64;
   with function Runtime_Limit return Interfaces.Unsigned_64;
   --  No overlap with any plane or cursor the firmware left live.
   with function Scanout_Clear (First, Bytes : Interfaces.Unsigned_64) return Boolean;
   with procedure Invalidate (Success : out Boolean);
   with procedure Log (Text : String);
   --  Read-only display MMIO (register BAR offset), all ones on failure.
   with function Read_MMIO (Offset : Interfaces.Unsigned_32) return Interfaces.Unsigned_32;
package Intel_GPU_Display_Executor is
   use Interfaces;

   --  Register as the GPU service so display.svc's manifest request
   --  (request-service gpu) resolves to this driver. Called once, after the
   --  supervisor's bootstrap message has been received.
   procedure Register_Service;

   --  Boot: choose the pipe from the inventory's plane-1 samples and the
   --  framebuffer the kernel reports, then grant and map its register page.
   procedure Select_Output
     (Planes : Intel_GPU_Firmware_Output.Primaries;
      Table_Bytes, Register_Base : Unsigned_64);
   function Output_Selected return Boolean;

   --  Pages the cursor needs from the driver's buffer arena: the staging
   --  image display writes, then the surfaces plus one guard page.
   Cursor_Pages : constant := 257;

   --  Boot, serialized, before the request loop: publish the cursor
   --  surfaces in GGTT and program the cursor's data-buffer watermark.
   procedure Setup_Cursor
     (Ledger : in out Intel_GPU_GGTT_Reservations.Ledger;
      Backing : Intel_GPU_Buffer_Reply.Backing);

   --  Pages per scanout buffer (frame plus a guard page), zero when the
   --  firmware mode cannot be flipped (no output, or a frame above one
   --  driver allocation, Intel_GPU_Buffer_Backing.Page_Count).
   function Scanout_Pages return Natural;
   type Scanout_Backings is array (0 .. 2) of Intel_GPU_Buffer_Reply.Backing;
   --  Boot, after Setup_Cursor (the cursor's DDB sequence goes first):
   --  publish the buffers and take the plane over with one latched flip.
   procedure Setup_Scanout
     (Ledger : in out Intel_GPU_GGTT_Reservations.Ledger;
      Backings : Scanout_Backings);

   function Owns_Label (Label : Unsigned_32) return Boolean;
   --  Answers every request it owns; the reply is sent here.
   procedure Handle (Sender : CuBit.Messages.Process_ID;
                     Request : CuBit.Messages.Message);
   --  Once per loop turn: flip completion (answers the owed Present at
   --  latch) and the per-second cursor and flip lines.
   procedure Service;
   procedure Report (Now_Ms : Unsigned_64);
   --  Deadline (SYSCALL_GETTIME milliseconds) for the loop's idle wait so
   --  the owed flip is polled just after the predicted vblank;
   --  Unsigned_64'Last when nothing is owed.
   function Wake_Ms (Now_Ms : Unsigned_64) return Unsigned_64;
end Intel_GPU_Display_Executor;

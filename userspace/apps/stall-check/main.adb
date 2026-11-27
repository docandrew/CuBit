with CuBit.Console_Route;
with CuBit.Process_Calls;
with CuBit.Waits;
with Interfaces; use Interfaces;
with CuBit.Messages; use CuBit.Messages;
with CuBit.Handles.IPC;
with CuBit.Handles.Startup;
with CuBit.Startup_Tables;
with Startup;
with CuBit.Monotonic;

--  Kernel stuck-call panel check (headless test scanout-repair): one call
--  to this process's own endpoint, which nobody receives, held until its
--  deadline. Past the kernel's stuck-call threshold (Process.IPC
--  Stuck_Call_Ms, 3 s) the kernel draws its panel on the boot framebuffer;
--  the report after the call ends erases it. display.svc must then present
--  a whole desktop frame, so no black band stays on screen.
procedure Main is
   use ASCII;
   package ST renames CuBit.Startup_Tables;
   package HIPC renames CuBit.Handles.IPC;
   --  Longer than Stuck_Call_Ms plus one report period (2 s): the panel
   --  is drawn at least once.
   Stall_Ms : constant := 5_500;
   --  Two report periods: the panel is erased before this process ends.
   Settle_Ms : constant := 4_500;
   Probe_Label : constant Unsigned_32 := 16#0E01#;
   Self : constant CuBit.Handles.Endpoint :=
     CuBit.Handles.Startup.Endpoint_Named (ST.Self_Text);
   Msg : Message := NULL_MESSAGE;
   Tag : MessageTag;
   Now : Unsigned_64;
   Ignore : Unsigned_64;
begin
   --  ABI-002: debug text into its logstore ring, never the kernel console.
   CuBit.Console_Route.Route_Named ("logstore");
   if not CuBit.Handles.Valid (Self) then
      debugPrint ("stall-check: FAIL startup capabilities" & LF);
      return;
   end if;
   Now := CuBit.Monotonic.Milliseconds;
   Msg.tag := (label => Probe_Label, length => 0, protocol => 0, ipc => <>, reserved => 0);
   debugPrint ("stall-check: holding a call for" & Unsigned_64'Image (Unsigned_64'(Stall_Ms)) & " ms" & LF);
   Tag := HIPC.Call (Self, Msg, Now + Stall_Ms);
   debugPrint ("stall-check: call ended label=" & Unsigned_32'Image (Tag.label) & LF);
   Ignore := CuBit.Waits.Sleep_For (Settle_Ms);
   debugPrint ("stall-check: done" & LF);
   CuBit.Process_Calls.Exit_Process (CuBit.Process_Calls.Success);
end Main;

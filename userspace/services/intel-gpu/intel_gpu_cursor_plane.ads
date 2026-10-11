with Interfaces;
with Intel_GPU_Cursor_Program;
with Intel_GPU_Cursor_Surfaces;
with Intel_GPU_Cursor_DDB;
--  The Intel cursor plane behind display.svc's GPU plane protocol
--  (CuBit.GPU_Plane_Protocol: Show, Move, Hide) for the one pipe that
--  scans out the firmware framebuffer (docs/display-planes.md, DISP-003).
--
--  - Show reads the image display staged, picks a surface the display
--    engine cannot be reading (Intel_GPU_Cursor_Surfaces.Pick), copies the
--    image there at the pitch of the smallest legal cursor that holds it,
--    flushes it from the CPU caches (the display engine does not snoop),
--    then programs the cursor in i915 order (FBC, control, position, base).
--  - Move writes CURPOS and CURBASE, the base write arming the position
--    for the next vblank (i915 i9xx_cursor_update_arm). The newest position
--    wins in hardware; no frame is touched and nothing waits.
--  - A position entirely off the pipe disables the plane until a later
--    move brings it back.
--
--  The formal writers belong to the selected pipe and are gated there on
--  display ownership and the pipe's power reference.
generic
   with function Program (Values : Intel_GPU_Cursor_Program.Register_Values)
     return Boolean;
   with function Move (Values : Intel_GPU_Cursor_Program.Register_Values)
     return Boolean;
   --  A register of the pipe's page (offset within the page); all ones when
   --  it cannot be read.
   with function Read (Offset : Interfaces.Unsigned_32) return Interfaces.Unsigned_32;
   with procedure Write (Offset, Value : Interfaces.Unsigned_32; Written : out Boolean);
   with function Flush (Address, Bytes : Interfaces.Unsigned_64) return Boolean;
   --  Monotonic milliseconds, and one bounded pause while waiting for a
   --  vblank (Configure only, at boot).
   with function Now_Ms return Interfaces.Unsigned_64;
   with procedure Pause;
package Intel_GPU_Cursor_Plane is
   use Interfaces;
   package CP renames Intel_GPU_Cursor_Program;
   package CS renames Intel_GPU_Cursor_Surfaces;

   Display_Version : constant CP.Display_Version := 13;

   type Status is (Accepted, Bad_State);

   --  Staging: CPU address of the Edge x Edge staging image. Surfaces_CPU
   --  and Surfaces_GGTT: the first of Surface_Count consecutive surfaces of
   --  Surface_Bytes each, in CPU and GGTT space.
   --
   --  When the firmware left the cursor no data buffer, Configure gives it
   --  one (Intel_GPU_Cursor_DDB.Plan) in two latched steps, so the plane-1
   --  and cursor ranges never overlap, even for one frame:
   --  1. plane 1's watermarks and shrunk PLANE_BUF_CFG, armed by rewriting
   --     PLANE_SURF with its current value (i915's noarm-then-SURF commit);
   --     wait until PIPE_FRMCOUNT has advanced twice;
   --  2. the cursor's watermarks and CUR_BUF_CFG, armed by the disabled
   --     cursor's CURBASE write; wait again.
   procedure Configure
     (Staging, Surfaces_CPU : Unsigned_64; Surfaces_GGTT : Unsigned_32;
      Pipe_Width, Pipe_Height : Positive; Single_Pipe : Boolean;
      Result : out CS.Readiness);

   --  The registers Configure read before and after, for the log.
   type DDB_Snapshot is record
      Plane_Buffer, Cursor_Buffer, Plane_Surface : Unsigned_32 := 0;
      Other_Buffers : Intel_GPU_Cursor_DDB.Other_Planes := [others => 0];
      Plane_WM, Cursor_WM : Intel_GPU_Cursor_DDB.Watermarks := [others => 0];
   end record;
   function Before return DDB_Snapshot;
   function After return DDB_Snapshot;
   function Plan_State return Intel_GPU_Cursor_DDB.Outcome;
   function Plane_Levels_Disabled return Natural;
   function Ready return Boolean;

   procedure Show (X, Y : Integer; Result : out Status);
   procedure Move_To (X, Y : Integer; Result : out Status);
   procedure Hide (Result : out Status);

   --  What the last Show programmed, for the log.
   function Image return CP.Image;
   function Enabled return Boolean;

   type Counters is record
      Shows, Moves, Move_Writes, Hides, Off_Pipe, Failures : Natural := 0;
   end record;
   --  Counts since the previous call.
   procedure Take (Item : out Counters);

   --  Diagnostic: the cursor DDB words read by Configure.
   function Buffer_Config return Unsigned_32;
   function Watermark return Unsigned_32;
end Intel_GPU_Cursor_Plane;

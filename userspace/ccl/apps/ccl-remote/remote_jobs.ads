------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Builds the remote session starts (docs/remote-management.md,
--  "Asynchronous work and disconnects"): ccl-build.app on a component in
--  the workspace, its two outlets captured, its result kept.
--
--  @description
--  A job belongs to this endpoint, not to the connection that started it:
--  a host that disconnects polls again later with the job's identity.
--  One build runs at a time (they share the Storehouse); Start answers
--  Busy meanwhile.
--
--  Output: each job keeps the newest Output_Bytes of what ccl-build wrote
--  (stdout lines as written, stderr lines prefixed "[stderr] ": whole
--  lines, however the outlets deliver them, and a last unterminated line
--  when the run ends; Remote_Output_Lines), with the total it ever wrote. A poll from an offset older than the oldest kept
--  byte is told the first kept offset, so a slow host sees exactly how much
--  it missed; nothing is dropped silently.
--
--  Retention (the orphan policy): a finished job is kept until the host
--  has polled it to its end after it finished, or until Orphan_Retention_Ms
--  after it finished when its slot is needed for a new job.
--
--  Cancel sends the running ccl-build an Interrupt (its libc ends it as
--  SIGINT would); steps it already started finish on their own.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces;
with Remote_Wire;

package Remote_Jobs is
   package W renames Remote_Wire;

   Maximum_Jobs : constant := 4;
   Output_Bytes : constant := 262_144;
   Orphan_Retention_Ms : constant := 3_600_000;

   --  Work: the work root ("@system/work"). Builds need it on the system
   --  volume, which holds the Storehouse and the toolchain (Start answers
   --  Unavailable otherwise). Without a process manager grant a started
   --  job ends Failed_To_Start, saying why.
   procedure Initialize (Work : String);

   procedure Start (Component : String; Job : out Interfaces.Unsigned_32; Result : out W.Status);

   --  The job's state, and its output from Offset in Output's first Length bytes.
   procedure Poll
     (Job : W.Job_Id; Offset : W.File_Offset; State : out W.Job_State;
      Exit_Code : out Interfaces.Unsigned_32; Total, First_Kept : out Interfaces.Unsigned_64;
      Output : out W.Byte_Array; Length : out Natural);

   procedure Cancel (Job : W.Job_Id; State : out W.Job_State);

   --  Drain outlets and collect exits: call often while Running.
   procedure Pump;
   function Running return Boolean;
end Remote_Jobs;

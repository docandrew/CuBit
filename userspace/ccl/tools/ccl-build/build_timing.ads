with Ada.Finalization;
with Ada.Real_Time;
with Interfaces;

--  Where a build's time goes (PERF-010): measurement only, the same on
--  Linux and CuBit (Ada.Real_Time's monotonic clock: on CuBit the libc
--  reads the published clock page, no system call).
--
--  Time is charged exclusively: to the innermost Category in effect, so
--  the categories of a step, or of a whole build, add up to its wall time.
--  A Scope object enters its category for as long as it lives (also when
--  an exception leaves it). Outside any scope time is Planning: ccl-build's
--  own work (declarations it has not been asked to time, step encoding,
--  path joins).
--
--  With Enable (ccl-build build --timings) each step and each build prints
--  one line of JSON on standard output, prefixed "timing " (the build's
--  "ran"/"hit" lines are unchanged). Schema ccl-build.timing/1:
--    kind "step":  label, identity (12 hex digits), ran (Boolean),
--                  wall_us, us {category: microseconds, nonzero only},
--                  counts {counter: count, nonzero only}
--    kind "build": components (the operands), wall_us, us and counts from
--                  this process's elaboration (its own start, after the
--                  system loaded it) to the end of the last component
--  Nothing is printed without Enable, but the accounting always runs
--  (a clock read per scope edge).
package Build_Timing is
   use Interfaces;

   type Category is
     (Planning,      --  outside any scope: ccl-build's own work
      Store_Open,    --  opening the Storehouse: its shared lock, liveness lock, work cleanup
      Declarations,  --  reading and checking build-params, catalog, component.ccl
      Import,        --  toolchain trees: hashing and recording them
      Source_Hash,   --  SHA-256 of source files and trees (snapshots)
      Output_Hash,   --  SHA-256 of a step's outputs
      Verify_Hash,   --  re-hashing a store entry this process first uses
      Store_Write,   --  copying into the Storehouse, sealing, publishing, fsync
      Recall,        --  action-cache lookups (without their Verify_Hash)
      Claim,         --  claiming a step directory (lock, owner record)
      Place_Inputs,  --  putting inputs in the step directory (links; copies on CuBit)
      Spawn,         --  starting the tool, until the child exists (CuBit: procmgr loads it)
      Tool_Wait,     --  the tool running until it exits and is reaped
      Release,       --  removing the step directory
      Export);       --  copying products to --out

   type Counter is
     (Steps_Ran, Steps_Hit,
      Files_Hashed, Bytes_Hashed,         --  every SHA-256 of a file
      Files_Copied, Bytes_Copied,         --  copies (CuBit's links, store entries, exports)
      Files_Linked,                       --  hard links (Linux)
      Directories_Made,                   --  in step directories
      Syncs,                              --  fsync of a file or directory
      Spawns,                             --  programs ccl-build started
      Tool_CPU_Us);                       --  the tools' user+system CPU (Linux wait4; 0 on CuBit)

   procedure Enable;
   function Enabled return Boolean;

   procedure Enter (C : Category);
   procedure Leave;

   type Scope (C : Category) is new Ada.Finalization.Limited_Controlled with null record;
   overriding procedure Initialize (S : in out Scope);
   overriding procedure Finalize (S : in out Scope);

   procedure Count (C : Counter; By : Unsigned_64 := 1);

   type Totals is private;
   function Now return Totals;

   --  One "timing" line for a step (since Start) or a build.
   procedure Report_Step (Label, Identity : String; Ran : Boolean; Start : Totals);
   procedure Report_Build (Components : String);

private
   type Category_Spans is array (Category) of Ada.Real_Time.Time_Span;
   type Counter_Values is array (Counter) of Unsigned_64;
   type Totals is record
      At_Time : Ada.Real_Time.Time;
      Spans : Category_Spans;
      Counts : Counter_Values;
   end record;
end Build_Timing;

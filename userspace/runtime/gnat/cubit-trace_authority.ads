pragma Ada_2022;
with Interfaces;

--  The manifest request for the kernel trace object (Request.Trace,
--  Trace_ABI): a request, never a grant. procmgr installs a copy with
--  Write only when the program's startup entry approves it, (trace
--  approve-declared), and only for a program the trusted startup plan
--  started: the benchmark programs. Its startup entry has kind Trace; the
--  program reads it with CuBit.Handles.Startup.Trace_Named and calls
--  CuBit.Handles.Tracing.
package CuBit.Trace_Authority with Pure, SPARK_Mode is
   --  Its .cubit.caps request kind (after the platform power object's 21).
   Manifest_Request : constant Interfaces.Unsigned_8 := 22;
   --  Its rights field in .cubit.caps: Write_Only (the encoding's 2).
   Manifest_Rights : constant Interfaces.Unsigned_8 := 2;
end CuBit.Trace_Authority;

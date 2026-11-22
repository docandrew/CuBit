------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The kernel trace object (Trace_ABI): reset the kernel's scheduling and
--  system-call trace, print its summary, and (LATENCY_TRACE kernels) the
--  calling CPU's event timeline, through Call on a Trace handle.
--
--  @description
--  A kernel object: the Call runs in the kernel with no process switch.
--  Benchmarks hold one (Request.Trace in the manifest, approved by the
--  startup plan) and bracket a measured run: Reset before, Summary after.
------------------------------------------------------------------------------
pragma Ada_2022;
with Trace_ABI;

package CuBit.Handles.Tracing is

   package ABI renames Trace_ABI;

   type Outcome (Answered : Boolean := False) is record
      case Answered is
         --  The kernel's answer (Done, or why not).
         when True  => Result : ABI.Outcome := ABI.Malformed;
         --  The handle names no trace object, or the reply was unreadable.
         when False => null;
      end case;
   end record;

   function Done (Result : Outcome) return Boolean is
     (Result.Answered and then ABI."=" (Result.Result, ABI.Done));

   function Perform (Object : Trace; Operation : ABI.Operation) return Outcome;

end CuBit.Handles.Tracing;

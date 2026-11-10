------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  What the GNAT binder's main program expects of the run time (RT-008).
--
--  @description
--  With Suppress_Standard_Library False (system.ads), gnatbind's adainit
--  stores the partition's settings into the __gl_* globals and main calls
--  __gnat_initialize, __gnat_runtime_initialize, __gnat_runtime_finalize
--  and __gnat_finalize. Upstream GNAT defines them in C (init.c,
--  adaint.c); here they are Ada. The values are init.c's defaults (the
--  binder overwrites most of them); the hooks have nothing to do on CuBit:
--  no signal handlers (faults are the kernel's), no argv (typed program
--  parameters), no exception tracebacks yet (EXC-001).
--
--  Nothing withs this package: the binder's references to these symbols
--  pull its object out of libgnat-user.a. It has no elaboration code.
------------------------------------------------------------------------------
with Interfaces.C;
with System;

package CuBit.Binder_Interface is
   pragma No_Elaboration_Code_All;

   subtype int is Interfaces.C.int;
   use type Interfaces.C.int;

   No_Address : constant System.Address := System'To_Address (0);
   --  Null_Address, but static (no elaboration code)

   Unset : constant int := -1;
   --  "Not specified" for priorities, CPUs, time slices and stack sizes

   Main_Priority : int := Unset
     with Export, Convention => C, External_Name => "__gl_main_priority";
   Time_Slice_Value : int := Unset
     with Export, Convention => C, External_Name => "__gl_time_slice_val";
   WC_Encoding : Character := 'n'
     with Export, Convention => C, External_Name => "__gl_wc_encoding";
   Locking_Policy : Character := ' '
     with Export, Convention => C, External_Name => "__gl_locking_policy";
   Queuing_Policy : Character := ' '
     with Export, Convention => C, External_Name => "__gl_queuing_policy";
   Task_Dispatching_Policy : Character := ' '
     with Export, Convention => C, External_Name => "__gl_task_dispatching_policy";
   Priority_Specific_Dispatching : System.Address := No_Address
     with Export, Convention => C, External_Name => "__gl_priority_specific_dispatching";
   Num_Specific_Dispatching : int := 0
     with Export, Convention => C, External_Name => "__gl_num_specific_dispatching";
   Main_CPU : int := Unset
     with Export, Convention => C, External_Name => "__gl_main_cpu";
   Interrupt_States : System.Address := No_Address
     with Export, Convention => C, External_Name => "__gl_interrupt_states";
   Num_Interrupt_States : int := 0
     with Export, Convention => C, External_Name => "__gl_num_interrupt_states";
   Unreserve_All_Interrupts : int := 0
     with Export, Convention => C, External_Name => "__gl_unreserve_all_interrupts";
   --  CuBit (EXC-001): every occurrence records its traceback, as
   --  gnatbind -E would ask (the binder sets this only when given -E), so
   --  an unhandled exception's report names where it came from.
   Exception_Tracebacks : int := 1
     with Export, Convention => C, External_Name => "__gl_exception_tracebacks";
   Exception_Tracebacks_Symbolic : int := 0
     with Export, Convention => C, External_Name => "__gl_exception_tracebacks_symbolic";
   Detect_Blocking : int := 0
     with Export, Convention => C, External_Name => "__gl_detect_blocking";
   Default_Stack_Size : int := Unset
     with Export, Convention => C, External_Name => "__gl_default_stack_size";
   Leap_Seconds_Support : int := 0
     with Export, Convention => C, External_Name => "__gl_leap_seconds_support";
   Bind_Env_Addr : System.Address := No_Address
     with Export, Convention => C, External_Name => "__gl_bind_env_addr";
   Interrupts_Default_To_System : int := 0
     with Export, Convention => C, External_Name => "__gl_interrupts_default_to_system";
   XDR_Stream : int := 0
     with Export, Convention => C, External_Name => "__gl_xdr_stream";

   procedure Initialize (SEH : System.Address)
     with Export, Convention => C, External_Name => "__gnat_initialize";
   procedure Finalize
     with Export, Convention => C, External_Name => "__gnat_finalize";
   procedure Runtime_Initialize (Install_Handler : int)
     with Export, Convention => C, External_Name => "__gnat_runtime_initialize";
   procedure Runtime_Finalize
     with Export, Convention => C, External_Name => "__gnat_runtime_finalize";

end CuBit.Binder_Interface;

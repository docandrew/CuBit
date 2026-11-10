------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    S Y S T E M . S O F T _ L I N K S                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2025, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains a set of subprogram access variables that access
--  some low-level primitives that are different depending whether tasking is
--  involved or not (e.g. the Get/Set_Jmpbuf_Address that needs to provide a
--  different value for each task). To avoid dragging in the tasking runtimes
--  all the time, we use a system of soft links where the links are
--  initialized to non-tasking versions, and then if the tasking support is
--  initialized, they are set to the real tasking versions.

--  CuBit (EXC-001): the user runtime has no Ada tasking, but CuBit processes
--  have threads (THREAD-001), so the "non-tasking" versions here are the
--  thread-safe ones:
--    - the current exception occurrence (Get_Current_Excep) is a
--      thread-local variable in the static TLS block, as the secondary
--      stack pointer is (CuBit.Thread_Areas): each thread raises and
--      handles its own exceptions;
--    - Lock_Task/Unlock_Task are a recursive futex lock (upstream: null);
--  and the links upstream keeps for the tasking runtime only are left out:
--  the secondary stack (CuBit.Thread_Areas finds it through the thread
--  pointer), SJLJ jump buffers (this runtime is ZCX), stack checking
--  (Stack_Check_Default is False), task masters, timed delays, task-specific
--  data records and the RTS lock links (System.OS_Locks is a futex lock).
--  Everything kept has upstream's name, type and default meaning.

with Ada.Exceptions;

package System.Soft_Links is
   pragma Preelaborate;

   subtype EOA is Ada.Exceptions.Exception_Occurrence_Access;
   subtype EO is Ada.Exceptions.Exception_Occurrence;

   --  First we have the access subprogram types used to establish the links.
   --  The approach is to establish variables containing access subprogram
   --  values, which by default point to dummy no tasking versions of routines.

   type No_Param_Proc     is access procedure;
   pragma Favor_Top_Level (No_Param_Proc);
   pragma Suppress_Initialization (No_Param_Proc);
   --  Some uninitialized objects of that type are initialized by the Binder
   --  so it is important that such objects are not reset to null during
   --  elaboration.

   type EO_Param_Proc     is access procedure (Excep : EO);
   pragma Favor_Top_Level (EO_Param_Proc);

   type Get_Integer_Call  is access function return Integer;
   pragma Favor_Top_Level (Get_Integer_Call);

   type Get_EOA_Call      is access function return EOA;
   pragma Favor_Top_Level (Get_EOA_Call);

   type Task_Name_Call is access
     function return String;
   pragma Favor_Top_Level (Task_Name_Call);

   --  Suppress checks on all these types, since we know the corresponding
   --  values can never be null (the soft links are always initialized).

   pragma Suppress (Access_Check, No_Param_Proc);
   pragma Suppress (Access_Check, EO_Param_Proc);
   pragma Suppress (Access_Check, Get_Integer_Call);
   pragma Suppress (Access_Check, Get_EOA_Call);
   pragma Suppress (Access_Check, Task_Name_Call);

   --  The following one is not related to tasking/no-tasking but to the
   --  traceback decorators for exceptions.

   type Traceback_Decorator_Wrapper_Call is access
     function (Traceback : System.Address;
               Len       : Natural)
               return      String;
   pragma Favor_Top_Level (Traceback_Decorator_Wrapper_Call);

   --  Declarations for the no tasking versions of the required routines

   procedure Abort_Defer_NT;
   --  Defer task abort (non-tasking case, does nothing)

   procedure Abort_Undefer_NT;
   --  Undefer task abort (non-tasking case, does nothing)

   procedure Abort_Handler_NT;
   --  Handle task abort (non-tasking case, does nothing). Currently, only VMS
   --  uses this.

   function Check_Abort_Status_NT return Integer;
   --  Returns Boolean'Pos (True) iff abort signal should raise
   --  Standard'Abort_Signal.

   procedure Task_Lock_NT;
   --  Lock out other threads. CuBit: a recursive futex lock (upstream, with
   --  no tasking: nothing).

   procedure Task_Unlock_NT;
   --  Release lock set by Task_Lock (CuBit: by the same thread)

   procedure Task_Termination_NT (Excep : EO);
   --  Handle task termination routines for the environment task (non-tasking
   --  case, does nothing).

   procedure Adafinal_NT;
   --  Shuts down the runtime system (non-tasking case)

   Abort_Defer : No_Param_Proc := Abort_Defer_NT'Access;
   pragma Suppress (Access_Check, Abort_Defer);
   --  Defer task abort (task/non-task case as appropriate)

   Abort_Undefer : No_Param_Proc := Abort_Undefer_NT'Access;
   pragma Suppress (Access_Check, Abort_Undefer);
   --  Undefer task abort (task/non-task case as appropriate)

   Abort_Handler : No_Param_Proc := Abort_Handler_NT'Access;
   --  Handle task abort (task/non-task case as appropriate)

   Check_Abort_Status : Get_Integer_Call := Check_Abort_Status_NT'Access;
   --  Called when Abort_Signal is delivered to the process.  Checks to
   --  see if signal should result in raising Standard'Abort_Signal.

   Lock_Task : No_Param_Proc := Task_Lock_NT'Access;
   --  Locks out other tasks. Preceding a section of code by Task_Lock and
   --  following it by Task_Unlock creates a critical region. This is used
   --  for ensuring that a region of non-tasking code (such as code used to
   --  allocate memory) is tasking safe. Note that it is valid for calls to
   --  Task_Lock/Task_Unlock to be nested, and this must work properly, i.e.
   --  only the corresponding outer level Task_Unlock will actually unlock.
   --  This means that the gigantic lock is not released until all the
   --  outermost Task_Lock calls have been released.

   Unlock_Task : No_Param_Proc := Task_Unlock_NT'Access;
   --  Releases lock previously set by call to Lock_Task. In the nested case,
   --  all nested locks must be released before other tasks competing for the
   --  tasking lock are released.

   Task_Termination_Handler : EO_Param_Proc := Task_Termination_NT'Access;
   --  Handle task termination routines (task/non-task case as appropriate)

   Finalize_Library_Objects : No_Param_Proc;
   pragma Export (C, Finalize_Library_Objects,
                  "__gnat_finalize_library_objects");
   --  Will be initialized by the binder

   Adafinal : No_Param_Proc := Adafinal_NT'Access;
   --  Performs the finalization of the Ada Runtime

   function Get_Current_Excep_NT return EOA;
   --  CuBit: the calling thread's occurrence (thread-local)

   Get_Current_Excep : Get_EOA_Call := Get_Current_Excep_NT'Access;

   function Task_Name_NT return String;

   Task_Name : Task_Name_Call := Task_Name_NT'Access;

   -------------------------------------
   -- Exception Tracebacks Soft-Links --
   -------------------------------------

   Library_Exception : EO;
   --  Library-level finalization routines use this common reference to store
   --  the first library-level exception which occurs during finalization.

   Library_Exception_Set : Boolean := False;
   --  Used in conjunction with Library_Exception, set when an exception has
   --  been stored.

   Traceback_Decorator_Wrapper : Traceback_Decorator_Wrapper_Call;
   --  Wrapper to the possible user specified traceback decorator to be
   --  called during automatic output of exception data.
   --
   --  The null value of this wrapper corresponds to the null value of the
   --  current actual decorator. This is ensured first by the null initial
   --  value of the corresponding variables, and then by Set_Trace_Decorator
   --  in g-exctra.adb.

   pragma Atomic (Traceback_Decorator_Wrapper);
   --  Since concurrent read/write operations may occur on this variable.
   --  See the body of Tailored_Exception_Traceback in
   --  Ada.Exceptions.Exception_Data for a more detailed description of the
   --  potential problems.

   procedure Save_Library_Occurrence (E : EOA);
   --  When invoked, this routine saves an exception occurrence into a hidden
   --  reference. Subsequent calls will have no effect.

   function Get_GNAT_Exception return Ada.Exceptions.Exception_Id;
   pragma Inline (Get_GNAT_Exception);
   --  This function obtains the Exception_Id from the Exception_Occurrence
   --  referenced by the Current_Excep field of the task specific data, i.e.
   --  the call is equivalent to
   --  Exception_Identity (Get_Current_Exception.all)

end System.Soft_Links;

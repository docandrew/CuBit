------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    S Y S T E M . S O F T _ L I N K S                     --
--                                                                          --
--                                 B o d y                                  --
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

--  CuBit (EXC-001): see the spec. The current occurrence is thread-local,
--  and the task lock is a recursive futex lock owned by a thread.

with System.Address_To_Access_Conversions;
with System.Storage_Elements;
with CuBit.Futex_Locks;
with CuBit.Thread_Areas;

package body System.Soft_Links is

   --  The calling thread's current exception occurrence (upstream: the
   --  environment task's NT_TSD.Current_Excep). Thread-local, so it lives
   --  below the thread pointer every CuBit thread that runs Ada code has.
   --  A thread-local variable may only be initialized statically, and
   --  Exception_Occurrence is private, so the occurrence is its zeroed
   --  storage: all zeros is exactly its default value (null Id and
   --  machine occurrence, no message, not raised, no tracebacks).

   Occurrence_Bytes : constant := 1024;
   pragma Compile_Time_Error
     (EO'Size > Occurrence_Bytes * System.Storage_Unit,
      "Exception_Occurrence outgrew its thread-local storage");

   type Occurrence_Storage is record
      Bytes : System.Storage_Elements.Storage_Array (1 .. Occurrence_Bytes);
   end record;
   for Occurrence_Storage'Alignment use Standard'Maximum_Alignment;

   Current_Excep : aliased Occurrence_Storage :=
     (Bytes => (others => 0)) with Thread_Local_Storage;

   package Occurrence_Addresses is
     new System.Address_To_Access_Conversions (EO);

   Thread_Start : constant System.Address := CuBit.Thread_Areas.Start_Main'Address;
   pragma Export (C, Thread_Start, "__cubit_soft_links_thread_start");
   --  A strong reference to the start code's thread-area setup (crt0 only
   --  names it weakly): a program that can raise always gets a thread
   --  pointer for Current_Excep. Exported so it is kept.

   ---------------
   -- Task lock --
   ---------------

   Lock_State : CuBit.Futex_Locks.Mutex;
   --  The lock itself; Owner and Depth are written only by its holder

   Owner_Marker : aliased Boolean := False with Thread_Local_Storage;
   --  Its address names the calling thread (each has its own TLS block)

   Owner : System.Address := System.Null_Address;
   pragma Atomic (Owner);
   Depth : Natural := 0;

   --------------------
   -- Abort_Defer_NT --
   --------------------

   procedure Abort_Defer_NT is
   begin
      null;
   end Abort_Defer_NT;

   ----------------------
   -- Abort_Handler_NT --
   ----------------------

   procedure Abort_Handler_NT is
   begin
      null;
   end Abort_Handler_NT;

   ----------------------
   -- Abort_Undefer_NT --
   ----------------------

   procedure Abort_Undefer_NT is
   begin
      null;
   end Abort_Undefer_NT;

   -----------------
   -- Adafinal_NT --
   -----------------

   procedure Adafinal_NT is
   begin
      --  Handle normal task termination by the environment task, but only
      --  for the normal task termination. In the case of Abnormal and
      --  Unhandled_Exception they must have been handled before, and the
      --  task termination soft link must have been changed so the task
      --  termination routine is not executed twice.

      Task_Termination_Handler.all (Ada.Exceptions.Null_Occurrence);

      --  Finalize all library-level controlled objects if needed

      if Finalize_Library_Objects /= null then
         Finalize_Library_Objects.all;
      end if;
   end Adafinal_NT;

   ---------------------------
   -- Check_Abort_Status_NT --
   ---------------------------

   function Check_Abort_Status_NT return Integer is
   begin
      return Boolean'Pos (False);
   end Check_Abort_Status_NT;

   --------------------------
   -- Get_Current_Excep_NT --
   --------------------------

   function Get_Current_Excep_NT return EOA is
   begin
      return EOA (Occurrence_Addresses.To_Pointer (Current_Excep'Address));
   end Get_Current_Excep_NT;

   ------------------------
   -- Get_GNAT_Exception --
   ------------------------

   function Get_GNAT_Exception return Ada.Exceptions.Exception_Id is
   begin
      return Ada.Exceptions.Exception_Identity (Get_Current_Excep.all.all);
   end Get_GNAT_Exception;

   -----------------------------
   -- Save_Library_Occurrence --
   -----------------------------

   procedure Save_Library_Occurrence (E : EOA) is
      use Ada.Exceptions;
   begin
      if not Library_Exception_Set then
         Library_Exception_Set := True;

         if E /= null then
            Ada.Exceptions.Save_Occurrence (Library_Exception, E.all);
         end if;
      end if;
   end Save_Library_Occurrence;

   ------------------
   -- Task_Lock_NT --
   ------------------

   procedure Task_Lock_NT is
      Self : constant System.Address := Owner_Marker'Address;
   begin
      if Owner = Self then
         Depth := Depth + 1;
      else
         CuBit.Futex_Locks.Lock (Lock_State);
         Owner := Self;
         Depth := 1;
      end if;
   end Task_Lock_NT;

   ------------------
   -- Task_Name_NT --
   ------------------

   function Task_Name_NT return String is
   begin
      return "main_task";
   end Task_Name_NT;

   -------------------------
   -- Task_Termination_NT --
   -------------------------

   procedure Task_Termination_NT (Excep : EO) is
      pragma Unreferenced (Excep);
   begin
      null;
   end Task_Termination_NT;

   --------------------
   -- Task_Unlock_NT --
   --------------------

   procedure Task_Unlock_NT is
   begin
      if Owner = Owner_Marker'Address and then Depth > 0 then
         Depth := Depth - 1;

         if Depth = 0 then
            Owner := System.Null_Address;
            CuBit.Futex_Locks.Unlock (Lock_State);
         end if;
      end if;
   end Task_Unlock_NT;

end System.Soft_Links;

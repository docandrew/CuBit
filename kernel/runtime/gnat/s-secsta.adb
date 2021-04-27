------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--               S Y S T E M . S E C O N D A R Y _ S T A C K                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2019, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
--                                                                          --
--                                                                          --
--                                                                          --
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

--  This is the HI-E version of this package

--  with Ada.Unchecked_Conversion;

package body System.Secondary_Stack is

   use type System.Parameters.Size_Type;

   function Get_Sec_Stack return SS_Stack_Ptr;
   pragma Import (C, Get_Sec_Stack, "__gnat_get_secondary_stack");
   --  Return the pointer to the secondary stack of the current task.
   --
   --  The package imports this function to permit flexibility in the storage
   --  of secondary stacks pointers to support a range of different ZFP and
   --  other restricted run-time scenarios without needing to recompile the
   --  run-time. A ZFP run-time will typically include a default implementation
   --  suitable for single thread applications (s-sssita.adb). However, users
   --  can replace this implementation by providing their own as part of their
   --  program (for example, if multiple threads need to be supported in a ZFP
   --  application).
   --
   --  Many Ravenscar run-times also use this mechanism to provide switch
   --  between efficient single task and multitask implementations without
   --  depending on System.Soft_Links.
   --
   --  In all cases, the binder will generate a default-sized secondary stack
   --  for the environment task if the secondary stack is used by the program
   --  being binded.
   --
   --  To support multithreaded ZFP-based applications, the binder supports
   --  the creation of additional secondary stacks using the -Qnnn binder
   --  switch. For the user to make use of these generated stacks, all threads
   --  need to call SS_Init with a null-object parameter to be assigned a
   --  stack. It is then the responsibility of the user to store the returned
   --  pointer in a way that can be can retrieved via the user implementation
   --  of the __gnat_get_secondary_stack function. In this scenario it is
   --  recommended to use thread local variables. For example, if the
   --  Thread_Local_Storage aspect is supported on the target:
   --
   --  pragma Warnings (Off);
   --  with System.Secondary_Stack; use System.Secondary_Stack;
   --  pragma Warnings (On);
   --
   --  package Secondary_Stack is
   --     Thread_Sec_Stack : System.Secondary_Stack.SS_Stack_Ptr := null
   --       with Thread_Local_Storage;
   --
   --     function Get_Sec_Stack return SS_Stack_Ptr
   --       with Export, Convention => C,
   --            External_Name => "__gnat_get_secondary_stack";
   --
   --    function Get_Sec_Stack return SS_Stack_Ptr is
   --    begin
   --       if Thread_Sec_Stack = null then
   --          SS_Init (Thread_Sec_Stack);
   --       end if;
   --
   --       return Thread_Sec_Stack;
   --    end Get_Sec_Stack;
   --  end Secondary_Stack;

   -----------------
   -- SS_Allocate --
   -----------------

   procedure SS_Allocate
     (Addr         : out Address;
      Storage_Size : SSE.Storage_Count)
   is
      use type System.Storage_Elements.Storage_Count;

      Max_Align   : constant SS_Ptr := SS_Ptr (Standard'Maximum_Alignment);
      Mem_Request : SS_Ptr;

      --  Get_Sec_Stack in CuBit is in the PerCPUData package.
      Stack : constant SS_Stack_Ptr := Get_Sec_Stack;
   begin
      --  Round up Storage_Size to the nearest multiple of the max alignment
      --  value for the target. This ensures efficient stack access. First
      --  perform a check to ensure that the rounding operation does not
      --  overflow SS_Ptr.

      if SSE.Storage_Count (SS_Ptr'Last) - Standard'Maximum_Alignment <
        Storage_Size
      then
         raise Storage_Error;
      end if;

      Mem_Request := ((SS_Ptr (Storage_Size) + Max_Align - 1) / Max_Align) *
                       Max_Align;

      --  Check if max stack usage is increasing

      if Stack.Max - Stack.Top - Mem_Request < 0  then
         --  If so, check if the stack is exceeded, noting Stack.Top points to
         --  the first free byte (so the value of Stack.Top on a fully
         --  allocated stack will be Stack.Size + 1). The comparison is formed
         --  to prevent integer overflows.

         if CUBIT_SS_SIZE - Stack.Top - Mem_Request < -1 then
            raise Storage_Error;
         end if;

         --  Record new max usage

         Stack.Max := Stack.Top + Mem_Request;
      end if;

      --  Set resulting address and update top of stack pointer

      Addr := Stack.Internal_Chunk (Stack.Top)'Address;
      Stack.Top := Stack.Top + Mem_Request;
   end SS_Allocate;

   ----------------
   -- SS_Get_Max --
   ----------------

   function SS_Get_Max return Long_Long_Integer is
   begin
      --  Stack.Max points to the first untouched byte in the stack, thus the
      --  maximum number of bytes that have been allocated on the stack is one
      --  less the value of Stack.Max.

      return Long_Long_Integer (Get_Sec_Stack.Max - 1);
   end SS_Get_Max;

   -------------
   -- SS_Init --
   -------------

   procedure SS_Init
     (Stack : SS_Stack_Ptr)
   is
      --  use Parameters;
   begin
      Stack.Top := 1;
      Stack.Max := 1;
   end SS_Init;

   -------------
   -- SS_Mark --
   -------------

   function SS_Mark return Mark_Id is
   begin
      return Mark_Id (Get_Sec_Stack.Top);
   end SS_Mark;

   ----------------
   -- SS_Release --
   ----------------

   procedure SS_Release (M : Mark_Id) is
   begin
      Get_Sec_Stack.Top := SS_Ptr (M);
   end SS_Release;

end System.Secondary_Stack;

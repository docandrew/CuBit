------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     S Y S T E M . T R A C E B A C K                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2025, Free Software Foundation, Inc.         --
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

--  This is the default version of this package

with System.Storage_Elements;

--  Note: this unit must be compiled using -fno-optimize-sibling-calls.
--  See comment below in body of Call_Chain for details on the reason.

package body System.Traceback is

   procedure Call_Chain
     (Traceback   : System.Address;
      Max_Len     : Natural;
      Len         : out Natural;
      Exclude_Min : System.Address := System.Null_Address;
      Exclude_Max : System.Address := System.Null_Address;
      Skip_Frames : Natural := 1);
   --  Same as the exported version, but takes Traceback as an Address

   ------------------
   -- C_Call_Chain --
   ------------------

   function C_Call_Chain
     (Traceback : System.Address;
      Max_Len   : Natural) return Natural
   is
      Val : Natural;
   begin
      Call_Chain (Traceback, Max_Len, Val);
      return Val;
   end C_Call_Chain;

   ----------------
   -- Call_Chain --
   ----------------

   --  CuBit (EXC-001): Backtrace is upstream's __gnat_backtrace for targets
   --  using the GCC unwinder (tracebak.c, USE_GCC_UNWINDER), written in Ada
   --  rather than imported from C: _Unwind_Backtrace (libgcc's DWARF
   --  unwinder, the one that propagates exceptions) walks the call chain
   --  from the .eh_frame tables, so frame pointers are not needed.

   function Backtrace
     (Traceback   : System.Address;
      Len         : Integer;
      Exclude_Min : System.Address;
      Exclude_Max : System.Address;
      Skip_Frames : Integer)
      return        Integer;
   pragma No_Inline (Backtrace);

   PC_Adjust : constant := -1;
   --  As tracebak.c on x86: the return address minus one, so the location
   --  is inside the call instruction (the line of the call, not the next)

   type Unwind_Reason_Code is new Integer;
   pragma Convention (C, Unwind_Reason_Code);
   URC_No_Reason   : constant Unwind_Reason_Code := 0;
   URC_Normal_Stop : constant Unwind_Reason_Code := 4;
   --  unwind.h's _Unwind_Reason_Code (an enum, int-sized): the two values
   --  a trace callback returns

   type Trace_State is record
      Traceback        : System.Address;
      Max_Len          : Integer;
      Exclude_Min      : System.Address;
      Exclude_Max      : System.Address;
      N_Frames_To_Skip : Integer;
      N_Frames_Skipped : Integer;
      N_Entries_Filled : Integer;
   end record;
   pragma Convention (C, Trace_State);
   --  tracebak.c's uw_data_t: the state of one walk

   type Trace_State_Access is access all Trace_State;
   pragma Convention (C, Trace_State_Access);
   pragma No_Strict_Aliasing (Trace_State_Access);

   function Trace_Callback
     (Context : System.Address;
      State   : Trace_State_Access) return Unwind_Reason_Code;
   pragma Convention (C, Trace_Callback);
   --  tracebak.c's trace_callback: called by _Unwind_Backtrace per frame

   type Trace_Function is access function
     (Context : System.Address;
      State   : Trace_State_Access) return Unwind_Reason_Code;
   pragma Convention (C, Trace_Function);

   function Unwind_Backtrace
     (Trace : Trace_Function;
      State : Trace_State_Access) return Unwind_Reason_Code;
   pragma Import (C, Unwind_Backtrace, "_Unwind_Backtrace");

   function Unwind_Get_IP (Context : System.Address) return System.Address;
   pragma Import (C, Unwind_Get_IP, "_Unwind_GetIP");

   --------------------
   -- Trace_Callback --
   --------------------

   function Trace_Callback
     (Context : System.Address;
      State   : Trace_State_Access) return Unwind_Reason_Code
   is
      PC : constant System.Address := Unwind_Get_IP (Context);
   begin
      if State.N_Frames_Skipped <= State.N_Frames_To_Skip then
         State.N_Frames_Skipped := State.N_Frames_Skipped + 1;
         return URC_No_Reason;
      end if;

      if State.N_Entries_Filled >= State.Max_Len then
         return URC_Normal_Stop;
      end if;

      if PC < State.Exclude_Min or else PC > State.Exclude_Max then
         declare
            type Entries is array (0 .. State.Max_Len - 1) of System.Address;
            Filled : Entries with Import, Address => State.Traceback;
         begin
            Filled (State.N_Entries_Filled) :=
              System.Storage_Elements."+" (PC, PC_Adjust);
            State.N_Entries_Filled := State.N_Entries_Filled + 1;
         end;
      end if;

      return URC_No_Reason;
   end Trace_Callback;

   ---------------
   -- Backtrace --
   ---------------

   function Backtrace
     (Traceback   : System.Address;
      Len         : Integer;
      Exclude_Min : System.Address;
      Exclude_Max : System.Address;
      Skip_Frames : Integer)
      return        Integer
   is
      State : aliased Trace_State :=
        (Traceback        => Traceback,
         Max_Len          => Len,
         Exclude_Min      => Exclude_Min,
         Exclude_Max      => Exclude_Max,
         N_Frames_To_Skip => Skip_Frames,
         N_Frames_Skipped => 0,
         N_Entries_Filled => 0);
      Ignore : Unwind_Reason_Code;
   begin
      Ignore := Unwind_Backtrace (Trace_Callback'Access, State'Unchecked_Access);
      return State.N_Entries_Filled;
   end Backtrace;

   procedure Call_Chain
     (Traceback   : System.Address;
      Max_Len     : Natural;
      Len         : out Natural;
      Exclude_Min : System.Address := System.Null_Address;
      Exclude_Max : System.Address := System.Null_Address;
      Skip_Frames : Natural := 1)
   is
   begin
      --  Note: Backtrace relies on the following call actually creating a
      --  stack frame. To ensure that this is the case, it is essential to
      --  compile this unit without sibling call optimization.

      --  We want the underlying engine to skip its own frame plus the
      --  ones we have been requested to skip ourselves.

      Len := Backtrace (Traceback   => Traceback,
                        Len         => Max_Len,
                        Exclude_Min => Exclude_Min,
                        Exclude_Max => Exclude_Max,
                        Skip_Frames => Skip_Frames + 1);
   end Call_Chain;

   procedure Call_Chain
     (Traceback   : in out System.Traceback_Entries.Tracebacks_Array;
      Max_Len     : Natural;
      Len         : out Natural;
      Exclude_Min : System.Address := System.Null_Address;
      Exclude_Max : System.Address := System.Null_Address;
      Skip_Frames : Natural := 1)
   is
   begin
      Call_Chain
        (Traceback'Address, Max_Len, Len,
         Exclude_Min, Exclude_Max,

         --  Skip one extra frame to skip the other Call_Chain entry as well

         Skip_Frames => Skip_Frames + 1);
   end Call_Chain;

end System.Traceback;

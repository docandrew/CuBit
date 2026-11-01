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

--  This is the HI-E version of this package, with CuBit's growable stacks
--  (CuBit.Secondary_Stack_Layout).

with Ada.Unchecked_Conversion;
with Interfaces;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;
with CuBit.Secondary_Stack_Layout;

package body System.Secondary_Stack is

   package Layout renames CuBit.Secondary_Stack_Layout;
   package ABI renames CuBit.Kernel_ABI;

   use type Interfaces.Unsigned_64;
   use type System.Parameters.Size_Type;
   use type System.Storage_Elements.Storage_Offset;

   --  What COMMIT_OWNED_MEMORY_PREFIX and RELEASE_OWNED_RESERVATION return
   --  when they did it (anything else: refused); what RESERVE_OWNED_MEMORY
   --  returns when it refused.
   Owned_Memory_Done    : constant Interfaces.Unsigned_64 := 0;
   Reservation_Refused  : constant Interfaces.Unsigned_64 := 0;

   pragma Compile_Time_Error
     (Layout.Maximum_Commit_Bytes /= ABI.Maximum_Owned_Commit_Bytes,
      "secondary stack commits differ from the kernel's bound");
   pragma Compile_Time_Error
     (Layout.Page_Bytes /= ABI.Page_Bytes, "secondary stack pages differ from the kernel's");
   pragma Compile_Time_Error
     (Layout.Header_Bytes < Standard'Maximum_Alignment,
      "secondary stack header is not aligned for its data");

   function Word (Value : SS_Ptr) return Interfaces.Unsigned_64 is
     (Interfaces.Unsigned_64 (Value));
   function Word (Where : System.Address) return Interfaces.Unsigned_64 is
     (Interfaces.Unsigned_64 (System.Storage_Elements.To_Integer (Where)));

   --  Why an allocation was refused.
   type Refusal is (Reservation_Exhausted, Commit_Refused);

   procedure Report_Overflow (Why : Refusal; Request : SS_Ptr; Stack : SS_Stack_Ptr);
   --  Allocation-free overflow report: why, the request, the stack's top,
   --  high water, committed data and reach, written straight to the debug
   --  console before the raise, so the last-chance handler's report has the
   --  numbers with it. Nothing here uses the secondary stack.

   procedure Report_Overflow (Why : Refusal; Request : SS_Ptr; Stack : SS_Stack_Ptr) is
      Number_Digits : constant := 20;
      Report_Length : constant := 200;
      Text : String (1 .. Report_Length);
      Used : Natural := 0;
      Ignore : Interfaces.Unsigned_64;

      procedure Put (S : String);
      procedure Put_Number (Value : SS_Ptr);

      procedure Put (S : String) is
      begin
         for C of S loop
            exit when Used = Text'Last;
            Used := Used + 1;
            Text (Used) := C;
         end loop;
      end Put;

      procedure Put_Number (Value : SS_Ptr) is
         Buffer : String (1 .. Number_Digits);
         Count : Natural := 0;
         Rest : Long_Long_Integer := Long_Long_Integer (Value);
      begin
         if Rest < 0 then
            Put ("-");
            Rest := -Rest;
         end if;
         loop
            Count := Count + 1;
            Buffer (Number_Digits - Count + 1) :=
              Character'Val (Character'Pos ('0') + Integer (Rest mod 10));
            Rest := Rest / 10;
            exit when Rest = 0 or else Count = Number_Digits;
         end loop;
         Put (Buffer (Number_Digits - Count + 1 .. Number_Digits));
      end Put_Number;
   begin
      Put ("secondary stack overflow: request=");
      Put_Number (Request);
      Put (" top=");
      Put_Number (Stack.Top);
      Put (" max=");
      Put_Number (Stack.Max);
      Put (" committed=");
      Put_Number (Stack.Committed);
      Put (" reservation=");
      Put_Number (Stack.Size);
      case Why is
         when Reservation_Exhausted => Put (" (reservation exhausted)");
         when Commit_Refused        => Put (" (commit refused)");
      end case;
      Put ((1 => ASCII.LF));
      Ignore := CuBit.Kernel_Calls.Call
        (ABI.Write, ABI.Console, Word (Text'Address), Interfaces.Unsigned_64 (Used));
   end Report_Overflow;

   function Get_Sec_Stack return SS_Stack_Ptr;
   pragma Import (C, Get_Sec_Stack, "__gnat_get_secondary_stack");
   --  Return the pointer to the secondary stack of the current task.
   --
   --  CuBit: CuBit.Thread_Areas defines it, from a thread-local pointer
   --  (each thread's own stack, made on its first use). It lives in another
   --  unit so a program may wrap it at link time (--wrap, Servo).

   function Data_Offset (Stack : SS_Stack_Ptr) return SSE.Storage_Offset is
     (Stack.Internal_Chunk'Address - Stack.Region);
   --  Where a growable stack's data starts in its region.

   function Grow (Stack : SS_Stack_Ptr; Last : SS_Ptr) return Boolean;
   --  Commit more of Stack's region until Internal_Chunk (1 .. Last) is
   --  backed (Last <= Stack.Size). False when the kernel refused a commit:
   --  what was committed before the refusal stays.

   function Grow (Stack : SS_Stack_Ptr; Last : SS_Ptr) return Boolean is
      Offset : constant Interfaces.Unsigned_64 := Interfaces.Unsigned_64 (Data_Offset (Stack));
      Needed : constant Layout.Region_Bytes := Offset + Word (Last);
      Committed : Layout.Region_Bytes := Interfaces.Unsigned_64 (Stack.Region_Committed);
      Step : Layout.Commit_Bytes;
   begin
      while Committed < Needed loop
         Step := Layout.Next_Commit (Committed, Needed);
         if CuBit.Kernel_Calls.Call
              (ABI.Commit_Owned_Memory_Prefix, Word (Stack.Region), Committed, Step)
            /= Owned_Memory_Done
         then
            return False;
         end if;
         Committed := Committed + Step;
         Stack.Region_Committed := SSE.Storage_Count (Committed);
         Stack.Committed := SS_Ptr (Committed - Offset);
      end loop;
      return True;
   end Grow;

   procedure Extend (Stack : SS_Stack_Ptr; Request : SS_Ptr) with No_Inline;
   --  The slow path of SS_Allocate: Request (at least 1) does not fit in
   --  what is committed. Commit enough of a growable stack, or report and
   --  raise Storage_Error when the reach is exhausted (checked before any
   --  memory is touched) or the kernel refuses (the frame quota).

   procedure Extend (Stack : SS_Stack_Ptr; Request : SS_Ptr) is
   begin
      if Request - 1 > Stack.Size - Stack.Top then
         Report_Overflow (Reservation_Exhausted, Request, Stack);
         raise Storage_Error;
      end if;
      if Stack.Region = System.Null_Address
        or else not Grow (Stack, Stack.Top + Request - 1)
      then
         Report_Overflow (Commit_Refused, Request, Stack);
         raise Storage_Error;
      end if;
   end Extend;

   -----------------
   -- SS_Allocate --
   -----------------

   procedure SS_Allocate
     (Addr         : out Address;
      Storage_Size : SSE.Storage_Count)
   is
      Max_Align   : constant SS_Ptr := SS_Ptr (Standard'Maximum_Alignment);
      Mem_Request : SS_Ptr;

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

      --  CuBit: the fast path compares with what is committed (Top points
      --  to the first free byte, so Top = Committed + 1 is a full committed
      --  part). The comparison is formed to prevent integer overflows.

      if Mem_Request - 1 > Stack.Committed - Stack.Top then
         Extend (Stack, Mem_Request);
      end if;

      --  Check if max stack usage is increasing

      if Stack.Max - Stack.Top - Mem_Request < 0 then
         Stack.Max := Stack.Top + Mem_Request;
      end if;

      --  Set resulting address and update top of stack pointer

      Addr := Stack.Internal_Chunk (Stack.Top)'Address;
      Stack.Top := Stack.Top + Mem_Request;
   end SS_Allocate;

   ---------------
   -- SS_Create --
   ---------------

   function SS_Create (Prefix : SSE.Storage_Count) return SS_Stack_Ptr is
      Base : Interfaces.Unsigned_64;
      First : Layout.Commit_Bytes;
      Header : System.Address;
      Ignore : Interfaces.Unsigned_64;
   begin
      if Prefix > Layout.Maximum_Prefix_Bytes then
         return null;
      end if;
      Base := CuBit.Kernel_Calls.Call (ABI.Reserve_Owned_Memory, Layout.Reservation_Bytes);
      if Base = Reservation_Refused then
         return null;
      end if;
      First := Layout.First_Commit (Interfaces.Unsigned_64 (Prefix));
      if CuBit.Kernel_Calls.Call (ABI.Commit_Owned_Memory_Prefix, Base, 0, First)
         /= Owned_Memory_Done
      then
         Ignore := CuBit.Kernel_Calls.Call
           (ABI.Release_Owned_Reservation, Base, Layout.Reservation_Bytes);
         return null;
      end if;
      Header := System.Storage_Elements.To_Address
        (System.Storage_Elements.Integer_Address
           (Base + Layout.Header_Offset (Interfaces.Unsigned_64 (Prefix))));
      declare
         --  The discriminant first: the stack's reach is fixed by where
         --  its data starts, which does not depend on it.
         Reach : SP.Size_Type with Import, Address => Header;
         function To_Stack is new Ada.Unchecked_Conversion (System.Address, SS_Stack_Ptr);
         Stack : constant SS_Stack_Ptr := To_Stack (Header);
         Offset : SSE.Storage_Offset;
      begin
         Stack.Region := System.Storage_Elements.To_Address
           (System.Storage_Elements.Integer_Address (Base));
         Offset := Data_Offset (Stack);
         Reach := SP.Size_Type (Layout.Usable_Bytes) - SP.Size_Type (Offset);
         Stack.Top := 1;
         Stack.Max := 1;
         Stack.Region_Committed := SSE.Storage_Count (First);
         Stack.Committed := SS_Ptr (First) - SS_Ptr (Offset);
         return Stack;
      end;
   end SS_Create;

   -------------
   -- SS_Free --
   -------------

   procedure SS_Free (Stack : SS_Stack_Ptr) is
      Region : constant System.Address := Stack.Region;
      Ignore : Interfaces.Unsigned_64;
   begin
      --  A refused release keeps the region (and its RAM) until the process
      --  exits; there is nothing better to do with it here.
      if Region /= System.Null_Address then
         Ignore := CuBit.Kernel_Calls.Call
           (ABI.Release_Owned_Reservation, Word (Region), Layout.Reservation_Bytes);
      end if;
   end SS_Free;

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
   begin
      Stack.Top := 1;
      Stack.Max := 1;
      Stack.Committed := Stack.Size;
      Stack.Region := System.Null_Address;
      Stack.Region_Committed := 0;
   end SS_Init;

   ---------------
   -- SS_Region --
   ---------------

   function SS_Region (Stack : SS_Stack_Ptr) return System.Address is
     (Stack.Region);

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

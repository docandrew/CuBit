------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
with Ada.Unchecked_Conversion;
with System; use type System.Address;
with System.Machine_Code; use System.Machine_Code;
with System.Storage_Elements; use System.Storage_Elements;
with CuBit.Kernel_ABI;
with CuBit.Kernel_Calls;
with CuBit.Secondary_Stack_Layout;

package body CuBit.Thread_Areas is

   package SS renames System.Secondary_Stack;
   package ABI renames CuBit.Kernel_ABI;
   package Layout renames CuBit.Secondary_Stack_Layout;
   use type SS.SS_Stack_Ptr;

   --  This thread's secondary stack: the slot, in the static TLS block.
   Current : SS.SS_Stack_Ptr := null with Thread_Local_Storage;

   --  The static TLS segment, from the linker script (userspace/c/link.ld,
   --  userspace/libc/link.ld): its initialized image, the image's bytes,
   --  the whole segment's bytes and its alignment. The last three are
   --  absolute symbols: their addresses are the values.
   TLS_Image : constant Storage_Element
   with Import, Convention => C, External_Name => "__cubit_tls_start";
   TLS_Image_Bytes : constant Storage_Element
   with Import, Convention => C, External_Name => "__cubit_tls_image_bytes";
   TLS_Segment_Bytes : constant Storage_Element
   with Import, Convention => C, External_Name => "__cubit_tls_bytes";
   TLS_Segment_Alignment : constant Storage_Element
   with Import, Convention => C, External_Name => "__cubit_tls_align";

   --  A thread control block's words (Layout.Control_Block_Bytes).
   Self_Offset  : constant := 0;
   Stack_Offset : constant := 8;

   function Value_Of (Where : System.Address) return Unsigned_64 is
     (Unsigned_64 (To_Integer (Where)));

   function To_Stack is new Ada.Unchecked_Conversion (System.Address, SS.SS_Stack_Ptr);

   procedure Put_Word (Where, Value : Unsigned_64);
   procedure Put_Word (Where, Value : Unsigned_64) is
      Item : Unsigned_64 with Import, Address => To_Address (Integer_Address (Where));
   begin
      Item := Value;
   end Put_Word;

   function Word_At (Where : Unsigned_64) return Unsigned_64;
   function Word_At (Where : Unsigned_64) return Unsigned_64 is
      Item : constant Unsigned_64 with Import, Address => To_Address (Integer_Address (Where));
   begin
      return Item;
   end Word_At;

   --  A fixed message; nothing here uses the secondary stack.
   procedure Write_Console (Text : String);
   procedure Write_Console (Text : String) is
      Ignore : Unsigned_64;
   begin
      Ignore := CuBit.Kernel_Calls.Call
        (ABI.Write, ABI.Console, Value_Of (Text'Address), Unsigned_64 (Text'Length));
   end Write_Console;

   --  The calling thread's thread pointer: its control block's first word.
   function Thread_Pointer return Unsigned_64;
   function Thread_Pointer return Unsigned_64 is
      Value : Unsigned_64;
   begin
      Asm ("movq %%fs:0, %0", Outputs => Unsigned_64'Asm_Output ("=r", Value), Volatile => True);
      return Value;
   end Thread_Pointer;

   procedure Set_Thread_Pointer (Value : Unsigned_64);
   procedure Set_Thread_Pointer (Value : Unsigned_64) is
   begin
      Asm ("wrfsbase %0", Inputs => Unsigned_64'Asm_Input ("r", Value),
           Clobber => "memory", Volatile => True);
   end Set_Thread_Pointer;

   --  A new thread area: a growable secondary stack whose region's prefix
   --  holds a copy of the static TLS image (the rest of the block zero)
   --  and the thread control block. Its thread pointer and stack, or
   --  No_Area and null.
   function Make_Area (Stack : out SS.SS_Stack_Ptr) return Unsigned_64;
   function Make_Area (Stack : out SS.SS_Stack_Ptr) return Unsigned_64 is
      Bytes : constant Unsigned_64 := Value_Of (TLS_Segment_Bytes'Address);
      Alignment : constant Unsigned_64 := Value_Of (TLS_Segment_Alignment'Address);
      Image_Bytes : constant Unsigned_64 := Value_Of (TLS_Image_Bytes'Address);
   begin
      Stack := null;
      if Bytes > Layout.Maximum_TLS_Bytes
        or else Image_Bytes > Bytes
        or else not Layout.Valid_TLS_Alignment (Alignment)
      then
         return No_Area;
      end if;
      Stack := SS.SS_Create (Storage_Count (Layout.Area_Prefix (Bytes, Alignment)));
      if Stack = null then
         return No_Area;
      end if;
      declare
         Base : constant Unsigned_64 := Value_Of (SS.SS_Region (Stack));
         Pointer : constant Unsigned_64 := Base + Layout.Thread_Pointer_Offset (Bytes, Alignment);
         Block_Bytes : constant Unsigned_64 := Layout.TLS_Block_Bytes (Bytes, Alignment);
         Image : constant Storage_Array (1 .. Storage_Offset (Image_Bytes))
         with Import, Address => TLS_Image'Address;
         Block : Storage_Array (1 .. Storage_Offset (Block_Bytes + Layout.Control_Block_Bytes))
         with Import, Address => To_Address (Integer_Address (Pointer - Block_Bytes));
      begin
         Block := [others => 0];
         Block (1 .. Image'Last) := Image;
         Put_Word (Pointer + Self_Offset, Pointer);
         Put_Word (Pointer + Stack_Offset, Value_Of (Stack.all'Address));
         return Pointer;
      end;
   end Make_Area;

   function Create return Unsigned_64 is
      Stack : SS.SS_Stack_Ptr;
      Pointer : constant Unsigned_64 := Make_Area (Stack);
   begin
      if Pointer /= No_Area then
         --  The new thread's slot: as far from its thread pointer as ours.
         Put_Word (Pointer + (Value_Of (Current'Address) - Thread_Pointer),
                   Value_Of (Stack.all'Address));
      end if;
      return Pointer;
   end Create;

   procedure Discard (Thread_Pointer : Unsigned_64) is
   begin
      if Thread_Pointer /= No_Area then
         SS.SS_Free (To_Stack (To_Address (Integer_Address
           (Word_At (Thread_Pointer + Stack_Offset)))));
      end if;
   end Discard;

   procedure Exit_Thread is
      Ignore : Unsigned_64;
   begin
      Release_Current;
      loop
         Ignore := CuBit.Kernel_Calls.Call (ABI.Thread_Exit);
      end loop;
   end Exit_Thread;

   --  The slow path of Current_Stack: this thread's first use.
   function New_Stack return SS.SS_Stack_Ptr with No_Inline;
   function New_Stack return SS.SS_Stack_Ptr is
      Stack : constant SS.SS_Stack_Ptr := SS.SS_Create (0);
   begin
      if Stack = null then
         Write_Console ("secondary stack: reservation refused for this thread" & ASCII.LF);
         raise Storage_Error;
      end if;
      return Stack;
   end New_Stack;

   function Current_Stack return SS.SS_Stack_Ptr is
   begin
      if Current = null then
         Current := New_Stack;
      end if;
      return Current;
   end Current_Stack;

   procedure Release_Current is
      Stack : constant SS.SS_Stack_Ptr := Current;
   begin
      if Stack /= null then
         --  First: the region may hold this very slot (a Create area).
         Current := null;
         SS.SS_Free (Stack);
      end if;
   end Release_Current;

   procedure Start_Main is
      Stack : SS.SS_Stack_Ptr;
      Pointer : constant Unsigned_64 := Make_Area (Stack);
      Ignore : Unsigned_64;
   begin
      if Pointer = No_Area then
         Write_Console ("secondary stack: no thread area for the main thread; exiting" & ASCII.LF);
         loop
            Ignore := CuBit.Kernel_Calls.Call (ABI.Exit_Process, Start_Refused_Status);
         end loop;
      end if;
      Set_Thread_Pointer (Pointer);
      Current := Stack;
   end Start_Main;

end CuBit.Thread_Areas;

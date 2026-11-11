------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
------------------------------------------------------------------------------
pragma Ada_2022;
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

   pragma Compile_Time_Error
     (Layout.Page_Bytes /= ABI.Page_Bytes, "thread area pages differ from the kernel's");
   pragma Compile_Time_Error
     (Layout.Maximum_Area_Bytes > ABI.Maximum_Owned_Bytes,
      "a thread area may not fit one owned-memory region");
   pragma Compile_Time_Error
     (Layout.Object_Alignment < Standard'Maximum_Alignment,
      "a thread area's stack object is not aligned for its data");

   --  This thread's secondary stack: the slot, in the static TLS block.
   Current : SS.SS_Stack_Ptr := null with Thread_Local_Storage;

   --  The base of this thread's area, when the runtime made it (0: none;
   --  musl's and Rust's threads), given back after its stack's chunks.
   Area_Base : Unsigned_64 := 0 with Thread_Local_Storage;

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

   --  What ALLOCATE_OWNED_MEMORY returns when it refused.
   Allocation_Refused : constant Unsigned_64 := 0;

   function Value_Of (Where : System.Address) return Unsigned_64 is
     (Unsigned_64 (To_Integer (Where)));

   procedure Put_Word (Where, Value : Unsigned_64);
   procedure Put_Word (Where, Value : Unsigned_64) is
      Item : Unsigned_64 with Import, Address => To_Address (Integer_Address (Where));
   begin
      Item := Value;
   end Put_Word;

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

   --  This program's thread areas, all alike: from its static TLS segment
   --  and the stack object's size. Valid is False when the segment or the
   --  object is larger than an area admits.
   type Area_Plan is record
      Valid : Boolean := False;
      TLS_Bytes : Layout.TLS_Bytes := 0;
      Alignment : Layout.TLS_Alignment := 1;
      Image_Bytes : Layout.TLS_Bytes := 0;
      Prefix : Layout.Prefix_Bytes := 0;
      Bytes : Layout.Region_Bytes := 0;
   end record;

   function Plan return Area_Plan;
   function Plan return Area_Plan is
      Bytes : constant Unsigned_64 := Value_Of (TLS_Segment_Bytes'Address);
      Alignment : constant Unsigned_64 := Value_Of (TLS_Segment_Alignment'Address);
      Image_Bytes : constant Unsigned_64 := Value_Of (TLS_Image_Bytes'Address);
      Object : constant Unsigned_64 :=
        Unsigned_64 (SS.SS_Object_Bytes (Layout.Chunk_Bytes));
      Result : Area_Plan;
   begin
      if Bytes <= Layout.Maximum_TLS_Bytes
        and then Image_Bytes <= Bytes
        and then Layout.Valid_TLS_Alignment (Alignment)
        and then Object in Layout.Object_Bytes
      then
         Result.Valid := True;
         Result.TLS_Bytes := Bytes;
         Result.Alignment := Alignment;
         Result.Image_Bytes := Image_Bytes;
         Result.Prefix := Layout.Area_Prefix (Bytes, Alignment);
         Result.Bytes := Layout.Area_Bytes (Result.Prefix, Object);
      end if;
      return Result;
   end Plan;

   --  Where an area's thread pointer is from its base.
   function Pointer_Offset (P : Area_Plan) return Unsigned_64 is
     (Layout.Thread_Pointer_Offset (P.TLS_Bytes, P.Alignment));

   --  Give back the area at Base.
   procedure Release_Area (Base : Unsigned_64);
   procedure Release_Area (Base : Unsigned_64) is
      P : constant Area_Plan := Plan;
      Ignore : Unsigned_64;
   begin
      --  A refused release keeps the area (and its RAM) until the process
      --  exits; there is nothing better to do with it here.
      if P.Valid then
         Ignore := CuBit.Kernel_Calls.Call (ABI.Release_Owned_Memory, Base, P.Bytes);
      end if;
   end Release_Area;

   --  A new thread area: owned memory holding a copy of the static TLS
   --  image (the rest of the block zero), the thread control block, and
   --  the thread's secondary stack object with its static chunk. Its
   --  thread pointer, base and stack, or No_Area, 0 and null.
   function Make_Area (Stack : out SS.SS_Stack_Ptr; Base : out Unsigned_64) return Unsigned_64;
   function Make_Area (Stack : out SS.SS_Stack_Ptr; Base : out Unsigned_64) return Unsigned_64 is
      P : constant Area_Plan := Plan;
   begin
      Stack := null;
      Base := 0;
      if not P.Valid then
         return No_Area;
      end if;
      Base := CuBit.Kernel_Calls.Call (ABI.Allocate_Owned_Memory, P.Bytes);
      if Base = Allocation_Refused then
         Base := 0;
         return No_Area;
      end if;
      declare
         Pointer : constant Unsigned_64 := Base + Pointer_Offset (P);
         Block_Bytes : constant Unsigned_64 := Layout.TLS_Block_Bytes (P.TLS_Bytes, P.Alignment);
         Image : constant Storage_Array (1 .. Storage_Offset (P.Image_Bytes))
         with Import, Address => TLS_Image'Address;
         Block : Storage_Array (1 .. Storage_Offset (Block_Bytes + Layout.Control_Block_Bytes))
         with Import, Address => To_Address (Integer_Address (Pointer - Block_Bytes));
      begin
         Block := [others => 0];
         Block (1 .. Image'Last) := Image;
         Stack := SS.SS_Place
           (To_Address (Integer_Address (Base + Layout.Object_Offset (P.Prefix))),
            Layout.Chunk_Bytes);
         Put_Word (Pointer + Self_Offset, Pointer);
         Put_Word (Pointer + Stack_Offset, Value_Of (Stack.all'Address));
         return Pointer;
      end;
   end Make_Area;

   --  Where the thread whose thread pointer is Pointer keeps Slot: as far
   --  from its thread pointer as the calling thread's.
   function Slot_Of (Pointer : Unsigned_64; Slot : System.Address) return Unsigned_64 is
     (Pointer + (Value_Of (Slot) - Thread_Pointer));

   function Create return Unsigned_64 is
      Stack : SS.SS_Stack_Ptr;
      Base : Unsigned_64;
      Pointer : constant Unsigned_64 := Make_Area (Stack, Base);
   begin
      if Pointer /= No_Area then
         --  The new thread's slots, in its copy of the static TLS block.
         Put_Word (Slot_Of (Pointer, Current'Address), Value_Of (Stack.all'Address));
         Put_Word (Slot_Of (Pointer, Area_Base'Address), Base);
      end if;
      return Pointer;
   end Create;

   procedure Discard (Thread_Pointer : Unsigned_64) is
   begin
      --  The thread never ran: its stack has no dynamic chunks.
      if Thread_Pointer /= No_Area then
         Release_Area (Thread_Pointer - Pointer_Offset (Plan));
      end if;
   end Discard;

   --  CuAlloc's thread exit (CuAlloc_Native), where the program has
   --  CuAlloc: the thread's heap is left for the next thread to adopt.
   --  Idempotent: a second call finds no heap and does nothing.
   procedure Release_Thread_Heap
   with Import, Convention => C, External_Name => "cualloc_thread_exit";
   pragma Weak_External (Release_Thread_Heap);

   procedure Exit_Thread is
      Ignore : Unsigned_64;
   begin
      Release_Current;
      loop
         Ignore := CuBit.Kernel_Calls.Call (ABI.Thread_Exit);
      end loop;
   end Exit_Thread;

   --  The slow path of Current_Stack: this thread's first use, for a thread
   --  whose TLS another runtime made (musl, Rust): its stack object on
   --  CuAlloc. A refusal raises Storage_Error (HEAP_REFUSED).
   function New_Stack return SS.SS_Stack_Ptr with No_Inline;
   function New_Stack return SS.SS_Stack_Ptr is
      Stack : SS.SS_Stack_Ptr := null;
   begin
      SS.SS_Init (Stack, Layout.Chunk_Bytes);
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
      Stack : SS.SS_Stack_Ptr := Current;
      Base : constant Unsigned_64 := Area_Base;
   begin
      --  First the stack's chunks (and its object, when it is a CuAlloc
      --  block) go back to this thread's heap; then the heap is left for
      --  adoption (again, when the caller left it already: these frees may
      --  have taken it back); last the area, which holds this thread's
      --  TLS block, CuAlloc's slot in it among them. Nothing thread-local
      --  is written after that.
      Current := null;
      Area_Base := 0;
      if Stack /= null then
         SS.SS_Free (Stack);
      end if;
      if Release_Thread_Heap'Address /= System.Null_Address then
         Release_Thread_Heap;
      end if;
      if Base /= 0 then
         Release_Area (Base);
      end if;
   end Release_Current;

   procedure Start_Main is
      Stack : SS.SS_Stack_Ptr;
      Base : Unsigned_64;
      Pointer : constant Unsigned_64 := Make_Area (Stack, Base);
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
      Area_Base := Base;
   end Start_Main;

end CuBit.Thread_Areas;

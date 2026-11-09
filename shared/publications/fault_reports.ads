------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Why the kernel stopped a process (LOG-004): a typed report, never
--  "segmentation fault". The kernel fills one when a CPU exception, a page
--  it cannot back, or another process stops a process, and writes it into
--  the kernel log ring (Kernel_Log_Publication) under the process's own
--  identity; logstore renders it and keeps its fields.
--
--  @description
--  What the report says:
--  - the fault kind, decoded from the CPU's vector (Kind_Of_Vector);
--  - for a page fault, the error code decoded into its parts
--    (Decode_Page_Fault): the access (read, write, instruction fetch), the
--    cause (page not present, a protection violation, a reserved bit set in
--    a paging structure) and the CPU mode;
--  - the faulting address (CR2) and what is there (Classify), from what the
--    kernel already tracks of the address space: the null page, the band
--    below the stack reservation (a stack overflow), a read-only or
--    no-execute page, kernel space, or nothing at all;
--  - the instruction (RIP) as an offset into the program's image, which the
--    host symbolizes from the image's separated debug file
--    (tools/image_debug.py: addr2line on the image's link address + offset);
--  - the stack pointer and where it is (Stack_Position_Of), as an offset
--    below the stack's top;
--  - for a process stopped by another, who stopped it.
--
--  Everything here is pure arithmetic over words: proved free of run-time
--  errors, and the decoding functions' contracts state what each bit
--  means (tests/kernel-log-ring).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Fault_Reports with Pure, SPARK_Mode is

   ---------------------------------------------------------------------------
   --  What stopped the process
   ---------------------------------------------------------------------------
   --  The CPU exceptions a user program can raise (Intel SDM vol. 3, 6.15),
   --  then the stops that are not exceptions: a page the kernel could not
   --  back with memory, and another process stopping it.
   type Fault_Kind is
     (No_Fault, Divide_Error, Debug_Trap, Breakpoint, Overflow, Bound_Range,
      Invalid_Opcode, Segment_Not_Present, Stack_Segment, General_Protection,
      Page_Fault, X87_Floating_Point, Alignment_Check, SIMD_Floating_Point,
      Virtualization, Control_Protection, Other_Exception, Out_Of_Memory,
      Stopped_By_Process);
   for Fault_Kind use
     (No_Fault => 0, Divide_Error => 1, Debug_Trap => 2, Breakpoint => 3,
      Overflow => 4, Bound_Range => 5, Invalid_Opcode => 6,
      Segment_Not_Present => 7, Stack_Segment => 8, General_Protection => 9,
      Page_Fault => 10, X87_Floating_Point => 11, Alignment_Check => 12,
      SIMD_Floating_Point => 13, Virtualization => 14,
      Control_Protection => 15, Other_Exception => 16, Out_Of_Memory => 17,
      Stopped_By_Process => 18);

   --  The exception vectors (Intel SDM vol. 3, table 6-1).
   Vector_Divide_Error        : constant := 0;
   Vector_Debug               : constant := 1;
   Vector_Breakpoint          : constant := 3;
   Vector_Overflow            : constant := 4;
   Vector_Bound_Range         : constant := 5;
   Vector_Invalid_Opcode      : constant := 6;
   Vector_Segment_Not_Present : constant := 11;
   Vector_Stack_Segment       : constant := 12;
   Vector_General_Protection  : constant := 13;
   Vector_Page_Fault          : constant := 14;
   Vector_X87_Floating_Point  : constant := 16;
   Vector_Alignment_Check     : constant := 17;
   Vector_SIMD_Floating_Point : constant := 19;
   Vector_Virtualization      : constant := 20;
   Vector_Control_Protection  : constant := 21;

   function Kind_Of_Vector (Vector : Unsigned_64) return Fault_Kind is
     (case Vector is
         when Vector_Divide_Error        => Divide_Error,
         when Vector_Debug               => Debug_Trap,
         when Vector_Breakpoint          => Breakpoint,
         when Vector_Overflow            => Overflow,
         when Vector_Bound_Range         => Bound_Range,
         when Vector_Invalid_Opcode      => Invalid_Opcode,
         when Vector_Segment_Not_Present => Segment_Not_Present,
         when Vector_Stack_Segment       => Stack_Segment,
         when Vector_General_Protection  => General_Protection,
         when Vector_Page_Fault          => Page_Fault,
         when Vector_X87_Floating_Point  => X87_Floating_Point,
         when Vector_Alignment_Check     => Alignment_Check,
         when Vector_SIMD_Floating_Point => SIMD_Floating_Point,
         when Vector_Virtualization      => Virtualization,
         when Vector_Control_Protection  => Control_Protection,
         when others                     => Other_Exception)
   with Post => (Kind_Of_Vector'Result = Page_Fault) = (Vector = Vector_Page_Fault)
                and then Kind_Of_Vector'Result not in No_Fault | Out_Of_Memory | Stopped_By_Process;

   ---------------------------------------------------------------------------
   --  A page fault's error code (Intel SDM vol. 3, 4.7)
   ---------------------------------------------------------------------------
   Present_Bit  : constant := 0;   --  1: protection violation; 0: not present
   Write_Bit    : constant := 1;   --  1: the access was a write
   User_Bit     : constant := 2;   --  1: the CPU was in user mode
   Reserved_Bit : constant := 3;   --  1: a reserved bit set in a paging entry
   Fetch_Bit    : constant := 4;   --  1: an instruction fetch

   function Bit_Set (Word : Unsigned_64; Bit : Natural) return Boolean is
     ((Shift_Right (Word, Bit) and 1) = 1)
   with Pre => Bit < 64;

   type Access_Kind is (No_Access, Read, Write, Execute);
   for Access_Kind use (No_Access => 0, Read => 1, Write => 2, Execute => 3);

   type Page_Cause is (No_Cause, Not_Present, Protection, Reserved_Entry);
   for Page_Cause use (No_Cause => 0, Not_Present => 1, Protection => 2, Reserved_Entry => 3);

   type Privilege is (User_Mode, Kernel_Mode);
   for Privilege use (User_Mode => 0, Kernel_Mode => 1);

   type Page_Fault_Code is record
      Operation : Access_Kind := No_Access;
      Cause     : Page_Cause := No_Cause;
      Mode      : Privilege := User_Mode;
   end record;

   function Decode_Page_Fault (Error_Code : Unsigned_64) return Page_Fault_Code is
     ((Operation => (if Bit_Set (Error_Code, Fetch_Bit) then Execute
                     elsif Bit_Set (Error_Code, Write_Bit) then Write
                     else Read),
       Cause     => (if Bit_Set (Error_Code, Reserved_Bit) then Reserved_Entry
                     elsif Bit_Set (Error_Code, Present_Bit) then Protection
                     else Not_Present),
       Mode      => (if Bit_Set (Error_Code, User_Bit) then User_Mode else Kernel_Mode)))
   with Post =>
     (Decode_Page_Fault'Result.Operation /= No_Access
      and then Decode_Page_Fault'Result.Cause /= No_Cause
      and then (Decode_Page_Fault'Result.Operation = Execute) = Bit_Set (Error_Code, Fetch_Bit)
      and then (if not Bit_Set (Error_Code, Fetch_Bit)
                then (Decode_Page_Fault'Result.Operation = Write) = Bit_Set (Error_Code, Write_Bit))
      and then (Decode_Page_Fault'Result.Mode = User_Mode) = Bit_Set (Error_Code, User_Bit)
      and then (Decode_Page_Fault'Result.Cause = Reserved_Entry) = Bit_Set (Error_Code, Reserved_Bit)
      and then (if not Bit_Set (Error_Code, Reserved_Bit)
                then (Decode_Page_Fault'Result.Cause = Protection) = Bit_Set (Error_Code, Present_Bit)));

   ---------------------------------------------------------------------------
   --  Where an address is
   ---------------------------------------------------------------------------
   --  The kernel's record of a process's address space, as it already keeps
   --  it (Process: the stack and heap reservations, the loaded image).
   --  Each range is First .. Limit - 1; an empty one has Limit <= First.
   type Layout is record
      Stack_First, Stack_Limit : Unsigned_64 := 0;
      Heap_First, Heap_Limit   : Unsigned_64 := 0;
      Image_First, Image_Limit : Unsigned_64 := 0;
   end record;

   --  The first page is never mapped: an access there is a null pointer
   --  (or a small offset from one).
   Null_Page_Bytes : constant := 4_096;
   --  Below the stack reservation nothing is mapped, so an access in this
   --  band, the most one stack frame plausibly skips, is the stack running
   --  out: a stack overflow.
   Stack_Guard_Bytes : constant := 64 * 1_024;
   --  User addresses end here (the canonical lower half).
   User_Limit : constant := 16#0000_8000_0000_0000#;

   type Address_Class is
     (Not_Applicable, Null_Page, Stack_Overflow, Unmapped, Read_Only,
      No_Execute, Reserved_Entry_Page, Kernel_Space, In_Stack, In_Heap, In_Image);
   for Address_Class use
     (Not_Applicable => 0, Null_Page => 1, Stack_Overflow => 2, Unmapped => 3,
      Read_Only => 4, No_Execute => 5, Reserved_Entry_Page => 6, Kernel_Space => 7,
      In_Stack => 8, In_Heap => 9, In_Image => 10);

   function Within (First, Limit, Address : Unsigned_64) return Boolean is
     (First < Limit and then Address >= First and then Address < Limit);

   function In_Guard (L : Layout; Address : Unsigned_64) return Boolean is
     (L.Stack_First < L.Stack_Limit and then Address < L.Stack_First
      and then L.Stack_First - Address <= Stack_Guard_Bytes);

   --  What is at Address, for a page fault with Code. The null page and
   --  kernel space first (whatever the access), then the protection that
   --  refused it, then the reservation it falls in.
   function Classify (Address : Unsigned_64; Code : Page_Fault_Code; L : Layout)
     return Address_Class
   is (if Address < Null_Page_Bytes then Null_Page
       elsif Address >= User_Limit then Kernel_Space
       elsif Code.Cause = Reserved_Entry then Reserved_Entry_Page
       elsif Code.Cause = Protection and then Code.Operation = Execute then No_Execute
       elsif Code.Cause = Protection and then Code.Operation = Write then Read_Only
       elsif In_Guard (L, Address) then Stack_Overflow
       elsif Within (L.Stack_First, L.Stack_Limit, Address) then In_Stack
       elsif Within (L.Heap_First, L.Heap_Limit, Address) then In_Heap
       elsif Within (L.Image_First, L.Image_Limit, Address) then In_Image
       else Unmapped)
   with Post => (if Address < Null_Page_Bytes then Classify'Result = Null_Page)
                and then (if Classify'Result = Stack_Overflow then In_Guard (L, Address))
                and then Classify'Result /= Not_Applicable;

   --  Where the stack pointer is.
   type Stack_Position is (Unknown, In_Stack_Reservation, In_Stack_Guard, Outside_Stack);
   for Stack_Position use
     (Unknown => 0, In_Stack_Reservation => 1, In_Stack_Guard => 2, Outside_Stack => 3);

   function Stack_Position_Of (RSP : Unsigned_64; L : Layout) return Stack_Position is
     (if L.Stack_Limit <= L.Stack_First then Unknown
      elsif Within (L.Stack_First, L.Stack_Limit, RSP) then In_Stack_Reservation
      elsif In_Guard (L, RSP) then In_Stack_Guard
      else Outside_Stack);

   --  An instruction's offset into the program's image, or No_Offset when
   --  it is outside it (the report then keeps the bare RIP).
   No_Offset : constant Unsigned_64 := Unsigned_64'Last;
   function Image_Offset (RIP : Unsigned_64; L : Layout) return Unsigned_64 is
     (if Within (L.Image_First, L.Image_Limit, RIP) then RIP - L.Image_First else No_Offset)
   with Post => (if Image_Offset'Result /= No_Offset
                 then Image_Offset'Result < L.Image_Limit - L.Image_First);

   ---------------------------------------------------------------------------
   --  Anchors: addresses as offsets from what they are in
   ---------------------------------------------------------------------------
   --  A detail never carries an absolute address where an offset will do,
   --  so it does not reveal the address-space layout even to the readers
   --  who may see it. (CuBit has no address-space randomisation today:
   --  images load at their link address and the stack top is fixed, so an
   --  offset reveals nothing an absolute would not. It is the form that
   --  stays harmless when randomisation comes.)
   --  Image: from the image's first byte. Stack_Top: below the stack's
   --  top (Stack_Limit - address). Stack_Bottom: below the stack's lowest
   --  byte (the overflow band). Heap: from the heap's first byte. Zero:
   --  the address itself, for the null page (small by definition).
   --  Unanchored: in no region the kernel tracks (a wild pointer: its
   --  value is the information) or kernel space; the address itself.
   type Anchor is (No_Anchor, Image, Stack_Top, Stack_Bottom, Heap, Zero, Unanchored);
   for Anchor use
     (No_Anchor => 0, Image => 1, Stack_Top => 2, Stack_Bottom => 3, Heap => 4,
      Zero => 5, Unanchored => 6);

   type Anchored is record
      From   : Anchor := No_Anchor;
      Offset : Unsigned_64 := 0;
   end record;

   --  Address as an offset from the region it is in.
   function Anchor_Of (Address : Unsigned_64; L : Layout) return Anchored is
     (if Address < Null_Page_Bytes then (Zero, Address)
      elsif Within (L.Image_First, L.Image_Limit, Address) then (Image, Address - L.Image_First)
      elsif Within (L.Stack_First, L.Stack_Limit, Address) then (Stack_Top, L.Stack_Limit - Address)
      elsif In_Guard (L, Address) then (Stack_Bottom, L.Stack_First - Address)
      elsif Within (L.Heap_First, L.Heap_Limit, Address) then (Heap, Address - L.Heap_First)
      else (Unanchored, Address))
   with Post => Anchor_Of'Result.From /= No_Anchor
                and then (if Anchor_Of'Result.From = Image
                          then Anchor_Of'Result.Offset < L.Image_Limit - L.Image_First)
                and then (if Anchor_Of'Result.From = Stack_Bottom
                          then Anchor_Of'Result.Offset <= Stack_Guard_Bytes);

   ---------------------------------------------------------------------------
   --  The report (the kernel log ring's fault part, 64 bytes)
   ---------------------------------------------------------------------------
   --  Two parts, two records (Kernel_Log_Publication.Audience): the
   --  summary every reader of the process's logs may see (what kind of
   --  fault, the access, who stopped it: Summary_Of keeps only those), and
   --  the detail only diagnostics readers may see (where: the address,
   --  its class, the instruction and stack pointer, each anchored). A
   --  fault can point at memory corruption an attacker could use. Neither
   --  part holds register values beyond RIP and RSP, or any byte of the
   --  process's memory: the kernel reads none to build it. The last
   --  system call is not kept: the kernel does not track it (a count
   --  only, per thread).
   type Report is record
      Kind         : Unsigned_8 := 0;    --  Fault_Kind'Enum_Rep
      Operation    : Unsigned_8 := 0;    --  Access_Kind'Enum_Rep (page faults)
      Cause        : Unsigned_8 := 0;    --  Page_Cause'Enum_Rep (page faults; detail)
      Mode         : Unsigned_8 := 0;    --  Privilege'Enum_Rep
      Where        : Unsigned_8 := 0;    --  Address_Class'Enum_Rep (detail)
      Stack        : Unsigned_8 := 0;    --  Stack_Position'Enum_Rep (detail)
      Vector       : Unsigned_8 := 0;    --  the CPU's vector (exceptions)
      Address_From : Unsigned_8 := 0;    --  Anchor'Enum_Rep (detail)
      Error_Code   : Unsigned_32 := 0;   --  the CPU's error code (detail)
      RIP_From     : Unsigned_8 := 0;    --  Anchor'Enum_Rep (detail)
      RSP_From     : Unsigned_8 := 0;    --  Anchor'Enum_Rep (detail)
      Spare        : Unsigned_16 := 0;
      Address      : Unsigned_64 := 0;   --  offset from Address_From (detail)
      RIP          : Unsigned_64 := 0;   --  offset from RIP_From (detail)
      RSP          : Unsigned_64 := 0;   --  offset from RSP_From (detail)
      Stopper      : Unsigned_64 := 0;   --  who stopped it (Stopped_By_Process)
      Spare_2      : Unsigned_64 := 0;
      Spare_3      : Unsigned_64 := 0;
   end record;
   for Report use record
      Kind         at 0 range 0 .. 7;
      Operation    at 1 range 0 .. 7;
      Cause        at 2 range 0 .. 7;
      Mode         at 3 range 0 .. 7;
      Where        at 4 range 0 .. 7;
      Stack        at 5 range 0 .. 7;
      Vector       at 6 range 0 .. 7;
      Address_From at 7 range 0 .. 7;
      Error_Code   at 8 range 0 .. 31;
      RIP_From     at 12 range 0 .. 7;
      RSP_From     at 13 range 0 .. 7;
      Spare        at 14 range 0 .. 15;
      Address      at 16 range 0 .. 63;
      RIP          at 24 range 0 .. 63;
      RSP          at 32 range 0 .. 63;
      Stopper      at 40 range 0 .. 63;
      Spare_2      at 48 range 0 .. 63;
      Spare_3      at 56 range 0 .. 63;
   end record;
   Report_Bytes : constant := 64;
   for Report'Size use Report_Bytes * 8;

   --  A CPU exception in user mode (Vector, Error_Code; Address is CR2 for
   --  a page fault) at RIP with RSP, in a process laid out as L: the
   --  detail.
   function Exception_Report
     (Vector, Error_Code, Address, RIP, RSP : Unsigned_64; L : Layout) return Report;

   --  A page the kernel could not back (no memory, or the process's
   --  quota): the detail.
   function Out_Of_Memory_Report
     (Address, RIP, RSP : Unsigned_64; L : Layout) return Report;

   --  Stopped by the process Stopper (its identity word). Nothing in it is
   --  restricted: it is its own summary.
   function Stopped_Report (Stopper : Unsigned_64) return Report;

   --  The part every reader may see: the kind, the access, the mode and
   --  who stopped it; everything else zero.
   function Summary_Of (R : Report) return Report is
     ((Kind => R.Kind, Operation => R.Operation, Mode => R.Mode, Stopper => R.Stopper,
       others => <>))
   with Post => Summary_Of'Result.Address = 0 and then Summary_Of'Result.RIP = 0
                and then Summary_Of'Result.RSP = 0 and then Summary_Of'Result.Error_Code = 0
                and then Summary_Of'Result.Where = 0 and then Summary_Of'Result.Address_From = 0
                and then Summary_Of'Result.Kind = R.Kind;

   ---------------------------------------------------------------------------
   --  Reading one back (a reader trusts nothing it copied)
   ---------------------------------------------------------------------------
   function Kind_Of (R : Report) return Fault_Kind;
   function Operation_Of (R : Report) return Access_Kind;
   function Where_Of (R : Report) return Address_Class;
   function Stack_Of (R : Report) return Stack_Position;
   function Anchor_Value (Code : Unsigned_8) return Anchor;

   --  Short names, as a person reads them.
   function Kind_Name (Kind : Fault_Kind) return String;
   function Operation_Name (Operation : Access_Kind) return String;
   function Access_Name (Operation : Access_Kind) return String;
   function Where_Name (Where : Address_Class) return String;
   function Stack_Name (Stack : Stack_Position) return String;
   function Anchor_Name (From : Anchor) return String;
end Fault_Reports;

-------------------------------------------------------------------------------
--  CuBitOS
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Device interrupt vectors: the kernel/userspace ABI (docs/msi-interrupts.md).
--
--  @description
--  One definition, used by the kernel (InterruptNumbers, Device_Vectors, the
--  system calls) and by userspace (CuBit.Device_Interrupts): the range of IDT
--  vectors the kernel hands to message-signaled devices, the largest block one
--  interrupt capability names, the word an allocation returns, and the layout
--  of the pending-vector set an IRQ doorbell event carries.
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;

package Device_Vector_ABI with
   SPARK_Mode => On,
   Pure
is
   ---------------------------------------------------------------------------
   --  The device range: every IDT vector from the first one above the legacy
   --  PIC/IOAPIC lines (32 .. 47) to the one below the kernel panic IPI (127).
   --  The kernel keeps 127 (panic), 128 (legacy syscall gate), 249
   --  (reschedule IPI) and 255 (spurious) for itself.
   ---------------------------------------------------------------------------
   First_Vector : constant := 48;
   Last_Vector  : constant := 126;
   Vector_Count : constant := Last_Vector - First_Vector + 1;

   subtype Device_Vector is Natural range First_Vector .. Last_Vector;

   --  The most vectors one interrupt capability names: one MSI-X function's
   --  worth for one manifest request (CCL.Resource_Sections
   --  MAX_INTERRUPT_VECTORS).
   Max_Block_Vectors : constant := 32;
   subtype Block_Size is Positive range 1 .. Max_Block_Vectors;

   --  The device manager's capability slot for the vector authority: a
   --  CAP_IRQ naming the whole range, installed by the kernel at boot.
   Vector_Authority_Slot : constant := 9;

   --  An xAPIC physical destination (CPU_Topology.APIC_ID).
   subtype Destination_ID is Natural range 0 .. 254;

   ---------------------------------------------------------------------------
   --  ALLOCATE_DEVICE_VECTORS returns one word: the block's first vector,
   --  its size and the local APIC that receives it (the owner's CPU).
   ---------------------------------------------------------------------------
   Field_Radix       : constant := 256;      -- one byte per field
   Head_Scale        : constant := 1;
   Count_Scale       : constant := Field_Radix;
   Destination_Scale : constant := Field_Radix * Field_Radix;
   Word_Limit        : constant := Destination_Scale * Field_Radix;

   function Allocation_Word
     (Head : Device_Vector; Count : Block_Size; Destination : Destination_ID)
      return Unsigned_64 is
     (Unsigned_64 (Head) * Head_Scale +
      Unsigned_64 (Count) * Count_Scale +
      Unsigned_64 (Destination) * Destination_Scale);

   function Field (Word : Unsigned_64; Scale : Unsigned_64) return Unsigned_64 is
     ((Word / Scale) mod Field_Radix)
     with Pre => Scale in Head_Scale | Count_Scale | Destination_Scale;

   --  Whether Word is one Allocation_Word can produce.
   function Valid_Allocation (Word : Unsigned_64) return Boolean is
     (Word < Word_Limit and then
      Field (Word, Head_Scale) in First_Vector .. Last_Vector and then
      Field (Word, Count_Scale) in 1 .. Max_Block_Vectors and then
      Field (Word, Destination_Scale) <= Unsigned_64 (Destination_ID'Last));

   function Head_Of (Word : Unsigned_64) return Device_Vector is
     (Device_Vector (Field (Word, Head_Scale)))
     with Pre => Valid_Allocation (Word);
   function Count_Of (Word : Unsigned_64) return Block_Size is
     (Block_Size (Field (Word, Count_Scale)))
     with Pre => Valid_Allocation (Word);
   function Destination_Of (Word : Unsigned_64) return Destination_ID is
     (Destination_ID (Field (Word, Destination_Scale)))
     with Pre => Valid_Allocation (Word);

   --  Decoding inverts encoding.
   procedure Allocation_Round_Trip
     (Head : Device_Vector; Count : Block_Size; Destination : Destination_ID)
     with Ghost,
         Post => Valid_Allocation (Allocation_Word (Head, Count, Destination))
          and then Head_Of (Allocation_Word (Head, Count, Destination)) = Head
          and then Count_Of (Allocation_Word (Head, Count, Destination)) = Count
          and then Destination_Of (Allocation_Word (Head, Count, Destination))
                  = Destination;

   ---------------------------------------------------------------------------
   --  READ_DEVICE_VECTOR fields (visibility; the allocation authority only).
   ---------------------------------------------------------------------------
   type Read_Field is (Interrupt_Count, Owner_Identity, Binding_State);
   for Read_Field use
     (Interrupt_Count => 0, Owner_Identity => 1, Binding_State => 2);

   --  Binding_State values.
   type Vector_Binding is (Unbound, Bound, Retired);
   for Vector_Binding use (Unbound => 0, Bound => 1, Retired => 2);

   ---------------------------------------------------------------------------
   --  The IRQ doorbell event (label 1) carries the owner's pending device
   --  vectors in its first Pending_Words words: bit B of word W is vector
   --  First_Vector + 64 * W + B. A legacy line sets the doorbell alone.
   ---------------------------------------------------------------------------
   Bits_Per_Word : constant := 64;
   Pending_Words : constant := (Vector_Count + Bits_Per_Word - 1) / Bits_Per_Word;
   subtype Pending_Word_Index is Natural range 0 .. Pending_Words - 1;
   subtype Bit_Index is Natural range 0 .. Bits_Per_Word - 1;

   function Word_Of (Vector : Device_Vector) return Pending_Word_Index is
     ((Vector - First_Vector) / Bits_Per_Word);
   function Bit_Of (Vector : Device_Vector) return Bit_Index is
     ((Vector - First_Vector) mod Bits_Per_Word);
   function Mask_Of (Vector : Device_Vector) return Unsigned_64 is
     (Shift_Left (Unsigned_64'(1), Bit_Of (Vector)));

   --  Distinct vectors never share a pending bit.
   procedure Pending_Bits_Distinct (A, B : Device_Vector)
     with Ghost,
         Post => (if A /= B then
                 Word_Of (A) /= Word_Of (B) or else Bit_Of (A) /= Bit_Of (B));
end Device_Vector_ABI;

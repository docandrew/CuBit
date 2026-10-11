------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  Message-signaled device interrupts - implementation
------------------------------------------------------------------------------
with System.Storage_Elements;

package body CuBit.Device_Interrupts is

   --  INSPECT_CAPABILITY's six words: kind, rights, tag, object, parameter,
   --  generation. CAP_IRQ is kind 5.
   type Inspection is array (0 .. 5) of Unsigned_64;
   KIND_WORD   : constant := 0;
   OBJECT_WORD : constant := 3;
   PARAM_WORD  : constant := 4;
   CAP_IRQ     : constant Unsigned_64 := 5;
   INSPECTED   : constant Unsigned_64 := 1;
   REFUSED     : constant Unsigned_64 := Unsigned_64'Last;

   function Block_Of (Slot : CapabilitySlot) return Interrupt_Block is
      Data : aliased Inspection := (others => 0);
      Self : constant Unsigned_64 := syscall (SYSCALL_GETPID);
      Result : Unsigned_64;
   begin
      Result := syscall
        (SYSCALL_INSPECT_CAPABILITY, Self, Slot,
         Unsigned_64 (System.Storage_Elements.To_Integer (Data'Address)));
      if Result /= INSPECTED or else Data (KIND_WORD) /= CAP_IRQ
        or else Data (OBJECT_WORD) not in First_Vector .. Last_Vector
        or else Data (PARAM_WORD) not in 1 .. Max_Block_Vectors
        or else Data (PARAM_WORD) - 1 > Last_Vector - Data (OBJECT_WORD)
      then
         return (others => <>);
      end if;
      return (Valid => True,
              Head  => Device_Vector (Data (OBJECT_WORD)),
              Count => Block_Size (Data (PARAM_WORD)));
   end Block_Of;

   function Fired (Event : Message; Block : Interrupt_Block; Index : Vector_Index)
     return Boolean
   is
      Vector : constant Device_Vector := Block.Head + Index;
   begin
      return Is_Doorbell (Event)
        and then (Event.words (Word_Of (Vector)) and Mask_Of (Vector)) /= 0;
   end Fired;

   function Allocate
     (Owner : Process_ID; Count : Block_Size; Slot : CapabilitySlot;
      CPU : Natural) return Allocation
   is
      Word : constant Unsigned_64 := syscall
        (SYSCALL_ALLOCATE_DEVICE_VECTORS, To_Word (Owner),
         Unsigned_64 (Count), Slot, Unsigned_64 (CPU));
   begin
      if Word = REFUSED or else not Valid_Allocation (Word) then
         return (others => <>);
      end if;
      return (Valid       => True,
              Head        => Head_Of (Word),
              Count       => Count_Of (Word),
              Destination => Destination_Of (Word));
   end Allocate;

   function Release (Head : Device_Vector) return Boolean is
     (syscall (SYSCALL_RELEASE_DEVICE_VECTORS, Unsigned_64 (Head)) = 0);

   function Read (Vector : Device_Vector; Field : Read_Field) return Unsigned_64 is
     (syscall (SYSCALL_READ_DEVICE_VECTOR, Unsigned_64 (Vector),
               Unsigned_64 (Read_Field'Enum_Rep (Field))));

end CuBit.Device_Interrupts;

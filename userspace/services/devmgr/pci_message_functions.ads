------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The order devmgr programs and tears down one PCI function's MSI or MSI-X
--  (docs/msi-interrupts.md)
--
--  @description
--  Generic over the hardware access so the same sequence runs in devmgr
--  (port-I/O configuration space, the MSI-X table page mapped from the BAR)
--  and against the device model in tests/msi-vectors. Every table or
--  configuration write goes through a PCI_Message_Interrupts plan.
--
--  Lifecycle of a message-signaled function:
--   1. Program: the kernel has allocated the vectors; every entry is
--      written masked and MSI-X is enabled with the function masked (MSI:
--      written with MSI Enable clear, then enabled with one message).
--   2. Open: once the driver reports ready, entries are unmasked and the
--      function mask dropped. Messages raised while masked are held in the
--      PBA and delivered now: nothing is lost.
--   3. Close: every entry masked (MSI: MSI Enable cleared) BEFORE the
--      vectors are released to the kernel, so a stale device can never
--      send into a vector that was handed to someone else.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with Device_Vector_ABI; use Device_Vector_ABI;
with PCI_Message_Interrupts; use PCI_Message_Interrupts;

generic
   --  One 32-bit write to an MSI-X table entry register.
   with procedure Write_Entry
     (Index : Entry_Index; Register : Entry_Register; Value : Unsigned_32);
   --  The function's capability registers, by offset from the capability.
   with function Read_Config_16 (Offset : Natural) return Unsigned_16;
   with procedure Write_Config_16 (Offset : Natural; Value : Unsigned_16);
   with procedure Write_Config_32 (Offset : Natural; Value : Unsigned_32);
package PCI_Message_Functions with SPARK_Mode => On is

   subtype Entry_Count is Natural range 0 .. Max_Block_Vectors;

   --  MSI-X: entries 0 .. Count - 1 send vectors Head .. Head + Count - 1.
   procedure Program_MSIX
     (Head : Device_Vector; Count : Block_Size; Destination : Destination_ID)
   with Pre => Count - 1 <= Last_Vector - Head;

   procedure Open_MSIX (Count : Block_Size);

   procedure Close_MSIX (Count : Block_Size);

   --  MSI: the single message sends Vector.
   procedure Program_MSI (Vector : Device_Vector; Destination : Destination_ID);

   --  MSI programmed with MSI Enable left clear.
   procedure Prepare_MSI (Vector : Device_Vector; Destination : Destination_ID);

   procedure Close_MSI;

end PCI_Message_Functions;

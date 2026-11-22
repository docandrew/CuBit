------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  The conventional PCI capability list, walked safely (HW-002 follow-up)
--
--  @description
--  A function's capabilities are a linked list in its 256-byte conventional
--  configuration space (PCI Local Bus 3.0, 6.7): the Capabilities List bit
--  of Status says there is one, the Capabilities Pointer (0x34) names the
--  first entry, and each entry is an ID byte and a next-pointer byte. The
--  pointers come from the device, so the walk trusts none of them:
--   * a pointer's two low bits are reserved and masked off;
--   * an entry lies after the 64-byte standard header, at 0x40 .. 0xFC; a
--     nonzero pointer into the header ends the walk as Truncated;
--   * every entry is visited at most once; a pointer back to a visited
--     entry ends the walk as Looped (an absent function reads all ones,
--     which is a one-entry loop at 0xFC);
--   * an MSI or MSI-X entry whose structure would run past the end of
--     configuration space is not used, and the walk reports Truncated.
--  Proved free of run-time errors and the postconditions below
--  (tests/pci-capabilities, gnatprove level 2).
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;
with PCI_Message_Interrupts;

package PCI_Capabilities with
   SPARK_Mode => On,
   Pure
is
   --  Conventional configuration space (PCI Local Bus 3.0, 6.1).
   Config_Space_Bytes : constant := 256;
   subtype Config_Offset is Natural range 0 .. Config_Space_Bytes - 1;

   --  Status register (offset 6) and its Capabilities List bit (bit 4).
   Status_Offset            : constant Config_Offset := 16#06#;
   Status_Capabilities_List : constant Unsigned_8 := 16#10#;
   --  The first entry's offset, in a type-0 or type-1 header.
   Capabilities_Pointer_Offset : constant Config_Offset := 16#34#;
   --  Reserved low bits of every capability pointer.
   Pointer_Mask : constant Unsigned_8 := 16#FC#;
   --  An entry's next pointer is the byte after its ID.
   Next_Pointer_Offset : constant := 1;
   --  Entries follow the 64-byte standard header; dword aligned.
   First_Entry  : constant Config_Offset := 16#40#;
   Last_Entry   : constant Config_Offset := 16#FC#;
   Entry_Stride : constant := 4;
   --  Distinct entry positions: the most entries a list can have.
   Maximum_Entries : constant := (Last_Entry - First_Entry) / Entry_Stride + 1;

   --  An entry's offset, or No_Entry.
   subtype Entry_Reference is Config_Offset;
   No_Entry : constant Entry_Reference := 0;

   --  Capability IDs (PCI Code and ID Assignment 1.11, 2).
   MSI_ID  : constant Unsigned_8 := PCI_Message_Interrupts.MSI_CAPABILITY_ID;
   MSIX_ID : constant Unsigned_8 := PCI_Message_Interrupts.MSIX_CAPABILITY_ID;
   Vendor_Specific_ID : constant Unsigned_8 := 16#09#;

   --  The smallest MSI structure (32-bit address: ID, next, control,
   --  address, data) and the MSI-X structure (ID, next, control, table and
   --  PBA locators), PCI Local Bus 3.0 6.8.1 and 6.8.2.
   MSI_Minimum_Bytes  : constant := 10;
   MSIX_Structure_Bytes : constant := 12;

   --  MSI Message Control: Multiple Message Capable, bits 3:1, log2 of the
   --  vectors the function can send (0 .. 5: 1 .. 32 vectors; 6 and 7 are
   --  reserved, counted as the one vector every MSI function has).
   MSI_Multiple_Capable_Mask  : constant Unsigned_16 := 16#000E#;
   MSI_Multiple_Capable_Shift : constant := 1;
   Maximum_MSI_Vectors : constant := 32;
   subtype MSI_Vector_Count is Natural range 0 .. Maximum_MSI_Vectors;
   subtype MSIX_Vector_Count is Natural range 0 .. PCI_Message_Interrupts.MAX_TABLE_ENTRIES;

   --  How a walk ended.
   --  No_List: the Status bit is clear, or the first pointer is zero.
   --  Complete: a next pointer of zero ended it.
   --  Truncated: a pointer into the standard header, or an MSI / MSI-X
   --  structure past the end of configuration space.
   --  Looped: a pointer to an entry already visited.
   type Walk_Status is (No_List, Complete, Truncated, Looped);

   --  A function's message-signalled interrupt capabilities.
   type Interrupt_Facts is record
      Status       : Walk_Status := No_List;
      MSI          : Entry_Reference := No_Entry;
      MSI_Vectors  : MSI_Vector_Count := 0;
      MSIX         : Entry_Reference := No_Entry;
      MSIX_Vectors : MSIX_Vector_Count := 0;
   end record;

   --  Vectors an MSI function can send: 2 ** Multiple Message Capable; a
   --  reserved encoding counts as the one vector every MSI function has.
   type MSI_Capable_Field is mod 2 ** 3;
   MSI_Vectors_By_Field : constant array (MSI_Capable_Field) of MSI_Vector_Count :=
     [0 => 1, 1 => 2, 2 => 4, 3 => 8, 4 => 16, 5 => Maximum_MSI_Vectors, 6 | 7 => 1];
   function MSI_Vectors_Of (Control : Unsigned_16) return MSI_Vector_Count is
     (MSI_Vectors_By_Field (MSI_Capable_Field
        (Shift_Right (Control and MSI_Multiple_Capable_Mask, MSI_Multiple_Capable_Shift))))
   with Post => MSI_Vectors_Of'Result in 1 .. Maximum_MSI_Vectors;

   function Fits (At_Entry : Entry_Reference; Bytes : Positive) return Boolean is
     (Bytes <= Config_Space_Bytes and then At_Entry <= Config_Space_Bytes - Bytes);

   generic
      --  One byte of the function's configuration space.
      with function Read (Offset : Config_Offset) return Unsigned_8;
   package Walks is
      --  The first entry with ID, or No_Entry (none before the walk ended).
      function Find (ID : Unsigned_8) return Entry_Reference
      with Post => (if Find'Result /= No_Entry then
                      Find'Result in First_Entry .. Last_Entry
                      and then Find'Result mod Entry_Stride = 0
                      and then Read (Find'Result) = ID);

      --  The function's MSI and MSI-X entries (the first of each that
      --  fits in configuration space) and their vector counts.
      function Interrupts return Interrupt_Facts
      with Post =>
        (if Interrupts'Result.MSI = No_Entry then Interrupts'Result.MSI_Vectors = 0
         else Read (Interrupts'Result.MSI) = MSI_ID
              and then Interrupts'Result.MSI in First_Entry .. Last_Entry
              and then Fits (Interrupts'Result.MSI, MSI_Minimum_Bytes)
              and then Interrupts'Result.MSI_Vectors >= 1)
        and then
        (if Interrupts'Result.MSIX = No_Entry then Interrupts'Result.MSIX_Vectors = 0
         else Read (Interrupts'Result.MSIX) = MSIX_ID
              and then Interrupts'Result.MSIX in First_Entry .. Last_Entry
              and then Fits (Interrupts'Result.MSIX, MSIX_Structure_Bytes)
              and then Interrupts'Result.MSIX_Vectors >= 1)
        and then
        (if Interrupts'Result.Status = No_List then
           Interrupts'Result.MSI = No_Entry and then Interrupts'Result.MSIX = No_Entry);
   end Walks;
end PCI_Capabilities;

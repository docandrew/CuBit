------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Message-signaled device interrupts for drivers and devmgr
--  (docs/msi-interrupts.md)
--
--  @description
--  A driver's interrupts are one capability in its manifest-named slot: a
--  CAP_IRQ naming Count consecutive device vectors from Head, installed by
--  the kernel when devmgr allocated them. MSI-X entry I of the device sends
--  vector Head + I. The IRQ doorbell event (label EVENT_IRQ) says which of
--  the driver's vectors fired since it last took the doorbell; the device's
--  own registers say what happened.
--
--  devmgr alone holds the vector authority and uses Allocate, Release and
--  Read.
------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with Device_Vector_ABI; use Device_Vector_ABI;
with CuBit.Messages; use CuBit.Messages;

package CuBit.Device_Interrupts is

   --  The IRQ doorbell's label (kernel IPC_Labels.EVENT_IRQ).
   EVENT_IRQ : constant Unsigned_32 := 16#0001#;

   subtype Vector_Index is Natural range 0 .. Max_Block_Vectors - 1;

   type Interrupt_Block is record
      Valid : Boolean := False;
      Head  : Device_Vector := Device_Vector'First;
      Count : Block_Size := 1;
   end record;

   --  The block the CAP_IRQ in Slot names; Valid False if Slot holds none.
   function Block_Of (Slot : CapabilitySlot) return Interrupt_Block;

   function Is_Doorbell (Event : Message) return Boolean is
     (Event.tag.label = EVENT_IRQ);

   --  Whether the doorbell Event reports vector Head + Index of Block.
   function Fired (Event : Message; Block : Interrupt_Block; Index : Vector_Index)
     return Boolean
   with Pre => Block.Valid and then Index < Block.Count
     and then Index <= Last_Vector - Block.Head;

   ---------------------------------------------------------------------------
   --  devmgr: the vector authority (Device_Vector_ABI.Vector_Authority_Slot)
   ---------------------------------------------------------------------------
   type Allocation is record
      Valid       : Boolean := False;
      Head        : Device_Vector := Device_Vector'First;
      Count       : Block_Size := 1;
      Destination : Destination_ID := 0;
   end record;

   --  Bind Count vectors to Owner, delivered to CPU, and install Owner's
   --  CAP_IRQ in its empty Slot.
   function Allocate
     (Owner : Process_ID; Count : Block_Size; Slot : CapabilitySlot;
      CPU : Natural) return Allocation;

   --  Free a block after masking it at the device.
   function Release (Head : Device_Vector) return Boolean;

   --  A vector's interrupt count, owner identity or binding; all ones when
   --  refused.
   function Read (Vector : Device_Vector; Field : Read_Field) return Unsigned_64;

end CuBit.Device_Interrupts;

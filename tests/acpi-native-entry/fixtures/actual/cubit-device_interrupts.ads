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

with CuBit.Handles;

package CuBit.Device_Interrupts is

   --  The IRQ doorbell's label (kernel IPC_Labels.EVENT_IRQ).
   EVENT_IRQ : constant Unsigned_32 := 16#0001#;

   subtype Vector_Index is Natural range 0 .. Max_Block_Vectors - 1;

   type Interrupt_Block is record
      Valid : Boolean := False;
      Head  : Device_Vector := Device_Vector'First;
      Count : Block_Size := 1;
   end record;

   --  The block a driver's interrupt handle names (its startup "device"
   --  or similar entry); Valid False if it names no vector block.
   function Block_Of (Item : CuBit.Handles.Interrupt) return Interrupt_Block;

   function Is_Doorbell (Event : Message) return Boolean is
     (Event.tag.label = EVENT_IRQ);

   --  Whether the doorbell Event reports vector Head + Index of Block.
   function Fired (Event : Message; Block : Interrupt_Block; Index : Vector_Index)
     return Boolean
   with Pre => Block.Valid and then Index < Block.Count
     and then Index <= Last_Vector - Block.Head;

   ---------------------------------------------------------------------------
   --  devmgr: the vector authority (its boot root, a CuBit.Handles.Interrupt)
   ---------------------------------------------------------------------------
   type Allocation is record
      Valid       : Boolean := False;
      Head        : Device_Vector := Device_Vector'First;
      Count       : Block_Size := 1;
      Destination : Destination_ID := 0;
      --  The owner's CAP_IRQ handle in its own table (for its startup
      --  table); Why when not Valid.
      Owner_Handle : CuBit.Handles.Foreign_Handle;
      Why          : CuBit.Handles.Refusal := CuBit.Handles.ABI.Invalid_Argument;
   end record;

   --  Bind Count vectors to Owner, delivered to CPU, and place Owner's
   --  CAP_IRQ in its table through Space (before Owner first runs).
   function Allocate
     (Owner : Process_ID; Space : CuBit.Handles.Cspace; Count : Block_Size;
      CPU : Natural; Authority : CuBit.Handles.Interrupt) return Allocation;

   --  Free a block after masking it at the device.
   function Release
     (Head : Device_Vector; Authority : CuBit.Handles.Interrupt) return Boolean;

   --  A vector's interrupt count, owner identity or binding; all ones when
   --  refused.
   function Read
     (Vector : Device_Vector; Field : Read_Field; Authority : CuBit.Handles.Interrupt)
      return Unsigned_64;

end CuBit.Device_Interrupts;

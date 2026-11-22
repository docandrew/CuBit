------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Legacy interrupt lines by handle (Interrupt_Line_ABI): routing one to a
--  process (a launcher) and acknowledging one (its owner).
--
--  @description
--  An owner holds an Interrupt handle naming exactly its line (read right,
--  no grant). Interrupts arrive as the IRQ doorbell event
--  (CuBit.Device_Interrupts.Is_Doorbell) on its ordinary receive.
--
--  Level-triggered lines (ACPI's SCI; PCI INTx): the kernel masks the
--  line when it delivers it, so it cannot fire again while the owner works.
--  The owner must, in this order:
--    1. read its source's status and handle the event;
--    2. clear the source (for the SCI: write 1 to the PM1/GPE status bits
--       it handled, or disable what it cannot clear);
--    3. Acknowledge (Line, Vector).
--  Acknowledge unmasks the line once no owner of it is still waiting. A
--  source still (or again) asserted then interrupts again at once: nothing
--  that happened between steps 1 and 3 is lost. Acknowledging without
--  clearing therefore re-delivers immediately (one interrupt per
--  acknowledgement, never a storm); not acknowledging leaves the line
--  masked, for every owner of a shared line.
--
--  Edge-triggered lines (PS/2, ATA): never masked by delivery;
--  Acknowledge is accepted and changes nothing.
--
--  Acknowledge is ENABLE_IRQ's owner form (Interrupt_Line_ABI.
--  Route_Acknowledge): "enable my line again". One system call per
--  level interrupt, and no system call exists only for it.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interrupt_Line_ABI;
with CuBit.Process_IDs;

package CuBit.Handles.Lines is

   package LA renames Interrupt_Line_ABI;
   subtype Process_ID is CuBit.Process_IDs.Process_ID;

   --  The launcher: subscribe Owner to line Vector, delivered to CPU with
   --  Trigger and Polarity. Lines holds the line (Grant); Authority is
   --  process authority (Grant) over Owner.
   function Route
     (Vector    : LA.Line_Vector;
      Owner     : Process_ID;
      CPU       : LA.Route_CPU;
      Trigger   : LA.Trigger_Mode;
      Polarity  : LA.Pin_Polarity;
      Authority : Process;
      Lines     : Interrupt) return LA.Route_Outcome;

   type Acknowledgement (Accepted : Boolean := False) is record
      case Accepted is
         when True =>
            --  Every interrupt the kernel took on the line since boot.
            Delivered : LA.Delivery_Count;
         when False =>
            --  No_Line_Authority: Line is not this process's read handle
            --  naming exactly one line; Not_Line_Owner: it is not routed
            --  that line; Malformed_Acknowledge: Vector is not Line's line.
            Why : LA.Route_Outcome := LA.No_Line_Authority;
      end case;
   end record;

   --  The owner: its source is cleared (see above). Vector is the line
   --  Line names (the owner was told it with the handle).
   function Acknowledge (Line : Interrupt; Vector : LA.Line_Vector) return Acknowledgement;

end CuBit.Handles.Lines;

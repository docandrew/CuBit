-------------------------------------------------------------------------------
--  CuBitOS
--  Copyright (C) 2026 Jon Andrew
--
--  @summary
--  Legacy interrupt lines (Interrupt_Line_ABI): who owns each line, its
--  trigger mode and polarity, and the level-triggered mask-until-
--  acknowledged protocol, applied to the I/O APIC.
--
--  @description
--  Replaces the owner-only table Capabilities.IRQ. Every state change is an
--  Interrupt_Line_Policy transition (proved) followed by its
--  Hardware_Action, both under one spinlock that also serializes the I/O
--  APIC's index/data register pair, so an interrupt on one CPU and an
--  acknowledgement on another cannot interleave.
--
--  The lines are the inputs 1 .. 15 of the first I/O APIC the MADT names,
--  provided it serves GSIs from 0 (Set_IO_APIC); a second I/O APIC's
--  inputs are not lines here. Without an I/O APIC the lines are the 8259
--  PIC's inputs, edge-triggered only (it cannot hold a level line masked
--  per input across an acknowledgement in this protocol).
-------------------------------------------------------------------------------
with Interfaces; use Interfaces;
with System;

with Interrupt_Line_ABI;
with Interrupt_Line_Policy;

package Interrupt_Lines with
    SPARK_Mode => On
is
    package ABI renames Interrupt_Line_ABI;
    subtype Line_Vector is ABI.Line_Vector;

    --  A process identity word (Process.ProcessID); 0 is no owner.
    subtype Owner_Word is Unsigned_64;

    type Recipient_List is array (Interrupt_Line_Policy.Owner_Slot) of Owner_Word;

    ---------------------------------------------------------------------------
    --  Set_IO_APIC: once at boot, the mapped register window of the driven
    --  I/O APIC and the first GSI it serves.
    ---------------------------------------------------------------------------
    procedure Set_IO_APIC (Base : System.Address; GSI_Base : Unsigned_32);

    ---------------------------------------------------------------------------
    --  Route: subscribe Owner to the line (idempotent) and program the
    --  input to CPU with Trigger and Polarity: masked while a previous
    --  delivery still waits for an owner's acknowledgement.
    ---------------------------------------------------------------------------
    procedure Route
      (Vector   : Line_Vector;
       Owner    : Owner_Word;
       CPU      : ABI.Route_CPU;
       Trigger  : ABI.Trigger_Mode;
       Polarity : ABI.Pin_Polarity;
       Outcome  : out ABI.Route_Outcome)
    with Pre => Owner /= Interrupt_Line_Policy.No_Owner;

    ---------------------------------------------------------------------------
    --  Deliver: the interrupt handler took Vector. A level line is masked
    --  here, before the caller's EOI; Recipients are the owners to notify
    --  (zero slots are none).
    ---------------------------------------------------------------------------
    procedure Deliver (Vector : Line_Vector; Recipients : out Recipient_List);

    ---------------------------------------------------------------------------
    --  Acknowledge: Owner has cleared its source. Unmasks a held level line
    --  when no owner is still waiting. Accepted False when Owner does not
    --  own the line. Delivered: the line's delivery count.
    ---------------------------------------------------------------------------
    procedure Acknowledge
      (Vector    : Line_Vector;
       Owner     : Owner_Word;
       Accepted  : out Boolean;
       Delivered : out ABI.Delivery_Count)
    with Pre => Owner /= Interrupt_Line_Policy.No_Owner;

    ---------------------------------------------------------------------------
    --  Release_Process: Owner exited; it leaves every line it owned.
    ---------------------------------------------------------------------------
    procedure Release_Process (Owner : Owner_Word)
    with Pre => Owner /= Interrupt_Line_Policy.No_Owner;

end Interrupt_Lines;

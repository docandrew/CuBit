------------------------------------------------------------------------------
--  CuBit
--  Copyright (C) 2026 Jon Andrew
--  SPDX-License-Identifier: GPL-3.0-or-later
--
--  @summary
--  The guest's 16550A UART (COM1) as the monitor models it (VMM-001,
--  docs/hypervisor.md section 6): its registers, a receive FIFO that the
--  window's keys fill, and the interrupt it raises on its ISA IRQ.
--
--  @description
--  Sources: the TI/National PC16550D datasheet (register map, IIR
--  priorities, FCR trigger levels) and what Linux's 8250 driver
--  (drivers/tty/serial/8250/8250_port.c) expects of a 16550A: FIFOs
--  reported in IIR bits 6-7 once FCR enables them, LSR's data-ready bit
--  while the receive FIFO holds a byte, the receive interrupt above THRE,
--  and a pending interrupt reaching the IRQ line only while MCR's OUT2 is
--  set (the PC wiring).
--
--  Transmission is instant: THR is empty again as soon as it is written, so
--  every THR write is a new THRE interrupt. Nothing is ever lost on
--  receive: Receive takes a byte only when the FIFO has room (the monitor
--  stops taking keys meanwhile), so there is no overrun and LSR never
--  reports an error. Received bytes leave the FIFO only when the guest
--  reads RBR, or when the guest resets the FIFO (FCR), and Write then says
--  how many it discarded.
--
--  Approximated: the character timeout (IIR 0Ch) is reported as soon as
--  the FIFO holds fewer bytes than its trigger level, instead of after four
--  idle character times: input arrives in whole key sequences, so the line
--  is idle at once. Loopback (MCR bit 4) mirrors the modem lines but does
--  not loop transmitted bytes back into the receive FIFO.
--
--  The interrupt is an edge on the IOAPIC pin: every operation says when
--  the line rose (or, for a THR write, fell and rose again). Proved (SPARK,
--  level 2): no edge is missed and none is raised while the line is low, a
--  register read never raises one, and the FIFO is first in, first out,
--  with no byte lost, duplicated or reordered.
------------------------------------------------------------------------------
pragma Ada_2022;
with Interfaces; use Interfaces;

package Guest_UART with SPARK_Mode, Pure is

   --  Registers by offset from the base port (PC16550D Table 2).
   type Register is range 0 .. 7;
   Data_Register      : constant Register := 0;  --  RBR, THR; DLL while DLAB
   Interrupt_Enable   : constant Register := 1;  --  IER; DLM while DLAB
   Interrupt_Identity : constant Register := 2;  --  IIR read, FCR write
   Line_Control       : constant Register := 3;
   Modem_Control      : constant Register := 4;
   Line_Status        : constant Register := 5;
   Modem_Status       : constant Register := 6;
   Scratch            : constant Register := 7;

   --  IER.
   IER_Received_Data : constant Unsigned_8 := 16#01#;   --  ERBFI
   IER_THR_Empty     : constant Unsigned_8 := 16#02#;   --  ETBEI
   IER_Writable      : constant Unsigned_8 := 16#0F#;   --  ERBFI, ETBEI, ELSI, EDSSI
   --  IIR: the interrupt identity in bits 0-3, FIFOs enabled in 6-7.
   IIR_No_Interrupt      : constant Unsigned_8 := 16#01#;
   IIR_THR_Empty         : constant Unsigned_8 := 16#02#;
   IIR_Received_Data     : constant Unsigned_8 := 16#04#;
   IIR_Character_Timeout : constant Unsigned_8 := 16#0C#;
   IIR_Identity_Mask     : constant Unsigned_8 := 16#0F#;
   IIR_FIFOs_Enabled     : constant Unsigned_8 := 16#C0#;
   --  FCR.
   FCR_Enable        : constant Unsigned_8 := 16#01#;
   FCR_Clear_Receive : constant Unsigned_8 := 16#02#;
   FCR_Clear_Transmit : constant Unsigned_8 := 16#04#;
   FCR_Trigger_Shift : constant := 6;               --  bits 6-7: 1, 4, 8, 14 bytes
   --  LCR.
   LCR_DLAB : constant Unsigned_8 := 16#80#;
   --  MCR.
   MCR_DTR      : constant Unsigned_8 := 16#01#;
   MCR_RTS      : constant Unsigned_8 := 16#02#;
   MCR_OUT1     : constant Unsigned_8 := 16#04#;
   MCR_OUT2     : constant Unsigned_8 := 16#08#;    --  gates the IRQ line on a PC
   MCR_Loopback : constant Unsigned_8 := 16#10#;
   MCR_Writable : constant Unsigned_8 := 16#1F#;
   --  LSR.
   LSR_Data_Ready        : constant Unsigned_8 := 16#01#;
   LSR_THR_Empty         : constant Unsigned_8 := 16#20#;
   LSR_Transmitter_Empty : constant Unsigned_8 := 16#40#;
   --  MSR.
   MSR_CTS : constant Unsigned_8 := 16#10#;
   MSR_DSR : constant Unsigned_8 := 16#20#;
   MSR_RI  : constant Unsigned_8 := 16#40#;
   MSR_DCD : constant Unsigned_8 := 16#80#;

   --  The 16550A's receive FIFO; with FIFOs disabled only RBR holds a byte.
   FIFO_Depth : constant := 16;
   subtype Byte_Count is Natural range 0 .. FIFO_Depth;
   subtype Slot is Natural range 0 .. FIFO_Depth - 1;
   type Slots is array (Slot) of Unsigned_8;

   type UART is record
      IER, LCR, MCR, SCR, DLL, DLM, FCR : Unsigned_8 := 0;
      THR_Pending : Boolean := False;   --  a THRE interrupt not yet taken
      Store : Slots := [others => 0];
      Head  : Slot := 0;                --  the oldest received byte
      Count : Byte_Count := 0;
   end record;

   function FIFO_Enabled (U : UART) return Boolean is ((U.FCR and FCR_Enable) /= 0);
   function DLAB (U : UART) return Boolean is ((U.LCR and LCR_DLAB) /= 0);

   function Capacity (U : UART) return Byte_Count is (if FIFO_Enabled (U) then FIFO_Depth else 1);
   function Valid (U : UART) return Boolean is (U.Count <= Capacity (U));
   --  Bytes Receive may take now.
   function Room (U : UART) return Byte_Count is (Capacity (U) - U.Count)
     with Pre => Valid (U);

   --  The receive interrupt's level: FCR bits 6-7 (1, 4, 8 or 14 bytes);
   --  one byte with FIFOs disabled.
   function Trigger_Level (U : UART) return Byte_Count is
     (if not FIFO_Enabled (U) then 1
      else (case Shift_Right (U.FCR, FCR_Trigger_Shift) is
               when 0 => 1, when 1 => 4, when 2 => 8, when others => 14));

   --  The I-th oldest byte in the receive FIFO.
   function Received (U : UART; I : Slot) return Unsigned_8 is
     (U.Store ((U.Head + I) mod FIFO_Depth));

   --  IIR's identity, by the datasheet's priority: received data at the
   --  trigger level, then the character timeout below it (FIFO mode), then
   --  THRE. No line-status or modem-status interrupt ever occurs.
   function Identity (U : UART) return Unsigned_8 is
     (if (U.IER and IER_Received_Data) /= 0 and then U.Count > 0 and then U.Count >= Trigger_Level (U)
      then IIR_Received_Data
      elsif (U.IER and IER_Received_Data) /= 0 and then U.Count > 0 then IIR_Character_Timeout
      elsif (U.IER and IER_THR_Empty) /= 0 and then U.THR_Pending then IIR_THR_Empty
      else IIR_No_Interrupt);

   --  The IRQ line: an interrupt pending and OUT2 set.
   function Line (U : UART) return Boolean is
     (Identity (U) /= IIR_No_Interrupt and then (U.MCR and MCR_OUT2) /= 0);

   --  A guest read of register R. Never raises the line.
   procedure Read (U : in out UART; R : Register; Value : out Unsigned_8)
     with Pre  => Valid (U),
          Post => Valid (U)
            and then (if Line (U) then Line (U'Old))
            and then U.IER = U'Old.IER and then U.LCR = U'Old.LCR and then U.MCR = U'Old.MCR
            and then U.FCR = U'Old.FCR
            --  RBR pops the oldest byte; the rest keep their order.
            and then (if R = Data_Register and then not DLAB (U'Old) and then U'Old.Count > 0 then
                        Value = Received (U'Old, 0) and then U.Count = U'Old.Count - 1
                        and then (for all I in 0 .. U.Count - 1 =>
                                    Received (U, I) = Received (U'Old, I + 1))
                      else
                        U.Count = U'Old.Count
                        and then (for all I in 0 .. U.Count - 1 => Received (U, I) = Received (U'Old, I)))
            and then (if R = Line_Status then ((Value and LSR_Data_Ready) /= 0) = (U.Count > 0))
            and then (if R = Interrupt_Identity then (Value and IIR_Identity_Mask) = Identity (U'Old));

   type Write_Effect is record
      Transmitted : Boolean := False;   --  the guest sent Byte (a THR write)
      Byte        : Unsigned_8 := 0;
      Raise_IRQ   : Boolean := False;   --  an edge on the IRQ line
      Discarded   : Byte_Count := 0;    --  received bytes a FIFO reset discarded
   end record;

   --  A guest write of Value to register R.
   procedure Write (U : in out UART; R : Register; Value : Unsigned_8; Effect : out Write_Effect)
     with Pre  => Valid (U),
          Post => Valid (U)
            and then (if Effect.Raise_IRQ then Line (U))
            and then (if Line (U) and then not Line (U'Old) then Effect.Raise_IRQ)
            and then Effect.Discarded = U'Old.Count - U.Count
            --  Only a FIFO reset the guest asked for (FCR: clear, or FIFOs
            --  switched on or off) discards; every other write keeps them.
            and then (if R /= Interrupt_Identity
                        or else ((Value and FCR_Clear_Receive) = 0
                                 and then ((Value and FCR_Enable) /= 0) = FIFO_Enabled (U'Old))
                      then U.Count = U'Old.Count
                        and then (for all I in 0 .. U.Count - 1 => Received (U, I) = Received (U'Old, I)))
            and then (if Effect.Transmitted then R = Data_Register and then Effect.Byte = Value);

   --  The host's byte B into the receive FIFO (a key the window took).
   procedure Receive (U : in out UART; B : Unsigned_8; Raise_IRQ : out Boolean)
     with Pre  => Valid (U) and then Room (U) > 0,
          Post => Valid (U)
            and then U.Count = U'Old.Count + 1
            and then Received (U, U'Old.Count) = B
            and then (for all I in 0 .. U'Old.Count - 1 => Received (U, I) = Received (U'Old, I))
            and then U.IER = U'Old.IER and then U.LCR = U'Old.LCR and then U.MCR = U'Old.MCR
            and then U.FCR = U'Old.FCR
            and then (if Raise_IRQ then Line (U))
            and then (if Line (U) and then not Line (U'Old) then Raise_IRQ);
end Guest_UART;
